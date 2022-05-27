%%%-------------------------------------------------------------------
%%% @author yangcancai

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

%%% @doc
%%%
%%% @end
%%% Created : 2022-02-14T04:14:22+00:00
%%%-------------------------------------------------------------------

-module(h2_demo).

-author("yangcancai").

-export([start/0, run/0, run/1, request/3]).

start() ->
    {ok, _} = h2_demo_client:start_link(),
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/push/", h2_demo_handler_push, []},
                                 {"/3/device/:device_id",h2_demo_apns_handler,[]},
                                 {"/:id/", h2_demo_handler, []},
                                 {"/static/[...]",
                                  cowboy_static,
                                  {dir, "priv/", [{mimetypes, cow_mimetypes, all}]}}]}]),
    {ok, _} =
        cowboy:start_clear(h2_demo_server, [{port, 8443}], #{env => #{dispatch => Dispatch}}),
   start_tls(Dispatch),
    run(),
    ok.

start_tls(Dispatch) ->
  {ok, _} = cowboy:start_tls(h2_demo_server_tls,
    [
      {port, 443},
      {cacertfile, "priv/cacert.pem"},
      {certfile, "priv/cert.pem"},
      {keyfile, "priv/key.pem"},
      {password,"123456"},
%%        {log_level, debug},
%%      {verify, verify_peer},
      {reuseaddr, true},
      {versions, ['tlsv1.2','tlsv1.3']},
      {session_tickets, stateless}
    ],
    #{env => #{dispatch => Dispatch}}
  ).

run() ->
    {ok, Pid} = gun:open("www.example.com", 443, #{protocols => [http2],
      transport => ssl,
      transport_opts => [
      {certfile, "priv/www.example.com.usr.cert.pem"},
      {keyfile, "priv/www.example.com.usr.key.pem"},
      {password, "123456"},
      {versions, ['tlsv1.2', 'tlsv1.3']},
      {session_tickets, auto}
        ]}),
    persistent_term:put(h2_demo, {Pid, erlang:monitor(process, Pid)}).

run(ID) ->
    request("/" ++ erlang:integer_to_list(ID), [], <<>>).

request(Path, Headers, Body) ->
    {Pid, _MRef} = persistent_term:get(h2_demo),
    gun:post(Pid, Path, Headers, Body, #{reply_to => self()}).