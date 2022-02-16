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
                                 {"/:id/", h2_demo_handler, []},
                                 {"/static/[...]",
                                  cowboy_static,
                                  {dir, "priv/", [{mimetypes, cow_mimetypes, all}]}}]}]),
    {ok, _} =
        cowboy:start_clear(h2_demo_server, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    run(),
    ok.

run() ->
    {ok, Pid} = gun:open("127.0.0.1", 8080, #{protocols => [http2]}),
    persistent_term:put(h2_demo, {Pid, erlang:monitor(process, Pid)}).

run(ID) ->
    request("/" ++ erlang:integer_to_list(ID), [], <<>>).

request(Path, Headers, Body) ->
    {Pid, _MRef} = persistent_term:get(h2_demo),
    gun:post(Pid, Path, Headers, Body, #{reply_to => self()}).