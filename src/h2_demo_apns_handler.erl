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
%%% Created : 2022-02-17T03:26:44+00:00
%%%-------------------------------------------------------------------
-module(h2_demo_apns_handler).

-author("yangcancai").

-export([init/2, terminate/3]).
-include_lib("cool_tools/include/cool_tools_logger.hrl").
init(Req0, State) ->
  DeviceID = cowboy_req:binding(device_id, Req0),
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  Headers = cowboy_req:headers(Req1),
  ?LOG_DEBUG("DeviceID = ~p, Headers = ~p, Body = ~p",[DeviceID, Headers, Body]),
  N = rand:uniform(10),
  case N of
    _ when N < 3->
      timer:sleep(1000);
    _->
      ignore
  end,
  Req2 = cowboy_req:reply(200, maps:merge(Headers,#{<<"apns-id">> => apns_id(Headers)}), Req1),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
apns_id(#{<<"apns_id">> := ApnsID}) ->
  ApnsID;
apns_id(_) ->
  cool_tools:to_binary(string:to_upper(cool_tools:uuid_v1_string(standard))).