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
%%% Created : 2022-02-16T07:48:47+00:00
%%%-------------------------------------------------------------------
-module(h2_demo_client).

-author("yangcancai").

-behaviour(gen_server).

%% API
-export([start_link/0, post/3, gun/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(h2_demo_client_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
post(Path, Header, Data) ->
    ?MODULE ! {post, Path, Header, Data}.
gun(Fun) when is_function(Fun) ->
    ?MODULE ! {gun, Fun}.
%% @doc Spawns the server and registers the local name (unique)
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
    {ok, #{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: #h2_demo_client_state{}) ->
                     {reply, Reply :: term(), NewState :: #h2_demo_client_state{}} |
                     {reply,
                      Reply :: term(),
                      NewState :: #h2_demo_client_state{},
                      timeout() | hibernate} |
                     {noreply, NewState :: #h2_demo_client_state{}} |
                     {noreply, NewState :: #h2_demo_client_state{}, timeout() | hibernate} |
                     {stop,
                      Reason :: term(),
                      Reply :: term(),
                      NewState :: #h2_demo_client_state{}} |
                     {stop, Reason :: term(), NewState :: #h2_demo_client_state{}}.
handle_call(_Request, _From, State = #h2_demo_client_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #h2_demo_client_state{}) ->
                     {noreply, NewState :: #h2_demo_client_state{}} |
                     {noreply, NewState :: #h2_demo_client_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #h2_demo_client_state{}}.
handle_cast(_Request, State = #h2_demo_client_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #h2_demo_client_state{}) ->
                     {noreply, NewState :: #h2_demo_client_state{}} |
                     {noreply, NewState :: #h2_demo_client_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #h2_demo_client_state{}}.
handle_info({gun, Fun}, State) when is_function(Fun) ->
  {Req, Ref} = Fun(),
  {noreply, State#{Ref => Req}};
handle_info({post, Path, Headers, Data}, State) ->
    Ref = h2_demo:request(Path, Headers, Data),
    {noreply, State#{Ref => {Path, Headers, Data}}};
handle_info({gun_up, _ConnPid, _Http2}, State) ->
    {noreply, State};
handle_info({gun_push, ConnPid, StreamRef, PushedStreamRef, Method, Host, Headers},
            State) ->
    io:format("gun_push ~p~n",
              [{gun_push, ConnPid, StreamRef, PushedStreamRef, Method, Host, Headers}]),
    {noreply, State#{PushedStreamRef => <<>>}};
handle_info({gun_response, _ConnPid, StreamRef, fin, Status, Headers}, State) ->
    no_data,
    io:format("gun_response no_data: req=~p, resp=~p~n",
              [maps:get(StreamRef, State), {Status, Headers}]),
    {noreply, maps:remove(StreamRef, State)};
handle_info({gun_response, _ConnPid, StreamRef, nofin, Status, Headers}, State) ->
    {noreply, State#{{StreamRef, headers} => {Status, Headers}}};
handle_info({gun_data, _ConnPid, StreamRef, nofin, Data}, State) ->
    Old = maps:get({StreamRef, gun_data}, State, <<>>),
    {noreply, State#{{StreamRef, gun_data} => <<Old/binary, Data/binary>>}};
handle_info({gun_data, _ConnPid, StreamRef, fin, Data}, State) ->
    Old = maps:get({StreamRef, gun_data}, State, <<>>),
    Headers = maps:get({StreamRef, headers}, State),
    AllData = <<Old/binary, Data/binary>>,
    io:format("gun_response data: req=~p, resp=~p~n",
              [maps:get(StreamRef, State), {Headers, AllData}]),
    S1 = maps:remove({StreamRef, gun_data}, State),
    {noreply, maps:remove(StreamRef, S1)};
handle_info({'DOWN', _MRef, process, _ConnPid, _Reason}, State) ->
    error_logger:error_msg("Oops!"),
    {stop, normal, State};
handle_info(Info, State = #h2_demo_client_state{}) ->
    error_logger:error_msg("unknown info: ~", Info),
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #h2_demo_client_state{}) ->
                   term().
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.

terminate(_Reason, _State = #h2_demo_client_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #h2_demo_client_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #h2_demo_client_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #h2_demo_client_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
