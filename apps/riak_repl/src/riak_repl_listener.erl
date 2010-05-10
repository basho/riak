%% -------------------------------------------------------------------
%%
%% riak_handoff_listener: entry point for TCP-based handoff
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc entry point for TCP-based handoff

-module(riak_repl_listener).
-behavior(gen_nb_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-record(state, {ipaddr, portnum}).

start_link(IPAddr, PortNum) ->
    gen_nb_server:start_link(?MODULE, IPAddr, PortNum, [IPAddr, PortNum]).

init([IPAddr, PortNum]) -> 
    {ok, #state{ipaddr=IPAddr, portnum=PortNum}}.

sock_opts() -> [binary, {packet, 4}, {reuseaddr, true}, {backlog, 64}].

handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

new_connection(Socket, State) ->
    io:format("~p: ~p~n", [?MODULE, Socket]),
    gen_tcp:controlling_process(Socket, none),
    {ok, State}.

