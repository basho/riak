%% -------------------------------------------------------------------
%%
%% riak_handoff_receiver: incoming data handler for TCP-based handoff
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

%% @doc incoming data handler for TCP-based handoff

-module(riak_handoff_receiver).
-include("riakserver_pb.hrl").
-behaviour(gen_server2).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, partition, vnode}).

start_link(Socket) ->
    gen_server2:start_link(?MODULE, [Socket], []).

init([Socket]) -> 
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {ok, #state{sock=Socket}}.

handle_info({tcp_closed, Socket}, State=#state{sock=Socket}) ->
    {stop, normal, State};
handle_info({tcp, _Sock, Data}, State=#state{sock=Socket}) ->
    [MsgType|MsgData] = Data,
    NewState = process_message(MsgType,MsgData,State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState}.

process_message(0, MsgData, State) ->
    <<Partition:160/integer>> = MsgData,
    {ok, VNode} = gen_server2:call(riak_vnode_master, {get_vnode, Partition}, 60000),  
    State#state{partition=Partition, vnode=VNode};
process_message(1, MsgData, State=#state{vnode=VNode}) ->
    % header of 1 is a riakobject_pb
    RO_PB = riakserver_pb:decode_riakobject_pb(zlib:unzip(MsgData)),
    BKey = {RO_PB#riakobject_pb.bucket,RO_PB#riakobject_pb.key},
    Msg = {diffobj, {BKey, RO_PB#riakobject_pb.val}},
    ok = gen_fsm:sync_send_all_state_event(VNode, Msg, 60000),
    State;
process_message(2, _MsgData, State=#state{sock=Socket}) ->
    ok = gen_tcp:send(Socket, <<2:8,"sync">>),
    State.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

