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

-module(riak_core_handoff_receiver).
-include_lib("riak_core/include/riak_core_handoff.hrl").
-behaviour(gen_server2).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock :: port(), 
                partition :: non_neg_integer(), 
                vnode_mod = riak_kv_vnode:: module(),
                vnode :: pid(), 
                count = 0 :: non_neg_integer()}).


start_link(Socket) ->
    gen_server2:start_link(?MODULE, [Socket], []).

init([Socket]) -> 
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {ok, #state{sock=Socket, count=0}}.

handle_info({tcp_closed,Socket},State=#state{sock=Socket,partition=Partition,count=Count}) ->
    error_logger:info_msg("Handoff receiver for partition ~p exiting after processing ~p"
                          " objects~n", [Partition, Count]),
    {stop, normal, State};
handle_info({tcp, _Sock, Data}, State=#state{sock=Socket}) ->
    [MsgType|MsgData] = Data,
    NewState = process_message(MsgType,MsgData,State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState}.

process_message(?PT_MSG_INIT, MsgData, State=#state{vnode_mod=VNodeMod}) ->
    <<Partition:160/integer>> = MsgData,
    error_logger:info_msg("Receiving handoff data for partition ~p:~p~n", [VNodeMod, Partition]),
    {ok, VNode} = riak_core_vnode_master:get_vnode_pid(Partition, VNodeMod),
    State#state{partition=Partition, vnode=VNode};
process_message(?PT_MSG_OBJ, MsgData, State=#state{vnode=VNode, count=Count}) ->
    Msg = {handoff_data, MsgData},
    ok = gen_fsm:sync_send_all_state_event(VNode, Msg, 60000),
    State#state{count=Count+1};
process_message(?PT_MSG_OLDSYNC, MsgData, State=#state{sock=Socket}) ->
    ok = gen_tcp:send(Socket, <<?PT_MSG_OLDSYNC:8,"sync">>),
    <<VNodeModBin/binary>> = MsgData,
    VNodeMod = binary_to_atom(VNodeModBin, utf8),
    State#state{vnode_mod=VNodeMod};
process_message(?PT_MSG_SYNC, _MsgData, State=#state{sock=Socket}) ->
    ok = gen_tcp:send(Socket, <<?PT_MSG_SYNC:8, "sync">>),
    State;
process_message(?PT_MSG_CONFIGURE, MsgData, State) ->
    ConfProps = binary_to_term(MsgData),
    State#state{vnode_mod=proplists:get_value(vnode_mod, ConfProps),
                partition=proplists:get_value(partition, ConfProps)};
process_message(_, _MsgData, State=#state{sock=Socket}) ->
    ok = gen_tcp:send(Socket, <<255:8,"unknown_msg">>),
    State.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

