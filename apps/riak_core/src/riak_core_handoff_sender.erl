%% -------------------------------------------------------------------
%%
%% riak_handoff_sender: send a partition's data via TCP-based handoff
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

%% @doc send a partition's data via TCP-based handoff

-module(riak_core_handoff_sender).
-export([start_link/3]).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("riak_core/include/riak_core_handoff.hrl").
-define(ACK_COUNT, 1000).

start_link(TargetNode, Module, Partition) ->
    Self = self(),
    Pid = spawn_link(fun()->start_fold(TargetNode, Module,Partition, Self) end),
    {ok, Pid}.

start_fold(TargetNode, Module, Partition, ParentPid) ->
     try
         error_logger:info_msg("Starting handoff of partition ~p ~p to ~p~n", 
                               [Module, Partition, TargetNode]),
         [_Name,Host] = string:tokens(atom_to_list(TargetNode), "@"),
         {ok, Port} = get_handoff_port(TargetNode),
         {ok, Socket} = gen_tcp:connect(Host, Port, 
                                        [binary, 
                                         {packet, 4}, 
                                         {header,1}, 
                                         {active, false}], 15000),

         %% Piggyback the sync command from previous releases to send
         %% the vnode type across.  If talking to older nodes they'll
         %% just do a sync, newer nodes will decode the module name.
         %% After 0.12.0 the calls can be switched to use PT_MSG_SYNC
         %% and PT_MSG_CONFIGURE
         VMaster = list_to_atom(atom_to_list(Module) ++ "_master"),
         ModBin = atom_to_binary(Module, utf8),
         Msg = <<?PT_MSG_OLDSYNC:8,ModBin/binary>>,
         ok = gen_tcp:send(Socket, Msg),
         {ok,[?PT_MSG_OLDSYNC|<<"sync">>]} = gen_tcp:recv(Socket, 0),
         M = <<?PT_MSG_INIT:8,Partition:160/integer>>,
         ok = gen_tcp:send(Socket, M),
         {Socket,ParentPid,Module,_Ack,SentCount} = 
             riak_core_vnode_master:sync_command({Partition, node()},
                                                 ?FOLD_REQ{
                                                    foldfun=fun visit_item/3,
                                                    acc0={Socket,ParentPid,Module,0,0}},
                                                 VMaster, infinity),
         error_logger:info_msg("Handoff of partition ~p ~p to ~p completed: sent ~p objects~n", 
                               [Module, Partition, TargetNode, SentCount]),
         gen_fsm:send_event(ParentPid, handoff_complete)
         %% Socket will be closed when this process exits
     catch
         Err:Reason ->
             error_logger:error_msg("Handoff sender ~p ~p failed ~p:~p\n", 
                                    [Module, Partition, Err,Reason])
     end.

visit_item(K, V, {Socket, ParentPid, Module, ?ACK_COUNT, Total}) ->
    M = <<?PT_MSG_OLDSYNC:8,"sync">>,
    ok = gen_tcp:send(Socket, M),
    {ok,[?PT_MSG_OLDSYNC|<<"sync">>]} = gen_tcp:recv(Socket, 0),
    visit_item(K, V, {Socket, ParentPid, Module, 0, Total});
visit_item(K, V, {Socket, ParentPid, Module, Ack, Total}) ->
    BinObj = Module:encode_handoff_item(K, V),
    M = <<?PT_MSG_OBJ:8,BinObj/binary>>,
    ok = gen_tcp:send(Socket, M),
    {Socket, ParentPid, Module, Ack+1, Total+1}.
    

get_handoff_port(Node) when is_atom(Node) ->
    case catch(gen_server2:call({riak_core_handoff_listener, Node}, handoff_port)) of
        {'EXIT', _}  ->
            %% Check old location from previous release
            gen_server2:call({riak_kv_handoff_listener, Node}, handoff_port);
        Other -> Other
    end.








