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
-export([start_link/4]).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("riakserver_pb.hrl").
-define(ACK_COUNT, 1000).

start_link(TargetNode, Module, Partition, BKeyList) ->
    Self = self(),
    Pid = spawn_link(fun()->start_fold(TargetNode, Module,Partition, BKeyList, Self) end),
    {ok, Pid}.

start_fold(TargetNode, Module, Partition, BKeyList, ParentPid) ->
    error_logger:info_msg("Starting handoff of partition ~p to ~p~n", 
                          [Partition, TargetNode]),
    [_Name,Host] = string:tokens(atom_to_list(TargetNode), "@"),
    {ok, Port} = get_handoff_port(TargetNode),
    {ok, Socket} = gen_tcp:connect(Host, Port, 
                                   [binary, 
                                    {packet, 4}, 
                                    {header,1}, 
                                    {active, once}], 15000),
    VMaster = list_to_atom(atom_to_list(Module) ++ "_master"),
    ModBin = atom_to_binary(Module, utf8),
    M = <<0:8,Partition:160/integer,ModBin/binary>>,
    ok = gen_tcp:send(Socket, M),
    case BKeyList of
        all ->
            riak_core_vnode_master:sync_command({Partition, node()},
                                                ?FOLD_REQ{
                                                   foldfun=fun folder/3,
                                                   acc0={Socket,ParentPid,[]}},
                                                VMaster);
        _ ->
            inner_fold({Socket,ParentPid,[]},BKeyList)
    end,
    error_logger:info_msg("Handoff of partition ~p to ~p completed~n", 
                          [Partition, TargetNode]),
    gen_fsm:send_event(ParentPid, handoff_complete).

inner_fold(_FoldArg,[]) -> ok;
inner_fold(FoldArg,[{B,K}|Tail]) ->
    {_Socket,ParentPid,_Count} = FoldArg,
    case gen_fsm:sync_send_event(ParentPid, {get_binary, {B,K}}, infinity) of
        {ok, V} ->
            inner_fold(folder({B,K},V,FoldArg),Tail);
        _ ->
            inner_fold(FoldArg,Tail)
    end.
            
folder({B,K}, V, {Socket, ParentPid, []}) ->
    gen_tcp:controlling_process(Socket, self()),
    visit_item({B,K}, V, {Socket, ParentPid, 0});
folder({B,K}, V, AccIn) ->
    visit_item({B,K}, V, AccIn).

visit_item({B,K}, V, {Socket, ParentPid, ?ACK_COUNT}) ->
    M = <<2:8,"sync">>,
    ok = gen_tcp:send(Socket, M),
    inet:setopts(Socket, [{active, false}]),
    {ok,[2|<<"sync">>]} = gen_tcp:recv(Socket, 0),
    inet:setopts(Socket, [{active, once}]),
    visit_item({B,K}, V, {Socket, ParentPid, 0});
visit_item({B,K}, V, {Socket, ParentPid, Acc}) ->
    D = zlib:zip(riakserver_pb:encode_riakobject_pb(
                   #riakobject_pb{bucket=B, key=K, val=V})),
    M = <<1:8,D/binary>>,
    ok = gen_tcp:send(Socket, M),
    {Socket, ParentPid, Acc+1}.
    

get_handoff_port(Node) when is_atom(Node) ->
    gen_server2:call({riak_core_handoff_listener, Node}, handoff_port).







