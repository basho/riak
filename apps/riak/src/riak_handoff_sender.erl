%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.    

-module(riak_handoff_sender).
-export([start_link/3]).
-include("riakserver_pb.hrl").

start_link(TargetNode, Partition, BKeyList) ->
    case global:set_lock({handoff_token, {node(), Partition}}, [node()], 0) of
        true ->
            Self = self(),
            {ok, spawn_link(fun()->start_fold(TargetNode, Partition, BKeyList, Self) end)};
        false ->
            {error, locked}
    end.
            

start_fold(TargetNode, Partition, BKeyList, ParentPid) ->
    [_Name,Host] = string:tokens(atom_to_list(TargetNode), "@"),
    {ok, Port} = get_handoff_port(TargetNode),
    {ok, Socket} = gen_tcp:connect(Host, Port, 
                                   [binary, 
                                    {packet, 4}, 
                                    {header,1}, 
                                    {active, once}], 15000),
    M = <<0:8,Partition:160/integer>>,
    ok = gen_tcp:send(Socket, M),
    case BKeyList of
        all ->
            gen_server2:call(riak_vnode_master, 
                     {fold, {Partition, fun folder/3, {Socket, ParentPid, []}}},
                     infinity);
        _ ->
            inner_fold({Socket,ParentPid,[]},BKeyList)
    end,
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

visit_item({B,K}, V, {Socket, ParentPid, 100}) ->
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
    gen_server2:call({riak_handoff_listener, Node}, handoff_port).







