-module(riak_handoff_sender).
-export([start_link/3]).
-include("riakserver_pb.hrl").

start_link(TargetNode, Partition, BKeyList) ->
    Self = self(),
    spawn_link(fun() -> start_fold(TargetNode, Partition, BKeyList, Self) end).

start_fold(TargetNode, Partition, _BKeyList, ParentPid) ->
    [_Name,Host] = string:tokens(atom_to_list(TargetNode), "@"),
    {ok, Port} = get_handoff_port(TargetNode),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary,{packet, 4}, {header,1}], 15000),
    gen_server2:call(riak_vnode_master, 
                     {fold, {Partition, fun folder/3, {Socket, ParentPid, []}}},
                     infinity).


folder({B,K}, V, {Socket, ParentPid, []}) ->
    gen_tcp:controlling_process(Socket, self()),
    visit_item({B,K}, V, {Socket, ParentPid, []});
folder({B,K}, V, AccIn) ->
    visit_item({B,K}, V, AccIn).

visit_item({B,K}, V, {Socket, ParentPid, _Acc}) ->
    D = riakserver_pb:encode_riakobject_pb(
          #riakobject_pb{bucket=B, key=K, val=V}),
    M = <<1:8,D/binary>>,
    io:format("sending: ~p~n", [size(M)]),
    ok = gen_tcp:send(Socket, M),
    {Socket, ParentPid, none}.
    

get_handoff_port(Node) when is_atom(Node) ->
    gen_server2:call({riak_handoff_listener, Node}, handoff_port).







