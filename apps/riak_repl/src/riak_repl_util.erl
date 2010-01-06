-module(riak_repl_util).
-include("riak_repl.hrl").
-export([make_peer_info/0,
         vnode_master_call/2,
         validate_peer_info/2,
         get_partitions/1,
         wait_for_riak/1]).

make_peer_info() ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    {ok, RiakVSN} = application:get_key(riak, vsn),
    {ok, ReplVSN} = application:get_key(riak, vsn),
    #peer_info{riak_version=RiakVSN,
               repl_version=ReplVSN,
               ring=Ring}.
    

validate_peer_info(T=#peer_info{}, M=#peer_info{}) ->
    TheirPartitions = get_partitions(T),
    OurPartitions = get_partitions(M),
    TheirPartitions =:= OurPartitions.

get_partitions(#peer_info{ring=Ring}) ->
    lists:sort([P || {P, _} <- riak_ring:all_owners(Ring)]).

vnode_master_call(Node, Message) ->
    gen_server:call({riak_vnode_master, Node}, Message, ?REPL_MERK_TIMEOUT).

wait_for_riak(PPid) ->
    case [A || {A,_,_} <- application:which_applications(), A =:= riak] of
        [] -> 
            timer:sleep(1000),
            wait_for_riak(PPid);
        _ -> PPid ! riak_up
    end.
