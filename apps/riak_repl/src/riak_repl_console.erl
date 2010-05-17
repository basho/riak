%% Riak EnterpriseDS
%% Copyright 2007-2009 Basho Technologies, Inc. All Rights Reserved.
-module(riak_repl_console).
-author('Andy Gross <andy@basho.com>').
-include("riak_repl.hrl").
-export([add_listener/1, del_listener/1]).

add_listener([NodeName, IP, Port]) ->
    Ring = get_ring(),
    Listener = make_listener(NodeName, IP, Port),
    NewRing = riak_repl_ring:add_listener(Ring, Listener),
    ok = maybe_set_ring(Ring, NewRing).

del_listener([NodeName, IP, Port]) ->
    Ring = get_ring(),
    Listener = make_listener(NodeName, IP, Port),
    NewRing = riak_repl_ring:del_listener(Ring, Listener),
    ok = maybe_set_ring(Ring, NewRing).

%% helper functions

make_listener(NodeName, IP, Port) ->
    #repl_listener{nodename=list_to_atom(NodeName),
                   listen_addr={IP, list_to_integer(Port)}}.

maybe_set_ring(_R, _R) -> ok;
maybe_set_ring(_R1, R2) ->
    riak_core_ring_manager:set_my_ring(R2),
    riak_core_ring_manager:write_ringfile(),
    ok.

get_ring() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    riak_repl_ring:ensure_config(Ring).
