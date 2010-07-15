%% -------------------------------------------------------------------
%%
%% riak_core: Core Active Preference Lists
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
%% Get active preference list - preference list with secondary nodes
%% substituted.
%% -------------------------------------------------------------------
-module(riak_core_apl).
-export([active_owners/1, active_owners/2,
         get_apl/3, get_apl/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type index() :: non_neg_integer().
-type n_val() :: non_neg_integer().
-type ring() :: term().
-type preflist() :: [{index(), node()}].

%% Return preflist of all active primary nodes (with no
%% substituion of fallbacks).  Used to simulate a
%% preflist with N=ring_size
-spec active_owners(atom()) -> preflist().
active_owners(Service) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    active_owners(Ring, riak_core_node_watcher:nodes(Service)).

-spec active_owners(ring(), [node()]) -> preflist().
active_owners(Ring, UpNodes) ->
    UpNodes1 = ordsets:from_list(UpNodes),
    Primaries = riak_core_ring:all_owners(Ring),
    {Up, _Pangs} = check_up(Primaries, UpNodes1, [], []),
    lists:reverse(Up).

%% Get the active preflist taking account of which nodes are up
-spec get_apl(binary(), n_val(), atom()) -> preflist().
get_apl(DocIdx, N, Service) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    get_apl(DocIdx, N, Ring, riak_core_node_watcher:nodes(Service)).

%% Get the active preflist taking account of which nodes are up
%% for a given ring/upnodes list
-spec get_apl(binary(), n_val(), ring(), [node()]) -> preflist().
get_apl(DocIdx, N, Ring, UpNodes) ->
    UpNodes1 = ordsets:from_list(UpNodes),
    Preflist = riak_core_ring:preflist(DocIdx, Ring),
    {Primaries, Fallbacks} = lists:split(N, Preflist),
    {Up, Pangs} = check_up(Primaries, UpNodes1, [], []),
    lists:reverse(Up) ++ find_fallbacks(Pangs, Fallbacks, UpNodes1, []).

%% Split a preference list into up and down lists
-spec check_up(preflist(), [node()], preflist(), preflist()) -> {preflist(), preflist()}.
check_up([], _UpNodes, Up, Pangs) ->
    {Up, Pangs};
check_up([{Partition,Node}|Rest], UpNodes, Up, Pangs) ->
    case is_up(Node, UpNodes) of
        true ->
            check_up(Rest, UpNodes, [{Partition, Node} | Up], Pangs);
        false ->
            check_up(Rest, UpNodes, Up, [{Partition, Node} | Pangs])
    end.

%% Find fallbacks for downed nodes in the preference list
-spec find_fallbacks(preflist(), preflist(), [node()], preflist()) -> preflist().
find_fallbacks(_Pangs, [], _UpNodes, Secondaries) ->
    Secondaries;
find_fallbacks([], _Fallbacks, _UpNodes, Secondaries) ->
    Secondaries;
find_fallbacks([{Partition, _Node}|Rest]=Pangs, [{_,FN}|Fallbacks], UpNodes, Secondaries) ->
    case is_up(FN, UpNodes) of
        true ->
            find_fallbacks(Rest, Fallbacks, UpNodes, [{Partition, FN} | Secondaries]);
        false ->
            find_fallbacks(Pangs, Fallbacks, UpNodes, Secondaries)
    end.
        
%% Return true if a node is up
is_up(Node, UpNodes) ->
    ordsets:is_element(Node, UpNodes).

-ifdef(TEST).

smallest_test() ->
    Ring = riak_core_ring:fresh(1,node()),
    ?assertEqual([{0,node()}],  get_apl(last_in_ring(), 1, Ring, [node()])).

four_node_test() ->
    Nodes = [nodea, nodeb, nodec, noded],
    Ring = perfect_ring(8, Nodes),
    ?assertEqual([{0,nodea},
                  {182687704666362864775460604089535377456991567872,nodeb},
                  {365375409332725729550921208179070754913983135744,nodec}], 
                 get_apl(last_in_ring(), 3, Ring, Nodes)),
    %% With a node down
    ?assertEqual([{182687704666362864775460604089535377456991567872,nodeb},
                  {365375409332725729550921208179070754913983135744,nodec},
                  {0,noded}], 
                 get_apl(last_in_ring(), 3, Ring, [nodeb, nodec, noded])),
    %% With two nodes down
    ?assertEqual([{365375409332725729550921208179070754913983135744,nodec},
                  {0,nodec},
                  {182687704666362864775460604089535377456991567872,noded}],
                 get_apl(last_in_ring(), 3, Ring,  [nodec, noded])),
    %% With the other two nodes down
    ?assertEqual([{0,nodea},
                  {182687704666362864775460604089535377456991567872,nodeb},
                  {365375409332725729550921208179070754913983135744,nodea}],
                 get_apl(last_in_ring(), 3, Ring,  [nodea, nodeb])).

    
%% Create a perfect ring - RingSize must be a multiple of nodes
perfect_ring(RingSize, Nodes) when RingSize rem length(Nodes) =:= 0 ->
    Ring = riak_core_ring:fresh(RingSize,node()),
    Owners = riak_core_ring:all_owners(Ring),
    TransferNode =
        fun({Idx,_CurOwner}, {Ring0, [NewOwner|Rest]}) ->
                {riak_core_ring:transfer_node(Idx, NewOwner, Ring0), Rest ++ [NewOwner]}
        end,
    {PerfectRing, _} = lists:foldl(TransferNode, {Ring, Nodes}, Owners),
    PerfectRing.

last_in_ring() ->
    <<1461501637330902918203684832716283019655932542975:160/unsigned>>.
-endif.
