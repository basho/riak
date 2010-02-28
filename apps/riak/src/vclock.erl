%% -------------------------------------------------------------------
%%
%% vclock: vector clocks as inspired by Lamport and Mattern
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

%% @reference Leslie Lamport (1978). "Time, clocks, and the ordering of events in a distributed system". Communications of the ACM 21 (7): 558-565. 

%% @reference Friedemann Mattern (1988). "Virtual Time and Global States of Distributed Systems". Workshop on Parallel and Distributed Algorithms: pp. 215-226

%% @doc vector clocks as inspired by Lamport and Mattern.

-module(vclock).

-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([fresh/0,descends/2,merge/1,get_counter/2,get_timestamp/2,
	increment/2,all_nodes/1,equal/2,prune/3]).

-include_lib("eunit/include/eunit.hrl").

% @type vclock() = [vc_entry].
% @type   vc_entry() = {node(), {counter(), timestamp()}}.
% The timestamp is present but not used, in case a client wishes to inspect it.
% @type   node() = term().
% Nodes can have any term() as a name, but they must differ from each other.
% @type   counter() = integer().
% @type   timestamp() = integer().

% @doc Create a brand new vclock.
% @spec fresh() -> vclock()
fresh() ->
    [].

%% @todo Use common_test or other good test framework, and write more tests.
%
% @doc Serves as both a trivial test and some example code.
example_test() ->
    A = vclock:fresh(),
    B = vclock:fresh(),
    A1 = vclock:increment(a, A),
    B1 = vclock:increment(b, B),
    true = vclock:descends(A1,A),
    true = vclock:descends(B1,B),
    false = vclock:descends(A1,B1),
    A2 = vclock:increment(a, A1),
    C = vclock:merge([A2, B1]),
    C1 = vclock:increment(c, C),
    true = vclock:descends(C1, A2),
    true = vclock:descends(C1, B1),
    false = vclock:descends(B1, C1),
    false = vclock:descends(B1, A1),
    ok.

% @doc Return true if Va is a direct descendant of Vb, else false -- remember, a vclock is its own descendant!
% @spec descends(Va :: vclock(), Vb :: vclock()) -> bool()
descends(_, []) ->
    % all vclocks descend from the empty vclock
    true;
descends(Va, Vb) ->
    [{NodeB, {CtrB, _T}}|RestB] = Vb,
    CtrA = 
	case proplists:get_value(NodeB, Va) of
	    undefined ->
		false;
	    {CA, _TSA} -> CA
	end,
    case CtrA of
	false -> false;
	_ -> 
	    if
		CtrA < CtrB ->
		    false;
		true ->
		    descends(Va,RestB)
	    end
    end.

% @doc Combine all VClocks in the input list into their least possible
%      common descendant.
% @spec merge(VClocks :: [vclock()]) -> vclock()
merge([])             -> [];
merge([SingleVclock]) -> SingleVclock;
merge([First|Rest])   -> merge(Rest, lists:keysort(1, First)).

merge([], NClock) -> NClock;
merge([AClock|VClocks],NClock) ->
    merge(VClocks, merge(lists:keysort(1, AClock), NClock, [])).

merge([], [], AccClock) -> lists:reverse(AccClock);
merge([], [Left|Rest], AccClock) -> merge([], Rest, [Left|AccClock]);
merge(Left, [], AccClock) -> merge([], Left, AccClock);
merge(V=[{Node1,{Ctr1,TS1}}|VClock],
      N=[{Node2,{Ctr2,TS2}}|NClock], AccClock) ->
    if Node1 < Node2 ->
            merge(VClock, N, [{Node1,{Ctr1,TS1}}|AccClock]);
       Node1 > Node2 ->
            merge(V, NClock, [{Node2,{Ctr2,TS2}}|AccClock]);
       true ->
            {Ctr,TS} = if Ctr1 > Ctr2 -> {Ctr1,TS1};
                          true        -> {Ctr2,TS2}
                       end,
            merge(VClock, NClock, [{Node1,{Ctr,TS}}|AccClock])
    end.

% @doc Get the counter value in VClock set from Node.
% @spec get_counter(Node :: node(), VClock :: vclock()) -> counter()
get_counter(Node, VClock) ->
    case proplists:get_value(Node, VClock) of
	{Ctr, _TS} -> Ctr;
	undefined -> undefined
    end.

% @doc Get the timestamp value in a VClock set from Node.
% @spec get_timestamp(Node :: node(), VClock :: vclock()) -> timestamp()
get_timestamp(Node, VClock) ->
    case proplists:get_value(Node, VClock) of
	{_Ctr, TS} -> TS;
	undefined -> undefined
    end.

% @doc Increment VClock at Node.
% @spec increment(Node :: node(), VClock :: vclock()) -> vclock()
increment(Node, VClock) ->
    {{Ctr, TS},NewV} = case lists:keytake(Node, 1, VClock) of
                           false ->
                               {{1, timestamp()}, VClock};
                           {value, {_N, {C, _T}}, ModV} ->
                               {{C + 1, timestamp()}, ModV}
                       end,
    [{Node,{Ctr,TS}}|NewV].

% @doc Return the list of all nodes that have ever incremented VClock.
% @spec all_nodes(VClock :: vclock()) -> [node()]
all_nodes(VClock) ->
    [X || {X,{_,_}} <- VClock].

% @private
timestamp() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

% @doc Compares two VClocks for equality.
%      Not very fast.
% @spec equal(VClockA :: vclock(), VClockB :: vclock()) -> true | false
equal(VA,VB) ->
    VSet1 = sets:from_list(VA),
    VSet2 = sets:from_list(VB),
    case length(sets:to_list(sets:subtract(VSet1,VSet2))) > 0 of
        true -> false;
        false ->
            case length(sets:to_list(sets:subtract(VSet2,VSet1))) > 0 of
                true -> false;
                false -> true
            end
    end.

% @doc Possibly shrink the size of a vclock, depending on current age and size.
% @spec prune(V::vclock(), Now::integer(), BucketProps::term()) -> vclock()
prune(V,Now,BucketProps) ->
    SortV = lists:sort(fun({_,{_,A}},{_,{_,B}}) -> A < B end, V),
    prune_vclock1(SortV,Now,BucketProps).
% @private
prune_vclock1(V,Now,BProps) ->
    case length(V) =< proplists:get_value(small_vclock,BProps) of
        true -> V;
        false ->
            {_,{_,HeadTime}} = hd(V),
            case (Now - HeadTime) < proplists:get_value(young_vclock,BProps) of
                true -> V;
                false -> prune_vclock1(V,Now,BProps,HeadTime)
            end
    end.
% @private
prune_vclock1(V,Now,BProps,HeadTime) ->
    % has a precondition that V is longer than small and older than young
    case length(V) > proplists:get_value(big_vclock,BProps) of
        true -> prune_vclock1(tl(V),Now,BProps);
        false ->
            case (Now - HeadTime) > proplists:get_value(old_vclock,BProps) of
                true -> prune_vclock1(tl(V),Now,BProps);
                false -> V
            end
    end.

prune_small_test() ->
    % vclock with less entries than small_vclock will be untouched
    Now = riak_util:moment(),
    OldTime = Now - 32000000,
    SmallVC = [{<<"1">>, {1, OldTime}},
               {<<"2">>, {2, OldTime}},
               {<<"3">>, {3, OldTime}}],
    Props = [{small_vclock,4}],
    ?assertEqual(lists:sort(SmallVC), lists:sort(prune(SmallVC, Now, Props))).

prune_young_test() ->
    % vclock with all entries younger than young_vclock will be untouched
    Now = riak_util:moment(),
    NewTime = Now - 1,
    VC = [{<<"1">>, {1, NewTime}},
          {<<"2">>, {2, NewTime}},
          {<<"3">>, {3, NewTime}}],
    Props = [{small_vclock,1},{young_vclock,1000}],
    ?assertEqual(lists:sort(VC), lists:sort(prune(VC, Now, Props))).

prune_big_test() ->
    % vclock not preserved by small or young will be pruned down to
    % no larger than big_vclock entries
    Now = riak_util:moment(),
    NewTime = Now - 1000,
    VC = [{<<"1">>, {1, NewTime}},
          {<<"2">>, {2, NewTime}},
          {<<"3">>, {3, NewTime}}],
    Props = [{small_vclock,1},{young_vclock,1},
             {big_vclock,2},{old_vclock,100000}],
    ?assert(length(prune(VC, Now, Props)) =:= 2).

prune_old_test() ->
    % vclock not preserved by small or young will be pruned down to
    % no larger than big_vclock and no entries more than old_vclock ago
    Now = riak_util:moment(),
    NewTime = Now - 1000,
    OldTime = Now - 100000,    
    VC = [{<<"1">>, {1, NewTime}},
          {<<"2">>, {2, OldTime}},
          {<<"3">>, {3, OldTime}}],
    Props = [{small_vclock,1},{young_vclock,1},
             {big_vclock,2},{old_vclock,10000}],
    ?assert(length(prune(VC, Now, Props)) =:= 1).

accessor_test() ->
    VC = [{<<"1">>, {1, 1}},
          {<<"2">>, {2, 2}}],
    ?assertEqual(1, get_counter(<<"1">>, VC)),
    ?assertEqual(1, get_timestamp(<<"1">>, VC)),
    ?assertEqual(2, get_counter(<<"2">>, VC)),
    ?assertEqual(2, get_timestamp(<<"2">>, VC)),
    ?assertEqual(undefined, get_counter(<<"3">>, VC)),
    ?assertEqual(undefined, get_timestamp(<<"3">>, VC)),
    ?assertEqual([<<"1">>, <<"2">>], all_nodes(VC)).

merge_test() ->
    VC1 = [{<<"1">>, {1, 1}},
           {<<"2">>, {2, 2}},
           {<<"4">>, {4, 4}}],
    VC2 = [{<<"3">>, {3, 3}},
           {<<"4">>, {3, 3}}],
    ?assertEqual([], merge(vclock:fresh())),
    ?assertEqual([{<<"1">>,{1,1}},{<<"2">>,{2,2}},{<<"3">>,{3,3}},{<<"4">>,{4,4}}],
                 merge([VC1, VC2])).
