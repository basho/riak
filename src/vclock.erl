%% @copyright 2007-2008 Basho Technologies

%% @reference Leslie Lamport (1978). "Time, clocks, and the ordering of events in a distributed system". Communications of the ACM 21 (7): 558-565. 

%% @reference Friedemann Mattern (1988). "Virtual Time and Global States of Distributed Systems". Workshop on Parallel and Distributed Algorithms: pp. 215-226

%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>

%% @doc A simple Erlang implementation of vector clocks as inspired by Lamport logical clocks.

%% Copyright 2007-2008 Basho Technologies

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%% http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(vclock).

-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([fresh/0,descends/2,merge/1,get_counter/2,get_timestamp/2,
	increment/2,all_nodes/1,equal/2]).
-export([example_test/0]).

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
