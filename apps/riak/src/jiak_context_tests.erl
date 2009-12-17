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

%% @doc Tests for jiak_context.  These must be in a separate module,
%%      because eunit doesn't work with parameterized modules (due to
%%      the inability to export zero-arity functions from them).
-module(jiak_context_tests).

-include_lib("eunit/include/eunit.hrl").

diff_test() ->
    A = jiak_context:new([], []),
    ?assertEqual([], A:diff()),
    B = A:set_diff([{bogus, diff}]),
    ?assertEqual([{bogus, diff}], B:diff()).

single_prop_test() ->
    A = jiak_context:new([], []),
    B = A:set_prop(foo, bar),
    ?assertEqual(bar, B:get_prop(foo)),
    C = B:set_prop(foo, quux),
    ?assertEqual(quux, C:get_prop(foo)).

multi_prop_test() ->
    A = jiak_context:new([], []),
    B = A:set_props([{foo, 1},{bar,2},{baz,3}]),
    ?assertEqual([1,2,3], [ B:get_prop(P) || P <- [foo, bar, baz] ]),
    C = B:set_props([{foo, 10},{quux,20}]),
    ?assertEqual([10,2,3,20],
                 [ C:get_prop(P) || P <- [foo, bar, baz, quux] ]).

all_together_test() ->
    A = jiak_context:new([{diff, 1}], [{prop1, 1}]),
    B = A:set_diff([{diff, 2}]),
    C = B:set_prop(prop1, 2),
    D = C:set_props([{prop2, 1}]),
    ?assertEqual([{diff, 2}], D:diff()),
    ?assertEqual([2, 1], [ D:get_prop(P) || P <- [prop1, prop2] ]).
