%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
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
%% @doc Verify some MapReduce internals.
%%
%% This test used to be in riak_kv's test/mapred_test.erl. It was
%% called `compat_javascript_test_'. It has been moved here to avoid
%% the fragile setup and teardown stages that frequently broke eunit
%% testing.
-module(mapred_javascript).
-behavior(riak_test).
-export([
         %% riak_test api
         confirm/0
        ]).
-compile([export_all]). %% because we run tests as ?MODULE:T(Nodes)
-include_lib("eunit/include/eunit.hrl").

-define(INTS_BUCKET, <<"foonum">>).
-define(NUM_INTS, 5).
-define(JS_BUCKET, <<"jsfuns">>).
-define(NOTFOUND_BKEY, {<<"does not">>, <<"exist">>}).
-define(MAP_JS, <<"function(v) { return [v.values[0].data]; }">>).
-define(REDUCE_JS, <<"function(v) {
                         Sum = function(A, B) { return A+B; };
                         return [ v.reduce(Sum) ];
                      }">>).

confirm() ->
    Nodes = rt:build_cluster(3),

    load_test_data(Nodes),
    
    [ begin
          lager:info("Running test ~p", [T]),
          ?MODULE:T(Nodes)
      end
      || T<- [jsanon_source,
              jsanon_bkey,
              jsfun,
              js_notfound,
              keydata] ],
    pass.

load_test_data([Node|_]) ->
    %% creates foonum/1..5 - this is what populates ?INTS_BUCKET
    lager:info("Filling INTS_BUCKET (~s)", [?INTS_BUCKET]),
    ok = rpc:call(Node, riak_kv_mrc_pipe, example_setup, [?NUM_INTS]),

    lager:info("Adding Javascript source objects"),
    Map = riakc_obj:new(?JS_BUCKET, <<"map">>, ?MAP_JS, "text/plain"),
    Red = riakc_obj:new(?JS_BUCKET, <<"reduce">>, ?REDUCE_JS, "text/plain"),

    C = rt:pbc(Node),
    ok = riakc_pb_socket:put(C, Map),
    ok = riakc_pb_socket:put(C, Red),
    riakc_pb_socket:stop(C).


rpcmr(Node, Inputs, Query) ->
    rpc:call(Node, riak_kv_mrc_pipe, mapred, [Inputs, Query]).

%% @doc map & reduce with jsanon-Source
jsanon_source(Nodes) ->
    run_js_test(Nodes, {jsanon, ?MAP_JS}, {jsanon, ?REDUCE_JS}).

%% @doc map & reduce with jsanon-Bucket/Key
jsanon_bkey(Nodes) ->
    run_js_test(Nodes,
                {jsanon, {?JS_BUCKET, <<"map">>}},
                {jsanon, {?JS_BUCKET, <<"reduce">>}}).

%% @doc map & reduce with jsfun
jsfun(Nodes) ->
    run_js_test(Nodes,
                {jsfun, <<"Riak.mapValues">>},
                {jsfun, <<"Riak.reduceSum">>}).

run_js_test([Node|_], MapFun, ReduceFun) ->
    Spec = [{map, MapFun, <<>>, true},
            {reduce, ReduceFun, <<>>, true}],
    {ok, [MapRs, ReduceRs]} = rpcmr(Node, ?INTS_BUCKET, Spec),
    ?assertEqual(5, length(MapRs)),
    ExpectR = (?NUM_INTS * (?NUM_INTS+1)) div 2,
    ?assertEqual([ExpectR], ReduceRs).

%% @doc objects not found for JS map turn into
%% {not_found, {Bucket, Key}, KeyData} tuples
js_notfound([Node|_]) ->
    Spec = [{map, {jsfun, <<"Riak.mapValues">>}, <<>>, true},
            {reduce,
             {jsanon, <<"function(v) {
                            F = function(O) {
                                   if ((O[\"not_found\"] &&
                                        O.not_found[\"bucket\"]) ||
                                       O[\"mapred_test_pass\"])
                                      return {mapred_test_pass:1};
                                   else
                                      return O;
                                }
                            return v.map(F);
                         }">>},
             <<>>, true}],
    ?assertEqual({ok, [[{not_found,
                         ?NOTFOUND_BKEY,
                         undefined}],
                       [{struct,[{<<"mapred_test_pass">>,1}]}]]},
                 rpcmr(Node, [?NOTFOUND_BKEY], Spec)).
    
keydata([Node|_]) ->
    UnMap = <<"function(O, KD) {
                  R = {b:O.bucket, k:O.key};
                  if (KD != \"undefined\")
                     R.d = KD;
                  return [R];
               }">>,
    Normalize = fun({{B,K},D}) -> {struct, [{<<"b">>, B},
                                            {<<"k">>, K},
                                            {<<"d">>, D}]};
                   ({B,K})     -> {struct, [{<<"b">>, B},
                                            {<<"k">>, K}]};
                   ([B,K])     -> {struct, [{<<"b">>, B},
                                            {<<"k">>, K}]};
                   ([B,K,D])   -> {struct, [{<<"b">>, B},
                                            {<<"k">>, K},
                                            {<<"d">>, D}]}
                end,
    Spec = [{map, {jsanon, UnMap}, none, true}],
    Inputs = [{?INTS_BUCKET, <<"bar1">>},
              {{?INTS_BUCKET, <<"bar2">>}, <<"keydata works">>},
              [?INTS_BUCKET, <<"bar3">>],
              [?INTS_BUCKET, <<"bar4">>, <<"keydata still works">>]],
    {ok, Results} = rpcmr(Node, Inputs, Spec),
    SortedNormal = lists:sort([ Normalize(I) || I <- Inputs ]),
    ?assertEqual(SortedNormal, lists:sort(Results)).
