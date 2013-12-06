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
%% called `compat_basic1_test_'. It has been moved here to avoid the
%% fragile setup and teardown stages that frequently broke eunit
%% testing.
-module(mapred_basic_compat).
-behavior(riak_test).
-export([
         %% riak_test api
         confirm/0,

         %% test helpers
         inputs_gen_seq/3,
         inputs_gen_bkeys_1/3
        ]).
-compile([export_all]). %% because we call ?MODULE:TestName
-include_lib("eunit/include/eunit.hrl").

-define(INTS_BUCKET, <<"foonum">>).
-define(LINK_BUCKET, <<"link bucket">>).

confirm() ->
    Nodes = rt:build_cluster(3),

    load_test_data(Nodes),
    rt:load_modules_on_nodes([?MODULE], Nodes),
    
    [ begin
          lager:info("Running test ~p", [T]),
          ?MODULE:T(Nodes)
      end
      || T <- [empty_query,
               reduce_zero_inputs,
               keep_both,
               keep_neither,
               keep_first_only,
               keep_second_only,
               explicity_rereduce,
               error_not_found_propagation,
               basic_link,
               link_not_found,
               keydata,
               key_filters,
               modfun_generator1,
               modfun_generator2] ],

    pass.

load_test_data([Node|_]) ->
    %% creates foonum/1..5 - this is what populates ?INTS_BUCKET
    lager:info("Filling INTS_BUCKET (~s)", [?INTS_BUCKET]),
    ok = rpc:call(Node, riak_kv_mrc_pipe, example_setup, []),
    
    lager:info("Adding Link object"),
    Obj = riakc_obj:new(?LINK_BUCKET,
                        <<"yo">>,
                        <<"link val">>,
                        "text/plain"),
    MD = riakc_obj:add_link(
           riakc_obj:get_update_metadata(Obj),
           [{<<"link 1">>, [{?LINK_BUCKET, <<"nokey-1">>}]},
            {<<"link 2">>, [{?LINK_BUCKET, <<"nokey-2">>}]}]),

    C = rt:pbc(Node),
    ok = riakc_pb_socket:put(C, riakc_obj:update_metadata(Obj, MD)),
    riakc_pb_socket:stop(C).

rpcmr(Node, Inputs, Query) ->
    rpc:call(Node, riak_kv_mrc_pipe, mapred, [Inputs, Query]).

%% @doc This will trigger a traversal of IntsBucket, but because the
%% query is empty, the MapReduce will traverse the bucket and send
%% BKeys down the pipe.
empty_query([Node|_]) ->
    {ok, BKeys} = rpcmr(Node, ?INTS_BUCKET, []),
    ?assertEqual(5, length(BKeys)),
    ?assertEqual({?INTS_BUCKET, <<"bar1">>}, hd(lists:sort(BKeys))).

%% @doc AZ 479: Reduce with zero inputs -> call reduce once w/empty list
reduce_zero_inputs([Node|_]) ->
    Spec = [{reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true}],
    ?assertEqual({ok, [0]}, rpcmr(Node, [], Spec)).

%% @doc Basic compatibility: keep both stages
keep_both([Node|_]) ->
    Spec =  [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true},
             {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true}],
    {ok, [MapRs, ReduceRs]} = rpcmr(Node, ?INTS_BUCKET, Spec),
    ?assertEqual(5, length(MapRs)),
    ?assertEqual([15], ReduceRs).

%% @doc Basic compat: keep neither stages -> no output
keep_neither([Node|_]) ->
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, false}],
    %% "Crazy" semantics: if only 1 keeper stage, then
    %% return List instead of [List].
    ?assertEqual({ok, []}, rpcmr(Node, ?INTS_BUCKET, Spec)).

%% @doc Basic compat: keep first stage only, want 'crazy' result",
keep_first_only([Node|_]) ->
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, false}],
    %% "Crazy" semantics: if only 1 keeper stage, then
    %% return List instead of [List].
    {ok, MapRs} = rpcmr(Node, ?INTS_BUCKET, Spec),
    ?assertEqual(5, length(MapRs)).

%% @doc Basic compat: keep second stage only, want 'crazy' result
keep_second_only([Node|_]) ->
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true}],
    %% "Crazy" semantics: if only 1 keeper stage, then
    %% return List instead of [List].
    ?assertEqual({ok, [15]}, rpcmr(Node, ?INTS_BUCKET, Spec)).

%% @doc Explicit rereduce
explicity_rereduce([Node|_]) ->
    RedSpec = {reduce, {modfun, riak_kv_mapreduce, reduce_sum}, none, true},
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}]
        ++ lists:duplicate(5, RedSpec),
    ?assertMatch({ok, [_, [15],[15],[15],[15],[15]]},
                 rpcmr(Node, ?INTS_BUCKET, Spec)).

%% @doc Make certain that {error, not_found} goes down the pipe from a
%% map phase.
error_not_found_propagation([Node|_]) ->
    Inputs = [{<<"no-such-bucket">>, <<"no-such-key!">>}],
    Spec =  [{map, {modfun, riak_kv_mapreduce, map_object_value},
              {struct,[{<<"sub">>,[<<"0">>]}]}, false},
             {reduce, {modfun, riak_kv_mapreduce, reduce_string_to_integer},
              none,true}],
    ?assertEqual({ok, [0]}, rpcmr(Node, Inputs, Spec)).

%% @doc Basic link phase
basic_link([Node|_]) ->
    Spec = [{link, '_', <<"link 1">>, true}],
    ?assertEqual({ok, [ [?LINK_BUCKET, <<"nokey-1">>, <<"link 1">>] ]},
                 rpcmr(Node, ?LINK_BUCKET, Spec)).

%% @doc Link phase + notfound
link_not_found([Node|_]) ->
    Inputs = [{<<"no">>, K} || K <- [<<"no1">>, <<"no2">>]],
    Spec = [{link, '_', '_', true}],
    ?assertEqual({ok, []}, rpcmr(Node, Inputs, Spec)).

%% @doc KeyData
keydata([Node|_]) ->
    UnMap = fun(O, undefined, _) ->
                    [{riak_object:bucket(O),
                      riak_object:key(O)}];
               (O, KeyData, _) ->
                    [{{riak_object:bucket(O),
                       riak_object:key(O)},
                      KeyData}]
            end,
    Normalize = fun({{B,K},D}) -> {{B,K},D};
                   ({B,K})     -> {B,K};
                   ([B,K])     -> {B,K};
                   ([B,K,D])   -> {{B,K},D}
                end,
    Spec = [{map, {qfun, UnMap}, none, true}],
    Inputs = [{?INTS_BUCKET, <<"bar1">>},
              {{?INTS_BUCKET, <<"bar2">>}, <<"keydata works">>},
              [?INTS_BUCKET, <<"bar3">>],
              [?INTS_BUCKET, <<"bar4">>, <<"keydata still works">>]],
    {ok, Results} = rpcmr(Node, Inputs, Spec),
    SortedNormal = lists:sort([ Normalize(I) || I <- Inputs ]),
    ?assertEqual(SortedNormal, lists:sort(Results)).

%% @doc Key Filters
key_filters([Node|_]) ->
    %% filter sould match only "bar4" key
    Inputs = {?INTS_BUCKET, [[<<"ends_with">>, <<"r4">>]]},
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}],
    ?assertEqual({ok, [4]}, rpcmr(Node, Inputs, Spec)).

%% @doc modfun for inputs generator
modfun_generator1([Node|_]) ->
    Inputs = {modfun, ?MODULE, inputs_gen_seq, 6},
    Spec = [{reduce, {modfun, riak_kv_mapreduce, reduce_sum},none,true}],
    ?assertEqual({ok, [21]}, rpcmr(Node, Inputs, Spec)).

%% @doc runs on riak node
inputs_gen_seq(Pipe, Max, _Timeout) ->
    [riak_pipe:queue_work(Pipe, X) || X <- lists:seq(1, Max)],
    riak_pipe:eoi(Pipe),
    ok.

%% @doc modfun for inputs generator: make BKeys for conventional phases
modfun_generator2([Node|_]) ->
    Inputs = {modfun, ?MODULE, inputs_gen_bkeys_1, {?INTS_BUCKET, 1, 5}},
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value},
             none, false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_string_to_integer},
             none,false},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum},
             none,true}],
    ?assertEqual({ok, [15]}, rpcmr(Node, Inputs, Spec)).

%% @doc runs on riak node
inputs_gen_bkeys_1(Pipe, {Bucket, Start, End}, _Timeout) ->
    BKeys = [{Bucket, list_to_binary("bar"++integer_to_list(X))} ||
                 X <- lists:seq(Start, End)],
    [riak_pipe:queue_work(Pipe, BK) || BK <- BKeys],
    riak_pipe:eoi(Pipe),
    ok.
