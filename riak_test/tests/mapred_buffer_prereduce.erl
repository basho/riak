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
%% called `compat_buffer_and_prereduce_test_'. It has been moved here
%% to avoid the fragile setup and teardown stages that frequently
%% broke eunit testing.
-module(mapred_buffer_prereduce).
-behavior(riak_test).
-export([
         %% riak_test api
         confirm/0
        ]).
-include_lib("eunit/include/eunit.hrl").

-define(INTS_BUCKET, <<"foonum">>).
-define(NUM_INTS, 1000).

confirm() ->
    Nodes = rt:build_cluster(3),

    load_test_data(Nodes),
    
    [ begin
          lager:info("Running test ~s (m:~p, r:~p)",
                     [T, M, R]),
          test_batch(Nodes, M, R)
      end
      || {T, M, R} <- [{"default", none, none},
                       {"reduce batch 10", none,
                        [{reduce_phase_batch_size, 10}]},
                       {"reduce batch 0", none,
                        [{reduce_phase_batch_size, 0}]},
                       {"reduce only once", none,
                        [reduce_phase_only_1]},
                       {"predreduce batch 7",
                        [do_prereduce, reduce_phase_only_1],
                        [{reduce_phase_batch_size, 7}]}] ],
    pass.

load_test_data([Node|_]) ->
    %% creates foonum/1..5 - this is what populates ?INTS_BUCKET
    lager:info("Filling INTS_BUCKET (~s)", [?INTS_BUCKET]),
    ok = rpc:call(Node, riak_kv_mrc_pipe, example_setup, [?NUM_INTS]).

rpcmr(Node, Inputs, Query) ->
    rpc:call(Node, riak_kv_mrc_pipe, mapred, [Inputs, Query]).

test_batch([Node|_], MapArg, ReduceArg) ->
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value},
             MapArg, true},
            {reduce, {modfun, riak_kv_mapreduce, reduce_sum},
             ReduceArg, true}],
    {ok, [MapRs, ReduceRs]} = rpcmr(Node, ?INTS_BUCKET, Spec),
    ExpectR = (?NUM_INTS * (?NUM_INTS+1)) div 2,
    ?assertEqual([ExpectR], ReduceRs),
    ?assertEqual(?NUM_INTS, length(MapRs)).
