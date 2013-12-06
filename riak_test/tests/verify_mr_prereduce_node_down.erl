%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
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
%% @doc This test was designed to provoke a specific failure in
%% MapReduce when one node is down, and a prereduce phase is used. The
%% test simply counts items in a bucket, but it will occasionally get
%% a result of `[]' (the empty list) or `[0]' instead of `[Count]'.
%%
%% The bug was determined to be in the choice of static hash for the
%% final reduce phase. It did not take into account node liveness, and
%% therefor might assign the reduce worker to a vnode on a node that
%% was down.
%%
%% This test is based on one submitted by Alexander Gunin to the
%% riak-users mailing list as an issue reproducer.
%%
%% [http://lists.basho.com/pipermail/riak-users_lists.basho.com/2013-January/010896.html]
-module(verify_mr_prereduce_node_down).

-export([
         %% riak_test's entry
         confirm/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% @doc riak_test callback
confirm() ->
    NodeCount = 4,
    lager:info("Build ~b-node cluster", [NodeCount]),
    [Primary,ToKill|_] = rt:build_cluster(NodeCount),

    %% We need one node down for this test
    rt:stop(ToKill),

    %% store our test data
    Bucket = <<"verify_mr_prereduce_node_down">>,
    ObjCount = 100,
    lager:info("Loading ~b objects of test data", [ObjCount]),
    [] = rt:systest_write(Primary, 1, ObjCount, Bucket, 3),

    %% run the query a bunch
    C = rt:pbc(Primary),
    TestCount = 100,
    lager:info("Running the MR query ~b times", [TestCount]),
    Runs = [ run_query(C, Bucket) || _ <- lists:seq(1, TestCount) ],

    lager:info("Evaluating results"),

    %% Errors == failures that even Riak thinks were failures
    %% Correct == correct answers
    %% Incorrect == failures that Riak thought were correct
    SupposedCorrectFun = fun({ok, _}) -> true; (_) -> false end,
    ActualCorrectFun = fun({ok, V}) -> V == [{1, [ObjCount]}] end,
    {Supposed, Errors} = lists:partition(SupposedCorrectFun, Runs),
    {Correct, Incorrect} = lists:partition(ActualCorrectFun, Supposed),

    %% asserting that all queries gave the correct answer; asserting
    %% more than just Correct == TestCount, such that failures print
    %% out details about how they failed
    ?assertEqual({TestCount, [], []},
                 {length(Correct), Incorrect, Errors}),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

%% result should be a count of the objects in the bucket
run_query(C, Bucket) ->
    riakc_pb_socket:mapred(
      C, Bucket,
      %% this prereduce is key - with it, we'll get
      %% {ok, []} results in the broken case; without
      %% it, we'll get error tuples
      [{map, {modfun, riak_kv_mapreduce, map_identity}, [do_prereduce], false},
       %% counting inputs works because the inputs are riak_objects
       %% (not integers, which might confuse the counting
       {reduce, {modfun, riak_kv_mapreduce, reduce_count_inputs}, none, true}]).
