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
-module(secondary_index_tests).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-define(BUCKET, <<"2ibucket">>).

confirm() ->
    Nodes = rt:build_cluster(3),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes)),
    
    Pid = rt:pbc(hd(Nodes)),
    
    [put_an_object(Pid, N) || N <- lists:seq(0, 20)],
    
    assertExactQuery(Pid, [<<"obj5">>], <<"field1_bin">>, <<"val5">>),
    assertExactQuery(Pid, [<<"obj5">>], <<"field2_int">>, <<"5">>),
    assertRangeQuery(Pid, [<<"obj10">>, <<"obj11">>, <<"obj12">>], <<"field1_bin">>, <<"val10">>, <<"val12">>),
    assertRangeQuery(Pid, [<<"obj10">>, <<"obj11">>, <<"obj12">>], <<"field2_int">>, 10, 12),
    assertRangeQuery(Pid, [<<"obj10">>, <<"obj11">>, <<"obj12">>], <<"$key">>, <<"obj10">>, <<"obj12">>),

    lager:info("Delete an object, verify deletion..."),
    riakc_pb_socket:delete(Pid, ?BUCKET, <<"obj5">>),
    riakc_pb_socket:delete(Pid, ?BUCKET, <<"obj11">>),
    
    lager:info("Sleeping for 5 seconds. Make sure the tombstone is reaped..."),
    timer:sleep(5000),
    
    assertExactQuery(Pid, [], <<"field1_bin">>, <<"val5">>),
    assertExactQuery(Pid, [], <<"field2_int">>, <<"5">>),
    assertRangeQuery(Pid, [<<"obj10">>, <<"obj12">>], <<"field1_bin">>, <<"val10">>, <<"val12">>),
    assertRangeQuery(Pid, [<<"obj10">>, <<"obj12">>], <<"field2_int">>, 10, 12),
    assertRangeQuery(Pid, [<<"obj10">>, <<"obj12">>], <<"$key">>, <<"obj10">>, <<"obj12">>),

    %% Verify the $key index, and riak_kv#367 regression
    assertRangeQuery(Pid, [<<"obj6">>], <<"$key">>, <<"obj6">>, <<"obj6">>),
    assertRangeQuery(Pid, [<<"obj6">>, <<"obj7">>], <<"$key">>, <<"obj6">>, <<"obj7">>),

    %% Verify bignum sort order in sext -- eleveldb only (riak_kv#499)
    TestIdxVal = 1362400142028,
    put_an_object(Pid, TestIdxVal),
    assertRangeQuery(Pid,
                     [<<"obj1362400142028">>],
                     <<"field2_int">>,
                     1000000000000,
                     TestIdxVal),
    pass.

put_an_object(Pid, N) ->
    lager:info("Putting object ~p", [N]),
    Indexes = [{"field1_bin", list_to_binary(io_lib:format("val~p", [N]))}, 
               {"field2_int", N}],
    MetaData = dict:from_list([{<<"index">>, Indexes}]),
    Robj0 = riakc_obj:new(?BUCKET, list_to_binary(io_lib:format("obj~p", [N]))),
    Robj1 = riakc_obj:update_value(Robj0, io_lib:format("data~p", [N])),
    Robj2 = riakc_obj:update_metadata(Robj1, MetaData),
    riakc_pb_socket:put(Pid, Robj2).


assertExactQuery(Pid, Expected, Index, Value) -> 
    lager:info("Searching Index ~p for ~p", [Index, Value]),
    {ok, Results} = riakc_pb_socket:get_index(Pid, ?BUCKET, Index, Value),
    ActualKeys = lists:sort(Results),
    lager:info("Expected: ~p", [Expected]),
    lager:info("Actual  : ~p", [ActualKeys]),
    ?assertEqual(Expected, ActualKeys). 

assertRangeQuery(Pid, Expected, Index, StartValue, EndValue) ->
    lager:info("Searching Index ~p for ~p-~p", [Index, StartValue, EndValue]),
    {ok, Results} = riakc_pb_socket:get_index(Pid, ?BUCKET, Index, StartValue, EndValue),
    ActualKeys = lists:sort(Results),
    lager:info("Expected: ~p", [Expected]),
    lager:info("Actual  : ~p", [ActualKeys]),
    ?assertEqual(Expected, ActualKeys).
