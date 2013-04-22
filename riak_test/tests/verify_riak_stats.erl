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
-module(verify_riak_stats).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

%% You should have curl installed locally to do this.
confirm() ->
    Nodes = rt:deploy_nodes(1),
    [Node1] = Nodes,
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node1])),
    Stats1 = get_stats(Node1),
    %% make sure a set of stats have valid values
    verify_nz(Stats1,[<<"cpu_nprocs">>,
                      <<"mem_total">>,
                      <<"mem_allocated">>,
                      <<"sys_logical_processors">>,
                      <<"sys_process_count">>,
                      <<"sys_thread_pool_size">>,
                      <<"sys_wordsize">>,
                      <<"ring_num_partitions">>,
                      <<"ring_creation_size">>,
                      <<"memory_total">>,
                      <<"memory_processes">>,
                      <<"memory_processes_used">>,
                      <<"memory_system">>,
                      <<"memory_atom">>,
                      <<"memory_atom_used">>,
                      <<"memory_binary">>,
                      <<"memory_code">>,
                      <<"memory_ets">>]),
    
    lager:info("perform 5 x  PUT and a GET to increment the stats"),
    lager:info("as the stat system only does calcs for > 5 readings"),
    
    C = rt:httpc(Node1),
    [rt:httpc_write(C, <<"systest">>, <<X>>, <<"12345">>) || X <- lists:seq(1, 5)],
    [rt:httpc_read(C, <<"systest">>, <<X>>) || X <- lists:seq(1, 5)],
    
    Stats2 = get_stats(Node1),
    
    %% make sure the stats that were supposed to increment did
    verify_inc(Stats1, Stats2, [{<<"node_gets">>, 10},
                                {<<"node_puts">>, 5},
                                {<<"node_gets_total">>, 10},
                                {<<"node_puts_total">>, 5},
                                {<<"vnode_gets">>, 30},
                                {<<"vnode_puts">>, 15},
                                {<<"vnode_gets_total">>, 30},
                                {<<"vnode_puts_total">>, 15}]),

    %% verify that fsm times were tallied
    verify_nz(Stats2, [<<"node_get_fsm_time_mean">>,
                       <<"node_get_fsm_time_median">>,
                       <<"node_get_fsm_time_95">>,
                       <<"node_get_fsm_time_99">>,
                       <<"node_get_fsm_time_100">>,
                       <<"node_put_fsm_time_mean">>,
                       <<"node_put_fsm_time_median">>,
                       <<"node_put_fsm_time_95">>,
                       <<"node_put_fsm_time_99">>,
                       <<"node_put_fsm_time_100">>]),
                       
    
    lager:info("Make PBC Connection"),
    Pid = rt:pbc(Node1),
    
    Stats3 = get_stats(Node1),

    rt:systest_write(Node1, 1),
    %% make sure the stats that were supposed to increment did
    verify_inc(Stats2, Stats3, [{<<"pbc_connects_total">>, 1},
                                {<<"pbc_connects">>, 1},
                                {<<"pbc_active">>, 1}]),
    
    

    lager:info("Force Read Repair"),
    rt:pbc_write(Pid, <<"testbucket">>, <<"1">>, <<"blah!">>),
    rt:pbc_set_bucket_prop(Pid, <<"testbucket">>, [{n_val, 4}]),
    
    Stats4 = get_stats(Node1),
    verify_inc(Stats3, Stats4, [{<<"read_repairs_total">>, 0},
                                {<<"read_repairs">>, 0}]),
    
    _Value = rt:pbc_read(Pid, <<"testbucket">>, <<"1">>),

    Stats5 = get_stats(Node1),

    verify_inc(Stats3, Stats5, [{<<"read_repairs_total">>, 1},
                                {<<"read_repairs">>, 1}]),

    pass.

verify_inc(Prev, Props, Keys) ->
    [begin
         Old = proplists:get_value(Key, Prev, 0),
         New = proplists:get_value(Key, Props, 0),
         lager:info("~s: ~p -> ~p (expected ~p)", [Key, Old, New, Old + Inc]),
         ?assertEqual(New, (Old + Inc))
     end || {Key, Inc} <- Keys].

verify_nz(Props, Keys) ->
    [?assertNotEqual(proplists:get_value(Key,Props,0), 0) || Key <- Keys].

get_stats(Node) ->
    timer:sleep(10000),
    StatString = os:cmd(io_lib:format("curl -s -S ~s/stats", [rt:http_url(Node)])),
    {struct, Stats} = mochijson2:decode(StatString),
    %%lager:debug(StatString),
    Stats.