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
-module(verify_leave).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-import(rt, [build_cluster/1,
             leave/1,
             wait_until_unpingable/1,
             status_of_according_to/2,
             remove/2]).

confirm() ->
    %% Bring up a 3-node cluster for the test
    Nodes = build_cluster(3),
    [Node1, Node2, Node3] = Nodes,

    %% Have node2 leave
    lager:info("Have ~p leave", [Node2]),
    leave(Node2),
    ?assertEqual(ok, wait_until_unpingable(Node2)),

    %% Verify node2 no longer owns partitions, all node believe it invalid
    lager:info("Verify ~p no longer owns partitions and all nodes believe "
               "it is invalid", [Node2]),
    Remaining1 = Nodes -- [Node2],
    rt:wait_until_nodes_agree_about_ownership(Remaining1),
    [?assertEqual(invalid, status_of_according_to(Node2, Node)) || Node <- Remaining1],

    %% Have node1 remove node3
    lager:info("Have ~p remove ~p", [Node1, Node3]),
    remove(Node1, Node3),
    ?assertEqual(ok, wait_until_unpingable(Node3)),

    %% Verify node3 no longer owns partitions, all node believe it invalid
    lager:info("Verify ~p no longer owns partitions, and all nodes believe "
               "it is invalid", [Node3]),
    Remaining2 = Remaining1 -- [Node3],
    rt:wait_until_nodes_agree_about_ownership(Remaining2),
    [?assertEqual(invalid, status_of_according_to(Node3, Node)) || Node <- Remaining2],
    pass.
