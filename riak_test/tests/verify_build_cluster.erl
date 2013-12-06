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
-module(verify_build_cluster).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-import(rt, [wait_until_nodes_ready/1,
             wait_until_no_pending_changes/1]).

confirm() ->
    %% Deploy a set of new nodes
    lager:info("Deploying 4 nodes"),
    %% handoff_concurrency needs to be raised to make the leave operation faster.
    %% most clusters go up to 10, but this one is one louder, isn't it?
    [Node1, Node2, Node3, Node4] = Nodes = rt:deploy_nodes(4, [{riak_core, [{handoff_concurrency, 11}]}]), 

    %% Ensure each node owns 100% of it's own ring
    lager:info("Ensure each nodes 100% of it's own ring"),

    [rt:wait_until_owners_according_to(Node, [Node]) || Node <- Nodes],

    lager:info("Loading some data up in this cluster."),
    ?assertEqual([], rt:systest_write(Node1, 0, 1000, <<"verify_build_cluster">>, 2)),

    lager:info("joining Node 2 to the cluster... It takes two to make a thing go right"),
    rt:join(Node2, Node1),
    wait_and_validate([Node1, Node2]),

    lager:info("joining Node 3 to the cluster"),
    rt:join(Node3, Node1),
    wait_and_validate([Node1, Node2, Node3]),

    lager:info("joining Node 4 to the cluster"),
    rt:join(Node4, Node1),
    wait_and_validate(Nodes),

    lager:info("taking Node 1 down"),
    rt:stop(Node1),
    ?assertEqual(ok, rt:wait_until_unpingable(Node1)),
    wait_and_validate(Nodes, [Node2, Node3, Node4]),

    lager:info("taking Node 2 down"),
    rt:stop(Node2),
    ?assertEqual(ok, rt:wait_until_unpingable(Node2)),
    wait_and_validate(Nodes, [Node3, Node4]),

    lager:info("bringing Node 1 up"),
    rt:start(Node1),
    ok = rt:wait_until_pingable(Node1),
    wait_and_validate(Nodes, [Node1, Node3, Node4]),
    lager:info("bringing Node 2 up"),
    rt:start(Node2),
    ok = rt:wait_until_pingable(Node2),
    wait_and_validate(Nodes),

    % leave 1, 2, and 3
    lager:info("leaving Node 1"),
    rt:leave(Node1),
    ?assertEqual(ok, rt:wait_until_unpingable(Node1)),
    wait_and_validate([Node2, Node3, Node4]),

    lager:info("leaving Node 2"),
    rt:leave(Node2),
    ?assertEqual(ok, rt:wait_until_unpingable(Node2)),
    wait_and_validate([Node3, Node4]),

    lager:info("leaving Node 3"),
    rt:leave(Node3),
    ?assertEqual(ok, rt:wait_until_unpingable(Node3)),

    % verify 4
    wait_and_validate([Node4]),

    pass.

wait_and_validate(Nodes) -> wait_and_validate(Nodes, Nodes).
wait_and_validate(RingNodes, UpNodes) ->
    lager:info("Wait until all nodes are ready and there are no pending changes"),
    ?assertEqual(ok, rt:wait_until_nodes_ready(UpNodes)),
    ?assertEqual(ok, rt:wait_until_all_members(UpNodes)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(UpNodes)),
    lager:info("Ensure each node owns a portion of the ring"),
    [rt:wait_until_owners_according_to(Node, RingNodes) || Node <- UpNodes],
    [rt:wait_for_service(Node, riak_kv) || Node <- UpNodes],
    lager:info("Verify that you got much data... (this is how we do it)"),
    ?assertEqual([], rt:systest_read(hd(UpNodes), 0, 1000, <<"verify_build_cluster">>, 2)),
    done.