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
-module(verify_staged_clustering).
-behavior(riak_test).
-export([confirm/0]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    Nodes = rt:deploy_nodes(4),
    [Node1, Node2, Node3, Node4] = Nodes,
    Nodes123 = [Node1, Node2, Node3],
    Nodes23 = [Node2, Node3],

    lager:info("Join ~p and ~p to ~p", [Node2, Node3, Node1]),
    [stage_join(Node, Node1) || Node <- Nodes23],
    ?assertEqual(ok, rt:wait_until_all_members(Nodes123)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(Nodes123)),

    lager:info("Ensure that ~p has not yet claimed partitions", [Node2]),
    [?assertEqual([Node1], rt:owners_according_to(Node)) || Node <- Nodes123],

    lager:info("Commit without first printing the plan. This should fail"),
    commit_staged(Node1),

    lager:info("Print staged plan and then commit"),
    print_staged(Node1),
    commit_staged(Node1),

    lager:info("Ensure that ~p now own all partitions", [Nodes123]),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes123)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(Nodes123)),
    rt:assert_nodes_agree_about_ownership(Nodes123),

    lager:info("Join ~p to the cluster", [Node4]),
    stage_join(Node4, Node1),
    ?assertEqual(ok, rt:wait_until_all_members(Nodes)),

    lager:info("Stage replacement of ~p with ~p", [Node2, Node4]),
    stage_replace(Node1, Node2, Node4),

    lager:info("Print staged plan and commit"),
    print_staged(Node1),
    commit_staged(Node1),

    Nodes134 = [Node1, Node3, Node4],
    lager:info("Ensure that ~p now own all partitions", [Nodes134]),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes134)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(Nodes134)),
    rt:assert_nodes_agree_about_ownership(Nodes134),
    
    lager:info("Verify that ~p shutdown after being replaced", [Node2]),
    ?assertEqual(ok, rt:wait_until_unpingable(Node2)),

    lager:info("Restart ~p and re-join to cluster", [Node2]),
    rt:start(Node2),
    stage_join(Node2, Node1),
    ?assertEqual(ok, rt:wait_until_all_members(Nodes)),

    lager:info("Schedule force-replace of ~p with ~p", [Node3, Node2]),
    stage_force_replace(Node4, Node3, Node2),

    lager:info("Print staged plan and commit"),
    print_staged(Node4),
    commit_staged(Node4),

    Nodes124 = [Node1, Node2, Node4],
    lager:info("Ensure that ~p now own all partitions", [Nodes124]),
    ?assertEqual(ok, rt:wait_until_nodes_ready(Nodes124)),
    ?assertEqual(ok, rt:wait_until_no_pending_changes(Nodes124)),
    rt:assert_nodes_agree_about_ownership(Nodes124),

    lager:info("Stage leave of ~p", [Node2]),
    stage_leave(Node1, Node2),
    lager:info("Stage force-remove of ~p", [Node4]),
    stage_remove(Node1, Node4),

    lager:info("Print staged plan and verify clear_staged works"),
    print_staged(Node1),
    clear_staged(Node1),
    commit_staged(Node1),

    lager:info("Re-stage leave of ~p and force-remove of ~p", [Node2, Node4]),
    stage_leave(Node1, Node2),
    stage_remove(Node1, Node4),
    lager:info("Print staged plan and commit"),
    print_staged(Node1),
    commit_staged(Node1),

    lager:info("Verify that ~p is the only remaining cluster member", [Node1]),
    ?assertEqual(ok, rt:wait_until_no_pending_changes([Node1])),
    ?assertEqual([Node1], rt:owners_according_to(Node1)),
    ?assertEqual(ok, rt:wait_until_all_members([Node1])),

    lager:info("Test verify_staged_clustering: Passed"),
    pass.

n(Atom) ->
    atom_to_list(Atom).

stage_join(Node, OtherNode) ->
    %% rpc:call(Node, riak_kv_console, staged_join, [[n(OtherNode)]]).
    rt:admin(Node, ["cluster", "join", n(OtherNode)]).

stage_leave(Node, OtherNode) ->
    %% rpc:call(Node, riak_core_console, stage_leave, [[n(OtherNode)]]).
    rt:admin(Node, ["cluster", "leave", n(OtherNode)]).

stage_remove(Node, OtherNode) ->
    %% rpc:call(Node, riak_core_console, stage_remove, [[n(OtherNode)]]).
    rt:admin(Node, ["cluster", "force-remove", n(OtherNode)]).

stage_replace(Node, Node1, Node2) ->
    %% rpc:call(Node, riak_core_console, stage_replace, [[n(Node1), n(Node2)]]).
    rt:admin(Node, ["cluster", "replace", n(Node1), n(Node2)]).

stage_force_replace(Node, Node1, Node2) ->
    %% rpc:call(Node, riak_core_console, stage_force_replace, [[n(Node1), n(Node2)]]).
    rt:admin(Node, ["cluster", "force-replace", n(Node1), n(Node2)]).

print_staged(Node) ->
    %% rpc:call(Node, riak_core_console, print_staged, [[]]).
    rt:admin(Node, ["cluster", "plan"]).

commit_staged(Node) ->
    %% rpc:call(Node, riak_core_console, commit_staged, [[]]).
    rt:admin(Node, ["cluster", "commit"]).

clear_staged(Node) ->
    %% rpc:call(Node, riak_core_console, clear_staged, [[]]).
    rt:admin(Node, ["cluster", "clear"]).

stage_join_rpc(Node, OtherNode) ->
    rpc:call(Node, riak_core, staged_join, [OtherNode]).

stage_leave_rpc(Node, OtherNode) ->
    rpc:call(Node, riak_core_claimant, leave_member, [OtherNode]).

stage_remove_rpc(Node, OtherNode) ->
    rpc:call(Node, riak_core_claimant, remove_member, [OtherNode]).

stage_replace_rpc(Node, Node1, Node2) ->
    rpc:call(Node, riak_core_claimant, replace, [Node1, Node2]).

stage_force_replace_rpc(Node, Node1, Node2) ->
    rpc:call(Node, riak_core_claimant, force_replace, [Node1, Node2]).

plan_staged_rpc(Node) ->
    rpc:call(Node, riak_core_claimant, plan, []).

commit_staged_rpc(Node) ->
    rpc:call(Node, riak_core_claimant, commit, []).

clear_staged_rpc(Node) ->
    rpc:call(Node, riak_core_claimant, clear, []).
