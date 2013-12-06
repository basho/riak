%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
%%
%% Newer versions of Riak may use different on-disk formats of riak.
%% When performing a rolling downgrade, the downgraded nodes may not
%% be able to read the newer on-disk format. This tests that riak_kv_format
%% correctly downgrades the on-disk format on a node while performing a
%% rolling downgrade.
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
-module(verify_riak_object_reformat).
-behaviour(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    N = 3,
    TestMetaData = riak_test_runner:metadata(),
    DowngradeVsn = proplists:get_value(upgrade_version, TestMetaData, previous),
    Nodes = [Node1|_] = rt:build_cluster(N),

    confirm_object_format(Nodes, v1),

    lager:info("Writing 100 keys in format v1 to ~p", [Node1]),
    rt:systest_write(Node1, 100, N),
    ?assertEqual([], rt:systest_read(Node1, 100, N)),
    lager:info("100 keys successfully written to ~p", [Node1]),

    %% TODO: introduce some handoff
    [begin
         lager:info("Reformatting objects and downgrading ~p", [Node]),
         run_reformat(Node, Node =:= Node1), %% wait for handoffs on one node, kill on rest
         rt:wait_until_ring_converged(Nodes),
         confirm_object_format(Nodes, v0),
         rt:upgrade(Node, DowngradeVsn), %% use upgrade to downgrade
         rt:wait_for_service(Node, riak_kv),
         lager:info("Ensuring keys still readable on ~p", [Node]),
         ?assertEqual([], rt:systest_read(Node, 100, N))
     end || Node <- Nodes],
    ok.

confirm_object_format(Nodes, Version) when is_list(Nodes) ->
    [confirm_object_format(Node, Version) || Node <- Nodes];
confirm_object_format(Node, Version) ->
    ?assertEqual({Node, Version},
                 {Node,
                  rpc:call(Node, riak_core_capability, get, [{riak_kv, object_format}, v0])}).

run_reformat(Node, KillHandoffs) ->
    {_Success, _Ignore, Error} = rpc:call(Node,
                                          riak_kv_reformat,
                                          run,
                                          [v0, KillHandoffs]),
    ?assertEqual(0, Error),
    ok.
