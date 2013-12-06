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
-module(verify_kv_health_check).
-behaviour(riak_test).
-export([confirm/0]).

confirm() ->
    [Node1, Node2, _Node3] = rt:build_cluster(3),

    %% add intercept that delays handling of vnode commands
    %% on a single node (the "slow" node)
    rt_intercept:load_code(Node1),
    rt_intercept:add(Node1, {riak_kv_vnode,
                             [{{handle_command, 3}, slow_handle_command}]}),
    lager:info("Installed intercept to delay handling of requests by kv_vnode on ~p",
               [Node1]),

    %% lets use some reasonable threshold values so we aren't here forever
    DisableThreshold = 10,
    EnableThreshold = 9,
    ok = rpc:call(Node1,
                  application,
                  set_env,
                  [riak_kv, vnode_mailbox_limit, {EnableThreshold, DisableThreshold}]),

    %% make DisableThreshold+5 requests and trigger the health check explicitly
    %% we only need to backup one vnode's msg queue on the node to fail the health check
    %% so we read the same key again and again
    C = rt:pbc(Node2),
    [riakc_pb_socket:get(C, <<"b">>, <<"k">>) || _ <- lists:seq(1,DisableThreshold+5)],
    ok = rpc:call(Node1, riak_core_node_watcher, check_health, [riak_kv]),

    lager:info("health check should disable riak_kv on ~p shortly", [Node1]),
    rt:wait_until(Node1,
                  fun(N) ->
                          Up = rpc:call(N, riak_core_node_watcher, services, [N]),
                          not lists:member(riak_kv, Up)
                  end),
    lager:info("health check successfully disabled riak_kv on ~p", [Node1]),
    lager:info("health check should re-enable riak_kv on ~p after some messages have been processed",
               [Node1]),
    %% wait for health check timer to do its thing, don't explicitly execute it
    rt:wait_for_service(Node1, riak_kv),
    lager:info("health check successfully re-enabled riak_kv on ~p", [Node1]),
    riakc_pb_socket:stop(C),
    pass.
