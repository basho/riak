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
-module(rolling_capabilities).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    TestMetaData = riak_test_runner:metadata(),
    Count = 4,
    OldVsn = proplists:get_value(upgrade_version, TestMetaData, previous),

    ExpectedCurrent = [{riak_core, vnode_routing, proxy},
                       {riak_core, staged_joins, true},
                       {riak_kv, legacy_keylisting, false},
                       {riak_kv, listkeys_backpressure, true},
                       {riak_kv, mapred_2i_pipe, true},
                       {riak_kv, mapred_system, pipe},
                       {riak_kv, vnode_vclocks, true},
                       {riak_kv, anti_entropy, enabled_v1}],

    %% Assuming default 1.1.4 app.config settings, the only difference
    %% between rolling and upgraded should be 'staged_joins'. Explicitly
    %% test rolling values to ensure we don't fallback to default settings.
    ExpectedOld = case OldVsn of
        legacy ->   [{riak_core, vnode_routing, proxy},
                     {riak_core, staged_joins, false},
                     {riak_kv, legacy_keylisting, false},
                     {riak_kv, listkeys_backpressure, true},
                     {riak_kv, mapred_2i_pipe, true},
                     {riak_kv, mapred_system, pipe},
                     {riak_kv, vnode_vclocks, true}];
        previous -> [{riak_core, vnode_routing, proxy},
                     {riak_core, staged_joins, true},
                     {riak_kv, legacy_keylisting, false},
                     {riak_kv, listkeys_backpressure, true},
                     {riak_kv, mapred_2i_pipe, true},
                     {riak_kv, mapred_system, pipe},
                     {riak_kv, vnode_vclocks, true}];
        _ -> []
    end,
    
    lager:info("Deploying Riak ~p cluster", [OldVsn]),
    Nodes = rt:build_cluster([OldVsn || _ <- lists:seq(1,Count)]),
    lists:foldl(fun(Node, Upgraded) ->
                        rt:upgrade(Node, current),
                        Upgraded2 = Upgraded ++ [Node],
                        lager:info("Verifying rolling/old capabilities"),
                        (Upgraded2 == Nodes)
                            orelse check_capabilities(Upgraded2, ExpectedOld),
                        Upgraded2
                end, [], Nodes),
    lager:info("Verifying final/upgraded capabilities"),
    check_capabilities(Nodes, ExpectedCurrent),
    lager:info("Test ~p passed", [?MODULE]),
    pass.

check_capabilities(Nodes, Expected) ->

    CapCheck = fun(Node) ->
        Caps = rt:capability(Node, all),
        Results = [ proplists:get_value({ExpProj, ExpCap}, Caps) =:= ExpVal || {ExpProj, ExpCap, ExpVal} <- Expected ],
        lists:all(fun(X) -> X =:= true end, Results)
    end,

    [?assertEqual(ok, rt:wait_until(N, CapCheck)) || N <- Nodes],
    ok.

