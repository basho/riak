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
-module(verify_basic_upgrade).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

confirm() ->
    TestMetaData = riak_test_runner:metadata(),
    OldVsn = proplists:get_value(upgrade_version, TestMetaData, previous),

    Nodes = [Node1|_] = rt:build_cluster([OldVsn, OldVsn, OldVsn, OldVsn]),

    lager:info("Writing 100 keys to ~p", [Node1]),
    rt:systest_write(Node1, 100, 3),
    ?assertEqual([], rt:systest_read(Node1, 100, 1)),

    [upgrade(Node, current) || Node <- Nodes],

    %% Umm.. technically, it'd downgrade
    [upgrade(Node, OldVsn) || Node <- Nodes],
    pass.

upgrade(Node, NewVsn) ->
    lager:info("Upgrading ~p to ~p", [Node, NewVsn]),
    rt:upgrade(Node, NewVsn),
    rt:wait_for_service(Node, riak_kv),
    lager:info("Ensuring keys still exist"),
    rt:systest_read(Node, 100, 1),
    ?assertEqual([], rt:systest_read(Node, 100, 1)),
    ok.
