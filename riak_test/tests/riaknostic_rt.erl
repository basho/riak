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
-module(riaknostic_rt).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

%% Change when a new release comes out.
-define(RIAKNOSTIC_URL, "https://github.com/basho/riaknostic/downloads/riaknostic-1.0.2.tar.gz").

%% REQUIRES (sh, curl, tar)

confirm() ->
    %% Build a small cluster
    [Node1, _Node2] = rt:build_cluster(2, []),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node1])),

    %% Install riaknostic for Riak versions below 1.3.0
    riaknostic_bootstrap(Node1),

    %% Run through all tests on Node1
    check_riaknostic_execute(Node1),
    check_riaknostic_usage(Node1),
    check_riaknostic_command_list(Node1),
    check_riaknostic_log_levels(Node1),
    check_riaknostic_conn_failure(Node1),

    %% Done!
    lager:info("Test riaknostic: PASS"),
    pass.

riaknostic_bootstrap(Node) ->
    lager:info("Check if riaknostic is installed"),
    {ok, RiaknosticOut1} = rt:admin(Node, ["diag"]),
    riaknostic_install((rt:str(RiaknosticOut1, "is not present!")), Node).

%% riaknostic is already installed, move along
riaknostic_install(false, _Node) ->
    ok;

%% install riaknostic
riaknostic_install(true, Node) ->
    %% Install
    lager:info("Installing Riaknostic"),
    {ok, LibDir} = rpc:call(Node, application, get_env, [riak_core, platform_lib_dir]),
    Cmd = io_lib:format("sh -c \"cd ~s && curl -O -L ~s && tar xzf ~s\"",
                        [LibDir, ?RIAKNOSTIC_URL, filename:basename(?RIAKNOSTIC_URL)]),
    lager:info("Running command: ~s", [Cmd]),
    lager:debug("~p~n", [rpc:call(Node, os, cmd, [Cmd])]),
    ok.

%% Check that riaknostic executes
check_riaknostic_execute(Node) ->
    %% Execute
    lager:info("**  Check Riaknostic executes"),
    {ok, RiaknosticOut} = rt:admin(Node, ["diag"]),
    ?assertNot(rt:str(RiaknosticOut, "is not present!")),
    ?assertNot(rt:str(RiaknosticOut, "[debug]")),
    ok.

%% Check that riaknostic gives a usage message
check_riaknostic_usage(Node) ->
    %% Check usage message
    lager:info("**  Run Riaknostic usage message"),
    {ok, RiaknosticOut} = rt:admin(Node, ["diag", "--help"]),
    ?assert(rt:str(RiaknosticOut, "Usage: riak-admin")),
    ok.

%% Check that riaknostic gives a command listing
check_riaknostic_command_list(Node) ->
    %% Check commands list
    lager:info("**  Run Riaknostic commands list message"),
    {ok, RiaknosticOut} = rt:admin(Node, ["diag", "--list"]),
    ?assert(rt:str(RiaknosticOut, "Available diagnostic checks")),
    ?assert(rt:str(RiaknosticOut, "  disk           ")),
    ?assert(rt:str(RiaknosticOut, "  dumps          ")),
    ?assert(rt:str(RiaknosticOut, "  memory_use     ")),
    ?assert(rt:str(RiaknosticOut, "  nodes_connected")),
    ?assert(rt:str(RiaknosticOut, "  ring_membership")),
    ?assert(rt:str(RiaknosticOut, "  ring_preflists ")),
    ?assert(rt:str(RiaknosticOut, "  ring_size      ")),
    ?assert(rt:str(RiaknosticOut, "  search         ")),
    ok.

%% Check that log levels can be set
check_riaknostic_log_levels(Node) ->
    %% Check log levels
    lager:info("**  Run Riaknostic with a different log level"),
    {ok, RiaknosticOut} = rt:admin(Node, ["diag", "--level", "debug"]),
    ?assert(rt:str(RiaknosticOut, "[debug]")),
    ok.

%% Check that a connection failure message is output if node is stopped
check_riaknostic_conn_failure(Node) ->
    %% Check node conn failure when stopped
    lager:info("**  Riaknostic warns of node connection failure when stopped"),
    rt:stop_and_wait(Node),
    {ok, RiaknosticOut} = rt:admin(Node, ["diag"]),
    ?assert(rt:str(RiaknosticOut, "[warning] Could not connect")),
    ok.
