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
-module(verify_commit_hooks).
-include_lib("eunit/include/eunit.hrl").
-behavior(riak_test).
-export([confirm/0]).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    lager:info("Loading the hooks module into ~p", [Node]),
    rt:load_modules_on_nodes([hooks], [Node]),

    lager:info("Setting pid of test (~p) in application environment of ~p for postcommit hook", [self(), Node]),
    ?assertEqual(ok, rpc:call(Node, application, set_env, [riak_test, test_pid, self()])),

    lager:info("Installing commit hooks on ~p", [Node]),
    ?assertEqual(ok, rpc:call(Node, hooks, set_hooks, [])),

    lager:info("Checking precommit atom failure reason."),
    HTTP = rt:httpc(Node),
    ?assertMatch({error, {ok, "500", _, _}},
                 rt:httpc_write(HTTP, <<"failatom">>, <<"key">>, <<"value">>)),

    lager:info("Checking Bug 1145 - string failure reason"),
    ?assertMatch({error, {ok, "403", _, _}},
                 rt:httpc_write(HTTP, <<"failstr">>, <<"key">>, <<"value">>)),

    lager:info("Checking Bug 1145 - binary failure reason"),
    ?assertMatch({error, {ok, "403", _, _}},
                 rt:httpc_write(HTTP, <<"failbin">>, <<"key">>, <<"value">>)),

    lager:info("Checking that bucket without commit hooks passes."),
    ?assertEqual(ok, rt:httpc_write(HTTP, <<"fail">>, <<"key">>, <<"value">>)),

    lager:info("Checking that bucket with passing precommit passes."),
    ?assertEqual(ok, rt:httpc_write(HTTP, <<"failkey">>, <<"key">>, <<"value">>)),

    lager:info("Checking that bucket with failing precommit fails."),
    ?assertMatch({error, {ok, "403", _, _}},
                 rt:httpc_write(HTTP, <<"failkey">>, <<"fail">>, <<"value">>)),

    lager:info("Checking fix for BZ1244 - riak_kv_wm_object makes call to riak_client:get/3 with invalid type for key"),
    ?assertMatch({error, {ok, "201", _, _}},
                 rt:httpc_write(HTTP, <<"bz1244bucket">>, undefined, <<"value">>)),

    lager:info("Checking that postcommit fires."),
    ?assertMatch(ok, rt:httpc_write(HTTP, <<"postcommit">>, <<"key">>, <<"value">>)),

    receive
        {wrote, _Bucket, _Key}=Msg ->
            ?assertEqual({wrote, <<"postcommit">>, <<"key">>}, Msg),
            pass
    after 2000
        ->
            lager:error("Postcommit did not send a message within 2 seconds!"),
            ?assert(false)
    end.
