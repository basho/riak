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
%% @doc Verify that the riak_pipe example use functions work.

-module(pipe_verify_examples).

-export([confirm/0]).

-include_lib("eunit/include/eunit.hrl").

-define(NODE_COUNT, 3).

confirm() ->
    lager:info("Build ~b node cluster", [?NODE_COUNT]),
    Nodes = rt:build_cluster(?NODE_COUNT),
    
    verify_example(Nodes),
    verify_example_transform(Nodes),
    verify_example_reduce(Nodes),

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

verify_example([RN|_]) ->
    lager:info("Run riak_pipe:example/0"),
    ?assertMatch({eoi, [{empty_pass, "hello"}], _Trc},
                 rpc:call(RN, riak_pipe, example, [])).

verify_example_transform([RN|_]) ->
    lager:info("Run riak_pipe:example_transform/0"),
    ?assertEqual({eoi, [{"generic transform", 55}], []},
                 rpc:call(RN, riak_pipe, example_transform, [])).

verify_example_reduce([RN|_]) ->
    lager:info("Run riak_pipe:example_reduce/0"),
    {eoi, Res, []} = rpc:call(RN, riak_pipe, example_reduce, []),
    ?assertEqual([{"sum reduce", {a, [55]}},
                  {"sum reduce", {b, [155]}}],
                 lists:sort(Res)).
