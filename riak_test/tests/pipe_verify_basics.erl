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
%% @doc Verify some basic things about riak_pipe.
%%
%% Important: this test loads this module on each Riak node, such that
%% it can reference its functions in pipe workers.
%%
%% These tests used to be known as riak_pipe:basic_test_/0.

-module(pipe_verify_basics).

-export([
         %% riak_test's entry
         confirm/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(NODE_COUNT, 3).

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

confirm() ->
    lager:info("Build ~b node cluster", [?NODE_COUNT]),
    Nodes = rt:build_cluster(?NODE_COUNT),

    rt:load_modules_on_nodes([?MODULE], Nodes),

    verify_order(Nodes),
    verify_trace_filtering(Nodes),
    verify_recursive_countdown_1(Nodes),
    verify_recursive_countdown_2(Nodes),

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

%% @doc generic driver used as a riak_pipe:generic_transform
%% argument. Sends the input '1', then sends eoi.
order_fun(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 1),
    riak_pipe:eoi(Pipe).

%% @doc generic driver used as a riak_pipe:generic_transform
%% argument. Causes a fitting to pass as output, its input multiplied
%% by two.
mult_by_2(X) ->
    2 * X.

verify_order([RN|_]) ->
    lager:info("Verifying fittings operate in order"),

    AllLog = [{log, sink}, {trace, all}],
    {eoi, Res, Trace} = 
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun mult_by_2/1, fun order_fun/1, AllLog, 5]),

    ?assertMatch([{_, 32}], Res),
    ?assertEqual(0, length(rt_pipe:extract_trace_errors(Trace))),
    Qed = rt_pipe:extract_queued(Trace),
    %% NOTE: The msg to the sink doesn't appear in Trace
    ?assertEqual([1,2,4,8,16], [X || {_, X} <- Qed]).

verify_trace_filtering([RN|_]) ->
    lager:info("Verify that trace messages are filtered"),
    {eoi, _Res, Trace1} = 
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun mult_by_2/1, fun order_fun/1,
                  [{log,sink}, {trace, [eoi]}], 5]),
    {eoi, _Res, Trace2} = 
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun mult_by_2/1, fun order_fun/1,
                  [{log,sink}, {trace, all}], 5]),
    %% Looking too deeply into the format of the trace
    %% messages, since they haven't gelled yet, is madness.
    ?assert(length(Trace1) < length(Trace2)).

verify_recursive_countdown_1([RN|_]) ->
    lager:info("Verify recurse_input"),
    Spec = [#fitting_spec{name=counter,
                          module=riak_pipe_w_rec_countdown}],
    Opts = [{sink, rt_pipe:self_sink()}],
    {ok, Pipe} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, 3]),
    riak_pipe:eoi(Pipe),
    {eoi, Res, []} = riak_pipe:collect_results(Pipe),
    ?assertEqual([{counter,0},{counter,1},{counter,2},{counter,3}], Res).

verify_recursive_countdown_2([RN|_]) ->
    lager:info("Verify nondeterministic recurse_input"),
    verify_recursive_countdown_2(RN, 10).

verify_recursive_countdown_2(RN, Retries) when Retries > 0 ->
    Spec = [#fitting_spec{name=counter,
                          module=riak_pipe_w_rec_countdown,
                          arg=testeoi}],
    Options = [{sink, rt_pipe:self_sink()},{trace,[restart]},{log,sink}],
    {ok, Pipe} = rpc:call(RN, riak_pipe, exec, [Spec, Options]),
    ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, 3]),
    riak_pipe:eoi(Pipe),
    {eoi, Res, Trc} = riak_pipe:collect_results(Pipe),
    ?assertEqual([{counter,0},{counter,0},{counter,0},
                  {counter,1},{counter,2},{counter,3}],
                 Res),
    case Trc of
        [{counter,{trace,[restart],{vnode,{restart,_}}}}] ->
            ok;
        [] ->
            lager:info("recursive countdown test #2 did not"
                       " trigger the done/eoi race it tests."
                       " Retries left: ~b", [Retries-1]),
            verify_recursive_countdown_2(RN, Retries-1)
    end;
verify_recursive_countdown_2(_, _) ->
    lager:warning("recursive countdown test #2 did not"
                  " trigger the done/eoi race it tests."
                  " Consider re-running.").
