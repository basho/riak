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
%% @doc Verify that inputs are forwarded properly if a pipe worker
%% fails to restart.
%%
%% Important: this test loads this module and {@link rt_pipe} on each
%% Riak node, such that it can reference their functions in pipe
%% workers.
%%
%% IMPORTANT: this test must be run on a ONE-node cluster, because
%% riak_pipe_w_crash uses ETS to determine a "restart" situation, and
%% sets the fitting process as the heir of the table, so it survives
%% the worker's restart
%%
%% These tests used to be a component of riak_pipe:exception_test_/0.

-module(pipe_verify_restart_input_forwarding).

-export([
         %% riak_test's entry
         confirm/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

%% must be 1 for verify_worker_restart_failure_input_forwarding
-define(NODE_COUNT, 1).

-define(ERR_LOG, [{log, sink}, {trace, [error]}]).
-define(ALL_LOG, [{log, sink}, {trace, all}]).

%% @doc riak_test callback
confirm() ->
    lager:info("Build ~b node cluster", [?NODE_COUNT]),
    Nodes = rt:build_cluster(?NODE_COUNT),

    rt:load_modules_on_nodes([?MODULE, rt_pipe], Nodes),

    verify_worker_restart_failure_input_forwarding(Nodes),

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

verify_worker_restart_failure_input_forwarding([RN]) ->
    lager:info("Verify input forwarding after worker restart failure"),

    %% make a worker fail, and then also fail to restart, and check
    %% that the input that killed it generates a processing error,
    %% while the inputs that were queued for it get sent to another
    %% vnode
    Spec = [#fitting_spec{name=restarter,
                          module=riak_pipe_w_crash,
                          arg=init_restartfail,
                          %% use nval=2 to get some failover
                          nval=2}],
    Opts = [{sink, rt_pipe:self_sink()},
            {log, sink},
            {trace,[error,restart,restart_fail,queue]}],
    {ok, Pipe} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),

    Inputs1 = lists:seq(0,127),
    Inputs2 = lists:seq(128,255),
    Inputs3 = lists:seq(256,383),

    %% sleep, send more inputs

    %% this should make one of the riak_pipe_w_crash workers die with
    %% unprocessed items in its queue, and then also deliver a few
    %% more inputs to that worker, which will be immediately
    %% redirected to an alternate vnode

    %% send many inputs, send crash, send more inputs
    [ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, N]) || N <- Inputs1],
    ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, init_restartfail]),
    [ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, N]) || N <- Inputs2],
    %% one worker should now have both the crashing input and a valid
    %% input following it waiting in its queue - the test is whether
    %% or not that valid input following the crash gets redirected
    %% correctly

    %% wait for the worker to crash, then send more input at it
    %% - the test is whether the new inputs are redirected correctly
    timer:sleep(2000),
    [ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, N]) || N <- Inputs3],

    %% flush the pipe
    ok = riak_pipe:eoi(Pipe),
    {eoi, Results, Trace} = riak_pipe:collect_results(Pipe),

    %% all results should have completed correctly
    ?assertEqual(length(Inputs1++Inputs2++Inputs3), length(Results)),

    %% There should be one trace errors:
    %% - the processing error (worker crash)
    Errors = rt_pipe:extract_trace_errors(Trace),
    ?assertEqual(1, length(Errors)),
    ?assert(is_list(hd(Errors))),
    ?assertMatch(init_restartfail, proplists:get_value(input, hd(Errors))),
    Restarter = proplists:get_value(partition, hd(Errors)),
    %% ... and also one message about the worker
    %% restart failure
    ?assertMatch([Restarter], rt_pipe:extract_restart_fail(Trace)),

    Queued = rt_pipe:extract_queued(Trace),

    %% find out who caught the restartfail
    Restarted = [ P || {P, init_restartfail} <- Queued ],
    ?assertMatch([Restarter], Restarted),

    %% what input arrived after the crashing input,
    %% but before the crash?
    {_PreCrashIn, PostCrashIn0} =
        lists:splitwith(fun is_integer/1,
                        [ I || {P,I} <- Queued, P == Restarter]),
    %% drop actual crash input
    PostCrashIn = tl(PostCrashIn0),
    %% make sure the input was actually enqueued
    %% before the crash (otherwise test was bogus)
    ?assert(length(PostCrashIn) > 0),

    %% so where did the post-crash inputs end up?
    ReQueued = lists:map(
                 fun(I) ->
                         Re = [ P || {P,X} <- Queued,
                                     X == I,
                                     P /= Restarter ],
                         ?assertMatch([_Part], Re),
                         hd(Re)
                 end,
                 PostCrashIn),
    ?assertMatch([_Requeuer], lists:usort(ReQueued)),
    [Requeuer|_] = ReQueued,

    %% finally, did the inputs destined for the crashed worker that
    %% were sent *after* the worker crashed, also get forwarded to the
    %% correct location?
    Destined = lists:filter(
                 fun(I) ->
                         [{P,_}] = rpc:call(RN, riak_core_apl, get_apl,
                                            [chash:key_of(I), 1, riak_pipe]),
                         P == Restarter
                 end,
                 Inputs3),
    Forwarded = lists:map(
                  fun(I) ->
                          [Part] = [P || {P,X} <- Queued, X == I],
                          Part
                  end,
                  Destined),
    ?assertMatch([_Forward], lists:usort(Forwarded)),
    [Forward|_] = Forwarded,

    %% consistent hash means this should be the same
    ?assertEqual(Requeuer, Forward).
