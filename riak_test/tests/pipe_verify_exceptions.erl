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
%% @doc Verify some exceptional cases in riak_pipe.
%%
%% Important: this test loads this module and {@link rt_pipe} on each
%% Riak node, such that it can reference their functions in pipe
%% workers.
%%
%% These tests used to be known as riak_pipe:exception_test_/0.

-module(pipe_verify_exceptions).

-export([
         %% riak_test's entry
         confirm/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

-define(NODE_COUNT, 3).

-define(ERR_LOG, [{log, sink}, {trace, [error]}]).
-define(ALL_LOG, [{log, sink}, {trace, all}]).

%% @doc riak_test callback
confirm() ->
    lager:info("Build ~b node cluster", [?NODE_COUNT]),
    Nodes = rt:build_cluster(?NODE_COUNT),

    rt:load_modules_on_nodes([?MODULE, rt_pipe], Nodes),

    verify_xbad1(Nodes),
    verify_xbad2(Nodes),
    verify_tail_worker_crash(Nodes),
    verify_vnode_crash(Nodes),
    verify_head_fitting_crash(Nodes),
    verify_middle_fitting_normal(Nodes),
    verify_middle_fitting_crash(Nodes),
    verify_tail_fitting_crash(Nodes),
    verify_worker_init_exit(Nodes),
    verify_worker_init_badreturn(Nodes),
    verify_worker_limit_one(Nodes),
    verify_worker_limit_multiple(Nodes),
    verify_under_worker_limit_one(Nodes),
    verify_queue_limit(Nodes),
    verify_vnode_death(Nodes),
    verify_restart_after_eoi(Nodes),

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

%%% TESTS

xbad1(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, [1, 2, 3]),
    ok = riak_pipe:queue_work(Pipe, [4, 5, 6]),
    ok = riak_pipe:queue_work(Pipe, [7, 8, bummer]),
    ok = riak_pipe:queue_work(Pipe, [10, 11, 12]),
    riak_pipe:eoi(Pipe).

verify_xbad1([RN|_]) ->
    lager:info("Verify correct error message from worker (xbad1)"),

    {eoi, Res, Trace} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun lists:sum/1, fun xbad1/1, ?ERR_LOG, 1]),

    %% three of the workers will succeed (the ones that receive only
    %% integers in their lists)
    ?assertMatch([{_, 6}, {_, 15}, {_, 33}], lists:sort(Res)),

    %% the one that received the atom 'bummer' will fail with a
    %% 'badarith' error
    [{_, {trace, [error], {error, Ps}}}] = Trace,
    ?assertEqual(error, proplists:get_value(type, Ps)),
    ?assertEqual(badarith, proplists:get_value(error, Ps)),
    ?assertEqual([7, 8, bummer], proplists:get_value(input, Ps)).

xbad2(Pipe) ->
    [ok = riak_pipe:queue_work(Pipe, N) || N <- lists:seq(0,2)],
    ok = riak_pipe:queue_work(Pipe, 500),
    exit({success_so_far, riak_pipe:collect_results(Pipe, 100)}).

verify_xbad2([RN|_]) ->
    lager:info("Verify work done before crashing without eoi (xbad2)"),

    %% we get a badrpc because the code exits, but it includes the
    %% test data we want
    {badrpc, {'EXIT', {success_so_far, {timeout, Res, Trace}}}} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1, fun xbad2/1, ?ERR_LOG, 3]),

    %% 3 fittings, send 0, 1, 2, 500; crash before eoi
    ?assertMatch([{_, 497}], Res),
    ?assertEqual(3, length(rt_pipe:extract_trace_errors(Trace))).

tail_worker_crash(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 100),
    timer:sleep(100),
    ok = riak_pipe:queue_work(Pipe, 1),
    riak_pipe:eoi(Pipe).

verify_tail_worker_crash([RN|_]) ->
    lager:info("Verify work done before tail worker crash"),

    {eoi, Res, Trace} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1,
                  fun tail_worker_crash/1,
                  ?ERR_LOG,
                  2]),

    %% first input is 100
    %% first worker decrements & passes on 99
    %% second worker decrements & passes on 98
    ?assertMatch([{_, 98}], Res),
    
    %% second input is 1
    %% first worker decrements & passes on 0
    %% second worker decrements & explodes
    ?assertEqual(1, length(rt_pipe:extract_trace_errors(Trace))).

vnode_crash(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 100),
    %% give time for input to be processed
    timer:sleep(100),
    rt_pipe:kill_all_pipe_vnodes(),
    %% give time for vnodes to actually die
    timer:sleep(100),
    riak_pipe:eoi(Pipe).

verify_vnode_crash([RN|_]) ->
    lager:info("Verify eoi still flows through after vnodes crash"),
    {eoi, Res, Trace} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1,
                  fun vnode_crash/1,
                  ?ERR_LOG,
                  2]),
    ?assertMatch([{_, 98}], Res),
    ?assertEqual(0, length(rt_pipe:extract_trace_errors(Trace))).

head_fitting_crash(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, [1, 2, 3]),
    [{_, Head}|_] = Pipe#pipe.fittings,
    rt_pipe:crash_fitting(Head),
    {error, [worker_startup_failed]} =
        riak_pipe:queue_work(Pipe, [4, 5, 6]),
    %% Again, just for fun ... still fails
    {error, [worker_startup_failed]} =
        riak_pipe:queue_work(Pipe, [4, 5, 6]),
    exit({success_so_far, riak_pipe:collect_results(Pipe, 100)}).

verify_head_fitting_crash([RN|_]) ->
    lager:info("Verify errors during head fitting crash"),

    %% we get a badrpc because the code exits, but it includes the
    %% test data we want
    {badrpc, {'EXIT', {success_so_far, {timeout, Res, Trace}}}} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun lists:sum/1, fun head_fitting_crash/1, ?ERR_LOG, 1]),

    %% the first input, [1,2,3], gets through
    ?assertMatch([{_, 6}], Res),
    %% but we get an exit trace, and no more outputs afterward
    ?assertEqual(1, length(rt_pipe:extract_fitting_died_errors(Trace))).

middle_fitting_normal(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 20),
    timer:sleep(100),
    FittingPids = [ P || {_, #fitting{pid=P}} <- Pipe#pipe.fittings],

    %% exercise riak_pipe_fitting:workers/1. There's a single worker
    %% on vnode 0, because riak_pipe:generic_transform uses
    %% chash=riak_pipe:zero_fun
    {ok,[0]} = riak_pipe_fitting:workers(hd(FittingPids)),

    %% send fitting bogus messages - fitting should ignore because
    %% they're not known
    gen_fsm:send_event(hd(FittingPids), bogus_message),
    {error, unknown} =
        gen_fsm:sync_send_event(hd(FittingPids), bogus_message),
    gen_fsm:sync_send_all_state_event(hd(FittingPids), bogus_message),
    hd(FittingPids) ! bogus_message,

    %% send bogus done message - fitting should ignore it because
    %% 'asdf' is not a working vnode pid
    [{_, Head}|_] = Pipe#pipe.fittings,
    MyRef = Head#fitting.ref,
    ok = gen_fsm:sync_send_event(hd(FittingPids), {done, MyRef, asdf}),

    %% kill fittings in the middle
    Third = lists:nth(3, FittingPids),
    rt_pipe:crash_fitting(Third,  fun() -> exit(normal) end),
    Fourth = lists:nth(4, FittingPids),
    rt_pipe:crash_fitting(Fourth, fun() -> exit(normal) end),

    %% This message will be lost in the middle of the
    %% pipe, but we'll be able to notice it via
    %% extract_trace_errors/1.
    ok = riak_pipe:queue_work(Pipe, 30),
    exit({success_so_far, riak_pipe:collect_results(Pipe, 100)}).

verify_middle_fitting_normal([RN|_]) ->
    lager:info("Verify middle fitting normal"),

    {badrpc, {'EXIT', {success_so_far, {timeout, Res, Trace}}}} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1,
                  fun middle_fitting_normal/1,
                  ?ERR_LOG,
                  5]),

    %% first input of 20 should have made it to the end, decremented
    %% by 1 at each of 5 fittings
    ?assertMatch([{_, 15}], Res),

    %% fittings 3 and 4 were killed
    ?assertEqual(2, length(rt_pipe:extract_fitting_died_errors(Trace))),

    %% second input, of 30, should generate errors as it reaches the
    %% killed third fitting
    ?assertEqual(1, length(rt_pipe:extract_trace_errors(Trace))).

middle_fitting_crash(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 20),
    %% wait for input to likely be processed
    timer:sleep(100),

    %% watch the builder to avoid a race later
    Builder = Pipe#pipe.builder,
    BuilderMonitor = erlang:monitor(process, Builder),

    FittingPids = [ P || {_, #fitting{pid=P}} <- Pipe#pipe.fittings ],
    Third = lists:nth(3, FittingPids),
    rt_pipe:crash_fitting(Third),
    Fourth = lists:nth(4, FittingPids),
    rt_pipe:crash_fitting(Fourth),

    %% avoid racing w/pipeline shutdown
    receive
        {'DOWN', BuilderMonitor, process, Builder, _} -> ok
    after 5000 ->
            lager:warning("timed out waiting for builder to exit"),
            demonitor(BuilderMonitor, [flush])
    end,

    %% first fitting should also be dead
    {error,[worker_startup_failed]} = riak_pipe:queue_work(Pipe, 30),
    %% this eoi will have no effect if the test is running correctly
    riak_pipe:eoi(Pipe),
    exit({success_so_far, riak_pipe:collect_results(Pipe, 100)}).

verify_middle_fitting_crash([RN|_]) ->
    lager:info("Verify pipe tears down when a fitting crashes (middle)"),

    {badrpc, {'EXIT', {success_so_far, {timeout, Res, Trace}}}} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1,
                  fun middle_fitting_crash/1,
                  ?ERR_LOG,
                  5]),

    %% first input, 20, makes it through, decremented once at each of
    %% five fittings
    ?assertMatch([{_, 15}], Res),

    %% all fittings die because their peers die
    ?assertEqual(5, length(rt_pipe:extract_fitting_died_errors(Trace))),

    %% no errors are generated, though, because the pipe is gone
    %% before the second input is sent
    ?assertEqual(0, length(rt_pipe:extract_trace_errors(Trace))).

%% TODO: It isn't clear to me if TailFittingCrash is
%% really any different than MiddleFittingCrash.  I'm
%% trying to exercise the patch in commit cb0447f3c46
%% but am not having much luck.  {sigh}
tail_fitting_crash(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 20),
    timer:sleep(100),
    FittingPids = [ P || {_, #fitting{pid=P}} <- Pipe#pipe.fittings ],
    Last = lists:last(FittingPids),
    rt_pipe:crash_fitting(Last),
    %% try to avoid racing w/pipeline shutdown
    timer:sleep(100),
    {error,[worker_startup_failed]} = riak_pipe:queue_work(Pipe, 30),
    riak_pipe:eoi(Pipe),
    exit({success_so_far, riak_pipe:collect_results(Pipe, 100)}).

verify_tail_fitting_crash([RN|_]) ->
    lager:info("Verify pipe tears down when a fitting crashes (tail)"),

    {badrpc, {'EXIT', {success_so_far, {timeout, Res, Trace}}}} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1,
                  fun tail_fitting_crash/1,
                  ?ERR_LOG,
                  5]),

    %% first input, 20, makes it through, decremented once at each of
    %% five fittings
    ?assertMatch([{_, 15}], Res),

    %% all fittings die because their peers die
    ?assertEqual(5, length(rt_pipe:extract_fitting_died_errors(Trace))),

    %% no errors are generated, though, because the pipe is gone
    %% before the second input is sent
    ?assertEqual(0, length(rt_pipe:extract_trace_errors(Trace))).

verify_worker_init_exit([RN|_]) ->
    lager:info("Verify error on worker startup failure (init_exit)"),
    Spec = [#fitting_spec{name="init crash",
                          module=riak_pipe_w_crash,
                          arg=init_exit,
                          chashfun=follow}],
    Opts = [{sink, rt_pipe:self_sink()}|?ERR_LOG],
    {ok, Pipe} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    {error, [worker_startup_failed]} =
        rpc:call(RN, riak_pipe, queue_work, [Pipe, x]),
    ok = riak_pipe:eoi(Pipe),
    ?assertEqual({eoi, [], []}, riak_pipe:collect_results(Pipe)).

verify_worker_init_badreturn([RN|_]) ->
    lager:info("Verify error on worker startup failure (init_badreturn)"),
    Spec = [#fitting_spec{name="init crash",
                          module=riak_pipe_w_crash,
                          arg=init_badreturn,
                          chashfun=follow}],
    Opts = [{sink, rt_pipe:self_sink()}|?ERR_LOG],
    {ok, Pipe} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    {error, [worker_startup_failed]} =
        rpc:call(RN, riak_pipe, queue_work, [Pipe, x]),
    ok = riak_pipe:eoi(Pipe),
    ?assertEqual({eoi, [], []}, riak_pipe:collect_results(Pipe)).

send_1_100(Pipe) ->
    ok = riak_pipe:queue_work(Pipe, 100),
    %% Sleep so that we don't have workers being shutdown before
    %% the above work item gets to the end of the pipe.
    timer:sleep(100),
    riak_pipe:eoi(Pipe).

verify_worker_limit_one([RN|_]) ->
    lager:info("Verify worker limit for one pipe"),
    PipeLen = 90,
    {eoi, Res, Trace} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun rt_pipe:decr_or_crash/1,
                  fun send_1_100/1,
                  ?ALL_LOG,
                  PipeLen]),
    ?assertEqual([], Res),
    Started = rt_pipe:extract_init_started(Trace),
    ?assertEqual(PipeLen, length(Started)),
    [Ps] = rt_pipe:extract_trace_errors(Trace), % exactly one error!
    ?assertEqual({badmatch,{error,[worker_limit_reached]}},
                 proplists:get_value(error, Ps)).

verify_worker_limit_multiple([RN|_]) ->
    lager:info("Verify worker limit for multiple pipes"),
    PipeLen = 90,
    Spec = lists:duplicate(
             PipeLen,
             #fitting_spec{name="worker limit mult pipes",
                           module=riak_pipe_w_xform,
                           arg=fun rt_pipe:xform_or_crash/3,
                           %% force all workers onto one vnode
                           chashfun={riak_pipe, zero_part}}),
    {ok, Pipe1} = rpc:call(RN, riak_pipe, exec,
                           [Spec, [{sink, rt_pipe:self_sink()}|?ALL_LOG]]),
    {ok, Pipe2} = rpc:call(RN, riak_pipe, exec,
                           [Spec, [{sink, rt_pipe:self_sink()}|?ALL_LOG]]),
    ok = rpc:call(RN, riak_pipe, queue_work, [Pipe1, 100]),
    %% plenty of time to start all workers
    timer:sleep(100),
    %% At worker limit, can't even start 1st worker @ Head2
    ?assertEqual({error, [worker_limit_reached]},
                 rpc:call(RN, riak_pipe, queue_work, [Pipe2, 100])),
    {timeout, [], Trace1} = riak_pipe:collect_results(Pipe1, 500),
    {timeout, [], Trace2} = riak_pipe:collect_results(Pipe2, 500),
    %% exactly one error: the 65th worker will fail to start
    ?assertMatch([_], rt_pipe:extract_trace_errors(Trace1)),
    ?assertEqual([], rt_pipe:extract_queued(Trace2)),
    %% cleanup before next test
    riak_pipe:destroy(Pipe1),
    riak_pipe:destroy(Pipe2).

verify_under_worker_limit_one([RN|_]) ->
    lager:info("Verify that many workers + many fittings still under limit"),
    
    %% 20 * Ring size > worker limit, if indeed the worker
    %% limit were enforced per node instead of per vnode.
    PipeLen = 20,
    Spec = lists:duplicate(
             PipeLen,
             #fitting_spec{name="foo",
                           module=riak_pipe_w_xform,
                           arg=fun rt_pipe:xform_or_crash/3}),
    {ok, Pipe1} = rpc:call(RN, riak_pipe, exec,
                           [Spec, [{sink, rt_pipe:self_sink()}|?ALL_LOG]]),
    [ok = rpc:call(RN, riak_pipe, queue_work, [Pipe1, X]) ||
        X <- lists:seq(101, 200)],
    riak_pipe:eoi(Pipe1),
    {eoi, Res, Trace1} = riak_pipe:collect_results(Pipe1, 500),
    %% all inputs make it through
    ?assertEqual(100, length(Res)),
    %% no errors
    ?assertEqual([], rt_pipe:extract_trace_errors(Trace1)).

sleep1fun(X) ->
    timer:sleep(1),
    X.

send_100_100(Pipe) ->
    [ok = riak_pipe:queue_work(Pipe, 100) ||
        _ <- lists:seq(1,100)],
    %% Sleep so that we don't have workers being shutdown before
    %% the above work item gets to the end of the pipe.
    timer:sleep(100),
    riak_pipe:eoi(Pipe).

verify_queue_limit([RN|_]) ->
    lager:info("Verify queue size limits are enforced"),
    verify_queue_limit(RN, 10).

verify_queue_limit(RN, Retries) when Retries > 0 ->
    {eoi, Res, Trace} =
        rpc:call(RN, riak_pipe, generic_transform,
                 [fun sleep1fun/1,
                  fun send_100_100/1,
                  ?ALL_LOG, 1]),

    %% all inputs make it through, after blocking
    ?assertEqual(100, length(Res)),

    %% we get as many unblocking messages as blocking messages
    Full = length(rt_pipe:extract_queue_full(Trace)),
    NoLongerFull = length(rt_pipe:extract_unblocking(Trace)),
    ?assertEqual(Full, NoLongerFull),
    
    case Full of
        [] ->
            lager:info("Queues were never full; Retries left: ~b",
                       [Retries-1]);
        _ ->
            ok
    end;
verify_queue_limit(_, _) ->
    lager:warning("Queues were never full; Consider re-running.").

verify_vnode_death([RN|_]) ->
    lager:info("Verify a vnode death does not kill the pipe"),

    {ok, Pipe} =
        rpc:call(RN, riak_pipe, exec,
                 [[#fitting_spec{name=vnode_death_test,
                                 module=riak_pipe_w_crash}],
                  [{sink, rt_pipe:self_sink()}]]),
    %% this should kill vnode such that it never
    %% responds to the enqueue request
    rpc:call(RN, riak_pipe, queue_work, [Pipe, vnode_killer]),
    riak_pipe:eoi(Pipe),
    {eoi, Res, []} = riak_pipe:collect_results(Pipe),
    ?assertEqual([], Res).

%% workers restarted because of recursive inputs should
%% not increase the "fail" counter
%%
%% methodology: send an input to partition A and
%% imediately send eoi; have A send a recursive input to
%% partition B; have B send a recursive input to C;
%% finally have C send a recursive in put back to A
%%
%% this flow should give worker A time to start shutting
%% down, but not to finish, resulting in an input in its
%% queue after it completes its done/1 function
verify_restart_after_eoi([RN|_]) ->
    lager:info("Verify worker restart via recursive inputs after eoi"),

    Inputs = [0, 1, 2, 0],
    ChashFun = fun([Head|_]) ->
                       chash:key_of(Head)
               end,
    Spec = [#fitting_spec{name=restarter,
                          module=riak_pipe_w_crash,
                          arg={recurse_done_pause, 500},
                          chashfun=ChashFun}],

    %% just make sure we are bouncing between partitions
    {ok, R} = rpc:call(RN, riak_core_ring_manager, get_my_ring, []),
    ?assert(riak_core_ring:preflist(
              ChashFun(Inputs), R) /=
                riak_core_ring:preflist(
                  ChashFun(tl(Inputs)), R)),
    ?assert(riak_core_ring:preflist(
              ChashFun(Inputs), R) /=
                riak_core_ring:preflist(
                  ChashFun(tl(tl(Inputs))), R)),

    {ok, Pipe} =
        rpc:call(RN, riak_pipe, exec,
                 [Spec,
                  [{sink, rt_pipe:self_sink()},
                   {log, sink},
                   {trace, [error, done, restart]}]]),
    ok = rpc:call(RN, riak_pipe, queue_work, [Pipe, Inputs]),
    riak_pipe:eoi(Pipe),
    {eoi, [], Trace} = riak_pipe:collect_results(Pipe),

    %% no error traces -- the error will say
    %% {reason, normal} if the worker received new
    %% inputs while shutting down due to eoi
    ?assertEqual([], rt_pipe:extract_trace_errors(Trace)),

    %% A should have restarted, but not counted failure
    [Restarted] = rt_pipe:extract_restart(Trace),
    Dones = rt_pipe:extract_vnode_done(Trace),
    RestartStats = proplists:get_value(Restarted, Dones),
    ?assertEqual(0, proplists:get_value(failures, RestartStats)).
