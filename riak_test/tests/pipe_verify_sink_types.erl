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
%% @doc Verify different riak_pipe sink types.
%%
%% These tests used to be known as riak_pipe:sink_types_test_/0

-module(pipe_verify_sink_types).

-export([
         %% riak_test's entry
         confirm/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

-define(NODE_COUNT, 3).
-define(ALL_LOG, [{log, sink}, {trace, all}]).

%% @doc riak_test callback
confirm() ->
    lager:info("Build ~b node cluster", [?NODE_COUNT]),
    Nodes = rt:build_cluster(?NODE_COUNT),

    verify_raw(Nodes),
    verify_fsm(Nodes),
    verify_fsm_timeout(Nodes),
    verify_fsm_sync_period(Nodes),
    verify_fsm_infinity_sync_period(Nodes),
    verify_invalid_type(Nodes),

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

%%% TESTS

%% @doc the basics: 'raw' is the default with nothing specified (so
%% all other tests should have covered it), but try specifying it
%% explicitly here
verify_raw([RN|_]) ->
    lager:info("Verify explicit 'raw' sink type"),
    Spec = [#fitting_spec{name=r,
                          module=riak_pipe_w_pass}],
    Opts = [{sink_type, raw},{sink, rt_pipe:self_sink()}],
    {ok, P} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    rpc:call(RN, riak_pipe, queue_work, [P, 1]),
    riak_pipe:eoi(P),
    Result = riak_pipe:collect_results(P, 1000),
    ?assertEqual({eoi, [{r, 1}], []}, Result).

%% @doc rt_pipe_test_sink *only* accepts results delivered as
%% gen_fsm events that are tagged as sync vs async
verify_fsm([RN|_]) ->
    lager:info("Verify 'fsm' sink type"),
    PipeRef = make_ref(),
    {ok, SinkPid} = rt_pipe_sink_fsm:start_link(PipeRef),
    Spec = [#fitting_spec{name=fs,
                          module=riak_pipe_w_pass}],
    Sink = #fitting{pid=SinkPid, ref=PipeRef},
    Opts = [{sink, Sink}, {sink_type, {fsm, 0, 5000}}],
    {ok, P} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    rpc:call(RN, riak_pipe, queue_work, [P, {sync, 1}]),
    riak_pipe:eoi(P),
    Result = rt_pipe_sink_fsm:get_results(SinkPid),
    ?assertEqual({eoi, [{fs, {sync, 1}}], []}, Result).

%% @doc purposefully disable acking one output, to trigger the timeout
%% on the gen_fsm:sync_send_event
verify_fsm_timeout([RN|_]) ->
    lager:info("Verify sink fsm timeout"),
    PipeRef = make_ref(),
    SinkOpts = [{skip_ack, [{fst,{sync, 2}}]}],
    {ok, SinkPid} = rt_pipe_sink_fsm:start_link(
                      PipeRef, SinkOpts),
    Spec = [#fitting_spec{name=fst,
                          module=riak_pipe_w_pass}],
    Sink = #fitting{pid=SinkPid, ref=PipeRef},
    Opts = [{log, sink},
            {trace, [error]},
            {sink, Sink},
            %% a very short timeout, to fit eunit
            {sink_type, {fsm, 0, 10}}],
    {ok, P} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    rpc:call(RN, riak_pipe, queue_work, [P, {sync, 1}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {sync, 2}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {sync, 3}]),
    riak_pipe:eoi(P),
    {eoi, Results, Logs} =
        rt_pipe_sink_fsm:get_results(SinkPid),

    %% make sure that all results did make it to the sink
    ?assertEqual([{fst, {sync, 1}}, {fst, {sync, 2}}, {fst, {sync, 3}}],
                 lists:sort(Results)),
    %% but that we also logged an error...
    [{fst,{trace,[error],{error,Props}}}] = Logs,
    %% ...about the input "2"...
    ?assertEqual({sync, 2},
                 proplists:get_value(input, Props)),
    %% ...timing out on its way to the sink
    ?assertEqual({badmatch,{error,timeout}},
                 proplists:get_value(error, Props)).

%% @doc make sure that the sink messages are sent synchronously on the
%% Period, and asynchronously otherwise
verify_fsm_sync_period([RN|_]) ->
    lager:info("Verify fsm sink sync period"),
    PipeRef = make_ref(),
    {ok, SinkPid} = rt_pipe_sink_fsm:start_link(PipeRef, []),
    %% force a single worker, to make it easy to test the sync period
    Spec = [#fitting_spec{name=fst,
                          module=riak_pipe_w_pass,
                          chashfun={riak_pipe, zero_part}}],
    Sink = #fitting{pid=SinkPid, ref=PipeRef},
    Opts = [{log, sink},
            {trace, [error]},
            {sink, Sink},
            {sink_type, {fsm, 2, 1000}}],
    {ok, P} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    rpc:call(RN, riak_pipe, queue_work, [P, {sync, 1}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {async, 2}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {async, 3}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {sync, 4}]),
    riak_pipe:eoi(P),
    {eoi, Results, []} =
        rt_pipe_sink_fsm:get_results(SinkPid),

    %% make sure that all results did make it to the sink
    %% ('async' sorts before 'sync')
    ?assertEqual([{fst, {async, 2}},
                  {fst, {async, 3}},
                  {fst, {sync, 1}},
                  {fst, {sync, 4}}],
                 lists:sort(Results)).

%% @doc infinite period means sink results are always delivered
%% asynchronously
verify_fsm_infinity_sync_period([RN|_]) ->
    PipeRef = make_ref(),
    {ok, SinkPid} = rt_pipe_sink_fsm:start_link(PipeRef, []),
    %% force a single worker, to make it easy to test the sync period
    Spec = [#fitting_spec{name=fst,
                          module=riak_pipe_w_pass,
                          chashfun={riak_pipe, zero_part}}],
    Sink = #fitting{pid=SinkPid, ref=PipeRef},
    Opts = [{log, sink},
            {trace, [error]},
            {sink, Sink},
            {sink_type, {fsm, infinity, 1000}}],
    {ok, P} = rpc:call(RN, riak_pipe, exec, [Spec, Opts]),
    rpc:call(RN, riak_pipe, queue_work, [P, {async, 1}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {async, 2}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {async, 3}]),
    rpc:call(RN, riak_pipe, queue_work, [P, {async, 4}]),
    riak_pipe:eoi(P),
    {eoi, Results, []} =
        rt_pipe_sink_fsm:get_results(SinkPid),

    %% make sure that all results did make it to the sink
    ?assertEqual([{fst, {async, 1}},
                  {fst, {async, 2}},
                  {fst, {async, 3}},
                  {fst, {async, 4}}],
                 lists:sort(Results)).

%% @doc ensure behavior is predictable when an unknown sink type is
%% specified
verify_invalid_type([RN|_]) ->
    Spec = [#fitting_spec{module=riak_pipe_w_pass}],
    case rpc:call(RN, riak_pipe, exec,
                  [Spec, [{sink_type, invalid}]]) of
        {invalid_sink_type, {sink_type, invalid}} ->
            %% hooray! the correct error
            ok;
        {ok, P} ->
            %% if we made it here, the test failed; kill the pipe and
            %% blow up
            riak_pipe:destroy(P),
            ?assert(false)
    end.
