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
%% @doc Verify handoff between riak_pipe vnodes.
%%
%% Important: this test loads this module and {@link rt_pipe} on each
%% Riak node, such that it can reference their functions in pipe
%% workers.
%%
%% This test is a largely rewritten version of riak_pipe:limits_test_/0.
%%
%% Strategy:
%%   1. start one node
%%   2. start pipes on that node
%%   3. put inputs in the pipe
%%      3a. use a worker that waits for a signal from the test process
%%      before processing its input, so that we can ensure active
%%      workers and queue contents
%%   4. start and join second node
%%   5. wait for agreement on owners
%%   6. add more inputs, to start some workers on second node
%%   7. give the signal to the workers to process things
%%   8. count archive commands/etc.

-module(pipe_verify_handoff).

-export([
         %% riak_test's entry
         confirm/0,
         
         %% test machinery
         runner_wait/1,
         collector/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

-define(NODE_COUNT, 2).
-define(ALL_LOG, [{log, sink}, {trace, all}]).

%% @doc riak_test callback
confirm() ->
    lager:info("Start ~b nodes", [?NODE_COUNT]),
    NodeDefs = lists:duplicate(?NODE_COUNT, {current, default}),
    Services = [riak_pipe],
    [Primary,Secondary] = Nodes = rt:deploy_nodes(NodeDefs, Services),
    %% Ensure each node owns 100% of it's own ring
    [?assertEqual([Node], rt:owners_according_to(Node)) || Node <- Nodes],

    lager:info("Load useful modules"),
    rt:load_modules_on_nodes([?MODULE, rt_pipe], Nodes),

    lager:info("Start run coordinator"),
    Runner = spawn_link(?MODULE, runner_wait, [[]]),

    P1Spec = [#fitting_spec{name="p1handoff",
                            module=riak_pipe_w_xform,
                            arg=pause_until_signal(Runner)}],
    P2Spec = [#fitting_spec{name="p2handoff",
                            module=riak_pipe_w_xform,
                            arg=pause_until_signal(Runner)}],

    lager:info("Start two pipes on Primary"),
    {ok, Pipe1} =
        rpc:call(Primary, riak_pipe, exec,
                 [P1Spec, [{sink, rt_pipe:self_sink()}|?ALL_LOG]]),
    {ok, Pipe2} =
        rpc:call(Primary, riak_pipe, exec,
                 [P2Spec, [{sink, rt_pipe:self_sink()}|?ALL_LOG]]),

    lager:info("Send some inputs to both pipes"),
    [ok = rpc:call(Primary, riak_pipe, queue_work, [Pipe1, X]) ||
        X <- lists:seq(1, 20)],
    [ok = rpc:call(Primary, riak_pipe, queue_work, [Pipe2, X]) ||
        X <- lists:seq(101, 120)],

    P1Status1 = pipe_status(Primary, Pipe1),
    P2Status1 = pipe_status(Primary, Pipe2),

    lager:info("Start and register intercept log collector"),
    Collector = spawn_link(Primary, ?MODULE, collector, []),
    rpc:call(Primary, erlang, register, [riak_test_collector, Collector]),

    lager:info("Install pipe vnode intercept"),
    Intercept = {riak_pipe_vnode,
                 [{{handle_handoff_command,3}, log_handoff_command}]},
    ok = rt_intercept:add(Primary, Intercept),

    lager:info("Join Secondary to Primary"),
    %% Give slave a chance to start and master to notice it.
    rt:join(Secondary, Primary),
    rt:wait_until_no_pending_changes(Nodes),
    rt:wait_until_nodes_agree_about_ownership(Nodes),

    lager:info("Unpause workers"),
    Runner ! go,

    ok = rt:wait_until_transfers_complete(Nodes),

    lager:info("Add more inputs to Pipe2"),
    [ok = rpc:call(Primary, riak_pipe, queue_work, [Pipe2, X]) ||
        X <- lists:seq(121, 140)],

    %% transfers completing takes so long that the pipe is extremely
    %% likely to have finished all of its inputs by now

    P1Status2 = pipe_status(Primary, Pipe1),
    P2Status2 = pipe_status(Primary, Pipe2),

    lager:info("Send eoi and collect results"),
    riak_pipe:eoi(Pipe1),
    riak_pipe:eoi(Pipe2),
    {eoi, Out1, Trace1} = riak_pipe:collect_results(Pipe1, 1000),
    {eoi, Out2, Trace2} = riak_pipe:collect_results(Pipe2, 1000),

    %% no errors on either pipe, all items make it through; if these
    %% are wrong, we dropped things somewhere
    ?assertEqual([], rt_pipe:extract_trace_errors(Trace1)),
    ?assertEqual(20, length(Out1)),
    ?assertEqual([], rt_pipe:extract_trace_errors(Trace2)),
    ?assertEqual(40, length(Out2)),

    %% VM trace verification
    timer:sleep(1000),
    lager:info("Collect intercept log"),
    PTraces = get_collection(Collector),

    %% time to compare things

    P1PrimaryWorkers1 = partitions_on_node(Primary, P1Status1),
    P1SecondaryWorkers2 = partitions_on_node(Secondary, P1Status2),
    P2PrimaryWorkers1 = partitions_on_node(Primary, P2Status1),
    P2SecondaryWorkers2 = partitions_on_node(Secondary, P2Status2),

    %% workers moved
    P1MovedPrimaryToSecondary = ordsets:intersection(
                                  ordsets:from_list(P1PrimaryWorkers1),
                                  ordsets:from_list(P1SecondaryWorkers2)),
    P2MovedPrimaryToSecondary = ordsets:intersection(
                                  ordsets:from_list(P2PrimaryWorkers1),
                                  ordsets:from_list(P2SecondaryWorkers2)),
    %% vnodes moved
    AllMovedPrimaryToSecondary = ordsets:union(
                                   P1MovedPrimaryToSecondary,
                                   P2MovedPrimaryToSecondary),

    PFoldReqs = [X || riak_core_fold_req_v1=X <- PTraces],
    PArchives = [X || cmd_archive=X <- PTraces],

    %% number of active vnodes migrating from Primary to Secondary,
    %% should be one fold per move, otherwise inputs were directed
    %% incorrectly after transfers settled
    ?assertEqual(length(AllMovedPrimaryToSecondary),
                 length(PFoldReqs)),

    %% number of workers migrating from Secondary to Primary, should
    %% be one archive per move, otherwise inputs were directed
    %% incorrectly after transfers settled
    ?assertEqual(length(P1MovedPrimaryToSecondary)
                 +length(P2MovedPrimaryToSecondary),
                 length(PArchives)),

    case ordsets:intersection(ordsets:from_list(P1PrimaryWorkers1),
                              ordsets:from_list(P2PrimaryWorkers1)) of
        [] ->
            lager:warning("Multiple archives in a single fold was not tested");
        _ ->
            ok
    end,

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

%%% Run pausing bits

%% @doc Create a worker function that asks the specified process for
%% permission before sending its input as output.
pause_until_signal(Runner) ->
  fun(I, P, D) ->
          Runner ! {wait, self()},
          receive go ->
                  riak_pipe_vnode_worker:send_output(I, P, D)
          end
  end.

%% @doc Phase one of worker-pausing process: just collect requests,
%% waiting for overall signal to allow processing to happen.
runner_wait(Waiting) ->
    receive
        go ->
            [ W ! go || W <- Waiting ],
            runner_go();
        {wait, W} ->
            runner_wait([W|Waiting])
    end.

%% @doc Phase two of worker-pausing process: just let workers do their
%% processing as soon as they ask.
runner_go() ->
    receive
        {wait, W} ->
            W ! go,
            runner_go()
    end.

%%% Status filtering bits

%% @doc Dig through a riak_pipe:status/1 response to determine which
%% partitions are on the given node.
partitions_on_node(Node, PipeStatus) ->
    [proplists:get_value(partition, W)
     || W <- PipeStatus,
        Node == proplists:get_value(node, W)].

%% @doc Call riak_pipe:status/1 on the given node, and extract the
%% status list from it. It is expected that the given pipe has exactly
%% one fitting.
pipe_status(Node, Pipe) ->
    [{_Name, Status}] = rpc:call(Node, riak_pipe, status, [Pipe]),
    Status.

%% @doc entry point for collector process
collector() ->
    collector([]).
collector(Acc) ->
    receive
        {send_collection, Ref, Pid} ->
            Pid ! {collection, Ref, lists:reverse(Acc)};
        Any ->
            collector([Any|Acc])
    end.

get_collection(Collector) ->
    Ref = make_ref(),
    Collector ! {send_collection, Ref, self()},
    receive {collection, Ref, Collection} ->
            Collection
    end.
