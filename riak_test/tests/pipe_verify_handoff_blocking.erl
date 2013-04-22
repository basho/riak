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
%% @doc Verify handoff of the blocking queue between riak_pipe vnodes.
%%
%% Important: this test loads this module and {@link rt_pipe} on each
%% Riak node, such that it can reference their functions in pipe
%% workers.
%%
%% This test is similar to pipe_verify_handoff, but specifically tests
%% handoff while input-senders are waiting for responses. Mostly this
%% is tested in the case where the input is put into a pipe worker's
%% blocking queue, but because message timing is not tightly
%% controlled, it may also trigger other cases.
%%
%% While an input is in-flight to the vnode, or in a worker's blocking
%% queue, the sender of that input is waiting on a response, while
%% also monitoring the vnode. During handoff, the vnode that should
%% respond to the sender will change, but the monitoring does not,
%% currently. This is a bug.
%% 
%% The testing strategy is to block workers while filling their queues
%% to the point they begin blocking, then add a node to the cluster
%% and watch their handoff progress. The slightly tricky point is that
%% we have to allow workers to process some inputs, or they won't
%% handoff (because the vnode waits for them to be between inputs to
%% archive), but we don't want them to process so many inputs that
%% they consume their blocking queues before handing off.
-module(pipe_verify_handoff_blocking).

-export([
         %% riak_test's entry
         confirm/0,
         
         %% test machinery
         runner_wait/1,
         queue_filler/3
        ]).

-include_lib("eunit/include/eunit.hrl").

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

-define(NODE_COUNT, 2).
-define(ALL_LOG, [{log, sink}, {trace, all}]).
-define(FILLER_COUNT, 5).

%% @doc riak_test callback
confirm() ->
    %% static list of inputs, so we keep hitting the same partitions
    Inputs = lists:seq(1, 20),

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

    Spec = [#fitting_spec{name="blockhandoff",
                          module=riak_pipe_w_xform,
                          arg=pause_until_signal(Runner)}],

    lager:info("Start pipe on Primary"),
    {ok, Pipe} =
        rpc:call(Primary, riak_pipe, exec,
                 [Spec, [{sink, rt_pipe:self_sink()}|?ALL_LOG]]),

    InitialInputCount = fill_queues(Primary, Pipe, Inputs),
    Fillers = keep_queues_full(Primary, Pipe, Inputs),

    _Status1 = pipe_status(Primary, Pipe),

    lager:info("Join Secondary to Primary"),
    %% Give slave a chance to start and master to notice it.
    rt:join(Secondary, Primary),
    rt:wait_until_no_pending_changes(Nodes),
    rt:wait_until_nodes_agree_about_ownership(Nodes),

    lager:info("Unpause workers"),
    Runner ! go,

    ok = rt:wait_until_transfers_complete(Nodes),

    FillerInputCount = stop_fillers(Fillers),

    %% if we make it this far, then no filler ever saw the vnode_down
    %% error message; otherwise badmatches in queue_filler/4 will have
    %% halted the test

    _Status2 = pipe_status(Primary, Pipe),

    lager:info("Send eoi and collect results"),
    riak_pipe:eoi(Pipe),
    {eoi, Out, Trace} = riak_pipe:collect_results(Pipe, 1000),

    %% no errors on either pipe, all items make it through; if these
    %% are wrong, we dropped things somewhere
    ?assertEqual([], rt_pipe:extract_trace_errors(Trace)),
    ?assertEqual(InitialInputCount+FillerInputCount, length(Out)),

    rt_pipe:assert_no_zombies(Nodes),

    lager:info("~s: PASS", [atom_to_list(?MODULE)]),
    pass.

%%% queue filling

%% @doc fill pipe vnode queues by repeatedly sending each input in the
%% input list until the queue reports timeout.
fill_queues(Node, Pipe, Inputs) ->
    lists:sum([ fill_queue(Node, Pipe, I, 0) || I <- Inputs ]).

%% @doc fill one vnode queue by repeatedly sending the same input
%% until it reports timeout
fill_queue(Node, Pipe, Input, Count) ->
    case rpc:call(Node, riak_pipe, queue_work, [Pipe, Input, noblock]) of
        {error, [timeout]} ->
            %% This queue is now full
            Count;
        ok ->
            %% not full yet; add more
            fill_queue(Node, Pipe, Input, Count+1)
    end.

%% @doc spawn workers that will keep sending inputs to the pipe in
%% order to keep the queues full to test handoff of blocking queues
keep_queues_full(Node, Pipe, Inputs) ->
    [ spawn_link(?MODULE, queue_filler, [Node, Pipe, shuffle(Inputs)])
      || _ <- lists:seq(1, ?FILLER_COUNT) ].

%% @doc Send each element of Inputs into Pipe, until told not to
queue_filler(Node, Pipe, Inputs) ->
    %% putting Inputs in a queue means we don't have to track out
    %% progress through them separately
    queue_filler(Node, Pipe, queue:from_list(Inputs), 0).

queue_filler(Node, Pipe, Inputs, Count) ->
    receive
        {stop, Owner} -> Owner ! {done, Count}
    after 0 ->
            {{value, I}, Q} = queue:out(Inputs),
            ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, I]),
            queue_filler(Node, Pipe, queue:in(I, Q), Count+1)
    end.

%% @doc tell all fillers to stop and collect and sum their send counts
stop_fillers(Fillers) ->
    lists:sum([ receive {done, Count} -> Count end
                || _ <- [ F ! {stop, self()} || F <- Fillers ] ]).

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

%% @doc Call riak_pipe:status/1 on the given node, and extract the
%% status list from it. It is expected that the given pipe has exactly
%% one fitting.
pipe_status(Node, Pipe) ->
    [{_Name, Status}] = rpc:call(Node, riak_pipe, status, [Pipe]),
    Status.

%% @doc Shuffle the elements of a list. (Thanks Micah)
shuffle([]) ->
    [];
shuffle([E]) ->
    [E];
shuffle(List) ->
    Max = length(List),
    Keyed = [{random:uniform(Max), E} || E <- List],
    Sorted = lists:sort(Keyed),
    [N || {_, N} <- Sorted].
