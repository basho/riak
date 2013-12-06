%% @doc Utilities for riak_pipe tests.
-module(rt_pipe).

-compile(export_all).

%% local copy of riak_pipe.hrl
-include("rt_pipe.hrl").

-include_lib("eunit/include/eunit.hrl").

%% macro-ize repeated, ignored trace structure
-define(TM(Detail), {_, {trace, _, Detail}}).

extract_trace_errors(Trace) ->
    [Ps || ?TM({error, Ps}) <- Trace].

extract_fitting_died_errors(Trace) ->
    [X || ?TM({vnode, {fitting_died, _}} = X) <- Trace].

extract_queued(Trace) ->
    [{Partition, X} || ?TM({vnode, {queued, Partition, X}}) <- Trace].

extract_queue_full(Trace) ->
    [Partition || ?TM({vnode, {queue_full, Partition, _}}) <- Trace].

extract_unblocking(Trace) ->
    [Partition || ?TM({vnode, {unblocking, Partition}}) <- Trace].

extract_restart_fail(Trace) ->
    [Partition || ?TM({vnode, {restart_fail, Partition, _}}) <- Trace].

extract_restart(Trace) ->
    [Partition || ?TM({vnode, {restart, Partition}}) <- Trace].

extract_vnode_done(Trace) ->
    [{Partition, Stats} || ?TM({vnode, {done, Partition, Stats}}) <- Trace].

extract_init_started(Trace) ->
    [ I || ?TM({fitting, init_started}=I) <- Trace ].

kill_all_pipe_vnodes() ->
    [exit(VNode, kill) ||
        VNode <- riak_core_vnode_master:all_nodes(riak_pipe_vnode)].

die_fun() ->
    exit(diedie).

crash_fitting(Fitting) ->
    crash_fitting(Fitting, fun die_fun/0).

%% @doc riak_pipe_fitting responds to a {test_crash, fun()} message by
%% evaluating the fun. This allows tests to force the process to crash
%% (see crash_fitting/1 and die_fun/0).
crash_fitting(#fitting{pid=Pid}, Fun) ->
    crash_fitting(Pid, Fun);
crash_fitting(Pid, Fun) when is_pid(Pid) ->
    (catch gen_fsm:sync_send_all_state_event(Pid, {test_crash, Fun})).

%% @doc usually used with the riak_pipe_w_xform fitting: at each
%% fitting, the xform worker will decrement the count by one.  If the
%% count ever gets to zero, then the worker will exit.  So, we want a
%% worker to crash if the pipeline length > input # at head of
%% pipeline.
decr_or_crash(0) ->
    exit(blastoff);
decr_or_crash(N) ->
    N - 1.

%% @doc Used as an arg for the riak_pipe_w_xform fitting. It causes
%% that fitting to fail when `Input' is `0', or to pass on `Input-1'
%% otherwise.
xform_or_crash(Input, Partition, FittingDetails) ->
    ok = riak_pipe_vnode_worker:send_output(
           decr_or_crash(Input),
           Partition,
           FittingDetails).

%% @doc Create a fitting specifying the current process as the
%% sink. For use in the second argument to riak_pipe:exec/2, as
%% `{sink, self_sink()}'.
self_sink() ->
    #fitting{pid=self(),
             ref=make_ref(),
             chashfun=sink}.

assert_no_zombies(Nodes) ->
    lager:info("Verify no zombie pipe processes"),
    ?assertEqual([], zombies(Nodes)).
    
%% @doc Find transient pipe processes sticking around. Should be run
%% after tests complete.
zombies(Nodes) ->
    L = [ {N, node_zombies(N)} || N <- Nodes ],
    [ Z || Z={_, P} <- L, P /= [] ].

%% @doc Find still-running pipe processes on a node. Transient pipe
%% processes store an `eunit' value in their dictionary.
node_zombies(Node) ->
    L = [ {Pid, rpc:call(Node, erlang, process_info, [Pid, dictionary])}
          || Pid <- rpc:call(Node, erlang, processes, []) ],
    %% process_info might return 'undefined', so filtering must be
    %% done in a second step
    [ {Pid, X} || {Pid, {dictionary, D}} <- L, {eunit, X} <- D ].
