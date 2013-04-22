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
%% @doc Verify some MapReduce internals.
%%
%% This test used to be in riak_kv's test/mapred_test.erl. It was
%% called `notfound_failover_test_'. It has been moved here
%% to avoid the fragile setup and teardown stages that frequently
%% broke eunit testing.
-module(mapred_notfound_failover).
-behavior(riak_test).
-export([
         %% riak_test api
         confirm/0
        ]).
-compile([export_all]). %% because we run ?MODULE:PrepareFun later
-include_lib("eunit/include/eunit.hrl").
-include("rt_pipe.hrl").

-define(INTS_BUCKET, <<"foonum">>).
-define(NUM_INTS, 1000).

confirm() ->
    %% we need the volatility of memory, so we can cause a replica
    %% notfound by killing a vnode
    rt:set_backend(memory),

    Nodes = rt:build_cluster(3),

    %% for our custom reduce phase
    rt:load_modules_on_nodes([?MODULE], Nodes),

    load_test_data(Nodes),
    
    [ begin
          lager:info("Running test ~p", [T]),
          run_test(Nodes, T)
      end
      || T <- [actual_notfound,
               replica_notfound] ],
    pass.

load_test_data([Node|_]) ->
    %% creates foonum/1..?NUM_INTS - this is what populates ?INTS_BUCKET
    lager:info("Filling INTS_BUCKET (~s)", [?INTS_BUCKET]),
    ok = rpc:call(Node, riak_kv_mrc_pipe, example_setup, [?NUM_INTS]).

rpcmr(Node, Inputs, Query) ->
    rpc:call(Node, riak_kv_mrc_pipe, mapred, [Inputs, Query]).

%% @doc check the condition that used to bring down a pipe in
%% https://github.com/basho/riak_kv/issues/290 (this version checks it
%% with an actual not-found)
actual_notfound(_Node, _ChashFun,
                _MissingBucket, _MissingKey, _MissingValue) ->
    ok.

%% @doc check the condition that used to bring down a pipe in
%% https://github.com/basho/riak_kv/issues/290 this version checks
%% with an object that is missing a replica
replica_notfound(Node, {HashMod, HashFun},
                 MissingBucket, MissingKey, MissingValue) ->
    %% create a value for the "missing" key
    Obj = riakc_obj:new(MissingBucket, MissingKey, MissingValue),
    C = rt:pbc(Node),
    ok = riakc_pb_socket:put(C, Obj, [{w, 3}]),
    riakc_pb_socket:stop(C),
    %% and now kill the first replica; this will make the vnode local
    %% to the kvget pipe fitting return an error (because it's the
    %% memory backend), so it will have to look at another kv vnode
    Hash = rpc:call(Node, HashMod, HashFun, [{MissingBucket, MissingKey}]),
    [{{PrimaryIndex, PrimaryNode},_}] =
        rpc:call(Node, riak_core_apl, get_primary_apl, [Hash, 1, riak_kv]),
    {ok, VnodePid} = rpc:call(PrimaryNode,
                              riak_core_vnode_manager, get_vnode_pid,
                              [PrimaryIndex, riak_kv_vnode]),
    exit(VnodePid, kill).

run_test([Node|_], PrepareFun) ->
    QLimit = 3,
    WaitRef = make_ref(),
    Spec =
        [{map, {modfun, riak_kv_mapreduce, map_object_value},
          <<"include_keydata">>, false},
         {reduce, {modfun, ?MODULE, reduce_wait_for_signal},
          [{reduce_phase_batch_size, 1}, {wait, {self(), WaitRef}}],
          true}],
    %% mapred_plan must happen on riak node to access ring manager
    PipeSpec = rpc:call(Node, riak_kv_mrc_pipe, mapred_plan, [Spec]),
    %% make it easier to fill
    SmallPipeSpec = [ S#fitting_spec{q_limit=QLimit} || S <- PipeSpec ],
    {ok, Pipe} = rpc:call(Node, riak_pipe, exec,
                          [SmallPipeSpec,
                           [{log, sink}, {trace, [error, queue_full]},
                            {sink, rt_pipe:self_sink()}]]),
    ExistingKey = {?INTS_BUCKET, <<"bar1">>},
    ChashFun = (hd(SmallPipeSpec))#fitting_spec.chashfun,
    {MissingBucket, MissingKey} =
        find_adjacent_key(Node, ChashFun, ExistingKey),

    ValueRef = term_to_binary(make_ref()),
    %% get the missing bucket/key into the right state
    ?MODULE:PrepareFun(Node, ChashFun, MissingBucket, MissingKey, ValueRef),

    %% get main workers spun up
    ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey]),
    receive {waiting, WaitRef, ReducePid} -> ok end,

    %% reduce is now blocking, fill its queue
    [ ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey])
      || _ <- lists:seq(1, QLimit) ],

    {NValMod,NValFun} = (hd(SmallPipeSpec))#fitting_spec.nval,
    NVal = rpc:call(Node, NValMod, NValFun, [ExistingKey]),

    %% each of N paths through the primary preflist
    [ fill_map_queue(Node, Pipe, QLimit, ExistingKey)
      || _ <- lists:seq(1, NVal) ],

    %% check get queue actually full
    ExpectedTOs = lists:duplicate(NVal, timeout),
    {error, ExpectedTOs} =
        rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey, noblock]),

    %% now inject a missing key that would need to
    %% failover to the full queue
    KeyDataRef = make_ref(),
    ok = rpc:call(Node, riak_pipe, queue_work,
           [Pipe, {{MissingBucket, MissingKey}, KeyDataRef}]),
    %% and watch for it to block in the reduce queue
    %% *this* is when pre-patched code would fail:
    %% we'll receive an [error] trace from the kvget fitting's
    %% failure to forward the bkey along its preflist
    ok = consume_queue_full(Pipe, 1),

    %% let the pipe finish
    riak_pipe:eoi(Pipe),
    ReducePid ! {continue, WaitRef},

    {eoi, Results, Logs} = riak_pipe:collect_results(Pipe),
    ExpectVal = case PrepareFun of
                    actual_notfound ->
                        %% the object does not exist, but we told the map
                        %% phase to send on its keydata - check for it
                        KeyDataRef;
                    replica_notfound ->
                        %% the object does exist (but one replica does
                        %% not), and we should have found it
                        ValueRef
                end,
    ?assert(lists:member({1, ExpectVal}, Results)),
    %% just to be a little extra cautious, check for
    %% other errors
    ?assertEqual([], [E || {_,{trace,[error],_}}=E <- Logs]).

fill_map_queue(Node, Pipe, QLimit, ExistingKey) ->
    %% give the map worker one more to block on
    ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey, noblock]),
    consume_queue_full(Pipe, 1),
    %% map is now blocking, fill its queue
    [ ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey, noblock])
      || _ <- lists:seq(1, QLimit) ],
    %% give the get worker one more to block on
    ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey, noblock]),
    consume_queue_full(Pipe, {xform_map, 0}),
    %% get is now blocking, fill its queue
    [ ok = rpc:call(Node, riak_pipe, queue_work, [Pipe, ExistingKey, noblock])
      || _ <- lists:seq(1, QLimit) ],
    ok.

find_adjacent_key(Node, {HashMod, HashFun}, ExistingKey) ->
    Hash = rpc:call(Node, HashMod, HashFun, [ExistingKey]),
    [ExistingHead|_] = rpc:call(Node, riak_core_apl, get_primary_apl,
                                [Hash, 2, riak_kv]),
    [K|_] = lists:dropwhile(
              fun(N) ->
                      K = {<<"foonum_missing">>,
                           list_to_binary(integer_to_list(N))},
                      KH = rpc:call(Node, HashMod, HashFun, [K]),
                      [_,Second] =
                          rpc:call(Node, riak_core_apl, get_primary_apl,
                                   [KH, 2, riak_kv]),
                      Second /= ExistingHead
              end,
              lists:seq(1, 1000)),
    {<<"foonum_missing">>, list_to_binary(integer_to_list(K))}.

consume_queue_full(Pipe, FittingName) ->
    {log, {FittingName, {trace, [queue_full], _}}} =
        riak_pipe:receive_result(Pipe, 5000),
    ok.

reduce_wait_for_signal(Inputs, Args) ->
    case get(waited) of
        true ->
            Inputs;
        _ ->
            {TestProc, WaitRef} = proplists:get_value(wait, Args),
            TestProc ! {waiting, WaitRef, self()},
            receive {continue, WaitRef} -> ok end,
            put(waited, true),
            Inputs
    end.

wait_until_dead(Pid) when is_pid(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _Obj, Info} ->
            Info
    after 10*1000 ->
            exit({timeout_waiting_for, Pid})
    end;
wait_until_dead(_) ->
    ok.
