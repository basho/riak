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
%% called `dead_pipe_test_'. It has been moved here to avoid the
%% fragile setup and teardown stages that frequently broke eunit
%% testing.
-module(mapred_dead_pipe).
-behavior(riak_test).
-export([
         %% riak_test api
         confirm/0
        ]).
-compile([export_all]). %% because we call ?MODULE:Testname
-include_lib("eunit/include/eunit.hrl").
-include("rt_pipe.hrl").

-define(INTS_BUCKET, <<"foonum">>).
-define(NUM_INTS, 5).
-define(JS_BUCKET, <<"jsfuns">>).
-define(NOTFOUND_BKEY, {<<"does not">>, <<"exist">>}).
-define(MAP_JS, <<"function(v) { return [v.values[0].data]; }">>).
-define(REDUCE_JS, <<"function(v) {
                         Sum = function(A, B) { return A+B; };
                         return [ v.reduce(Sum) ];
                      }">>).

confirm() ->
    Nodes = rt:build_cluster(3),

    %% to pick up fake_builder/1
    rt:load_modules_on_nodes([?MODULE], Nodes),

    [ begin
          lager:info("Running test ~p", [T]),
          ?MODULE:T(Nodes)
      end
      || T<- [synchronous,
              asynchronous] ],
    pass.

%% @doc Start and kill the pipe
start_dead_pipe(Node) ->
    Spec = [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}],
    {{ok, Pipe}, _NumKeeps} =
        rpc:call(Node, riak_kv_mrc_pipe, mapred_stream, [Spec]),
    riak_pipe:destroy(Pipe),
    Pipe.

%% @doc Verify that sending inputs to a pipe that has already stopped
%% raises an error (synchronous send)
synchronous([Node|_]) ->
    Pipe = start_dead_pipe(Node),
    {error, Reason} = rpc:call(Node, riak_kv_mrc_pipe,send_inputs,
                               [Pipe, [{<<"foo">>, <<"bar">>}]]),
    %% Each vnode should have received the input, but
    %% being unable to find the fitting process, returned
    %% `worker_startup_failed` (and probably also printed
    %% "fitting was gone before startup")
    ?assert(lists:member(worker_startup_failed, Reason)).

%% @doc Verify that sending inputs to a pipe that has already stopped
%% raises an error (async send)
asynchronous([Node|_]) ->
    Pipe = start_dead_pipe(Node),
    Shim = erlang:spawn(Node, sender_shim(Pipe, self())),
    ShimMon = erlang:monitor(process, Shim),
    receive
        {sender_death, Error} ->
            {error, Reason} = Error;
        {'DOWN', ShimMon, process, Shim, Error} ->
            Reason = [Error]
    end,
    %% Each vnode should have received the input, but
    %% being unable to find the fitting process, returned
    %% `worker_startup_failed` (and probably also printed
    %% "fitting was gone before startup")
    ?assert(lists:member(worker_startup_failed, Reason)).

%% @doc runs on riak node; we have to use a shim here because
%% riak_kv_mrc_pipe:send_inputs_async sets up a monitor, which would
%% be owned by the remote RPC process, instead of our test process, as
%% desired, so we'd never see the sender die
sender_shim(Pipe, TestProc) ->
    fun() ->
            %% this is a hack to make sure that the async sender
            %% doesn't die immediately upon linking to the
            %% already-dead builder
            PipeB = Pipe#pipe{builder=erlang:spawn(fake_builder(self()))},
            {Sender, SenderRef} = riak_kv_mrc_pipe:send_inputs_async(
                                    PipeB, [{<<"foo">>, <<"bar">>}]),
            receive
                {'DOWN', SenderRef, process, Sender, Error} ->
                    ok
            end,
            %% let the fake builder shut down now
            PipeB#pipe.builder ! test_over,
            %% and send the result back for processing
            TestProc ! {sender_death, Error}
    end.

%% @doc runs on riak node
fake_builder(TestProc) ->
    fun() ->
            Ref = erlang:monitor(process, TestProc),
            receive
                test_over ->
                    ok;
                {'DOWN',Ref,process,TestProc,_} ->
                    ok
            end
    end.
