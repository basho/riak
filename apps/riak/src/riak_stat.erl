%% -------------------------------------------------------------------
%%
%% riak_stat: collect, aggregate, and provide stats about the local node
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
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

%%      @doc riak_stat is a long-lived gen_server process for aggregating
%%      stats about the Riak node on which it is runing.
%%
%%      Update each stat with the exported function update/1.  Modify
%%      the internal function update/3 to add storage for new stats.
%%
%%      Get the latest aggregation of stats with the exported function
%%      get_stats/0.  Modify the internal function produce_stats/1 to
%%      change how stats are represented.
%%
%%      Riak will start riak_stat for you, if you have specified
%%      {riak_stat, true} in your config .erlenv file.
%%
%%      Current stats:
%%<dl><dt>  vnode_gets
%%</dt><dd> Total number of gets handled by all vnodes on this node
%%          in the last minute.
%%</dd><dd> update(vnode_get)
%%
%%</dd><dt> vnode_puts
%%</dt><dd> Total number of puts handled by all vnodes on this node
%%          in the last minute.
%%</dd><dd> update(vnode_put)
%%
%%</dd><dt> node_gets
%%</dt><dd> Number of gets coordinated by this node in the last
%%          minute.
%%</dd><dd> update({get_fsm_time, Microseconds})
%%
%%</dd><dt> node_get_fsm_time_mean
%%</dt><dd> Mean time, in microseconds, between when a riak_get_fsm is
%%          started and when it sends a reply to the client, for the
%%          last minute.
%%</dd><dd> update({get_fsm_time, Microseconds})
%%
%%</dd><dt> node_get_fsm_time_median
%%</dt><dd> Median time, in microseconds, between when a riak_get_fsm
%%          is started and when it sends a reply to the client, for
%%          the last minute.
%%</dd><dd> update({get_fsm_time, Microseconds})
%%
%%</dd><dt> node_get_fsm_time_95
%%</dt><dd> Response time, in microseconds, met or beaten by 95% of
%%          riak_get_fsm executions.
%%</dd><dd> update({get_fsm_time, Microseconds})
%%
%%</dd><dt> node_get_fsm_time_99
%%</dt><dd> Response time, in microseconds, met or beaten by 99% of
%%          riak_get_fsm executions.
%%</dd><dd> update({get_fsm_time, Microseconds})
%%
%%</dd><dt> node_get_fsm_time_100
%%</dt><dd> Response time, in microseconds, met or beaten by 100% of
%%          riak_get_fsm executions.
%%</dd><dd> update({get_fsm_time, Microseconds})
%%
%%</dd><dt> node_puts
%%</dt><dd> Number of puts coordinated by this node in the last
%%          minute.
%%</dd><dd> update({put_fsm_time, Microseconds})
%%
%%</dd><dt> node_put_fsm_time_mean
%%</dt><dd> Mean time, in microseconds, between when a riak_put_fsm is
%%          started and when it sends a reply to the client, for the
%%          last minute.
%%</dd><dd> update({put_fsm_time, Microseconds})
%%
%%</dd><dt> node_put_fsm_time_median
%%</dt><dd> Median time, in microseconds, between when a riak_put_fsm
%%          is started and when it sends a reply to the client, for
%%          the last minute.
%%</dd><dd> update({put_fsm_time, Microseconds})
%%
%%</dd><dt> node_put_fsm_time_95
%%</dt><dd> Response time, in microseconds, met or beaten by 95% of
%%          riak_put_fsm executions.
%%</dd><dd> update({put_fsm_time, Microseconds})
%%
%%</dd><dt> node_put_fsm_time_99
%%</dt><dd> Response time, in microseconds, met or beaten by 99% of
%%          riak_put_fsm executions.
%%</dd><dd> update({put_fsm_time, Microseconds})
%%
%%</dd><dt> node_put_fsm_time_100
%%</dt><dd> Response time, in microseconds, met or beaten by 100% of
%%          riak_put_fsm executions.
%%</dd><dd> update({put_fsm_time, Microseconds})
%%
%%</dd><dt> cpu_nprocs
%%</dt><dd> Value returned by {@link cpu_sup:nprocs/0}.
%%
%%</dd><dt> cpu_avg1
%%</dt><dd> Value returned by {@link cpu_sup:avg1/0}.
%%
%%</dd><dt> cpu_avg5
%%</dt><dd> Value returned by {@link cpu_sup:avg5/0}.
%%
%%</dd><dt> cpu_avg15
%%</dt><dd> Value returned by {@link cpu_sup:avg15/0}.
%%
%%</dd><dt> mem_total
%%</dt><dd> The first element of the tuple returned by
%%          {@link memsup:get_memory_data/0}.
%%
%%</dd><dt> mem_allocated
%%</dt><dd> The second element of the tuple returned by
%%          {@link memsup:get_memory_data/0}.
%%
%%</dd><dt> disk
%%</dt><dd> Value returned by {@link disksup:get_disk_data/0}.
%%</dd></dl>
-module(riak_stat).

-behaviour(gen_server2).

%% API
-export([start_link/0, get_stats/0, update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{vnode_gets,vnode_puts,vnode_gets_total,vnode_puts_total,
               node_gets_total, node_puts_total,
               get_fsm_time,put_fsm_time}).

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Start the server.  Also start the os_mon application, if it's
%%      not already running.
start_link() ->
    case application:start(os_mon) of
        ok -> ok;
        {error, {already_started, os_mon}} -> ok
    %% die if os_mon doesn't start
    end,
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec get_stats() -> proplist()
%% @doc Get the current aggregation of stats.
get_stats() ->
    gen_server2:call(?MODULE, get_stats).

%% @spec update(term()) -> ok
%% @doc Update the given stat.
update(Stat) ->
    gen_server2:cast(?MODULE, {update, Stat, riak_util:moment()}).

%% @private
init([]) ->
    {ok, #state{vnode_gets=spiraltime:fresh(),
                vnode_puts=spiraltime:fresh(),
                vnode_gets_total=0,
                vnode_puts_total=0,
                node_gets_total=0,
                node_puts_total=0,
                get_fsm_time=slide:fresh(),
                put_fsm_time=slide:fresh()}}.

%% @private
handle_call(get_stats, _From, State) ->
    {reply, produce_stats(State), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({update, Stat, Moment}, State) ->
    {noreply, update(Stat, Moment, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec update(Stat::term(), integer(), state()) -> state()
%% @doc Update the given stat in State, returning a new State.
update(vnode_get, Moment, State=#state{vnode_gets_total=VGT}) ->
    spiral_incr(#state.vnode_gets, Moment, State#state{vnode_gets_total=VGT+1});
update(vnode_put, Moment, State=#state{vnode_puts_total=VPT}) ->
    spiral_incr(#state.vnode_puts, Moment, State#state{vnode_puts_total=VPT+1});
update({get_fsm_time, Microsecs}, Moment, State=#state{node_gets_total=NGT}) ->
    slide_incr(#state.get_fsm_time, Microsecs, Moment, State#state{node_gets_total=NGT+1});
update({put_fsm_time, Microsecs}, Moment, State=#state{node_puts_total=NPT}) ->
    slide_incr(#state.put_fsm_time, Microsecs, Moment, State#state{node_puts_total=NPT+1});
update(_, _, State) ->
    State.

%% @spec spiral_incr(integer(), integer(), state()) -> state()
%% @doc Increment the value of a spiraltime structure at a given
%%      position of the State tuple.
spiral_incr(Elt, Moment, State) ->
    setelement(Elt, State,
               spiraltime:incr(1, Moment, element(Elt, State))).

%% @spec slide_incr(integer(), term(), integer(), state()) -> state()
%% @doc Update a slide structure at a given position in the
%%      STate tuple.
slide_incr(Elt, Reading, Moment, State) ->
    setelement(Elt, State,
               slide:update(element(Elt, State), Reading, Moment)).

%% @spec produce_stats(state()) -> proplist()
%% @doc Produce a proplist-formatted view of the current aggregation
%%      of stats.
produce_stats(State) ->
    Moment = spiraltime:n(),
    lists:append(
      [vnode_stats(Moment, State),
       node_stats(Moment, State),
       cpu_stats(),
       mem_stats(),
       disk_stats(),
       system_stats(),
       ring_stats(),
       config_stats()
      ]).

%% @spec spiral_minute(integer(), integer(), state()) -> integer()
%% @doc Get the count of events in the last minute from the spiraltime
%%      structure at the given element of the state tuple.
spiral_minute(Moment, Elt, State) ->
    Up = spiraltime:incr(0, Moment, element(Elt, State)),
    {_,Count} = spiraltime:rep_minute(Up),
    Count.

%% @spec slide_minute(integer(), integer(), state()) ->
%%         {Count::integer(), Mean::ustat(),
%%          {Median::ustat(), NinetyFive::ustat(),
%%           NinetyNine::ustat(), Max::ustat()}}
%% @type ustat() = undefined | number()
%% @doc Get the Count of readings, the Mean of those readings, and the
%%      Median, 95th percentile, 99th percentile, and Maximum readings
%%      for the last minute from the slide structure at the given
%%      element of the state tuple.
%%      If Count is 0, then all other elements will be the atom
%%      'undefined'.
slide_minute(Moment, Elt, State) ->
    {Count, Mean} = slide:mean(element(Elt, State), Moment),
    {_, Nines} = slide:nines(element(Elt, State), Moment),
    {Count, Mean, Nines}.

%% @spec vnode_stats(integer(), state()) -> proplist()
%% @doc Get the vnode-sum stats proplist.
vnode_stats(Moment, State=#state{vnode_gets_total=VGT, vnode_puts_total=VPT}) ->
    lists:append(
      [{F, spiral_minute(Moment, Elt, State)}
       || {F, Elt} <- [{vnode_gets, #state.vnode_gets},
                       {vnode_puts, #state.vnode_puts}]],
      [{vnode_gets_total, VGT}, 
       {vnode_puts_total, VPT}]).
          


%% @spec node_stats(integer(), state()) -> proplist()
%% @doc Get the node stats proplist.
node_stats(Moment, State=#state{node_gets_total=NGT, node_puts_total=NPT}) ->
    {Gets, GetMean, {GetMedian, GetNF, GetNN, GetH}} =
        slide_minute(Moment, #state.get_fsm_time, State),
    {Puts, PutMean, {PutMedian, PutNF, PutNN, PutH}} =
        slide_minute(Moment, #state.put_fsm_time, State),
    [{node_gets, Gets},
     {node_gets_total, NGT},
     {node_get_fsm_time_mean, GetMean},
     {node_get_fsm_time_median, GetMedian},
     {node_get_fsm_time_95, GetNF},
     {node_get_fsm_time_99, GetNN},
     {node_get_fsm_time_100, GetH},
     {node_puts, Puts},
     {node_puts_total, NPT},
     {node_put_fsm_time_mean, PutMean},
     {node_put_fsm_time_median, PutMedian},
     {node_put_fsm_time_95, PutNF},
     {node_put_fsm_time_99, PutNN},
     {node_put_fsm_time_100, PutH}].

%% @spec cpu_stats() -> proplist()
%% @doc Get stats on the cpu, as given by the cpu_sup module
%%      of the os_mon application.
cpu_stats() ->
    [{cpu_nprocs, cpu_sup:nprocs()},
     {cpu_avg1, cpu_sup:avg1()},
     {cpu_avg5, cpu_sup:avg5()},
     {cpu_avg15, cpu_sup:avg15()}].

%% @spec mem_stats() -> proplist()
%% @doc Get stats on the memory, as given by the memsup module
%%      of the os_mon application.
mem_stats() ->
    {Total, Alloc, _} = memsup:get_memory_data(),
    [{mem_total, Total},
     {mem_allocated, Alloc}].

%% @spec disk_stats() -> proplist()
%% @doc Get stats on the disk, as given by the disksup module
%%      of the os_mon application.
disk_stats() ->
    [{disk, disksup:get_disk_data()}].

system_stats() ->
    [{nodename, node()},
     {connected_nodes, nodes()},
     {sys_driver_version, list_to_binary(erlang:system_info(driver_version))},
     {sys_global_heaps_size, erlang:system_info(global_heaps_size)},
     {sys_heap_type, erlang:system_info(heap_type)},
     {sys_logical_processors, erlang:system_info(logical_processors)},
     {sys_otp_release, list_to_binary(erlang:system_info(otp_release))},
     {sys_process_count, erlang:system_info(process_count)},
     {sys_smp_support, erlang:system_info(smp_support)},
     {sys_system_version, list_to_binary(erlang:system_info(system_version))},
     {sys_system_architecture, list_to_binary(erlang:system_info(system_architecture))},
     {sys_threads_enabled, erlang:system_info(threads)},
     {sys_thread_pool_size, erlang:system_info(thread_pool_size)},
     {sys_wordsize, erlang:system_info(wordsize)}].

ring_stats() ->
    {ok, R} = riak_ring_manager:get_my_ring(),
    [{ring_members, riak_ring:all_members(R)},
     {ring_num_partitions, riak_ring:num_partitions(R)},
     {ring_ownership, list_to_binary(lists:flatten(io_lib:format("~p", [dict:to_list(
                        lists:foldl(fun({_P, N}, Acc) -> 
                                            case dict:find(N, Acc) of 
                                                {ok, V} ->
                                                    dict:store(N, V+1, Acc);
                                                error ->
                                                    dict:store(N, 1, Acc)
                                            end
                                    end, dict:new(), riak_ring:all_owners(R)))])))}].
                                          

config_stats() ->
    [{ring_creation_size, riak:get_app_env(ring_creation_size)},
     {storage_backend, riak:get_app_env(storage_backend)}].

