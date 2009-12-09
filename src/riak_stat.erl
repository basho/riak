%%%-------------------------------------------------------------------
%%% File    : riak_stat.erl
%%% Author  : Bryan Fink <bryan@basho.com>
%%% Description : Stats aggregator for Riak.
%%%
%%% Created : 10 Nov 2009 by Bryan Fink <bryan@basho.com>
%%%-------------------------------------------------------------------
-module(riak_stat).

-behaviour(gen_server2).

%% API
-export([start_link/0, get_stats/0, update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{vnode_gets,vnode_puts,
               get_fsm_time,put_fsm_time}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    case application:start(os_mon) of
        ok -> ok;
        {error, {already_started, os_mon}} -> ok
    %% die if os_mon doesn't start
    end,
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

get_stats() ->
    gen_server2:call(?MODULE, get_stats).

update(Stat) ->
    gen_server2:cast(?MODULE, {update, Stat, riak_util:moment()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{vnode_gets=spiraltime:fresh(),
                vnode_puts=spiraltime:fresh(),
                get_fsm_time=slide:fresh(),
                put_fsm_time=slide:fresh()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_stats, _From, State) ->
    {reply, produce_stats(State), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update, Stat, Moment}, State) ->
    {noreply, update(Stat, Moment, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

update(vnode_get, Moment, State) ->
    spiral_incr(#state.vnode_gets, Moment, State);
update(vnode_put, Moment, State) ->
    spiral_incr(#state.vnode_puts, Moment, State);
update({get_fsm_time, Microsecs}, Moment, State) ->
    slide_incr(#state.get_fsm_time, Microsecs, Moment, State);
update({put_fsm_time, Microsecs}, Moment, State) ->
    slide_incr(#state.put_fsm_time, Microsecs, Moment, State);
update(_, _, State) ->
    State.

spiral_incr(Elt, Moment, State) ->
    setelement(Elt, State,
               spiraltime:incr(1, Moment, element(Elt, State))).

slide_incr(Elt, Reading, Moment, State) ->
    setelement(Elt, State,
               slide:update(element(Elt, State), Reading, Moment)).

produce_stats(State) ->
    Moment = spiraltime:n(),
    lists:append(
      [vnode_stats(Moment, State),
       node_stats(Moment, State),
       cpu_stats(),
       mem_stats(),
       disk_stats()]).

spiral_minute(Moment, Elt, State) ->
    Up = spiraltime:incr(0, Moment, element(Elt, State)),
    {_,Count} = spiraltime:rep_minute(Up),
    Count.

slide_minute(Moment, Elt, State) ->
    {Count, Mean} = slide:mean(element(Elt, State), Moment),
    {_, Nines} = slide:nines(element(Elt, State), Moment),
    {Count, Mean, Nines}.

vnode_stats(Moment, State) ->
    [{F, spiral_minute(Moment, Elt, State)}
     || {F, Elt} <- [{vnode_gets, #state.vnode_gets},
                     {vnode_puts, #state.vnode_puts}]].

node_stats(Moment, State) ->
    {Gets, GetMean, {GetMedian, GetNF, GetNN, GetH}} =
        slide_minute(Moment, #state.get_fsm_time, State),
    {Puts, PutMean, {PutMedian, PutNF, PutNN, PutH}} =
        slide_minute(Moment, #state.put_fsm_time, State),
    [{node_gets, Gets},
     {node_get_fsm_time_mean, GetMean},
     {node_get_fsm_time_median, GetMedian},
     {node_get_fsm_time_95, GetNF},
     {node_get_fsm_time_99, GetNN},
     {node_get_fsm_time_100, GetH},
     {node_puts, Puts},
     {node_put_fsm_time_mean, PutMean},
     {node_put_fsm_time_median, PutMedian},
     {node_put_fsm_time_95, PutNF},
     {node_put_fsm_time_99, PutNN},
     {node_put_fsm_time_100, PutH}].

cpu_stats() ->
    [{cpu_nprocs, cpu_sup:nprocs()},
     {cpu_avg1, cpu_sup:avg1()},
     {cpu_avg5, cpu_sup:avg5()},
     {cpu_avg15, cpu_sup:avg15()}].

mem_stats() ->
    {Total, Alloc, _} = memsup:get_memory_data(),
    [{mem_total, Total},
     {mem_allocated, Alloc}].

disk_stats() ->
    [{disk, disksup:get_disk_data()}].
