%% Riak EnterpriseDS
%% Copyright 2007-2010 Basho Technologies, Inc. All Rights Reserved.
-module(riak_snmp_stat_poller).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-define(REFRESH, 60000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    prepare_mibs(),
    {ok, #state{}, ?REFRESH}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?REFRESH}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?REFRESH}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    poll_stats(),
    {noreply, State, ?REFRESH};
handle_info(_Info, State) ->
    {noreply, State, ?REFRESH}.

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

prepare_mibs() ->
    DefaultMibDir = filename:join(
                      code:priv_dir(riak_snmp),
                      "mibs"),
    MibDir = riak:get_app_env(mib_dir, DefaultMibDir),
    snmpa:load_mibs(filelib:wildcard(
                      filename:join(MibDir, "*.bin"))).

poll_stats() ->
    {ok, C} = riak:local_client(),
    lists:foreach(
      fun({N, Stats}) when is_list(Stats) ->
              insert_stats(N, Stats);
         ({N, Error}) ->
              error_logger:info_msg(
                "Unable to get stats for ~p: ~p", [N, Error])
      end,
      C:get_stats(local)).

insert_stats(_Node, Stats) ->
    [ snmp_generic:variable_set(
        {SnmpKey, volatile},
        gauge_coerce(proplists:get_value(StatKey, Stats)))
      || {StatKey, SnmpKey} <- [{vnode_gets_total, vnodeGets},
                                {vnode_puts_total, vnodePuts},
                                {node_gets_total, nodeGets},
                                {node_get_fsm_time_mean, nodeGetTimeMean},
                                {node_get_fsm_time_median, nodeGetTimeMedian},
                                {node_get_fsm_time_95, nodeGetTime95},
                                {node_get_fsm_time_99, nodeGetTime99},
                                {node_get_fsm_time_100, nodeGetTime100},
                                {node_puts_total, nodePuts},
                                {node_put_fsm_time_mean, nodePutTimeMean},
                                {node_put_fsm_time_median, nodePutTimeMedian},
                                {node_put_fsm_time_95, nodePutTime95},
                                {node_put_fsm_time_99, nodePutTime99},
                                {node_put_fsm_time_100, nodePutTime100}] ].

gauge_coerce(undefined)            -> 0;
gauge_coerce(I) when is_integer(I) -> I;
gauge_coerce(N) when is_number(N)  -> trunc(N).
