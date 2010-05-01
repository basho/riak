%%%-------------------------------------------------------------------
%%% File    : get_fsm_qc_vnode_master.erl
%%% Author  : Bryan Fink <bryan@mashtun.local>
%%% Description : 
%%%
%%% Created : 28 Apr 2010 by Bryan Fink <bryan@mashtun.local>
%%%-------------------------------------------------------------------
-module(get_fsm_qc_vnode_master).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {objects, partvals, history=[], repair_history=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, riak_kv_vnode_master}, ?MODULE, [], []).

start() ->
    gen_server:start({local, riak_kv_vnode_master}, ?MODULE, [], []).

get_history() ->
    gen_server:call(riak_kv_vnode_master, get_history).

get_repair_history() ->
    gen_server:call(riak_kv_vnode_master, get_repair_history).

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
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({set_data, Objects, Partvals}, _From, State) ->
    {reply, ok, set_data(Objects, Partvals, State)};
handle_call(get_history, _From, State) ->
    {reply, lists:reverse(State#state.history), State};
handle_call(get_repair_history, _From, State) ->
    {reply, lists:reverse(State#state.repair_history), State};
handle_call(Msg={vnode_del, _, _}, _From, State) ->
    {reply, ok, State#state{repair_history=[Msg|State#state.repair_history]}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({vnode_get, {Partition,Node},
             {FSM_pid,_BKey,ReqID}}, State) ->
    {Value, State1} = get_data(Partition, Node, State),
    case Value of
        {error, timeout} ->
            ok;
        _ ->
            gen_fsm:send_event(FSM_pid, 
                {r, Value, Partition, ReqID})
    end,
    {noreply, State1};
handle_cast(Msg={vnode_put, _, _}, State) ->
    {noreply, State#state{repair_history=[Msg|State#state.repair_history]}};    
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
code_change(_OldVsn, _State, _Extra) ->
    {ok, #state{history=[]}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

set_data(Objects, Partvals, State) ->
    State#state{objects=Objects, partvals=Partvals,
                history=[], repair_history=[]}.

get_data(Partition, Node, #state{objects=Objects, partvals=[Res|Rest]} = State) ->
    State1 = State#state{partvals = Rest,
                         history=[{{Partition,Node},Res}|State#state.history]},
    case Res of
        {ok, Lineage} ->
            {{ok, proplists:get_value(Lineage, Objects)}, State1};
        notfound ->
            {{error, notfound}, State1};
        timeout ->
            {{error, timeout}, State1};
        error ->
            {{error, error}, State1}
    end.