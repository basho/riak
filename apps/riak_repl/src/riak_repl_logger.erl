-module(riak_repl_logger).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {log, tref, q=[]}).

%% gen_server %%

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> do_initialize().
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(flush, State=#state{q=[]}) ->  {noreply, State};
handle_info(flush, State=#state{q=Q, log=Log}) -> do_flush(Q, Log, State);
handle_info({repl,R},State=#state{q=Q}) -> {noreply, State#state{q=[R|Q]}};
handle_info({disk_log,_,Log,Msg},State) -> handle_disk_log_msg(Log, Msg, State).
terminate(_Reason, State) -> do_terminate(State).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal handlers %% 

do_initialize() ->
    State = do_open_log(),
    ok = riak_repl_sink:add_receiver_pid(self()),    
    {ok, State}.

do_terminate(#state{tref=TRef, log=Log}) ->
    timer:cancel_timer(TRef),
    disk_log:sync(Log),
    ok = disk_log:close(TRef).

do_flush(Q, Log, State) ->
    disk_log:log_terms(Log, lists:reverse(Q)),
    {noreply, State#state{q=[]}}.

do_open_log() ->
    {ok, LogDir} = application:get_env(riak_repl, log_dir),
    LogFile = filename:join(LogDir, "riak_repl.LOG"),
    {ok, FlushIval} = application:get_env(riak_repl, log_flush_interval),
    {ok, TRef} = timer:send_interval(FlushIval*1000, flush),
    State = #state{tref=TRef, q=[]},
    maybe_repair_log(disk_log:open([{name, riak_repl_log},{file, LogFile},
                                    {notify, true}]), State).

maybe_repair_log({ok, Log}, State) -> State#state{log=Log};
maybe_repair_log({repaired, Log, {recovered, Rec}, {badbytes, Bad}}, State) ->
    error_logger:info_msg("Repaired log ~p.  Recovered ~p records, "
                          "~p bad bytes~n", [Log, Rec, Bad]),
    State#state{log=Log}.

handle_disk_log_msg(_Log, Msg, State) -> 
    io:format("got log msg ~p~n", [Msg]),
    {noreply, State}.
