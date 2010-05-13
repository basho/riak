-module(riak_repl_logger).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).
-record(state, {log, tref, q}).

%% gen_server %%

start_link(Name) -> gen_server:start_link(?MODULE,[Name],[]).
init([Name]) -> do_initialize(Name).
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(flush, State=#state{q=[]}) ->  {noreply, State};
handle_info(flush, State=#state{q=Q, log=Log}) -> do_flush(Q, Log, State);
handle_info({repl,R},State=#state{q=Q}) ->  ets:insert(Q, R), {noreply, State};
handle_info({disk_log,_,Log,Msg},State) -> handle_disk_log_msg(Log, Msg, State).
terminate(_Reason, State) -> do_terminate(State).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal handlers %% 

do_initialize(Name) ->
    State = do_open_log(Name),
    Q = ets:new(list_to_atom(Name), [ordered_set]),
    {ok, State#state{q=Q}}.

do_terminate(#state{tref=TRef, log=Log}) ->  
    timer:cancel_timer(TRef),
    disk_log:sync(Log),
    ok = disk_log:close(TRef).

do_flush(Q, Log, State) ->
    ets:foldl(fun(A,Acc) -> disk_log:alog(Log, A), Acc end, [], Q),
    disk_log:sync(Log),
    ets:delete_all_objects(Q),
    {noreply, State}.

do_open_log(Name) ->
    {ok, LogDir} = application:get_env(riak_repl, log_dir),
    LogFile = filename:join(
                LogDir, 
                lists:flatten(io_lib:format("~s.LOG", [Name]))),
    {ok, FlushIval} = application:get_env(riak_repl, log_flush_interval),
    {ok, TRef} = timer:send_interval(FlushIval*1000, flush),
    State = #state{tref=TRef},
    maybe_repair_log(disk_log:open([{name, list_to_atom(Name)},
                                    {file, LogFile},
                                    {notify, true}]), State).

maybe_repair_log({ok, Log}, State) -> State#state{log=Log};
maybe_repair_log({repaired, Log, {recovered, Rec}, {badbytes, Bad}}, State) ->
    error_logger:info_msg("Repaired log ~p.  Recovered ~p records, "
                          "~p bad bytes~n", [Log, Rec, Bad]),
    State#state{log=Log}.

handle_disk_log_msg(_Log, Msg, State) -> 
    io:format("got log msg ~p~n", [Msg]),
    {noreply, State}.
