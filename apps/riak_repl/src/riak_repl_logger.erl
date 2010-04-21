-module(riak_repl_logger).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([postcommit/1]).
-record(state, {log}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case disk_log:open([{name, "riak_repl_log"},
                        {file, "riak_repl_log.LOG"},
                        {type, wrap},
                        {size, {1073741824, 10}}]) of
        {ok, Log} ->
            {ok, #state{log=Log}};
        {repaired, Log, {recovered, Rec}, {badbytes, Bad}} ->
            error_logger:info_msg("Repaired log ~p.  Recovered ~p records, "
                                  "~p bad bytes~n", [Log, Rec, Bad]),
            {ok, #state{log=Log}}
    end.

postcommit(RObj) ->
    gen_server:cast(?MODULE, {postcommit, RObj}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({postcommit, _RObj}, State=#state{log=_Log}) ->
    %%disk_log:log(Log, RObj),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

