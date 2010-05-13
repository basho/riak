-module(riak_repl_sink).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([postcommit/1, set_leader_available/1]).
-record(state, {leader_available}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, #state{leader_available=false}}.

set_leader_available(LeaderAvailable) ->
    gen_server:call(?MODULE, {leader_available, LeaderAvailable}).

postcommit(RObj) ->
    gen_server:cast(?MODULE, {postcommit, {RObj}}).

handle_call({leader_available, true},_From,
            State=#state{leader_available=false}) ->
    %% spool_to_leader()
    {reply, ok, State#state{leader_available=true}};
handle_call({leader_available, true},_From,
            State=#state{leader_available=true}) ->
    {reply, ok, State};
handle_call({leader_available, false},_From,
            State=#state{leader_available=true}) ->
    %%start_local_logger(),
    {reply, ok, State#state{leader_available=false}};
handle_call({leader_available, false},_From,
            State=#state{leader_available=false}) ->
    {reply, ok, State}.

handle_cast({postcommit, Record}, State) ->
    Msg = {repl, Record},
    gen_leader:leader_call(riak_repl_leader, Msg),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

