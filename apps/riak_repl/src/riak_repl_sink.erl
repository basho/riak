-module(riak_repl_sink).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([postcommit/1, add_receiver_pid/1, remove_receiver_pid/1]).
-record(state, {receiver_pids=[]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.

postcommit(RObj) ->
    Bucket = riak_object:bucket(RObj),    
    Key = riak_object:key(RObj),
    gen_server:cast(?MODULE, {postcommit, {Bucket, Key}}).    

add_receiver_pid(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {add_receiver_pid, Pid}).

remove_receiver_pid(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {remove_receiver_pid, Pid}).

handle_call({add_receiver_pid, Pid}, _From, State=#state{receiver_pids=RPs}) ->
    case lists:member(Pid, RPs) of
        true ->  {reply, ok, State};
        false -> {reply, ok, State#state{receiver_pids=[Pid|RPs]}}
    end;
handle_call({remove_receiver_pid, Pid}, _From, State=#state{receiver_pids=RPs}) ->
    {reply, ok, State#state{receiver_pids=lists:delete(Pid, RPs)}}.

handle_cast({postcommit, _Record}, State=#state{receiver_pids=[]}) -> 
    {noreply, State};
handle_cast({postcommit, Record}, State=#state{receiver_pids=RPs}) -> 
    {ok, R} = riak_core_ring_manager:get_my_ring(),
    Partition = element(1,
                        hd(
                          riak_core_ring:preflist(
                            riak_core_util:chash_key(Record), R))),
    Msg = {repl, {now(), Partition, Record}},
    [gen_leader:leader_cast(P, Msg) || P <- RPs],
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

