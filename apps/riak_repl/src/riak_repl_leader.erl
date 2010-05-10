-module(riak_repl_leader).
-behaviour(gen_leader).
-export([start_link/0, init/1, elected/3, surrendered/3, handle_leader_call/4, 
         handle_leader_cast/3, from_leader/3, handle_call/4,
         handle_cast/3, handle_DOWN/3, handle_info/2, terminate/2,
         code_change/4]).
-export([get_state/0]).
-record(state, {logger}).

start_link() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Candidates = riak_core_ring:all_members(Ring),
    [net_adm:ping(C) || C <- Candidates],
    gen_leader:start_link(?MODULE, Candidates, [], ?MODULE, [], []).

init([]) ->
    {ok, Logger} = riak_repl_logger:start_link(atom_to_list(?MODULE)),
    {ok, #state{logger=Logger}}.

get_state() ->
    gen_leader:call(?MODULE, get_state).

elected(State, NewElection, _Node) ->
    error_logger:info_msg("elected: ~p~n", [NewElection]),
    riak_repl_sink:add_receiver_pid(self()),
    {ok, {i_am_leader, node()}, State}.

surrendered(State, Sync, _NewElection) ->
    error_logger:info_msg("surrendered: sync=~p~n", [Sync]),
    riak_repl_sink:remove_receiver_pid(self()),
    {ok, State}.

handle_leader_call(_Request, _From, State, _Election) ->
    {reply, reply, State}.

handle_leader_cast(Request, State=#state{logger=Logger}, _Election) ->
    Logger ! Request, 
    {noreply, State}.

from_leader({i_am_leader, _Node}, State, _NewElection) ->
    {ok, State};
from_leader(Command, State, _NewElection) ->
    error_logger:info_msg("from_leader: ~p~n", [Command]),
    {ok, State}.

handle_call(get_state, _From, State, _E) ->
    {reply, State, State}.

handle_cast(_Message, State, _E) ->
    {noreply, State}.

handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

  
