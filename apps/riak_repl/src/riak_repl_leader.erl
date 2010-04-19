-module(riak_repl_leader).
-behaviour(gen_leader).
-export([start_link/0, init/1, elected/2, surrendered/3, handle_leader_call/4, 
         handle_leader_cast/3, from_leader/3, handle_call/3,
         handle_cast/2, handle_DOWN/3, handle_info/2, terminate/2,
         code_change/4]).
-export([get_state/0]).
-record(state, {}).

start_link() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Candidates = riak_core_ring:all_members(Ring),
    [net_adm:ping(C) || C <- Candidates],
    gen_leader:start_link(?MODULE, Candidates, [], ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

get_state() ->
    gen_leader:call(?MODULE, get_state).

elected(State, NewElection) ->
    error_logger:info_msg("elected: ~p~n", [NewElection]),
    ensure_repl_server(State),
    riak_repl_server:set_leader(true),
    {ok, {i_am_leader, node()}, State}.

surrendered(State, Sync, _NewElection) ->
    error_logger:info_msg("surrendered: sync=~p~n", [Sync]),
    riak_repl_server:set_leader(false),
    {ok, State}.

handle_leader_call(_Request, _From, State, _Election) ->
    {reply, reply, State}.

handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

from_leader({i_am_leader, _Node}, State, _NewElection) ->
    riak_repl_server:set_leader(false),
    ensure_repl_server(State);
    
from_leader(Command, State, _NewElection) ->
    error_logger:info_msg("from_leader: ~p~n", [Command]),
    {ok, State}.
            

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_DOWN(Node, State, _Election) ->
    io:format("handle_DOWN: node=~p~n", [Node]),
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Election, _Extra) ->
    error_logger:info_msg("Code CHange!!!"),
    {ok, State}.

  
ensure_repl_server(State) ->  
    case whereis(riak_repl_server) of
        undefined ->
            error_logger:info_msg("starting riak_repl_server"),
            riak_repl_sup:start_module(riak_repl_server),
            {ok, State};
        P when is_pid(P) ->
            {ok, State}
    end.

