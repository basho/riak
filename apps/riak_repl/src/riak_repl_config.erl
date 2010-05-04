-module(riak_repl_config).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([set_ring/1]).
-record(state, {ring}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, do_initialize(#state{})}.

set_ring(Ring) -> gen_server:cast(?MODULE, {set_ring, Ring}).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast({set_ring, Ring}, State) -> {noreply, handle_set_ring(Ring, State)}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_initialize(State) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    State#state{ring=Ring}.

handle_set_ring(Ring, State) -> 
    io:format("got new ring ~p~n", [Ring]),
    State#state{ring=Ring}.
     
