%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_ring_handler).
-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

init([]) -> {ok, #state{}}.
handle_event({ring_update, Ring}, State) -> 
    {ok, handle_ring_update(Ring, State)};
handle_event(_Event, State) -> {ok, State}.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_ring_update(Ring, State) -> 
    riak_repl_config:set_ring(Ring),
    State.
