%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_stats).
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_server).
-include("riak_repl.hrl").
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).
-export([start_link/0,
         add_counter/1,
         add_counter/2,
         increment_counter/1,
         increment_counter/2]).
-record(state, {t}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    T = ets:new(?MODULE, [public, named_table, set, {write_concurrency, true}]),
    ets:insert(T, {bytes_sent, 0}),
    ets:insert(T, {bytes_recvd, 0}),
    {ok, #state{t=T}}.

add_counter(Name) ->
    add_counter(Name, 0).

add_counter(Name, InitVal) when is_atom(Name) andalso is_integer(InitVal) ->
    gen_server:call(?MODULE, {add_counter, Name, InitVal}).

increment_counter(Name) ->
    increment_counter(Name, 1).

increment_counter(Name, IncrBy) when is_atom(Name) andalso is_integer(IncrBy) ->
    %gen_server:cast(?MODULE, {increment_counter, Name, IncrBy}).
    ets:update_counter(?MODULE, Name, IncrBy).

handle_call({add_counter, Name, InitVal}, _From, State=#state{t=T}) -> 
    ets:insert(T, {Name, InitVal}),
    {reply, ok, State}.
handle_cast({increment_counter, Name, IncrBy}, State=#state{t=T}) -> 
    ets:update_counter(T, Name, IncrBy),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
         
