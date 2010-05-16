%% Riak EnterpriseDS
%% Copyright 2007-2009 Basho Technologies, Inc. All Rights Reserved.
-module(riak_repl).
-author('Andy Gross <andy@basho.com>').
-export([start/0, stop/0]).

start() ->
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(riak_core),
    ensure_started(riak_kv),
    application:start(riak_repl).

%% @spec stop() -> ok
stop() -> 
    application:stop(riak_repl).

%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
