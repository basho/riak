%% Riak EnterpriseDS
%% @copyright 2007-2010 Basho Technologies, Inc.
-module(riak_jmx).
-export([start/0, stop/0, stop/1]).

start() ->
    ensure_started(sasl),
    ensure_started(crypto),
    application:start(riak_jmx, permanent).

%% @spec stop() -> ok
%% @doc Stop the riak_jmx application and the calling process.
stop() -> stop("riak stop requested").

stop(Reason) ->
    % we never do an application:stop because that makes it very hard
    %  to really halt the runtime, which is what we need here.
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    init:stop().    

%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
