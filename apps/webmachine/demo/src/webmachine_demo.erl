-module(webmachine_demo).
-author('Andy Gross <andy@basho.com>').
-author('Justin Sheehy <justin@@basho.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the webmachine_demo server.
start() ->
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(webmachine_demo).

%% @spec stop() -> ok
%% @doc Stop the webmachine_demo server.
stop() ->
    Res = application:stop(webmachine_demo),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
