%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(stickynotes).
-author('author <author@example.com>').
-export([start/0, stop/0]).
-export([get_app_env/1]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the stickynotes server.
start() ->
    stickynotes_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    ensure_started(inets),
    http:set_option(max_keep_alive_length, 0),
    application:start(stickynotes).

%% @spec stop() -> ok
%% @doc Stop the stickynotes server.
stop() ->
    Res = application:stop(stickynotes),
    application:stop(inets),
    application:stop(webmachine),
    application:stop(crypto),
    Res.

get_app_env(Env) ->
    case application:get_env(stickynotes, Env) of
        {ok, Val} -> Val;
        _         -> undefined
    end.
