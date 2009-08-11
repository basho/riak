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
    case application:start(ibrowse) of
        OK when OK == ok;
                OK == {error, {already_started, ibrowse}} ->
            application:set_env(stickynotes, use_ibrowse, true);
        {error, _} ->
            ensure_started(inets),
            http:set_options([{max_keep_alive_length, 0}])
    end,
    application:start(stickynotes).

%% @spec stop() -> ok
%% @doc Stop the stickynotes server.
stop() ->
    Res = application:stop(stickynotes),
    case get_app_env(use_ibrowse) of
        true -> application:stop(ibrowse);
        _    -> application:stop(inets)
    end,
    application:stop(webmachine),
    application:stop(crypto),
    Res.

get_app_env(Env) ->
    case application:get_env(stickynotes, Env) of
        {ok, Val} -> Val;
        _         -> undefined
    end.
