%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(skel).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the skel server.
start() ->
    skel_deps:ensure(),
    ensure_started(crypto),
    application:start(skel).

%% @spec stop() -> ok
%% @doc Stop the skel server.
stop() ->
    Res = application:stop(skel),
    application:stop(crypto),
    Res.
