-module(erlang_js_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    case js_driver:load_driver() of
        false ->
            throw({error, {load_error, "Failed to load spidermonkey_drv.so"}});
        true ->
            Cache = {cache, {js_cache, start_link, []},
                     Restart, Shutdown, Type, [js_cache]},

            {ok, {SupFlags, [Cache]}}
    end.
