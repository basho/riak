%% Riak EnterpriseDS
%% @copyright 2007-2010 Basho Technologies, Inc.
-module(riak_jmx_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Processes = [],
    {ok, {{one_for_one, 10, 10}, Processes}}.
