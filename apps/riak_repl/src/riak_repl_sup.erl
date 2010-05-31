%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_sup).
-author('Andy Gross <andy@basho.com>').
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
    Processes = [{riak_repl_listener_sup,
                  {riak_repl_listener_sup, start_link, []},
                  permanent, infinity, supervisor, [riak_repl_listener_sup]},
                 {riak_repl_connector_sup,
                  {riak_repl_connector_sup, start_link, []},
                  permanent, infinity, supervisor, [riak_repl_connector_sup]},
                 {riak_repl_client_sup,
                  {riak_repl_client_sup, start_link, []},
                  permanent, infinity, supervisor, [riak_repl_client_sup]},
                 {riak_repl_server_sup,
                  {riak_repl_server_sup, start_link, []},
                  permanent, infinity, supervisor, [riak_repl_server_sup]},
                 {riak_repl_controller,
                  {riak_repl_controller, start_link, []},
                  permanent, 5000, worker, [riak_repl_controller]},
                 {riak_repl_stats,
                  {riak_repl_stats, start_link, []},
                  permanent, 5000, worker, [riak_repl_stat]},
                 {riak_repl_leader,
                  {riak_repl_leader, start_link, []},
                  permanent, 5000, worker, [riak_repl_leader]}],
    {ok, {{one_for_one, 9, 10}, Processes}}.
