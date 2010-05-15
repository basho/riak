%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_sup).
-author('Andy Gross <andy@basho.com>').
-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-export([start_config/0]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

start_config() ->
    ChildSpec = 
        {riak_repl_config,
         {riak_repl_config, start_link, []},
         permanent, 5000, worker, [riak_config]},
    supervisor:start_child(?MODULE, ChildSpec).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Processes = [{riak_repl_listener_sup,
                  {riak_repl_listener_sup, start_link, []},
                  permanent, infinity, supervisor, [riak_repl_listener_sup]},
                 {riak_repl_connector_sup,
                  {riak_repl_connector_sup, start_link, []},
                  permanent, infinity, supervisor, [riak_repl_connector_sup]},
                 {riak_repl_leader,
                  {riak_repl_leader, start_link, []},
                  permanent, 5000, worker, [riak_repl_leader]}],
    {ok, {{one_for_one, 9, 10}, Processes}}.
