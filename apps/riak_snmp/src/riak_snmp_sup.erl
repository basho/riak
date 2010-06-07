%% Riak EnterpriseDS
%% Copyright 2007-2010 Basho Technologies, Inc. All Rights Reserved.
-module(riak_snmp_sup).
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
    riak_snmp:start_snmp(),
    Processes = [
                 {riak_snmp_stat_poller,
                  {riak_snmp_stat_poller, start_link, []},
                  permanent, 5000, worker, [riak_snmp_stat_poller]}
                ],
    {ok, {{one_for_one, 10, 10}, Processes}}.
