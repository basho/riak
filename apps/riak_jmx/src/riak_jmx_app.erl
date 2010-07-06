%% Riak EnterpriseDS
%% @copyright 2007-2010 Basho Technologies, Inc.
-module(riak_jmx_app).
-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak_jmx.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->
    riak_core_util:start_app_deps(riak_jmx),
    riak_jmx_sup:start_link().

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak_jmx.
stop(_State) -> ok.

