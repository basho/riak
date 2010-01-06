-module(riak_snmp_app).
-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->
    riak_snmp_sup:start_link().

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak.
stop(_State) -> ok.

