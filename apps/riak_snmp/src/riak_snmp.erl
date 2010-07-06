%% Riak EnterpriseDS
%% Copyright 2007-2010 Basho Technologies, Inc. All Rights Reserved.
-module(riak_snmp).
-export([start/0, stop/0, stop/1]).
-export([start_snmp/0]).
-export([get_app_env/2]).

start() ->
    erlang:system_flag(fullsweep_after, 20),
    riak_core_util:start_app_deps(riak_snmp),
    application:start(riak_snmp, permanent).

start_snmp() ->
    riak_core_util:start_app_deps(riak_snmp),
    otp_mib:load(snmp_master_agent).

%% @spec stop() -> ok
%% @doc Stop the riak application and the calling process.
stop() -> stop("riak stop requested").

stop(Reason) ->
    % we never do an application:stop because that makes it very hard
    %  to really halt the runtime, which is what we need here.
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    init:stop().    

%% @spec get_app_env(Opt :: atom(), Default :: term()) -> term()
%% @doc The official way to get the values set in riak's configuration file.
%%      Will return Default if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(riak_snmp, Opt) of
	{ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
	    {ok, [[Val | _]]} -> Val;
	    error       -> Default
        end
    end.

