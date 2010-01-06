%% @copyright 2007-2009 Basho Technologies, Inc.
-module(riak_snmp).
-export([start/0, stop/0, stop/1]).
-export([start_snmp/0]).
-export([get_app_env/2]).

start() ->
    erlang:system_flag(fullsweep_after, 20),
    ensure_started(sasl),
    ensure_started(crypto),
    application:start(riak_snmp, permanent).

start_snmp() ->
    ensure_started(mnesia), % needed for otp_mib
    ensure_started(snmp),
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

%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
