%% Riak EnterpriseDS
%% @copyright 2007-2010 Basho Technologies, Inc. All Rights Reserved.
-module(riak_jmx).
-export([start/0, stop/0, stop/1]).

start() ->
    riak_core_util:start_app_deps(riak_jmx),
    application:start(riak_jmx, permanent).

%% @spec stop() -> ok
%% @doc Stop the riak_jmx application and the calling process.
stop() -> stop("riak stop requested").

stop(Reason) ->
    % we never do an application:stop because that makes it very hard
    %  to really halt the runtime, which is what we need here.
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    init:stop().    

