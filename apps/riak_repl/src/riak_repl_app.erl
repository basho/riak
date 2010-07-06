%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_app).
-author('Andy Gross <andy@basho.com>').
-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(Type :: term(), StartArgs :: term()) ->
%%          {ok,Pid} | ignore | {error,Error}
%% @doc The application:start callback for riak_repl.
%%      Arguments are ignored as all configuration is done via the erlenv file.
start(_Type, _StartArgs) ->
    riak_core_util:start_app_deps(riak_repl),
    IncarnationId = erlang:phash2({make_ref(), now()}),
    application:set_env(riak_repl, incarnation, IncarnationId),
    ok = ensure_dirs(),
    %% Spin up supervisor
    case riak_repl_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core_ring_events:add_handler(riak_repl_ring_handler, []),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @spec stop(State :: term()) -> ok
%% @doc The application:stop callback for riak_repl.
stop(_State) -> ok.

ensure_dirs() ->
    {ok, DataRoot} = application:get_env(riak_repl, data_root),
    LogDir = filename:join(DataRoot, "logs"),
    case filelib:ensure_dir(filename:join(LogDir, "empty")) of
        ok -> 
            application:set_env(riak_repl, log_dir, LogDir),
            ok;
        {error, Reason} ->
            Msg = io_lib:format("riak_repl couldn't create log dir ~p: ~p~n", [LogDir, Reason]),
            riak:stop(lists:flatten(Msg))
    end,
    {ok, Incarnation} = application:get_env(riak_repl, incarnation),
    WorkRoot = filename:join([DataRoot, "work"]),
    prune_old_workdirs(WorkRoot),
    WorkDir = filename:join([WorkRoot, integer_to_list(Incarnation)]),
    case filelib:ensure_dir(filename:join([WorkDir, "empty"])) of
        ok -> 
            application:set_env(riak_repl, work_dir, WorkDir),
            ok;
        {error, R} ->
            M = io_lib:format("riak_repl couldn't create work dir ~p: ~p~n", [WorkDir,R]),
            riak:stop(lists:flatten(M)),
            {error, R}
    end.

prune_old_workdirs(WorkRoot) ->
    case file:list_dir(WorkRoot) of
        {ok, SubDirs} ->
            DirPaths = [filename:join(WorkRoot, D) || D <- SubDirs],
            Cmds = [lists:flatten(io_lib:format("rm -rf ~s", [D])) || D <- DirPaths],
            [os:cmd(Cmd) || Cmd <- Cmds];
        _ ->
            ignore
    end.


    

