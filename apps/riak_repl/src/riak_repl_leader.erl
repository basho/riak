%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_leader).
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_leader).
-export([start_link/3,init/1,elected/3,surrendered/3,handle_leader_call/4, 
         handle_leader_cast/3, from_leader/3, handle_call/4,
         handle_cast/3, handle_DOWN/3, handle_info/2, terminate/2,
         code_change/4]).
-export([add_receiver_pid/1,
         postcommit/1,
         leader_node/0]).

-define(LEADER_OPTS, [{vardir, VarDir}, {bcast_type, all}]).

-record(state, {
          receivers=[] :: list(),
          leader_node=undefined :: atom()}).

start_link(Candidates, Workers, InstallHook) ->
    %%io:format("riak_repl_leader: c=~p, w=~p, h=~p~n", [Candidates, Workers, InstallHook]),
    process_flag(trap_exit, true),
    {ok, DataRootDir} = application:get_env(riak_repl, data_root),
    VarDir = filename:join(DataRootDir, "leader"),
    ok = filelib:ensure_dir(filename:join(VarDir, ".empty")),
    [net_adm:ping(C) || C <- Candidates],
    LOpts = [{workers, Workers}|?LEADER_OPTS],
    gen_leader:start_link(?MODULE,Candidates,LOpts,?MODULE, [InstallHook], []).

init([InstallHook]) ->
    riak_repl_listener:close_all_connections(),
    case InstallHook of
        true ->
            riak_repl:install_hook();
        false ->
            ignore
    end,
    {ok, #state{}}.

leader_node() ->
    gen_leader:call(?MODULE, leader_node).

postcommit(Object) ->
    gen_leader:leader_cast(?MODULE, {repl, Object}).

add_receiver_pid(Pid) when is_pid(Pid) ->
    gen_leader:leader_call(?MODULE, {add_receiver_pid, Pid}).

elected(State, _NewElection, _Node) ->
    error_logger:info_msg("Elected as replication leader~n"),
    riak_repl_controller:set_is_leader(true),
    {ok, {i_am_leader, node()}, State#state{leader_node=node()}}.

surrendered(State, {i_am_leader, Node}, _NewElection) ->
    error_logger:info_msg("Replication leadership surrendered to ~p~n", [Node]),
    riak_repl_controller:set_is_leader(false),
    {ok, State#state{leader_node=Node}}.

handle_leader_call({add_receiver_pid, Pid}, _From, 
                   State=#state{receivers=R}, _E) ->
    case lists:member(Pid, R) of
        true ->
            {reply, ok, State};
        false ->
            {reply, ok, State#state{receivers=[Pid|R]}}
    end.

handle_leader_cast({repl, Msg}, State, _Election) ->
    [P ! {repl, Msg} || P <- State#state.receivers],
    {noreply, State}.

from_leader({i_am_leader, Node}, State, _NewElection) ->
    {ok, State#state{leader_node=Node}};
from_leader(_Command, State, _NewElection) -> {ok, State}.

handle_call(leader_node, _From, State, _E) ->
    {reply, State#state.leader_node, State}.
handle_cast(_Message, State, _E) -> {noreply, State}.

handle_DOWN(_Node, State, _Election) ->
    {ok, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Election, _Extra) -> {ok, State}.

  
