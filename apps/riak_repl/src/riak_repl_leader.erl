-module(riak_repl_leader).
-behaviour(gen_leader).
-export([start_link/0, init/1, elected/3, surrendered/3, handle_leader_call/4, 
         handle_leader_cast/3, from_leader/3, handle_call/4,
         handle_cast/3, handle_DOWN/3, handle_info/2, terminate/2,
         code_change/4]).
-export([get_state/0, is_leader/0, ensure_connectors/1, add_receiver_pid/1]).
-record(state, {logger, is_leader, connectors=[], receivers=[], leader_node}).

start_link() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Candidates = riak_core_ring:all_members(Ring),
    {ok, DataRootDir} = application:get_env(riak_repl, data_root),
    VarDir = filename:join(DataRootDir, "leader"),
    ok = filelib:ensure_dir(filename:join(VarDir, ".empty")),
    LeaderOpts = [{vardir, VarDir}],
    [net_adm:ping(C) || C <- Candidates],
    gen_leader:start_link(?MODULE, Candidates, LeaderOpts, ?MODULE, [], []).

init([]) ->
    {ok, Logger} = riak_repl_logger:start_link(atom_to_list(?MODULE)),
    {ok, #state{logger=Logger, is_leader=false}}.

is_leader() ->
    gen_leader:call(?MODULE, is_leader).

get_state() ->
    gen_leader:call(?MODULE, get_state).

add_receiver_pid(Pid) when is_pid(Pid) ->
    gen_leader:leader_call(?MODULE, {add_receiver_pid, Pid}).

ensure_connectors(RemoteSites) ->
    gen_leader:call(?MODULE, {ensure_connectors, RemoteSites}).

elected(State, _NewElection, _Node) ->
    %%riak_repl_sink:set_leader_available(true),
    {ok, {i_am_leader, node()}, State#state{is_leader=true}}.

surrendered(State, Sync, _NewElection) ->
    error_logger:info_msg("surrendered: sync=~p~n", [Sync]),
    riak_repl_sink:set_leader_available(true),
    {ok, State#state{is_leader=false}}.

handle_leader_call({add_receiver_pid, Pid}, _From, 
                   State=#state{receivers=R}, _E) ->
    case lists:member(Pid, R) of
        true ->
            {reply, ok, State};
        false ->
            erlang:monitor(process, Pid),
            {reply, ok, State#state{receivers=[Pid|R]}}
    end;

handle_leader_call({repl, Msg}, _From, State, _Election) ->
    [P ! {repl, Msg} || P <- State#state.receivers],
    {reply, reply, State}.

handle_leader_cast(Request, State=#state{logger=Logger}, _Election) ->
    Logger ! Request, 
    {noreply, State}.

from_leader({i_am_leader, Node}, State, _NewElection) ->
    {ok, State#state{leader_node=Node}};
from_leader(Command, State, _NewElection) ->
    error_logger:info_msg("from_leader: ~p~n", [Command]),
    {ok, State}.

handle_call(get_state, _From, State, _E) ->
    {reply, State, State};
handle_call(is_leader, _From, State=#state{is_leader=IL}, _E) ->
    {reply, IL, State};
handle_call({ensure_connectors,RemoteSites},_From,
            State=#state{is_leader=true}, _E) ->
    NewState = handle_ensure_connectors(RemoteSites, State),
    {reply, ok, NewState};
handle_call({ensure_connectors,_RemoteSites},_From,
            State=#state{is_leader=false}, _E) ->
    {reply, ok, State}.

handle_cast(_Message, State, _E) ->
    {noreply, State}.

handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

handle_info({'DOWN', _MR, process, Pid, Info}, State) ->
    io:format("pid ~p died: ~p~n", [Pid, Info]),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("got other info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

handle_ensure_connectors(RemoteSites, State=#state{}) ->
    lists:foldl(
      fun({Site, {IP, Port}}, S) -> 
              ensure_connector(Site, IP, Port, S)
      end, State, RemoteSites).

ensure_connector(Site, IP, Port, State=#state{connectors=C}) ->
    case proplists:get_value(Site, C) of
        undefined ->
            ensure_site_dir(Site),
            {ok, Pid} = riak_repl_connector_sup:start_connector(IP, Port, Site),
            State#state{connectors=[{Site, {IP, Port, Pid}}|C]};
        _ ->
            State
    end.

ensure_site_dir(Site) ->
    ok = filelib:ensure_dir(
           filename:join([riak_repl_util:site_root_dir(Site), ".empty"])).
    
                        
    
  
