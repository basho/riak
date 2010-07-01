%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_controller).
-author('Andy Gross <andy@andygross.org>').
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([set_repl_config/1, set_is_leader/1]).

-include("riak_repl.hrl").

-type(ets_tid()  :: term()).
-type(mon_item() :: #repl_listener{} | #repl_site{}).

-record(state, {
          repl_config :: dict(),
          monitors    :: ets_tid(),
          is_leader   :: boolean()}).

-record(repl_monitor, {
          id     :: tuple(),
          item   :: mon_item(),
          monref :: reference(),
          pid    :: pid()}).

%% api
          
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_repl_config(ReplConfig) ->
    gen_server:call(?MODULE, {set_repl_config, ReplConfig}).

set_is_leader(IsLeader) when is_boolean(IsLeader) ->
    gen_server:call(?MODULE, {set_leader, IsLeader}).

%% gen_server 

init([]) ->
    Monitors = ets:new(repl_monitors, [{keypos, 2}]),
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    ReplConfig = 
        case riak_repl_ring:get_repl_config(Ring) of
            undefined ->
                riak_repl_ring:initial_config();
            RC -> RC
        end,
    erlang:send_after(0, self(), set_init_config),
    {ok, #state{repl_config=ReplConfig,
                monitors=Monitors,
                is_leader=false}}.

handle_call({set_repl_config, ReplConfig}, _From, State) ->
    handle_set_repl_config(ReplConfig, State),
    {reply, ok, State#state{repl_config=ReplConfig}};
handle_call({set_leader, true}, _From, State=#state{is_leader=true}) ->
    {reply, ok, State#state{is_leader=true}};
handle_call({set_leader, false}, _From, State=#state{is_leader=false}) ->
    {reply, ok, State#state{is_leader=false}};
handle_call({set_leader, true}, _From, State=#state{is_leader=false}) ->
    NewState = State#state{is_leader=true},
    handle_became_leader(NewState),
    {reply, ok, NewState};
handle_call({set_leader, false}, _From, State=#state{is_leader=true}) ->
    NewState = State#state{is_leader=false},
    handle_lost_leader(NewState),
    {reply, ok, NewState}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(set_init_config, State=#state{repl_config=RC}) ->
    handle_set_repl_config(RC, State),
    {noreply, State};
handle_info({'DOWN', MonRef, process, Pid, _}, State) ->
    handle_down(MonRef, Pid, State),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% handler functions

handle_became_leader(State=#state{repl_config=RC}) ->
    handle_sites(RC, State).

handle_lost_leader(State=#state{monitors=T}) ->
    Sites = [I || {repl_monitor, {repl_site, _}, I, _, _} <- ets:tab2list(T)],
    stop_sites(Sites, State),
    riak_repl_listener:close_all_connections().


handle_down(_MonRef, _Pid, _State) -> ok.

handle_set_repl_config(NewReplConfig,State) ->
    handle_listeners(NewReplConfig, State),
    handle_sites(NewReplConfig, State).

%% site management

handle_sites(NewReplConfig,State=#state{repl_config=_OldReplConfig}) ->
    NewSites = dict:fetch(sites, NewReplConfig),
    ensure_sites(NewSites, State),
    stop_sites([], State).

stop_sites([], _State) -> ok;
stop_sites([Site|Rest], State) ->
    stop_site(Site, State),
    stop_sites(Rest, State).

stop_site(S, State) -> 
    case get_monitor(S, State) of
        not_found ->
            ignore;
        #repl_monitor{pid=Pid, monref=MonRef} ->
            erlang:demonitor(MonRef),
            riak_repl_connector:stop(Pid),
            del_monitor(S, State)
    end.

ensure_sites([], _State) -> ok;
ensure_sites([Site|Rest], State) ->
    ensure_site(Site, State),
    ensure_sites(Rest, State).

ensure_site(S, State) when State#state.is_leader =:= true ->
    case get_monitor(S, State) of
        not_found ->
            {ok, Pid} = riak_repl_connector_sup:start_connector(S),
            monitor_item(S, Pid, State);
        _ ->
            ignore
    end;
ensure_site(_S, _State) -> ignore.
    

%% listener management

handle_listeners(NewReplConfig, State=#state{repl_config=OldReplConfig}) ->
    OldListeners = dict:fetch(listeners, OldReplConfig),
    NewListeners = dict:fetch(listeners, NewReplConfig),
    ToStop = sets:to_list(
               sets:subtract(
                 sets:from_list(OldListeners), 
                 sets:from_list(NewListeners))),
    stop_listeners(ToStop, State),
    ensure_listeners(NewListeners, State).

stop_listeners([], _State) -> ok;
stop_listeners([Listener|Rest], State) ->
    stop_listener(Listener, State),
    stop_listeners(Rest, State).

stop_listener(L, State) ->
    case get_monitor(L, State) of
        not_found ->
            ignore;
        #repl_monitor{pid=Pid, monref=MonRef} ->
            erlang:demonitor(MonRef),
            {IP, Port} = L#repl_listener.listen_addr,
            error_logger:info_msg("Stopping replication listener on ~s:~p~n",
                                  [IP, Port]),
            riak_repl_listener:stop(Pid),
            del_monitor(L, State)
    end.
    
ensure_listeners([], _State) -> ok;
ensure_listeners([Listener|Rest], State) ->
    ensure_listener(Listener, State),
    ensure_listeners(Rest, State).
    
ensure_listener(L, State) when L#repl_listener.nodename =:= node() ->
    case get_monitor(L, State) of
        not_found ->
            {ok, Pid} = riak_repl_listener_sup:start_listener(L),
            monitor_item(L, Pid, State);
        _ ->
            ignore
    end;
ensure_listener(_L, _State) -> ignore.


%% ets/monitor book-keeping

monitor_item(I, Pid, #state{monitors=M}) ->
    MonRec = #repl_monitor{id=monitor_id(I),
                           item=I,
                           monref=erlang:monitor(process,Pid), 
                           pid=Pid},
    ets:insert(M, MonRec).

get_monitor(I, #state{monitors=M}) ->
    case ets:match_object(M,#repl_monitor{id=monitor_id(I),
                                          item='_',
                                          monref='_',
                                          pid='_'}) of
        [] -> not_found;
        [#repl_monitor{}=R] -> R
    end.

del_monitor(I=#repl_monitor{}, #state{monitors=M}) -> ets:delete_object(M, I);
del_monitor(I, S=#state{monitors=M}) ->
    case get_monitor(I, S) of
        not_found -> nop;
        R -> ets:delete_object(M, R)
    end.
    
monitor_id(#repl_listener{nodename=NodeName}) -> {repl_listener, NodeName};
monitor_id(#repl_site{name=SiteName}) -> {repl_site, SiteName}.
