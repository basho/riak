%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_controller).
-author('Andy Gross <andy@andygross.org>').
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([ring_actions/1]).

-include("riak_repl.hrl").

-type(ets_tid() :: term()).
-type(mon_item() :: #repl_listener{} | #repl_site{}).

-record(state, {
          sites     :: ets_tid(), 
          listeners :: ets_tid(), 
          monitors  :: ets_tid()}).

-record(repl_monitor, {
          monref :: reference(),
          item :: mon_item(),
          pid :: pid()}).
          
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Sites = ets:new(repl_sites, [{keypos, 2}]),
    Listeners = ets:new(repl_listeners, [{keypos, 2}]),
    Monitors = ets:new(repl_monitors, [{keypos, 2}]),
    {ok, #state{sites=Sites, listeners=Listeners, monitors=Monitors}}.

ring_actions(Actions) when is_list(Actions) ->
    gen_server:call(?MODULE, {ring_actions, Actions}).

handle_call({ring_actions, Actions}, _From, State) ->
    handle_actions(Actions, State),
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info({'DOWN', MonRef, process, Pid, _}, State=#state{monitors=M}) -> 
    case ets:lookup(M, MonRef) of
        [] -> {noreply, State};
        [#repl_monitor{monref=MonRef,pid=Pid,item=#repl_listener{}=I}=MonRec] ->
            ets:delete_object(M, MonRec),
            start_listener(I, State),
            {noreply, State};
        [#repl_monitor{monref=MonRef,pid=Pid,item=#repl_site{}=I}=MonRec] ->
            ets:delete_object(MonRec),
            start_site(I, State)
    end.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_actions([], State) -> State;
handle_actions([A|T], State) -> handle_actions(T, handle_action(A, State)).

handle_action({start, R=#repl_listener{}}, State) -> start_listener(R, State);
handle_action({start, R=#repl_site{}}, State) ->     start_site(R, State);
handle_action({stop, R=#repl_listener{}}, State) ->  stop_listener(R, State);
handle_action({stop, R=#repl_site{}}, State) ->      stop_site(R, State).

start_listener(R=#repl_listener{nodename=N},
               State=#state{listeners=L, monitors=M}) when N =:= node() ->
    case ets:lookup(L, N) of
        [] ->
            ets:insert(L, R),
            {ok, Pid} = riak_repl_listener_sup:start_listener(R),
            ets:insert(M, make_monitor(R, Pid)),
            State;
        [#repl_listener{}=I] -> 
            %% already have record, see if it's running
            case ets:match_object(M, #repl_monitor{item=I,monref='_',pid='_'}) of
                [] ->
                    {ok, Pid} = riak_repl_listener_sup:start_listener(I),
                    ets:insert(M, make_monitor(R, Pid)),
                    State;
                [#repl_monitor{item=I}] ->
                    %% ignore, already running
                    State
            end
    end;
start_listener(#repl_listener{}, State) -> State.

stop_listener(#repl_listener{nodename=N}, 
              State=#state{listeners=L, monitors=M}) when N =:= node() ->
    case ets:lookup(L,N) of
        [] -> State;
        [#repl_listener{}=I] ->
            case ets:match_object(M, #repl_monitor{item=I,monref='_',pid='_'}) of
                [] ->
                    ets:delete_object(L, I),
                    State;
                [#repl_monitor{item=I,pid=Pid}=MonRec] ->
                    ets:delete_object(L, I),
                    ets:delete_object(M, MonRec),
                    {IP, Port} = I#repl_listener.listen_addr,
                    error_logger:info_msg("Stopping replication listener on ~s:~p~n",
                                          [IP, Port]),
                    riak_repl_listener:stop(Pid),
                    State
            end
    end;
stop_listener(#repl_listener{}, State) -> State.

start_site(R=#repl_site{name=N}, State=#state{sites=S, monitors=M}) ->
    case ets:lookup(S, N) of
        [] -> 
            case riak_repl_leader:leader_node() =:= node() of
                true ->
                    {IP, Port} = hd(R#repl_site.addrs),
                    riak_repl_util:ensure_site_dir(N),
                    {ok, Pid} = riak_repl_connector_sup:start_connector(IP, Port, N),
                    ets:insert(M, make_monitor(R, Pid)),
                    State;
                false ->
                    State
            end;
        [#repl_site{}=I] ->  
            %% already have record, see if it's running
            case ets:match_object(M, #repl_monitor{item=I,monref='_',pid='_'}) of
                [] ->
                    {IP, Port} = hd(R#repl_site.addrs),
                    riak_repl_util:ensure_site_dir(N),
                    {ok, Pid} = riak_repl_connector_sup:start_connector(IP, Port, N),
                    ets:insert(M, make_monitor(R, Pid)),
                    State;
                [#repl_monitor{item=I}] ->
                    %% ignore, already running
                    State
            end

    end.

stop_site(_R=#repl_site{}, State=#state{sites=_S, monitors=_M}) ->
    State.

make_monitor(I=#repl_listener{}, Pid) -> 
    MonRef = erlang:monitor(process, Pid),
    #repl_monitor{item=I, monref=MonRef, pid=Pid};
make_monitor(I=#repl_site{}, Pid) -> 
    MonRef = erlang:monitor(process, Pid),
    #repl_monitor{item=I, monref=MonRef, pid=Pid}.
    

    
    



                     
                         



