%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_connector).
-author('Andy Gross <andy@andygross.org>').
-include("riak_repl.hrl").
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site        :: #repl_site{}, 
                pending     :: list(), 
                tried = []  :: list(),
                connecting  :: boolean(),
                monref      :: reference(),
                tref        :: reference()}).

start_link(Site) ->
    gen_server:start_link(?MODULE, [Site], []).

init([Site]) ->
    TRef = erlang:send_after(?REPL_CONN_RETRY, self(), retry_connect),
    {ok, #state{site=Site, 
                pending=Site#repl_site.addrs, 
                tried=[],
                connecting=false,
                tref=TRef}}.

stop(Pid) ->
    gen_server:cast(Pid, stop).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(stop, State) -> 
    {stop, normal, State}.

handle_info({connect, ok, {Host, Port}, Pid}, State) ->
    error_logger:info_msg("Connected to ~p:~p~n", [Host, Port]),
    MonRef = erlang:monitor(process, Pid),
    {noreply, State#state{monref=MonRef, connecting=false}};
handle_info({connect, error, {_Host, _Port}, _Reason}, State) ->
    %%io:format("connect fail to ~p:~p:  ~p~n", [Host, Port, _Reason]),
    TRef = erlang:send_after(?REPL_CONN_RETRY, self(), retry_connect),
    {noreply, State#state{tref=TRef, connecting=false}};
handle_info({'DOWN',_MonRef,process,_P,_I},State) ->
    %io:format("got down message for process ~p~n", [_P]),
    case State#state.connecting of
        true -> 
            {noreply, State};
        false ->
            erlang:send_after(?REPL_CONN_RETRY, self(), retry_connect),
            {noreply, State#state{connecting=true}}
    end;
handle_info(retry_connect, State=#state{site=Site, tref=TRef}) ->
    {{Host, Port}, NewState} = next_connect_addr(State),
    %io:format("retrying connect to to ~p:~p~n", [Host, Port]),
    erlang:cancel_timer(TRef),
    Self = self(),
    spawn_link(fun() -> do_connect(Host, Port, Site#repl_site.name, Self) end),
    {noreply, NewState#state{connecting=true}};
handle_info({redirect, Host, Port}, State) ->
    %io:format("redirect to ~p:~p~n", [Host, Port]),
    NewState = State#state{pending=[{Host,Port}|State#state.pending]},
    handle_info(retry_connect, NewState#state{connecting=true});
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_connect(Host, Port, SiteName, PPid) ->
    case gen_tcp:connect(Host, Port, [binary, 
                                      {packet, 4}, 
                                      {keepalive, true},
                                      {nodelay, true}], 15000) of
        {ok, Socket} ->
            {ok, Pid} = riak_repl_client_sup:start_client(Socket, SiteName, PPid),
            ok = gen_tcp:controlling_process(Socket, Pid),
            PPid ! {connect, ok, {Host, Port}, Pid};
        {error, Reason} ->
            PPid ! {connect, error, {Host, Port}, Reason}
    end.

next_connect_addr(State=#state{pending=[], tried=[Addr|NewPending]}) ->
    {Addr, State#state{pending=NewPending, tried=[Addr]}};
next_connect_addr(State=#state{pending=[Addr|NewPending], tried=T}) ->
    {Addr, State#state{pending=NewPending, tried=[Addr|T]}}.
