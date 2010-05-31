%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_connector).
-author('Andy Gross <andy@andygross.org>').
-include("riak_repl.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site        :: #repl_site{}, 
                pending     :: list(), 
                current     :: repl_addr(),
                tried = []  :: list(),
                tref        :: reference()}).

start_link(Site) ->
    gen_server:start_link(?MODULE, [Site], []).

init([Site]) ->
    erlang:send_after(?REPL_CONN_RETRY, self(), retry_connect),
    {ok, #state{site=Site, pending=Site#repl_site.addrs, tried=[]}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({connect, ok, {Host, Port}, Pid}, State) ->
    io:format("connect ok to ~p:~p~n", [Host, Port]),
    _MonRef = erlang:monitor(process, Pid),
    {noreply, State};
handle_info({connect, error, {Host, Port}, _Reason}, State) ->
    %%io:format("connect fail to ~p:~p:  ~p~n", [Host, Port, _Reason]),
    erlang:send_after(?REPL_CONN_RETRY, self(), {retry_connect, Host, Port}),
    {noreply, State};
handle_info({'DOWN',_MonRef,process,_P,_I},State) ->
    io:format("got down message for process ~p~n", [_P]),
    %%erlang:send_after(?REPL_CONN_RETRY, self(), {retry_connect, Host, Port}),
    {noreply, State};

handle_info(retry_connect, State=#state{site=Site}) ->
    io:format("retry_connect: ~p~n", [State]),
    %%io:format("retrying connect to to ~p:~p~n", [Host, Port]),
    Self = self(),
    %%spawn_link(fun() -> do_connect(Host, Port, SiteName, Self) end),
    {noreply, State};
handle_info({redirect, Host, Port}, State) ->
    io:format("redirecting to ~p~p~n", [Host, Port]),
    %%{noreply, State#state{host=Host, port=Port}};
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_connect(Host, Port, SiteName, PPid) ->
    case gen_tcp:connect(Host, Port, [binary, 
                                      {packet, 4}, 
                                      {keepalive, true},
                                      {nodelay, true}], 15000) of
        {ok, Socket} ->
            {ok, Pid} = riak_repl_tcp_client:start(Socket, SiteName, PPid),
            ok = gen_tcp:controlling_process(Socket, Pid),
            PPid ! {connect, ok, {Host, Port}, Pid};
        {error, Reason} ->
            PPid ! {connect, error, {Host, Port}, Reason}
    end.
