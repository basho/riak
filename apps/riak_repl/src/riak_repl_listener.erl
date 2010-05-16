%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_listener).
-author('Andy Gross <andy@basho.com>').
-behavior(gen_nb_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-record(state, {
          ipaddr :: string(), 
          portnum :: non_neg_integer()
         }).

start_link(IPAddr, PortNum) ->
    gen_nb_server:start_link(?MODULE, IPAddr, PortNum, [IPAddr, PortNum]).

init([IPAddr, PortNum]) -> 
    {ok, #state{ipaddr=IPAddr, portnum=PortNum}}.

sock_opts() -> [binary, 
                {keepalive, true},
                {nodelay, true},
                {packet, 4}, 
                {reuseaddr, true}, 
                {backlog, 64}].

new_connection(Socket, State) ->
    {ok, SiteName} = gen_tcp:recv(Socket, 0),
    case riak_repl_tcp_server:start(Socket, binary_to_list(SiteName)) of
        {ok, Pid} ->       connection_made(Socket, Pid, State);
        {error, Reason} -> connection_error(Reason, binary_to_list(SiteName),
                                            State);
        ignore ->          {ok, State}
    end.

%% no-ops
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% helper functions

connection_made(Socket, Pid, State) ->
    gen_tcp:controlling_process(Socket, Pid),
    {ok, State}.

connection_error({Reason, Backtrace}, SiteName, State) ->
    io:format("~p:error accepting connection from site:~p:~p",
              [?MODULE, SiteName, {Reason, Backtrace}]),
    {ok, State};
connection_error(Reason, SiteName, State) ->
    io:format("~p:error accepting connection from site:~p:~p",
              [?MODULE, SiteName, Reason]),
    {ok, State}.


