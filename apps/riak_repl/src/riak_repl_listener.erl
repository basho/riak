-module(riak_repl_listener).
-behavior(gen_nb_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-record(state, {ipaddr, portnum}).

start_link(IPAddr, PortNum) ->
    gen_nb_server:start_link(?MODULE, IPAddr, PortNum, [IPAddr, PortNum]).

init([IPAddr, PortNum]) -> 
    {ok, #state{ipaddr=IPAddr, portnum=PortNum}}.

sock_opts() -> [binary, {packet, 4}, {reuseaddr, true}, {backlog, 64}].

handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

new_connection(Socket, State) ->
    {ok, SiteName} = gen_tcp:recv(Socket, 0),
    {ok, Pid} = riak_repl_tcp_server:start(Socket, binary_to_list(SiteName)),
    gen_tcp:controlling_process(Socket, Pid),
    {ok, State}.

