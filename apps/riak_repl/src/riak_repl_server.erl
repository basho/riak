-module(riak_repl_server).
-behaviour(gen_nb_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-record(state, {client, all, connected}).

start_link() ->
    PortNum = 
        case application:get_env(riak_repl, riak_repl_port) of
            undefined -> 9010;
            {ok, N} -> N
        end,
    IpAddr = 
        case application:get_env(riak_repl, riak_repl_ip) of
            undefined -> "0.0.0.0";
            {ok, IP} -> IP
        end,
    Peers = 
        case application:get_env(riak_repl, riak_repl_hosts) of
            undefined -> [];
            {ok, P}-> P
        end,
    gen_nb_server:start_link(?MODULE, IpAddr, PortNum, [Peers]).

init([Peers]) ->
    Self = self(),
    spawn(fun() -> riak_repl_util:wait_for_riak(Self) end),        
    {ok, #state{all=lists:sort(Peers)}}.

sock_opts() ->
    [binary, {packet, 4}, {reuseaddr, true}].

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(riak_up, State) ->
    do_connect(State#state.all),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

new_connection(Socket, State) ->
    {ok, Pid} = riak_repl_tcp_server:start(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    {ok, State}.

do_connect([]) ->
    ok;
do_connect([H|T]) ->
    [Host, StrPort] = string:tokens(H, ":"),
    Port = list_to_integer(StrPort),
    riak_repl_connector:connect(Host, Port),
    do_connect(T).
