-module(riak_handoff_listener).

-behavior(gen_nb_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).

start_link() ->
    PortNum = 
        case application:get_env(riak, riak_handoff_port) of
            undefined -> 8099;
            {ok, N} -> N
        end,
    IpAddr = 
        case application:get_env(riak, riak_handoff_ip) of
            undefined -> "0.0.0.0";
            {ok, IP} -> IP
        end,
    gen_nb_server:start_link(?MODULE, IpAddr, PortNum, []).

init([]) -> {ok, nostate}.

sock_opts() -> [binary, {packet, 4}, {reuseaddr, true}].

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

new_connection(Socket, State) ->
    {ok, Pid} = riak_handoff_receiver:start_link(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    {ok, State}.

