-module(riak_repl_connector).
-include("riak_repl.hrl").
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([connect/2]).
-record(state, {idx}).
-record(connrec, {hostport, pid, monref, timer}).
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{idx=ets:new(?MODULE, [{keypos, 2}])}}.

connect(Host, Port) ->
    gen_server:call(?MODULE, {connect, Host, Port}).

handle_call({connect, Host, Port}, _From, State) ->
    Self = self(),
    spawn(fun() -> do_connect(Host, Port, Self) end),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({connect, ok, {Host, Port}, Pid}, State) ->
    %io:format("connect ok to ~p:~p~n", [Host, Port]),
    MonRef = erlang:monitor(process, Pid),
    ConnRec = #connrec{hostport={Host,Port}, pid=Pid, monref=MonRef},
    add_connrec(ConnRec, State),
    {noreply, State};
handle_info({connect, error, {Host, Port}, _Reason}, State) ->
    %io:format("connect fail to ~p:~p:  ~p~n", [Host, Port, _Reason]),
    erlang:send_after(?REPL_CONN_RETRY, self(), {retry_connect, Host, Port}),
    {noreply, State};
handle_info({'DOWN', MonRef, process, _P, _I}, State) ->
    %io:format("got down message for process ~p~n", [_P]),
    case del_connrec(MonRef, State) of
        {Host,Port} ->
            erlang:send_after(?REPL_CONN_RETRY, self(), {retry_connect, Host, Port});
        _ -> 
            nop
    end,
    {noreply, State};
handle_info({retry_connect, Host, Port}, State) ->
    %io:format("retrying connect to to ~p:~p~n", [Host, Port]),
    Self = self(),
    spawn(fun() -> do_connect(Host, Port, Self) end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_connrec(Rec=#connrec{}, #state{idx=Idx}) ->
    ets:insert(Idx, Rec).

del_connrec(MonRef, #state{idx=Idx}) ->
    case ets:match(Idx, {connrec, '$1', '_', MonRef, '_'}) of
        [[HostPort]] ->
            ets:match_delete(Idx, {connrec, '_', '_', MonRef, '_'}),
            HostPort;
        _ ->
            notfound
    end.
                           
do_connect(Host, Port, PPid) ->
    case gen_tcp:connect(Host, Port, [binary,{packet, 4}], 15000) of
        {ok, Socket} ->
            {ok, Pid} = riak_repl_tcp_client:start(Socket),
            ok = gen_tcp:controlling_process(Socket, Pid),
            PPid ! {connect, ok, {Host, Port}, Pid};
        {error, Reason} ->
            PPid ! {connect, error, {Host, Port}, Reason}
    end.
