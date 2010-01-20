-module(riak_handoff_receiver).

-include("riakserver_pb.hrl").
-behaviour(gen_server2).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock}).

start_link(Socket) ->
    gen_server2:start_link(?MODULE, [Socket], []).

init([Socket]) -> 
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {ok, #state{sock=Socket}}.

handle_info({tcp_closed, Socket}, State=#state{sock=Socket}) ->
    {stop, normal, State};
handle_info({tcp, _Sock, Data},
            State=#state{sock=Socket}) ->
    [MsgType|MsgData] = Data,
    process_message(MsgType,MsgData),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State}.

process_message(1, MsgData) ->
    % header of 1 is a riakobject_pb
    RO_PB = riakserver_pb:decode_riakobject_pb(zlib:unzip(MsgData)),
    io:format("got a 1 ~p ~p~n",
              [RO_PB#riakobject_pb.bucket,RO_PB#riakobject_pb.key]);
process_message(2, _MsgData) ->
    % header of 2 is a request for ack
     io:format("got a 2~n").

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

