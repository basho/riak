-module(riak_repl_tcp_client).
-include("riak_repl.hrl").
-behaviour(gen_fsm).
-export([start/1]).
-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).
-export([wait_peerinfo/2,
         merkle_exchange/2]).

-record(state, {
          socket,
          client,
          my_pi
         }).

start(Socket) ->
    gen_fsm:start(?MODULE, [Socket], []).

init([Socket]) ->
    %%io:format("~p starting, sock=~p~n", [?MODULE, Socket]),
    inet:setopts(Socket, [{active, once}, {packet, 4}]),
    riak_repl_eventer:subscribe(self()),
    {ok, Client} = riak:local_client(),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    ok = send(Socket, {peerinfo, MyPeerInfo}),
    {ok, wait_peerinfo, #state{socket=Socket,
                               client=Client,
                               my_pi=MyPeerInfo}}.

wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo, socket=_Socket}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            {next_state, merkle_exchange, State};
        false ->
            %%io:format("invalid peer_info ~p~n", [TheirPeerInfo]),
            {stop, normal, State}
    end.

merkle_exchange({merkle, Partition, MerkleTree}, State=#state{socket=Socket}) ->
    %%io:format("got merkle tree for partition ~p~n", [Partition]),
    case diff_merkle(Partition, MerkleTree, State) of
        [] ->
            %%io:format("no merkle diff for partition ~p~n", [Partition]),
            ok = send(Socket, {ack, Partition, []}),
            {next_state, merkle_exchange, State};
        DiffVClocks ->
            %%io:format("got merkle diff of length ~p~n", [length(DiffVClocks)]),
            ok = send(Socket, {ack, Partition, DiffVClocks}),
            {next_state, merkle_exchange, State}
    end;
merkle_exchange({diff_response,Partition,{send, Sends}}, State=#state{socket=Socket}) ->
    %%io:format("got diff_response with ~p sendreqs~n", [length(Sends)]),
    ok = send(Socket, {ack, Partition, []}),
    [riak_repl_util:do_repl_put(O) || O <- Sends],
    {next_state, merkle_exchange, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp_closed, Socket}, _StateName, State=#state{socket=Socket}) ->
    %%io:format("tcp socket closed ~p~n", [Socket]),
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State=#state{socket=Socket}) ->
    case binary_to_term(zlib:unzip(Data)) of
        {remote_update, Obj} ->
            riak_repl_util:do_repl_put(Obj),
            inet:setopts(Socket, [{active, once}]),            
            %%io:format("wrote remote update~n"),
            {next_state, StateName, State};
        Msg ->
            R = ?MODULE:StateName(Msg, State),
            inet:setopts(Socket, [{active, once}]),            
            R
    end;
handle_info({local_update, Obj}, StateName, State=#state{socket=Socket}) ->
    %%io:format("got local update~n"),
    ok = send(Socket, {remote_update, Obj}),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) -> 
    riak_repl_eventer:unsubscribe(self()),
    ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.


send(Socket, Data) -> 
    ok = gen_tcp:send(Socket, zlib:zip(term_to_binary(Data))).

diff_merkle(Partition, MerkleTree, #state{my_pi=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_ring:index_owner(Ring, Partition),
    case riak_repl_util:vnode_master_call(OwnerNode, {get_merkle, Partition}) of
        {error, _Reason} ->
            %%io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
            %%          [?MODULE, Partition, OwnerNode, Reason]),
            [];
        OurMerkleTree ->
            DiffKeys = merkerl:diff(MerkleTree, OurMerkleTree),
            case riak_repl_util:vnode_master_call(OwnerNode,
                                                  {get_vclocks, Partition, DiffKeys}) of
                {error, _Reason} ->
                    %%io:format("~p:error getting vclock list for ~p from ~p: ~p~n",
                    %%[?MODULE, Partition, OwnerNode, Reason]),
                    [];
                VClocks ->
                    VClocks
            end
    end.
