-module(riak_repl_tcp_client).
-include("riak_repl.hrl").
-behaviour(gen_fsm).
-export([start/2]).
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
          sitename,
          client,
          my_pi
         }).

start(Socket, SiteName) ->
    gen_fsm:start(?MODULE, [Socket, SiteName], []).

init([Socket, SiteName]) ->
    io:format("~p starting, sock=~p, site=~p~n", [?MODULE, Socket, SiteName]),
    inet:setopts(Socket, [{active, once}, {packet, 4}]),
    {ok, Client} = riak:local_client(),
    MyPeerInfo = riak_repl_util:make_peer_info(),
    ok = send(Socket, term_to_binary({peerinfo, MyPeerInfo})),
    {ok, wait_peerinfo, #state{socket=Socket,
                               sitename=SiteName,
                               client=Client,
                               my_pi=MyPeerInfo}}.

wait_peerinfo({peerinfo, TheirPeerInfo}, State=#state{my_pi=MyPeerInfo, socket=_Socket}) ->
    case riak_repl_util:validate_peer_info(TheirPeerInfo, MyPeerInfo) of
        true ->
            {next_state, merkle_exchange, State};
        false ->
            io:format("invalid peer_info ~p~n", [TheirPeerInfo]),
            {stop, normal, State}
    end.

merkle_exchange({merkle, Partition, MerkleTree}, State=#state{socket=Socket}) ->
    io:format("got merkle tree for partition ~p~n", [Partition]),
    case diff_merkle(Partition, binary_to_term(MerkleTree), State) of
        [] ->
            io:format("no merkle diff for partition ~p~n", [Partition]),
            ok = send(Socket, term_to_binary({ack, Partition, []})),
            {next_state, merkle_exchange, State};
        DiffVClocks ->
            io:format("got merkle diff of length ~p~n", [length(DiffVClocks)]),
            ok = send(Socket, term_to_binary({ack, Partition, DiffVClocks})),
            {next_state, merkle_exchange, State}
    end;
merkle_exchange({diff_response,Partition,{send, Sends}}, State=#state{socket=Socket}) ->
    io:format("got diff_response with ~p sendreqs~n", [length(Sends)]),
    ok = send(Socket, term_to_binary({ack, Partition, []})),
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
    case binary_to_term(Data) of
        {remote_update, Obj} ->
            riak_repl_util:do_repl_put(Obj),
            inet:setopts(Socket, [{active, once}]),            
            io:format("wrote remote update~n"),
            {next_state, StateName, State};
        Msg ->
            R = ?MODULE:StateName(Msg, State),
            inet:setopts(Socket, [{active, once}]),            
            R
    end;
handle_info({local_update, Obj}, StateName, State=#state{socket=Socket}) ->
    io:format("got local update~n"),
    ok = send(Socket, term_to_binary({remote_update, Obj})),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->  ok.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.


send(Socket, Data) -> 
    ok = gen_tcp:send(Socket, Data).

diff_merkle(Partition, MerkleTree, #state{my_pi=#peer_info{ring=Ring}}) ->
    OwnerNode = riak_core_ring:index_owner(Ring, Partition),
    case riak_repl_util:vnode_master_call(OwnerNode, {get_merkle, Partition}) of
        {error, Reason} ->
            io:format("~p:error getting merkle tree for ~p from ~p: ~p~n",
                      [?MODULE, Partition, OwnerNode, Reason]),
            [];
        {ok, OurMerkleTree} ->
            MT = binary_to_term(OurMerkleTree),
            DiffKeys = merkerl:diff(MerkleTree, MT),
            case riak_repl_util:vnode_master_call(OwnerNode,
                                                  {get_vclocks, Partition, DiffKeys}) of
                {error, Reason} ->
                    io:format("~p:error getting vclock list for ~p from ~p: ~p~n",
                    [?MODULE, Partition, OwnerNode, Reason]),
                    [];
                VClocks -> VClocks
            end
    end.
