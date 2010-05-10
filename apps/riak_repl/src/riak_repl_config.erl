-module(riak_repl_config).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([set_ring/1, add_listener/1, add_listener/3]).
-record(state, {ring}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, do_initialize(#state{})}.

set_ring(Ring) -> gen_server:cast(?MODULE, {set_ring, Ring}).


add_listener([NodeName, ListenIP, Port]) ->
    add_listener(list_to_atom(NodeName), ListenIP, list_to_integer(Port)).

add_listener(NodeName, ListenIP, Port) ->
    gen_server:call(?MODULE, {add_local_listener, NodeName, ListenIP, Port}).

handle_call({add_local_listener, NodeName, ListenIP, Port}, _From, State) -> 
    {Reply, NewState} = 
        handle_add_local_listener(NodeName, ListenIP, Port, State),
    {reply, Reply, NewState}.
handle_cast({set_ring, Ring}, State) -> {noreply, handle_set_ring(Ring, State)}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_initialize(State) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    NewRing = ensure_repl_config(Ring),
    ensure_listeners(NewRing),
    State#state{ring=NewRing}.

handle_set_ring(Ring, State=#state{ring=Ring}) -> State;
handle_set_ring(Ring, State) -> State#state{ring=Ring}.

handle_add_local_listener(Node, ListenIP, Port, State=#state{ring=Ring}) ->
    {ok, ReplConfig} = riak_core_ring:get_meta(?MODULE, Ring),
    LocalListeners = dict:fetch(local_listeners, ReplConfig),
    NewListeners = 
        case proplists:get_value(Node, LocalListeners) of
            undefined ->
                [{Node, {ListenIP, Port}}|LocalListeners];
            _ ->
                [{Node, {ListenIP,Port}}|proplists:delete(Node, LocalListeners)]
        end,
    NewRing = case NewListeners =:= LocalListeners of
        true -> 
            Ring;
        false -> 
            NR = 
                riak_core_ring:update_meta(?MODULE,
                                              dict:store(
                                                local_listeners,
                                                NewListeners,
                                                ReplConfig),
                                              Ring),
            riak_core_ring_manager:set_my_ring(NR),
            NR
    end,
    ensure_listeners(NewRing),
    {ok, State#state{ring=NewRing}}.
            

initial_config() ->
    dict:from_list(
      [{local_listeners, []},
       {sites, []}]
      ).

ensure_repl_config(Ring) ->     
    case riak_core_ring:get_meta(?MODULE, Ring) of
        undefined ->
            riak_core_ring_manager:set_my_ring(
              riak_core_ring:update_meta(?MODULE, initial_config(), Ring)),
            {ok, R} = riak_core_ring_manager:get_my_ring(),
            R;
        {ok, _M}  ->
            Ring
    end.

ensure_listeners(Ring) ->    
    io:format("ensure: ~p~n", [Ring]),
    {ok, ReplConfig} = riak_core_ring:get_meta(?MODULE, Ring),
    LocalListeners = dict:fetch(local_listeners, ReplConfig),
    ToStart = [{IP, Port} || {Node, {IP, Port}} <- LocalListeners, 
                             Node =:= node()],
    case ToStart of 
        [] -> nop;
        [{IP, Port}] -> 
            R = riak_repl_listener_sup:start_listener(IP, Port),
            io:format("listener start result: ~p~n", [R])
    end.
