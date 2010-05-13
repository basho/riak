-module(riak_repl_config).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([set_ring/1, add_listener/1, add_listener/3]).
-export([add_site/1, add_site/3]).
-record(state, {ring, listeners=[]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, do_initialize(#state{})}.

set_ring(Ring) -> gen_server:cast(?MODULE, {set_ring, Ring}).

add_listener([NodeName, ListenIP, Port]) ->
    add_listener(list_to_atom(NodeName), ListenIP, list_to_integer(Port)).

add_listener(NodeName, ListenIP, Port) ->
    gen_server:call(?MODULE, {add_local_listener, NodeName, ListenIP, Port}).

add_site([IPAddr, PortNum, SiteName]) -> add_site(IPAddr, PortNum, SiteName).

add_site(IPAddr, PortNum, SiteName) ->
    gen_server:call(?MODULE, {add_site, IPAddr, PortNum, SiteName}).

handle_call({add_local_listener, NodeName, ListenIP, Port}, _From, State) -> 
    {Reply, NewState} = 
        handle_add_local_listener(NodeName, ListenIP, Port, State),
    {reply, Reply, NewState};
handle_call({add_site, IPAddr, PortNum, SiteName}, _From, State) -> 
    {Reply, NewState} = 
        handle_add_site(IPAddr, PortNum, SiteName, State),
    {reply, Reply, NewState}.

handle_cast({set_ring, Ring}, State) -> {noreply, handle_set_ring(Ring, State)}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_initialize(State0) ->
    {ok, Ring0} = riak_core_ring_manager:get_my_ring(),
    Ring1 = ensure_repl_config(Ring0),
    State1 = ensure_listeners(Ring1, State0),
    State2 = ensure_connectors(Ring1, State1),
    {ok, NewRing} = riak_core_ring_manager:get_my_ring(),
    State2#state{ring=NewRing}.

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
            riak_core_ring_manager:write_ringfile(),
            NR
    end,
    ensure_listeners(NewRing, State),
    {ok, State#state{ring=NewRing}}.

handle_add_site(IPAddr, PortNum, SiteName, State=#state{ring=Ring}) ->
    {ok, ReplConfig} = riak_core_ring:get_meta(?MODULE, Ring),
    Sites = dict:fetch(sites, ReplConfig),
    NewSites = 
        case proplists:get_value(SiteName, Sites) of
            undefined -> 
                [{SiteName, {IPAddr, PortNum}}|Sites];
            _ ->
                [{SiteName,{IPAddr,PortNum}}|proplists:delete(SiteName, Sites)]
        end,
    NewRing = case NewSites =:= Sites of
        true -> 
            Ring;
        false -> 
            NR = 
                riak_core_ring:update_meta(?MODULE,
                                              dict:store(
                                                sites,
                                                NewSites,
                                                ReplConfig),
                                           Ring),
            riak_core_ring_manager:set_my_ring(NR),
            riak_core_ring_manager:write_ringfile(),                      
            NR
    end,
    ensure_connectors(NewRing, State),
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

ensure_connectors(Ring, State) ->
    {ok, ReplConfig} = riak_core_ring:get_meta(?MODULE, Ring),
    Sites = dict:fetch(sites, ReplConfig),
    riak_repl_leader:ensure_connectors(Sites),
    State.

ensure_listeners(Ring, State=#state{listeners=L}) ->    
    {ok, ReplConfig} = riak_core_ring:get_meta(?MODULE, Ring),
    LocalListeners = dict:fetch(local_listeners, ReplConfig),
    ToStart = [{IP, Port} || {Node, {IP, Port}} <- LocalListeners, 
                             Node =:= node()],
    case ToStart of 
        [] -> State;
        [{IP, Port}] -> 
            case proplists:get_value({IP, Port}, L) of
                undefined ->
                    {ok, Pid} = riak_repl_listener_sup:start_listener(IP, Port),
                    State#state{listeners=[{{IP, Port}, Pid}|L]};
                _ ->
                    io:format("not starting duplicate listener for ~p",
                              [{IP, Port}]),
                    State
            end
    end.
