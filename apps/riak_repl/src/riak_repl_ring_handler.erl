%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_ring_handler).
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_event).
-includue("riak_repl.hrl").
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).
-record(state, {
          ring :: tuple(), 
          initialized=false :: boolean()}).

init([]) -> 
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    case riak_repl_ring:get_repl_config(Ring) of
        undefined -> 
            {ok, #state{ring=Ring, initialized=false}};
        RC ->
            Actions = diff_repl_configs(riak_repl_ring:initial_config(), RC),
            riak_repl_controller:ring_actions(Actions),
            {ok, #state{ring=Ring, initialized=true}}
    end.

handle_event({ring_update, Ring}, State=#state{ring=Ring}) -> 
    {ok, State};
handle_event({ring_update, NewRing}, State=#state{ring=Ring}) ->
    {Actions, R} = handle_ring_update(Ring, NewRing),
    riak_repl_controller:ring_actions(Actions),
    case R =:= Ring of 
        true -> 
            {ok, State#state{initialized=true}};
        false -> 
            riak_core_ring_manager:set_my_ring(R),
            {ok, State#state{ring=R, initialized=true}}
    end;
handle_event(_Event, State) -> {ok, State}.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_ring_update(OldRing, NewRing0) ->
    OldRC0 = riak_repl_ring:get_repl_config(OldRing),
    NewRC0 = riak_repl_ring:get_repl_config(NewRing0),
    {OldRC, NewRC} = case {OldRC0, NewRC0} of
        {undefined, undefined} -> {riak_repl_ring:initial_config(), 
                                   riak_repl_ring:initial_config()};
        {undefined, NewRC0} -> {riak_repl_ring:initial_config(),
                               NewRC0};
        {OldRC0, undefined} -> {OldRC0, OldRC0};                         
        _ -> {OldRC0, NewRC0}
    end,
    NewRing = riak_repl_ring:set_repl_config(NewRing0, NewRC),
    {diff_repl_configs(OldRC, NewRC), NewRing}.

diff_repl_configs(_RC1, _RC1) -> [];
diff_repl_configs(RC1, RC2) ->
    Sites1 = dict:fetch(sites, RC1),
    Sites2 = dict:fetch(sites, RC2),
    Listeners1 = dict:fetch(listeners, RC1),
    Listeners2 = dict:fetch(listeners, RC2),
    SiteActions = diff_sites(Sites1, Sites2),
    ListenerActions = diff_listeners(Listeners1, Listeners2),
    lists:append(SiteActions, ListenerActions).

diff_sites(_S, _S) -> [];
diff_sites(S1, S2) ->  diff(S1, S2).
diff_listeners(_L, _L) -> [];    
diff_listeners(L1, L2) -> diff(L1, L2).
diff(I1, I2) ->
    I3 = sets:from_list(I1), 
    I4 = sets:from_list(I2),
    ToStop = [{stop,I}||I<-sets:to_list(sets:subtract(I3, I4))],
    ToStart = [{start,I}||I<-sets:to_list(sets:subtract(I4, I3))],
    lists:append(ToStop, ToStart).
     
    

    
    
    
    
