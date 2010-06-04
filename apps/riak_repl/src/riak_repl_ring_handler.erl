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
        _ ->
            {ok, #state{ring=Ring, initialized=true}}
    end.

handle_event({ring_update, Ring}, State=#state{ring=Ring}) -> 
    {ok, State};
handle_event({ring_update, NewRing0}, State=#state{ring=Ring}) ->
    NewRing1 = handle_ring_update(Ring, NewRing0),
    NewReplConfig = riak_repl_ring:get_repl_config(NewRing1),
    riak_repl_controller:set_repl_config(NewReplConfig),
    case NewRing1 =:= Ring of 
        true -> 
            {ok, State#state{initialized=true}};
        false -> 
            F = fun(InRing, ReplConfig) ->
                  {new_ring, riak_repl_ring:set_repl_config(InRing, ReplConfig)}
                end,
            {ok, NewRing} = riak_core_ring_manager:ring_trans(F, NewReplConfig),
            riak_core_ring_manager:write_ringfile(),
            {ok, State#state{ring=NewRing, initialized=true}}
    end;
handle_event(_Event, State) -> {ok, State}.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_ring_update(OldRing, NewRing0) ->
    case diff_hosts(OldRing, NewRing0) of
        true ->
            supervisor:terminate_child(riak_repl_sup, riak_repl_leader),
            supervisor:restart_child(riak_repl_sup, riak_repl_leader);
        false ->
            ignore
    end,
    OldRC0 = riak_repl_ring:get_repl_config(OldRing),
    NewRC0 = riak_repl_ring:get_repl_config(NewRing0),
    {_OldRC, NewRC} = case {OldRC0, NewRC0} of
        {undefined, undefined} -> {riak_repl_ring:initial_config(), 
                                   riak_repl_ring:initial_config()};
        {undefined, NewRC0} -> {riak_repl_ring:initial_config(),
                               NewRC0};
        {OldRC0, undefined} -> {OldRC0, OldRC0};                         
        _ -> {OldRC0, NewRC0}
    end,
    case OldRC0 =:= NewRC of
        true ->
            NewRing0;
        false ->
            riak_repl_ring:set_repl_config(NewRing0, NewRC)
    end.

diff_hosts(R1, R2) ->
    H1 = lists:sort(riak_core_ring:all_members(R1)),
    H2 = lists:sort(riak_core_ring:all_members(R2)),
    H1 /=  H2.
