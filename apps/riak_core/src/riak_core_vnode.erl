-module(riak_core_vnode).
-behaviour(gen_fsm).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-export([start_link/2]).
-export([init/1, 
         active/2, 
         active/3, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).
-export([reply/2, test/2]).
-export([get_mod_index/1]).

-define(TIMEOUT, 60000).

-record(state, {
          index :: partition(),
          mod :: module(),
          modstate :: term(),
          handoff_q = not_in_handoff :: not_in_handoff | list()}).

start_link(Mod, Index) ->
    gen_fsm:start_link(?MODULE, [Mod, Index], []).

init([Mod, Index]) ->
    {ok, ModState} = Mod:init([Index]),
    {ok, active, #state{index=Index, mod=Mod, modstate=ModState}}.

get_mod_index(VNode) ->
    gen_fsm:sync_send_all_state_event(VNode, get_mod_index).

continue(State) ->
    {next_state, active, State, ?TIMEOUT}.

continue(State, NewModState) ->
    continue(State#state{modstate=NewModState}).

vnode_command(Sender, Request, State=#state{mod=Mod, modstate=ModState}) ->
    case Mod:handle_command(Request, Sender, ModState) of
        {reply, Reply, NewModState} ->
            reply(Sender, Reply),
            continue(State, NewModState);
        {noreply, NewModState} ->
            continue(State, NewModState);
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{modstate=NewModState}}
    end.

active(timeout, State=#state{index=Idx}) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Me = node(),
    case riak_core_ring:index_owner(Ring, Idx) of
        Me ->
            continue(State);
        TargetNode ->
            case net_adm:ping(TargetNode) of
                pang ->
                    continue(State);
                pong ->
                    %% do handoff here
                    continue(State)
            end
    end;
active(?VNODE_REQ{sender=Sender, request=Request}, State) ->
    vnode_command(Sender, Request, State).

active(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, active, State, ?TIMEOUT}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, ?TIMEOUT}.

handle_sync_event(get_mod_index, _From, StateName,
                  State=#state{index=Idx,mod=Mod}) ->
    {reply, {Mod, Idx}, StateName, State, ?TIMEOUT}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State, ?TIMEOUT}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.




-spec reply(sender(), term()) -> true.
reply({Type, _Ref, Pid}, Reply) ->
    case Type of
        fsm ->
            gen_fsm:send_event(Pid, Reply);
        server ->
            gen_server:reply(Pid, Reply);
        raw ->
            Pid ! Reply
    end.
                   

test(K, V) ->
    {ok, C} = riak:local_client(),
    O = riak_object:new(<<"corevnodetest">>, K, V),
    C:put(O, 2, 2),
    {ok, O1} = C:get(<<"corevnodetest">>, K, 1),
    <<"corevnodetest">> = riak_object:bucket(O1),
    K = riak_object:key(O1),
    V = riak_object:get_value(O1),
    O1.
