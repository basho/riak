-module(riak_core_vnode).
-behaviour(gen_fsm).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-export([behaviour_info/1]).
-export([start_link/2,
         send_command/2,
         send_command_after/2]).
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

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1},
     {handle_command,3},
     {start_handoff,2},
     {handoff_cancelled,2},
     {handle_handoff_data,3},
     {is_empty,1},
     {delete_and_exit,1}];
behaviour_info(_Other) ->
    undefined.

-define(TIMEOUT, 60000).
-define(LOCK_RETRY_TIMEOUT, 10000).

-record(state, {
          index :: partition(),
          mod :: module(),
          modstate :: term(),
          handoff_q = not_in_handoff :: not_in_handoff | list(),
          handoff_token :: non_neg_integer()}).

start_link(Mod, Index) ->
    gen_fsm:start_link(?MODULE, [Mod, Index], []).

%% Send a command message for the vnode module by Pid - 
%% typically to do some deferred processing after returning yourself
send_command(Pid, Request) ->
    gen_fsm:send_event(Pid, ?VNODE_REQ{request=Request}).

%% Sends a command to the FSM that called it after Time 
%% has passed.
send_command_after(Time, Request) ->
    gen_fsm:send_event_after(Time, ?VNODE_REQ{request=Request}).
    

init([Mod, Index]) ->
    %%TODO: Should init args really be an array if it just gets Init?
    {ok, ModState} = Mod:init([Index]),
    {ok, active, #state{index=Index, mod=Mod, modstate=ModState}, 0}.

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

active(timeout, State=#state{mod=Mod, modstate=ModState}) ->
    case should_handoff(State) of
        {true, TargetNode} ->
            case Mod:start_handoff(TargetNode, ModState) of
                {true, NewModState} ->
                    start_handoff(State#state{modstate=NewModState}, TargetNode);
                {false, NewModState} ->
                    continue(NewModState)
            end;
        false ->
            continue(State)
    end;
active(?VNODE_REQ{sender=Sender, request=Request}, State) ->
    vnode_command(Sender, Request, State);
active(handoff_complete, State=#state{mod=Mod, modstate=ModState,
                                      index=Idx, handoff_token=HT, handoff_q=HQ}) ->
    riak_core_handoff_manager:release_handoff_lock({Mod, Idx}, HT),
    case HQ of
        [] ->
            Mod:delete_and_exit(ModState),
            riak_core_handoff_manager:add_exclusion(Mod, Idx),
            {stop, normal, State};
        _ ->
            %% XXX Blaire TODO: need to do "list handoff" here.
            active(timeout, State)
    end.

active(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, active, State, ?TIMEOUT}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, ?TIMEOUT}.

handle_sync_event(get_mod_index, _From, StateName,
                  State=#state{index=Idx,mod=Mod}) ->
    {reply, {Mod, Idx}, StateName, State, ?TIMEOUT};
handle_sync_event({diffobj,{BKey,BinObj}}, _From, StateName, 
                  State=#state{mod=Mod, modstate=ModState}) ->
    case Mod:handle_handoff_data(BKey, binary_to_term(BinObj), ModState) of
        {reply, ok, NewModState} ->
            {reply, ok, StateName, State#state{modstate=NewModState}, ?TIMEOUT};
        {reply, {error, Err}, NewModState} ->
            error_logger:error_msg("Error storing handoff obj: ~p~n", [Err]),            
            {reply, {error, Err}, StateName, State#state{modstate=NewModState}, ?TIMEOUT}
    end.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State, ?TIMEOUT}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

should_handoff(#state{index=Idx}) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Me = node(),
    case riak_core_ring:index_owner(Ring, Idx) of
        Me ->
            false;
        TargetNode ->
            case net_adm:ping(TargetNode) of
                pang ->
                    false;
                pong ->
                    {true, TargetNode}
            end
    end.

start_handoff(State=#state{index=Idx, mod=Mod, modstate=ModState}, TargetNode) ->
    case Mod:is_empty(ModState) of
        {true, NewModState} ->
            {stop, Reason, NewModState1} = Mod:delete_and_exit(NewModState),
            riak_core_handoff_manager:add_exclusion(Mod, Idx),
            {stop, Reason, State#state{modstate=NewModState1}};
        {false, NewModState} ->  
            case riak_core_handoff_manager:get_handoff_lock({Mod, Idx}) of
                {error, max_concurrency} ->
                    {ok, NewModState1} = Mod:handoff_cancelled(NewModState),
                    NewState = State#state{modstate=NewModState1},
                    {next_state, active, NewState, ?LOCK_RETRY_TIMEOUT};
                {ok, HandoffToken} ->
                    NewState = State#state{modstate=NewModState, 
                                           handoff_token=HandoffToken,
                                           handoff_q=[]},
                    riak_core_handoff_sender:start_link(TargetNode, Mod, Idx, all),
                    {next_state, active, NewState, ?TIMEOUT}
            end
    end.
            

%% @doc Send a reply to a vnode request.  If 
%%      the Ref is undefined just send the reply
%%      for compatibility with pre-0.12 requestors.
%%      If Ref is defined, send it along with the
%%      reply.
%%      
-spec reply(sender(), term()) -> true.
reply({fsm, undefined, From}, Reply) ->
    gen_fsm:send_event(From, Reply);
reply({fsm, Ref, From}, Reply) ->
    gen_fsm:send_event(From, {Ref, Reply});
reply({server, undefined, From}, Reply) ->
    gen_server:reply(From, Reply);
reply({server, Ref, From}, Reply) ->
    gen_server:reply(From, {Ref, Reply});
reply({raw, Ref, From}, Reply) ->
    From ! {Ref, Reply}.
                   

test(K, V) ->
    {ok, C} = riak:local_client(),
    O = riak_object:new(<<"corevnodetest">>, K, V),
    C:put(O, 2, 2),
    {ok, O1} = C:get(<<"corevnodetest">>, K, 1),
    <<"corevnodetest">> = riak_object:bucket(O1),
    K = riak_object:key(O1),
    V = riak_object:get_value(O1),
    O1.
