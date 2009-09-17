%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.    

-module(riak_map_phase_fsm).
-behaviour(gen_fsm).

-export([start_link/5]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]). 

-record(state, {done,qterm,next_qterm,next_fsm,coord,acc,local_fsms,
                ring,pending}).

% done: whether we believe that we are no longer receiving input
% qterm: the m/r query term for this phase
% next_qterm: the m/r query term for the phase after this one
% next_fsm: the pid for the phase after this one
% coord: the pid for the mapreduce_fsm coordinating the overall request
% acc: true/false, whether or not the client wants this phase's results
% local_fsms: the vnode-local workers for this phase in the form
%             [{Partition, LocalPhasePid}]
% ring: a riak_ring structure
% pending: items we're waiting on, in the form:
%             [{BKeyData,[{Partition,VNode}]}]  (a preflist)

start_link(Ring,QTerm,NextFSM,NextQTerm,Coordinator) ->
    gen_fsm:start_link(?MODULE, [Ring,QTerm,NextFSM,NextQTerm,Coordinator], []).
%% @private
init([Ring,QTerm,NextFSM,NextQTerm,Coordinator]) ->
    {_,_,_,Acc} = QTerm,
    riak_eventer:notify(riak_map_phase_fsm, map_start, start),
    {ok,wait,#state{qterm=QTerm,next_fsm=NextFSM,next_qterm=NextQTerm,
      ring=Ring,coord=Coordinator,acc=Acc,local_fsms=[],pending=[],done=false}}.

wait({localphase_reply,Results,BKData}, StateData=#state{
       pending=Pending0,done=Done,next_fsm=NextFSM,coord=Coord,acc=Acc}) ->
    Pending = lists:foldl(fun(BKD,P) -> lists:keydelete(BKD,1,P) end,
                          Pending0, BKData),
    case NextFSM of
        final -> nop;
        _ -> gen_fsm:send_event(NextFSM, {input, Results})
    end,
    case Acc of
        false -> nop;
        true -> gen_fsm:send_event(Coord, {acc, {list, Results}})
    end,
    case Pending of
        [] -> 
            case Done of
                true -> finish(StateData);
                _ -> {next_state, wait, StateData#state{pending=Pending}}
            end;
        _ -> {next_state, wait, StateData#state{pending=Pending}}
    end;
wait({mapexec_error, ErrFSM, {BKey,KeyData}}, StateData= 
     #state{next_fsm=NextFSM,coord=Coord,pending=Pending0}) ->
    % single item fail passthrough, try another localphase
    riak_eventer:notify(riak_map_phase_fsm, mapexec_error,
                        {ErrFSM,BKey,KeyData}),
    BKData = {BKey,KeyData},
    {BKData, PrefList} = lists:keyfind(BKData, 1, Pending0),
    case PrefList of
        [] -> % out of local phase locations to try, time to die
            gen_fsm:send_event(Coord, {error, self(), "too many nodes failed"}),
            case NextFSM of
                final -> nop;
                _ -> gen_fsm:send_event(NextFSM, die)
            end,
            {stop,normal,StateData};
        [{Partition,VNode}|RestPList] ->
            NewSD = send_to_localphase({Partition,VNode,[BKData]},StateData),
            Pending = lists:keyreplace(BKData, 1, Pending0, {BKData,RestPList}),
            {next_state, wait, NewSD#state{pending=Pending}}
    end;
wait({localphase_finish,Partition}, StateData=#state{local_fsms=LocalFSMs}) ->
    {next_state, wait,
     StateData#state{local_fsms=lists:keydelete(Partition, 1, LocalFSMs)}};
wait(done, StateData=#state{pending=Pending}) ->
    riak_eventer:notify(riak_map_phase_fsm, done_inputs, done_inputs),
    case Pending of
        [] -> finish(StateData);
        _ -> {next_state, wait, StateData#state{done=true}}
    end;
wait({input,Inputs}, StateData0=#state{pending=Pending,ring=Ring}) ->
    {NewPend,GInputs} = group_inputs(Inputs,Ring),
    StateData = lists:foldl(fun send_to_localphase/2, StateData0, GInputs),
    {next_state, wait, StateData#state{pending=Pending ++ NewPend}};
wait(die, StateData=#state{next_fsm=NextFSM}) ->
    % there is a very slight possibility of a 'die' message arriving
    %  at an unintended process, due to multiple die messages being sent.
    %  (only in the case of rapid pid recyling)
    riak_eventer:notify(riak_map_phase_fsm, map_die, die),
    case NextFSM of
        final -> nop;
        _ -> gen_fsm:send_event(NextFSM, die)
    end,
    {stop,normal,StateData}.

group_inputs(Inputs,Ring) ->
% pending is [{BKData,[{Partition,VNode}]}]  (basically a preflist)
    % NewPend is in form of 'pending' w/o first partition,
    % GInputs is [{Partition,VNode,[BKData]}]
    {NewPend, Partitioned} = lists:unzip([{{I,tl(Targets)},{I,hd(Targets)}} ||
        {I,Targets} <- [make_single_input(I0,Ring) || I0 <- Inputs]]),
    % [{I,{P,V}}]
    GInputs0 = lists:foldl(fun add_to_group/2, [], Partitioned),
    GInputs = [{P,V,L} || {{P,V},L} <- GInputs0],
    {NewPend,GInputs}.
make_single_input(I0,Ring) ->
    % given either {B,K} or {{B,K},D}, produce:
    % { {{B,K},D}, [{Partition,VNode}]  }
    I = convert_input(I0),
    {{Bucket,Key},_KeyData} = I,
    BucketProps = riak_bucket:get_bucket(Bucket, Ring),
    N = proplists:get_value(n_val,BucketProps),
    Preflist = riak_ring:filtered_preflist(chash:key_of({Bucket,Key}), Ring, N),
    {Targets, _} = lists:split(N, Preflist), % Targets is [{Partition,VNode}]
    {I,Targets}.
add_to_group({BKD,{P,V}}, GI) ->
    case lists:keyfind({P,V}, 1, GI) of
        false -> [{{P,V},[BKD]}|GI];
        {{P,V},BKD_L} -> lists:keyreplace({P,V}, 1, GI, {{P,V},[BKD|BKD_L]})
    end.
    
send_to_localphase({Partition,VNode,Inputs},
                   StateData=#state{local_fsms=LocalFSMs,
                        qterm=QTerm,next_qterm=NextQTerm}) ->
    {FSM, LFSMs} = 
    case lists:keyfind(Partition, 1, LocalFSMs) of
        {Partition, TheFSM} -> {TheFSM, LocalFSMs};
        _ ->
            {ok, TheFSM} = riak_map_localphase:start_link(
                             QTerm,NextQTerm,VNode,self(),Partition),
            {TheFSM,[{Partition,TheFSM}|LocalFSMs]}
    end,
    gen_fsm:send_event(FSM,{input,Inputs}),
    StateData#state{local_fsms=LFSMs}.

finish(StateData=#state{next_fsm=NextFSM,local_fsms=LocalFSMs,coord=Coord}) ->
    [gen_fsm:send_event(LFSM, done) || {_P,LFSM} <- LocalFSMs],
    case NextFSM of
        final -> nop;
        _ -> gen_fsm:send_event(NextFSM, done)
    end,
    gen_fsm:send_event(Coord, {done, self()}),
    riak_eventer:notify(riak_map_phase_fsm, map_done, done),
    {stop,normal,StateData}.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
terminate(Reason, _StateName, _State) ->
    riak_eventer:notify(riak_map_phase_fsm, phase_end, Reason),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% @private
convert_input(I={{_B,_K},_D})
  when is_atom(_B) andalso (is_list(_K) orelse is_binary(_K)) -> I;
convert_input(I={_B,_K})
  when is_atom(_B) andalso (is_list(_K) orelse is_binary(_K)) -> {I,undefined}.

