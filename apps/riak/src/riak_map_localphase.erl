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

-module(riak_map_localphase).
-behaviour(gen_fsm).

-export([start_link/5]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]). 

-record(state, {qterm,phase_pid,next_qterm,vnode,partition,req_id,
               acc,pending,finished,done}).

start_link(QTerm,NextQTerm,VNode,PhasePid,Partition) ->
    gen_fsm:start_link(?MODULE,
                  [QTerm,NextQTerm,VNode,PhasePid,Partition], []).
%% @private
init([QTerm,NextQTerm,VNode,PhasePid,Partition]) ->
    ReqID = erlang:phash2({random:uniform(), self()}),    
    riak_eventer:notify(riak_map_localphase, start, {VNode,ReqID}),
    {ok,wait,
     #state{qterm=QTerm,phase_pid=PhasePid,next_qterm=NextQTerm,
            vnode=VNode,partition=Partition,acc=[],
            req_id=ReqID,pending=[],finished=[],done=false}}.

wait(done, StateData=#state{pending=Pending}) ->
    case Pending of
        [] -> finish(StateData);
        _ -> {next_state, wait, StateData#state{done=true}}
    end;
wait({input,Inputs0}, StateData=#state{qterm=QTerm,vnode=VN,pending=Pending,
                                       partition=Partition}) ->
    Inputs = [convert_input(I) || I <- Inputs0],
    riak_eventer:notify(riak_map_localphase, input_set, {QTerm,Partition,VN}),
    [gen_server:cast({riak_vnode_master, VN},
                     {vnode_map, {Partition,node()},
                      {self(),QTerm,BKey,KeyData}}) ||
        {BKey,KeyData} <- Inputs],
    {next_state, wait, StateData#state{pending=Inputs++Pending}};
wait({mapexec_error, {BKey,KeyData}, VN, ErrMsg},
     StateData=#state{phase_pid=PhasePid,pending=Pending}) ->
    riak_eventer:notify(riak_map_localphase, mapexec_vnode_err, {VN,ErrMsg}),
    gen_fsm:send_event(PhasePid, {mapexec_error, self(), {BKey,KeyData}}),
    {stop,normal,StateData#state{
                   pending=lists:delete({BKey,KeyData}, Pending)}};
wait({mapexec_reply, {BKey,KeyData}, RetVal, VN}, StateData0=#state{
                    acc=Acc0,done=Done,pending=Pending0,finished=Finished0}) ->
    riak_eventer:notify(riak_map_localphase, mapexec_reply, {VN,BKey}),
    Acc = RetVal ++ Acc0,
    Pending = lists:delete({BKey,KeyData}, Pending0),
    Finished = [{BKey,KeyData}|Finished0],
    StateData = StateData0#state{acc=Acc,pending=Pending,finished=Finished},
    case Done of
        true ->
            finish(StateData);
        false ->
            case Pending of
                [] ->
                    send_results(StateData),
                    {next_state, wait,
                     StateData#state{acc=[],finished=[]}};
                _ ->
                    {next_state, wait, StateData}
            end
    end.

%% @private
finish(StateData=#state{phase_pid=PhasePid,partition=Partition,
                                               pending=Pending}) ->
    send_results(StateData),
    case Pending of
        [] -> gen_fsm:send_event(PhasePid,{localphase_finish,Partition});
        _ ->
            [gen_fsm:send_event(PhasePid,
                               {mapexec_error, self(), {BKey,KeyData}}) ||
                {BKey,KeyData} <- Pending]
    end,
    {stop,normal,StateData}.

%% @private
send_results(_StateData=#state{next_qterm=NextQTerm,phase_pid=PhasePid,
                              finished=Finished,acc=Acc}) ->
    case maybe_reduce(Acc,NextQTerm) of
        {ok, Results} ->
            gen_fsm:send_event(PhasePid,
                               {localphase_reply, Results, Finished});
        {error, _Err} ->
            [gen_fsm:send_event(PhasePid,
                               {mapexec_error, self(), {BKey,KeyData}}) ||
                {BKey,KeyData} <- Finished]
    end.

%% @private
convert_input(I={{_B,_K},_D})
  when is_binary(_B) andalso (is_list(_K) orelse is_binary(_K)) -> I;
convert_input(I={_B,_K})
  when is_binary(_B) andalso (is_list(_K) orelse is_binary(_K)) -> {I,undefined}.

%% @private
maybe_reduce(Values,NextQTerm) ->
    case NextQTerm of
        {reduce,FunTerm,Arg,_Acc} ->
            try
                NewValues = case FunTerm of
                                {qfun,F} -> F(Values,Arg);
                                {modfun,M,F} -> M:F(Values,Arg)
                            end,
                {ok,NewValues}
            catch C:R ->
                    Reason = {C, R, erlang:get_stacktrace()},
                    {error, Reason}
            end;
        _ -> {ok, Values}
    end.

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
terminate(Reason, _StateName, _State=#state{vnode=VNode,req_id=ReqID}) ->
    riak_eventer:notify(riak_map_localphase, localphase_end,
                        {VNode,ReqID,Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
