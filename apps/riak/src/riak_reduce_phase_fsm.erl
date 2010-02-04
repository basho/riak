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

-module(riak_reduce_phase_fsm).
-behaviour(gen_fsm).

-export([start_link/5]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]).

-record(state, {done,qterm,next_fsm,coord,acc,reduced,fresh_input,timeout}).

start_link(_Ring,QTerm,NextFSM,Coordinator,Timeout) ->
    gen_fsm:start_link(?MODULE, [QTerm,NextFSM,Coordinator,Timeout], []).
%% @private
init([QTerm,NextFSM,Coordinator,Timeout]) ->
    {_,{_,_,_,Acc}} = QTerm,
    riak_eventer:notify(riak_reduce_phase_fsm, reduce_start, start),
    {ok,wait,#state{done=false,qterm=QTerm,next_fsm=NextFSM,fresh_input=false,
                    coord=Coordinator,acc=Acc,reduced=[],timeout=Timeout}}.

wait(timeout, StateData=#state{next_fsm=NextFSM, done=Done, acc=Acc, coord=Coord}) ->
    case perform_reduce(StateData) of
        {HasUpdates, NewStateData} when HasUpdates =:= true;
                                        HasUpdates =:= false ->
            case Done of
                false ->
                    {next_state, wait, NewStateData};
                true ->
                    case NextFSM of
                        final ->
                            nop;
                        _ ->
                            riak_phase_proto:send_inputs(NextFSM, NewStateData#state.reduced),
                            riak_phase_proto:done(NextFSM)
                    end,
                    case Acc of
                      false ->
                            riak_phase_proto:phase_done(Coord),
                            {stop, normal, NewStateData};
                        true ->
                            riak_phase_proto:phase_results(Coord, NewStateData#state.reduced),
                            riak_phase_proto:phase_done(Coord),
                            {stop, normal, NewStateData}
                    end
            end;
        {error, NewStateData} ->
            #state{reduced=Reason}=NewStateData,
            riak_phase_proto:error(Coord, Reason),
            {stop, normal, NewStateData}
    end;
wait(done, StateData) ->
    {next_state, wait, StateData#state{done=true}, 1};
wait({input,Inputs}, StateData=#state{reduced=Reduced, timeout=Timeout}) ->
    {next_state, wait,
     StateData#state{reduced=Inputs ++ Reduced, fresh_input=true}, Timeout};
wait(die, StateData=#state{next_fsm=NextFSM}) ->
    riak_eventer:notify(riak_reduce_phase_fsm, die, die),
    case NextFSM of
        final -> nop;
        _ -> riak_phase_proto:die(NextFSM)
    end,
    {stop,normal,StateData}.

perform_reduce(#state{fresh_input=false}=State) ->
    {false, State};
perform_reduce(#state{reduced=Reduced,
                      qterm={Lang,{reduce,FunTerm,Arg,_Acc}}}=State) ->
    try
        case {Lang, FunTerm} of
            {erlang, {qfun,F}} ->
                {true, State#state{reduced=F(Reduced,Arg)}};
            {erlang, {modfun,M,F}} ->
                {true, State#state{reduced=M:F(Reduced,Arg)}};
            {javascript, QTerm} ->
                case js_reduce(QTerm, Reduced, Arg) of
                    {ok, Result} ->
                        {true, State#state{reduced=Result}};
                    Error ->
                        {error, State#state{reduced=Error}}
                end
        end
    catch C:R ->
            Reason = {C, R, erlang:get_stacktrace()},
            {error, State#state{reduced=Reason}}
    end.

js_reduce(QTerm, Reduced, Arg) ->
    case riak_js_manager:blocking_dispatch({QTerm, Reduced, Arg}) of
        {ok, Result} ->
            {ok, Result};
        Error ->
            Error
    end.

%% @private
handle_event(_Event, _StateName, StateData) -> {stop,badmsg,StateData}.
%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.
%% @private
handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.
%% @private
terminate(Reason, _StateName, _State) ->
    riak_eventer:notify(riak_reduce_phase_fsm, phase_end, Reason),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
