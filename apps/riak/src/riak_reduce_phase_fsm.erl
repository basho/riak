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

-export([start_link/4]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]).

-record(state, {done,qterm,next_fsm,coord,acc,reduced,fresh_input}).

start_link(_Ring,QTerm,NextFSM,Coordinator) ->
    gen_fsm:start_link(?MODULE, [QTerm,NextFSM,Coordinator], []).
%% @private
init([QTerm,NextFSM,Coordinator]) ->
    {_,{_,_,_,Acc}} = QTerm,
    riak_eventer:notify(riak_reduce_phase_fsm, reduce_start, start),
    {ok,wait,#state{done=false,qterm=QTerm,next_fsm=NextFSM,fresh_input=false,
                    coord=Coordinator,acc=Acc,reduced=[]}}.

wait(timeout, StateData=#state{next_fsm=NextFSM,done=Done,
                               acc=Acc,fresh_input=Fresh,
                               qterm={Lang,{reduce,FunTerm,Arg,_Acc}},
                               coord=Coord,reduced=Reduced}) ->
    {Res,Red} = case Fresh of
        false ->
            {{next_state, wait, StateData#state{reduced=Reduced}},Reduced};
        true ->
            try
                NewReduced = case {Lang, FunTerm} of
                                 {erlang, {qfun,F}} ->
                                     F(Reduced,Arg);
                                 {erlang, {modfun,M,F}} ->
                                     M:F(Reduced,Arg);
                                 {javascript, QTerm} ->
                                     case js_reduce(QTerm, Reduced, Arg) of
                                         {ok, Result} ->
                                             Result;
                                         {error, Error} ->
                                             error_logger:error_msg("Javascript reduce error: ~p~n", [Error]),
                                             Reduced
                                     end
                             end,
                {{next_state, wait, StateData#state{reduced=NewReduced}}, NewReduced}
            catch C:R ->
                    Reason = {C, R, erlang:get_stacktrace()},
                    case NextFSM of
                        final -> nop;
                        _ -> riak_phase_proto:die(NextFSM)
                    end,
                    riak_phase_proto:error(Coord, Reason),
                    {{stop,normal,StateData},Reduced}
            end
    end,
    case Done of
        false -> Res;
        true ->
            case NextFSM of
                final -> nop;
                _ ->
                    riak_phase_proto:send_inputs(NextFSM, Red),
                    riak_phase_proto:done(NextFSM)
            end,
            case Acc of
                false -> nop;
                true -> riak_phase_proto:phase_results(Coord, Red)
            end,
            riak_phase_proto:phase_done(Coord),
            {stop,normal,StateData}
    end;
wait(done, StateData) ->
    {next_state, wait, StateData#state{done=true}, 1};
wait({input,Inputs}, StateData=#state{reduced=Reduced}) ->
    {next_state, wait,
     StateData#state{reduced=Inputs ++ Reduced, fresh_input=true}, 100};
wait(die, StateData=#state{next_fsm=NextFSM}) ->
    riak_eventer:notify(riak_reduce_phase_fsm, die, die),
    case NextFSM of
        final -> nop;
        _ -> riak_phase_proto:die(NextFSM)
    end,
    {stop,normal,StateData}.

js_reduce(QTerm, Reduced, Arg) ->
    case riak_js_manager:blocking_dispatch({QTerm, Reduced, Arg}) of
        {ok, Result} ->
            Result;
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
