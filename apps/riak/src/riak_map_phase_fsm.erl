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

-export([start_link/4]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]).

-record(state, {done,qterm,next_fsm,coord,acc,map_fsms,ring}).

start_link(Ring,QTerm,NextFSM,Coordinator) ->
    gen_fsm:start_link(?MODULE, [Ring,QTerm,NextFSM,Coordinator], []).
%% @private
init([Ring,QTerm,NextFSM,Coordinator]) ->
    {_,_,_,Acc} = QTerm,
    riak_eventer:notify(riak_map_phase_fsm, map_start, start),
    {ok,wait,#state{done=false,qterm=QTerm,next_fsm=NextFSM,
                    coord=Coordinator,acc=Acc,map_fsms=[],ring=Ring}}.

wait({mapexec_reply,Reply,MapFSM}, StateData=
     #state{done=Done,next_fsm=NextFSM,coord=Coord,acc=Acc,map_fsms=FSMs0}) ->
    FSMs = lists:delete(MapFSM,FSMs0),
    case NextFSM of
        final -> nop;
        _ -> gen_fsm:send_event(NextFSM, {input, Reply})
    end,
    case Acc of
        false -> nop;
        true -> gen_fsm:send_event(Coord, {acc, {list, Reply}})
    end,
    case FSMs =:= [] andalso Done =:= true of
        true ->
            finish(StateData);
        false ->
            {next_state, wait, StateData#state{map_fsms=FSMs}}
    end;

wait({mapexec_error, _ErrFSM, ErrMsg}, StateData=
     #state{next_fsm=NextFSM,coord=Coord}) ->
    riak_eventer:notify(riak_map_phase_fsm, error, ErrMsg),
    gen_fsm:send_event(Coord, {error, self(), ErrMsg}),
    case NextFSM of
        final -> nop;
        _ -> gen_fsm:send_event(NextFSM, die)
    end,
    {stop,normal,StateData};
wait(done, StateData=#state{map_fsms=FSMs}) ->
    riak_eventer:notify(riak_map_phase_fsm, done_inputs, done_inputs),
    case FSMs of
        [] -> finish(StateData);
        _ -> {next_state, wait, StateData#state{done=true}}
    end;
wait({input,Inputs0}, StateData=#state{qterm=QTerm,map_fsms=FSMs0,ring=Ring}) ->
    Inputs = [convert_input(I) || I <- Inputs0],
    NewFSMs = [FSM ||
               {ok,FSM} <- [riak_map_executor:start_link(Ring,Input,QTerm,self()) ||
                  Input <- Inputs]],
    FSMs = NewFSMs ++ FSMs0,
    {next_state, wait, StateData#state{map_fsms=FSMs}};
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

finish(StateData=#state{next_fsm=NextFSM,coord=Coord}) ->
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

convert_input(I={{_B,_K},_D})
  when is_binary(_B) andalso (is_list(_K) orelse is_binary(_K)) -> I;
convert_input(I={_B,_K})
  when is_binary(_B) andalso (is_list(_K) orelse is_binary(_K)) -> {I,undefined};
convert_input([B,K]) when is_binary(B), is_binary(K) -> {{B,K},undefined};
convert_input([B,K,D]) when is_binary(B), is_binary(K) -> {{B,K},D}.
