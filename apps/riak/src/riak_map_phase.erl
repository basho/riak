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

-module(riak_map_phase).

-behaviour(luke_phase).

-export([init/1, handle_input/3, handle_input_done/1, handle_event/2,
         handle_info/2, handle_timeout/1, terminate/2]).

-record(state, {done=false, qterm, acc=[], ring, fsms=[]}).

init([QTerm]) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    {ok, #state{ring=Ring, qterm=QTerm}}.

handle_input(Inputs0, #state{ring=Ring, qterm=QTerm, fsms=FSMs0}=State, Timeout) ->
    Inputs = [convert_input(I) || I <- Inputs0],
    NewFSMs = start_executors(Ring, Inputs, QTerm, Timeout),
    {no_output, State#state{fsms=NewFSMs ++ FSMs0}}.

handle_input_done(#state{fsms=[]}=State) ->
    luke_phase:complete(),
    {no_output, State};

handle_input_done(State) ->
    {no_output, State#state{done=true}}.

handle_event({mapexec_reply, Reply, Executor}, #state{done=Done, fsms=[Executor]}=State) ->
    if
        Done =:= true ->
            luke_phase:complete();
        true ->
            ok
    end,
    {output, Reply, State#state{fsms=[]}};
handle_event({mapexec_reply, Reply, Executor}, #state{fsms=FSMs0}=State) ->
    FSMs = lists:delete(Executor, FSMs0),
    {output, Reply, State#state{fsms=FSMs}};
handle_event({mapexec_error, _Executor, Reply}, State) ->
    {stop, Reply, State};
handle_event(_Event, State) ->
    {no_output, State}.

handle_info(_Info, State) ->
    {no_output, State}.

handle_timeout(State) ->
    {no_output, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions
convert_input(I={{_B,_K},_D})
  when is_binary(_B) andalso (is_list(_K) orelse is_binary(_K)) -> I;
convert_input(I={_B,_K})
  when is_binary(_B) andalso (is_list(_K) orelse is_binary(_K)) -> {I,undefined};
convert_input([B,K]) when is_binary(B), is_binary(K) -> {{B,K},undefined};
convert_input([B,K,D]) when is_binary(B), is_binary(K) -> {{B,K},D};
convert_input(I) -> I.

start_executors(Ring, Inputs, QTerm, Timeout) ->
    start_executors(Ring, Inputs, QTerm, Timeout, []).
start_executors(_Ring, [], _QTerm, _Timeout, Accum) ->
    lists:reverse(Accum);
start_executors(Ring, [H|T], QTerm, Timeout, Accum) ->
    case riak_map_executor:start_link(Ring, H, QTerm, Timeout, self()) of
        {ok, FSM} ->
            start_executors(Ring, T, QTerm, Timeout, [FSM|Accum]);
        {error, bad_input} ->
            throw({error, bad_input})
    end.
