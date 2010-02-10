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

-module(riak_reduce_phase).

-behaviour(luke_phase).

-export([init/1, handle_input/3, handle_input_done/1, handle_event/2,
         handle_timeout/1, handle_info/2, terminate/2]).

-record(state, {new_input=false, qterm, reduced=[]}).

%% @private
init([QTerm]) ->
    riak_eventer:notify(riak_reduce_phase_fsm, reduce_start, start),
    {ok, #state{qterm=QTerm}}.

handle_input(Inputs, #state{reduced=Reduced}=State0, _Timeout) ->
    State = State0#state{reduced=Inputs ++ Reduced, new_input=true},
    {no_output, State, 50}.

handle_input_done(#state{qterm=QTerm, reduced=Reduced0, new_input=NewInput}=State) ->
    case NewInput of
        true ->
            case perform_reduce(QTerm, Reduced0) of
                {ok, Reduced} ->
                    luke_phase:complete(),
                    {output, Reduced, State#state{reduced=Reduced}};
                Error ->
                    {stop, Error, State#state{reduced=[]}}
            end;
        false ->
            luke_phase:complete(),
            {no_output, State}
    end.

handle_timeout(#state{new_input=true, reduced=Reduced0, qterm=QTerm}=State) ->
    case perform_reduce(QTerm, Reduced0) of
        {ok, Reduced} ->
            {no_output, State#state{reduced=Reduced, new_input=false}};
        Error ->
            {stop, Error, State#state{reduced=[]}}
    end;
handle_timeout(#state{new_input=false}=State) ->
    {no_output, State}.

handle_event(_Event, State) ->
    {no_output, State}.

handle_info(_Info, State) ->
    {no_output, State}.

terminate(_Reason, _State) ->
    ok.

perform_reduce({Lang,{reduce,FunTerm,Arg,_Acc}},
               Reduced) ->
    try
        case {Lang, FunTerm} of
            {erlang, {qfun,F}} ->
                {ok, F(Reduced,Arg)};
            {erlang, {modfun,M,F}} ->
                {ok, M:F(Reduced,Arg)};
            {javascript, _} ->
                riak_js_manager:blocking_dispatch({FunTerm, Reduced, Arg})
        end
    catch C:R ->
            Reason = {C, R, erlang:get_stacktrace()},
            {error, Reason}
    end.
