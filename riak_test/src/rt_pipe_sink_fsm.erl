%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Sink fsm for eunit tests. It accumulates all pipe results/logs
%% until eoi, then delivers them to the caller of {@link
%% get_results/1}.
%%
%% There are two states: `acc' and `wait'
%%
%% The FSM starts out in `acc' simply accumulating results and logs.
%%
%% If a `get_results' arrives during `acc', the FSM stores the `From',
%% and continues accumulating results/logs in `acc'.
%%
%% If an `eoi' comes in during `acc' before `get_results', the FSM
%% transitions to `wait', where it stops accumulating results/logs,
%% and just waits for `get_results'.
%%
%% As soon as both `eoi' and `get_results' have arrived, the
%% results/logs are delivered to the caller of {@link get_results/1},
%% and the FSM exits.
%%
%% The sink can be told not to ack a list of `{FittingName, Output}'
%% pairs, for testing purposes, by passing such a list as the value of
%% a `skip_ack' property in the `Options' parameter of {@link
%% start_link/2}. For example, to tell the sink not to ack the output
%% `bar' from the fitting `foo':
%%
%% `rt_pipe_sink_fsm:start_link(PipeRef, [{skip_ack, [{foo,bar}]}])'
%%
%% The skipped output will still be added to the results list.
-module(rt_pipe_sink_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1,
         start_link/2,
         get_results/1]).

%% gen_fsm callbacks
-export([init/1,
         acc/2, acc/3,
         wait/2, wait/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("rt_pipe.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          ref,
          opts,
          from,
          results,
          logs
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(PipeRef) ->
    start_link(PipeRef, []).

start_link(PipeRef, Options) ->
    gen_fsm:start_link(?MODULE, [PipeRef, Options], []).

get_results(Sink) ->
    gen_fsm:sync_send_event(Sink, get_results).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([PipeRef, Opts]) ->
    {ok, acc, #state{ref=PipeRef, opts=Opts, results=[], logs=[]}}.

%% ACC: just accumulating
%%   - #pipe_eoi{} -> eoi
%%   - get_results -> respond
acc(#pipe_result{ref=Ref}=Result, #state{ref=Ref}=State) ->
    NewState = add(Result, State, async),
    {next_state, acc, NewState};
acc(#pipe_log{ref=Ref}=Result, #state{ref=Ref}=State) ->
    NewState = add(Result, State, async),
    {next_state, acc, NewState};
acc(#pipe_eoi{ref=Ref}, #state{ref=Ref}=State) ->
    case State#state.from of
        undefined ->
            {next_state, wait, State};
        From ->
            gen_fsm:reply(From, results(State)),
            {stop, normal, State}
    end;
acc(_, State) ->
    {next_state, acc, State}.

add(#pipe_result{from=F, result={Type, _}=R},
    #state{results=Acc}=State,
    Type) ->
    State#state{results=[{F, R}|Acc]};
add(#pipe_result{from=F, result=R},
    #state{logs=Acc}=State,
    Type) ->
    Msg = {wrong_type, [{expected, Type},
                        {received, R}]},
    State#state{logs=[{F, Msg}|Acc]};
add(#pipe_log{from=F, msg=M}, #state{logs=Acc}=State, _Type) ->
    State#state{logs=[{F,M}|Acc]}.

acc(#pipe_result{ref=Ref}=Result, _From, #state{ref=Ref}=State) ->
    NewState = add(Result, State, sync),
    #pipe_result{from=F, result=R} = Result,
    case should_skip_ack(F, R, NewState) of
        true ->
            {next_state, acc, NewState};
        false ->
            {reply, ok, acc, NewState}
    end;
acc(#pipe_log{ref=Ref}=Log, _From, #state{ref=Ref}=State) ->
    NewState = add(Log, State, sync),
    {reply, ok, acc, NewState};
acc(#pipe_eoi{ref=Ref}, _From, #state{ref=Ref}=State) ->
    case State#state.from of
        undefined ->
            {reply, ok, wait, State};
        From ->
            gen_fsm:reply(From, results(State)),
            {stop, normal, ok, State}
    end;
acc(get_results, From, State) ->
    {next_state, acc, State#state{from=From}}.

%% WAIT: pipe ended, holding on for get_results
%%   - get_results -> stop
wait(_, State) ->
    {next_state, wait, State}.
wait(get_results, _From, State) ->
    {stop, normal, results(State), State}.

%% unused FSM bits
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

results(#state{results=Results, logs=Logs}) ->
     {eoi, lists:reverse(Results), lists:reverse(Logs)}.    

should_skip_ack(From, Result, #state{opts=Opts}) ->
    SkipList = proplists:get_value(skip_ack, Opts, []),
    lists:member({From, Result}, SkipList).
