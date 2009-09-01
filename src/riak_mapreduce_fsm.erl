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

%% @doc riak_mapreduce_fsm is the driver of a mapreduce query.
%%
%%      Map phases are expected to have inputs of the form
%%      [{Bucket,Key}] or [{{Bucket,Key},KeyData}] (the first form is
%%      equivalent to [{{Bucket,Key},undefined}]) and will execute
%%      with locality to each key and must return a list that is valid
%%      input to the next phase
%%
%%      Reduce phases take any list, but the function must be
%%      commutative and associative, and the next phase will block
%%      until the reduce phase is entirely done, and the reduce fun
%%      must return a list that is valid input to the next phase
%%
%%      Valid terms for Query:
%%<ul>
%%<li>  {link, Bucket, Tag, Acc}</li>
%%<li>  {map, FunTerm, Arg, Acc}</li>
%%<li>  {reduce, FunTerm, Arg, Acc}</li>
%%</ul>
%%      where FunTerm is one of:
%% <ul>
%%<li>  {modfun, Mod, Fun} : Mod and Fun both atoms ->
%%         Mod:Fun(Object,KeyData,Arg)</li>
%%<li>  {qfun, Fun} : Fun is an actual fun ->
%%         Fun(Object,KeyData,Arg)</li>
%%</ul>
%% @type mapred_queryterm() =
%%         {map, mapred_funterm(), Arg :: term(),
%%          Accumulate :: boolean()} |
%%         {reduce, mapred_funterm(), Arg :: term(),
%%          Accumulate :: boolean()} |
%%         {link, Bucket :: riak_object:bucket(), Tag :: term(),
%%          Accumulate :: boolean()}
%% @type mapred_funterm() =
%%         {modfun, Module :: atom(), Function :: atom()}|
%%         {qfun, function()}
%% @type mapred_result() = [term()]

-module(riak_mapreduce_fsm).
-behaviour(gen_fsm).

-export([start/4]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]). 

-record(state, {client, reqid, acc, fsms, starttime, endtime, ring}).

start(Inputs,Query,Timeout,Client) ->
    gen_fsm:start(?MODULE, [Inputs,Query,Timeout,Client], []).
%% @private
init([Inputs,Query,Timeout,Client]) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    RealStartTime = riak_util:moment(),
    ReqID = erlang:phash2({random:uniform(), self(), RealStartTime}),
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_start,
              {ReqID, length(Inputs), length(Query)}),
    case check_query_syntax(Query) of
        ok ->
            FSMs = make_phase_fsms(Query, Ring), % Pid for each phase, in-order
            gen_fsm:send_event(hd(FSMs), {input, Inputs}),
            gen_fsm:send_event(hd(FSMs), done),
            StateData = #state{client=Client,fsms=FSMs,acc=[],reqid=ReqID,
                               starttime=riak_util:moment(),
                               endtime=Timeout+riak_util:moment(),
                               ring=Ring},
            {ok,wait,StateData,Timeout};
        {bad_qterm, QTerm} ->
            riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done,
                                {error, {bad_qterm, QTerm}}),
            Client ! {error, {bad_qterm, QTerm}},
            {stop,normal}
    end.

check_query_syntax([]) -> ok;
check_query_syntax([QTerm={QTermType,QT2,_QT3,Acc}|Rest])
  when is_boolean(Acc) ->
    case lists:member(QTermType, [link,map,reduce]) of
        false -> {bad_qterm, QTerm};
        true ->
            case QTermType of
                link ->
                    case is_atom(QT2) of
                        false -> {bad_qterm, QTerm};
                        true -> check_query_syntax(Rest)
                    end;
                _ -> % (map or reduce)
                    case QT2 of
                        {modfun, MF_M, MF_F} ->
                            case is_atom(MF_M) andalso is_atom(MF_F) of
                                false -> {bad_qterm, QTerm};
                                true -> check_query_syntax(Rest)
                            end;
                        {qfun, QF_F} ->
                            case is_function(QF_F) of
                                false -> {bad_qterm, QTerm};
                                true -> check_query_syntax(Rest)
                            end;
                        _ -> {bad_qterm, QTerm}
                    end
            end
    end;
check_query_syntax([BadQTerm|_]) -> {bad_qterm,BadQTerm}.

make_phase_fsms(Query, Ring) -> 
    make_phase_fsms(lists:reverse(Query),final,[], Ring).
make_phase_fsms([], _NextFSM, FSMs, _Ring) -> FSMs;
make_phase_fsms([QTerm|Rest], NextFSM, FSMs, Ring) -> 
    PhaseMod = case QTerm of
        {reduce, _, _, _} -> riak_reduce_phase_fsm;
        {map, _, _, _} -> riak_map_phase_fsm;
        {link, _, _, _} -> riak_map_phase_fsm
    end,
    {ok, Pid} = PhaseMod:start_link(Ring, QTerm, NextFSM, self()),
    make_phase_fsms(Rest,Pid,[Pid|FSMs], Ring).

wait({done,FSM}, StateData=#state{client=Client,acc=Acc,reqid=ReqID,
                                  endtime=End,fsms=FSMs0}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done_msg, {FSM,FSMs0}),
    FSMs = lists:delete(FSM,FSMs0),
    case FSMs of
        [] -> 
            riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done,
                                {ok, ReqID, length(Acc)}),
            Client ! {ok, Acc},
            {stop,normal,StateData};
        _ ->
            {next_state, wait, StateData#state{fsms=FSMs},
             End-riak_util:moment()}
    end;
wait({error, ErrFSM, ErrMsg}, StateData=#state{client=Client,reqid=ReqID,
                                               fsms=FSMs0}) ->
    FSMs = lists:delete(ErrFSM,FSMs0),
    [gen_fsm:send_event(FSM, die) || FSM <- FSMs],
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done, {error, ReqID}),
    Client ! {error, ErrMsg},
    {stop,normal,StateData};
wait({acc,Data}, StateData=#state{acc=Acc,endtime=End}) ->
    AccData = case Data of
        {single, X} -> [X|Acc];
        {list, X} -> X ++ Acc
    end,
    {next_state, wait, StateData#state{acc=AccData},End-riak_util:moment()};
wait(timeout, StateData=#state{reqid=ReqID,client=Client}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done, {timeout, ReqID}),
    Client ! {error, timeout},
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
terminate(Reason, _StateName, _State=#state{reqid=ReqID}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_end, {ReqID, Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

