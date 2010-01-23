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

-record(state, {client,reqid,fsms,starttime,timeout,ring,input_done}).

start(ReqId,Query,Timeout,Client) ->
    gen_fsm:start(?MODULE, [ReqId,Query,Timeout,Client], []).
%% @private
init([ReqId,Query,Timeout,Client]) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_start, {ReqId, Query}),
    case check_query_syntax(Query) of
        {ok, Query1} ->
            FSMs = make_phase_fsms(Query1, Ring), % Pid for each phase, in-order
            StateData = #state{client=Client,fsms=FSMs,reqid=ReqId,
                               starttime=riak_util:moment(),timeout=Timeout,
                               ring=Ring,input_done=false},
            {ok,wait,StateData,Timeout};
        {bad_qterm, QTerm} ->
            riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done,
                                {error, {bad_qterm, QTerm}}),
            {stop, {bad_qterm, QTerm}}
    end.

check_query_syntax(Query) ->
    check_query_syntax(Query, []).

check_query_syntax([], Accum) ->
    {ok, lists:reverse(Accum)};
check_query_syntax([QTerm={QTermType,QT2,QT3,Acc}|Rest], Accum)
  when is_boolean(Acc) ->
    case lists:member(QTermType, [link,map,reduce]) of
        false -> {bad_qterm, QTerm};
        true ->
            case QTermType of
                link ->
                    case (is_binary(QT2) orelse QT2 == '_') of
                        false -> {bad_qterm, QTerm};
                        true -> check_query_syntax(Rest)
                    end;
                _ -> % (map or reduce)
                    case QT2 of
                        {modfun, MF_M, MF_F} ->
                            case is_atom(MF_M) andalso is_atom(MF_F) of
                                false -> {bad_qterm, QTerm};
                                true -> check_query_syntax(Rest, [{erlang, QTerm}|Accum])
                            end;
                        {qfun, QF_F} ->
                            case is_function(QF_F) of
                                false -> {bad_qterm, QTerm};
                                true -> check_query_syntax(Rest, [{erlang, QTerm}|Accum])
                            end;
                        {jsanon, JS} when is_binary(JS) ->
                            check_query_syntax(Rest, [{javascript, QTerm}|Accum]);
                        {jsanon, {Bucket, Key}} when is_binary(Bucket),
                                                     is_binary(Key) ->
                            case fetch_js(Bucket, Key) of
                                {ok, JS} ->
                                    check_query_syntax(Rest, [{javascript, {map, {jsanon, JS}, QT3, Acc}}|Accum]);
                                _ ->
                                    {bad_qterm, QTerm}
                            end;
                        {jsfun, JS} when is_binary(JS) ->
                            check_query_syntax(Rest, [{javascript, QTerm}|Accum]);
                        _ -> {bad_qterm, QTerm}
                    end
            end
    end;
check_query_syntax([BadQTerm|_], _) -> {bad_qterm,BadQTerm}.

make_phase_fsms(Query, Ring) ->
    make_phase_fsms(lists:reverse(Query),final,[], Ring).
make_phase_fsms([], _NextFSM, FSMs, _Ring) -> FSMs;
make_phase_fsms([QTerm|Rest], NextFSM, FSMs, Ring) ->
    {ok, Pid} = case QTerm of
                    {_, {reduce, _, _, _}} ->
                        riak_phase_sup:new_reduce_phase(Ring, QTerm, NextFSM, self());
                    {_, {map, _, _, _}} ->
                        riak_phase_sup:new_map_phase(Ring, QTerm, NextFSM, self());
                    {_, {link, _, _, _}} ->
                        riak_phase_sup:new_map_phase(Ring, QTerm, NextFSM, self())
                end,
    make_phase_fsms(Rest,Pid,[Pid|FSMs], Ring).

wait({input,Inputs},
     StateData=#state{reqid=ReqId,timeout=Timeout,fsms=FSMs}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_got_input,
                        {ReqId, length(Inputs)}),
    riak_phase_proto:send_inputs(hd(FSMs), Inputs),
    {next_state, wait, StateData, Timeout};
wait(input_done, StateData=#state{reqid=ReqId,fsms=FSMs}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_done_input, {ReqId}),
    riak_phase_proto:done(hd(FSMs)),
    maybe_finish(StateData#state{input_done=true});
wait({done,FSM}, StateData=#state{fsms=FSMs0}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done_msg, {FSM,FSMs0}),
    FSMs = lists:delete(FSM,FSMs0),
    maybe_finish(StateData#state{fsms=FSMs});
wait({error, ErrFSM, ErrMsg}, StateData=#state{client=Client,reqid=ReqId,
                                               fsms=FSMs0}) ->
    FSMs = lists:delete(ErrFSM,FSMs0),
    riak_phase_proto:die_all(FSMs),
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done, {error, ReqId}),
    Client ! {ReqId, {error, ErrMsg}},
    {stop,normal,StateData};
wait({acc,Data}, StateData=#state{reqid=ReqId,client=Client,timeout=Timeout}) ->
    LData = case Data of
        {single, X} -> [X];
        {list, X} -> X
    end,
    Client ! {ReqId, {mr_results, LData}},
    {next_state, wait, StateData,Timeout};
wait(timeout, StateData=#state{reqid=ReqId,client=Client}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done, {timeout, ReqId}),
    Client ! {ReqId, {error, timeout}},
    {stop,normal,StateData}.

%% @private
maybe_finish(StateData=#state{input_done=Input_Done,fsms=FSMs,
                client=Client,reqid=ReqId,timeout=Timeout}) ->
    case Input_Done of
        false ->
            {next_state, wait, StateData, Timeout};
        true ->
            case FSMs of
                [] ->
                    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_done,
                                        {ok, ReqId}),
                    Client ! {ReqId, done},
                    {stop,normal,StateData};
                _ ->
                    {next_state, wait, StateData, Timeout}
            end
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
terminate(Reason, _StateName, _State=#state{reqid=ReqId}) ->
    riak_eventer:notify(riak_mapreduce_fsm, mr_fsm_end, {ReqId, Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

fetch_js(Bucket, Key) ->
    {ok, Client} = riak:local_client(),
    case Client:get(Bucket, Key, 1) of
        {ok, Obj} ->
            {ok, riak_object:get_value(Obj)};
        _ ->
            {error, bad_fetch}
    end.
