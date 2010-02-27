%% -------------------------------------------------------------------
%%
%% riak_mapred_query: driver for mapreduce query
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc driver for mapreduce query.
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

-module(riak_mapred_query).

-export([start/5]).

start(Node, Client, ReqId, Query0, Timeout) ->
    EffectiveTimeout = erlang:trunc(Timeout  * 1.1),
    case check_query_syntax(Query0) of
        {ok, Query} ->
            luke:new_flow(Node, Client, ReqId, Query, EffectiveTimeout);
        {bad_qterm, QTerm} ->
            {stop, {bad_qterm, QTerm}}
    end.

check_query_syntax(Query) ->
    check_query_syntax(lists:reverse(Query), []).

check_query_syntax([], Accum) ->
    {ok, Accum};
check_query_syntax([QTerm={QTermType, QueryFun, Misc, Acc}|Rest], Accum) when is_boolean(Acc) ->
    PhaseDef = case QTermType of
                   link ->
                       {phase_mod(link), phase_behavior(link, QueryFun, Acc), [{erlang, QTerm}]};
                   T when T =:= map orelse T=:= reduce ->
                       case QueryFun of
                           {modfun, Mod, Fun} when is_atom(Mod),
                                                   is_atom(Fun) ->
                               {phase_mod(T), phase_behavior(T, QueryFun, Acc), [{erlang, QTerm}]};
                           {qfun, Fun} when is_function(Fun) ->
                               {phase_mod(T), phase_behavior(T, QueryFun, Acc), [{erlang, QTerm}]};
                           {jsanon, JS} when is_binary(JS) ->
                               {phase_mod(T), phase_behavior(T, QueryFun, Acc), [{javascript, QTerm}]};
                           {jsanon, {Bucket, Key}} when is_binary(Bucket),
                                                        is_binary(Key) ->
                               case fetch_js(Bucket, Key) of
                                   {ok, JS} ->
                                       {phase_mod(T), phase_behavior(T, QueryFun, Acc), [{javascript,
                                                                                          {T, {jsanon, JS}, Misc, Acc}}]};
                                   _ ->
                                       {bad_qterm, QTerm}
                               end;
                           {jsfun, JS} when is_binary(JS) ->
                               {phase_mod(T), phase_behavior(T, QueryFun, Acc), [{javascript, QTerm}]};
                           _ ->
                               {bad_qterm, QTerm}
                       end
               end,
    case PhaseDef of
        {bad_qterm, _} ->
            PhaseDef;
        _ ->
            check_query_syntax(Rest, [PhaseDef|Accum])
    end.

phase_mod(link) ->
    riak_map_phase;
phase_mod(map) ->
    riak_map_phase;
phase_mod(reduce) ->
    riak_reduce_phase.

phase_behavior(link, _QueryFun, true) ->
    [accumulate];
phase_behavior(link, _QueryFun, false) ->
    [];
phase_behavior(map, _QueryFun, true) ->
    [accumulate];
phase_behavior(map, _QueryFun, false) ->
    [];
%% Turn off parallel converges for jsanon since
%% they take too long to execute and wind up
%% monopolizing the available JS VMs on a given node
phase_behavior(reduce, {FunType, _}, Accumulate) ->
    CP = if
             FunType =:= jsanon ->
                 1;
             true ->
                 2
         end,
    if
        Accumulate =:= true ->
            [{converge, CP}, accumulate];
        true ->
            [{converge, CP}]
    end;
phase_behavior(reduce, {modfun, _, _}, Accumulate) ->
    if
        Accumulate =:= true ->
            [{converge, 2}, accumulate];
        true ->
            [{converge, 2}]
    end.
fetch_js(Bucket, Key) ->
    {ok, Client} = riak:local_client(),
    case Client:get(Bucket, Key, 1) of
        {ok, Obj} ->
            {ok, riak_object:get_value(Obj)};
        _ ->
            {error, bad_fetch}
    end.
