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

-module(riak_put_fsm).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_fsm).

-export([start/5]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([initialize/2,waiting_vnode_w/2,waiting_vnode_dw/2]).

-record(state, {robj :: riak_object:riak_object(), 
                client :: {pid(), reference()}, 
                n :: pos_integer(), 
                w :: pos_integer(), 
                dw :: non_neg_integer(), 
                preflist :: [{pos_integer(), atom()}], 
                bkey :: {riak_object:bucket(), riak_object:key()},
                waiting_for :: list(),
                req_id :: pos_integer(), 
                starttime :: pos_integer(), 
                replied_w :: list(), 
                replied_dw :: list(), 
                replied_fail :: list(),
                timeout :: pos_integer(), 
                endtime :: pos_integer(), 
                ring :: riak_ring:riak_ring()
               }).

start(RObj,W,DW,Timeout,From) ->
    gen_fsm:start(?MODULE, [RObj,W,DW,Timeout,From], []).

%% @private
init([RObj0,W,DW,Timeout,Client]) ->
    {ok,Ring} = riak_ring_manager:get_my_ring(),
    StateData = #state{robj=RObj0, client=Client, w=W, dw=DW,
                       timeout=Timeout, ring=Ring},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{robj=RObj0,
                                      timeout=Timeout, ring=Ring}) ->
    RealStartTime = riak_util:moment(),
    Bucket = riak_object:bucket(RObj0),
    BucketProps = riak_bucket:get_bucket(Bucket, Ring),
    RObj = prune_vclock(update_metadata(RObj0),BucketProps),
    ReqID = erlang:phash2({random:uniform(), self(), RObj, RealStartTime}),
    Key = riak_object:key(RObj),
    riak_eventer:notify(riak_put_fsm, put_fsm_start,
                        {ReqID, RealStartTime, Bucket, Key}),
    DocIdx = chash:key_of({Bucket, Key}),
    Msg = {self(), {Bucket,Key}, RObj, ReqID},
    N = proplists:get_value(n_val,BucketProps),
    Preflist = riak_ring:filtered_preflist(DocIdx, Ring, N),
    {Targets, Fallbacks} = lists:split(N, Preflist),
    {Sent1, Pangs1} = riak_util:try_cast(vnode_put, Msg, Targets),
    Sent = case length(Sent1) =:= N of   % Sent is [{Index,TargetNode,SentNode}]
        true -> Sent1;
        false -> Sent1 ++ riak_util:fallback(vnode_put,Msg,Pangs1,Fallbacks)
    end,
    riak_eventer:notify(riak_put_fsm, put_fsm_sent,
                                {ReqID, [{T,S} || {_I,T,S} <- Sent]}),
    StateData = StateData0#state{
                  robj=RObj, n=N, preflist=Preflist, bkey={Bucket,Key},
                  waiting_for=Sent, req_id=ReqID, starttime=riak_util:moment(),
                  replied_w=[], replied_dw=[], replied_fail=[],
                  endtime=Timeout+riak_util:moment()},
    {next_state,waiting_vnode_w,StateData,Timeout}.

waiting_vnode_w({w, Idx, ReqID},
                  StateData=#state{w=W,dw=DW,req_id=ReqID,client=Client,
                                   replied_w=Replied0, endtime=End}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= W of
        true ->
            case DW of
                0 ->
                    Client ! ok,
                    riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                                        {ReqID, ok}),
                    {stop,normal,StateData};
                _ ->
                    NewStateData = StateData#state{replied_w=Replied},
                    {next_state,waiting_vnode_dw,NewStateData,
                     End-riak_util:moment()}
            end;
        false ->
            NewStateData = StateData#state{replied_w=Replied},
            {next_state,waiting_vnode_w,NewStateData,End-riak_util:moment()}
    end;
waiting_vnode_w({dw, Idx, _ReqID},
                  StateData=#state{replied_dw=Replied0, endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_dw=Replied},
    {next_state,waiting_vnode_w,NewStateData,End-riak_util:moment()};
waiting_vnode_w({fail, Idx, ReqID},
                  StateData=#state{n=N,w=W,client=Client,
                                   replied_fail=Replied0,endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= W of
        true ->
            {next_state,waiting_vnode_w,NewStateData,End-riak_util:moment()};
        false ->
            riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                                {ReqID, {error,too_many_fails,Replied}}),
            Client ! {error,too_many_fails},
            {stop,normal,NewStateData}
    end;
waiting_vnode_w(timeout, StateData=#state{client=Client,req_id=ReqID}) ->
    riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                        {ReqID, {error,timeout}}),
    Client ! {error,timeout},
    {stop,normal,StateData}.

waiting_vnode_dw({w, _Idx, ReqID},
          StateData=#state{req_id=ReqID, endtime=End}) ->
    {next_state,waiting_vnode_dw,StateData,End-riak_util:moment()};
waiting_vnode_dw({dw, Idx, ReqID},
                  StateData=#state{dw=DW, client=Client,
                                   replied_dw=Replied0, endtime=End}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= DW of
        true ->
            riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                                {ReqID, ok}),
            Client ! ok,
            {stop,normal,StateData};
        false ->
            NewStateData = StateData#state{replied_dw=Replied},
            {next_state,waiting_vnode_dw,NewStateData,End-riak_util:moment()}
    end;
waiting_vnode_dw({fail, Idx, ReqID},
                  StateData=#state{n=N,dw=DW,client=Client,
                                   replied_fail=Replied0,endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= DW of
        true ->
            {next_state,waiting_vnode_dw,NewStateData,End-riak_util:moment()};
        false ->
            riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                                {ReqID, {error,too_many_fails,Replied}}),
            Client ! {error,too_many_fails},
            {stop,normal,NewStateData}
    end;
waiting_vnode_dw(timeout, StateData=#state{client=Client,req_id=ReqID}) ->
    riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                        {ReqID, {error,timeout}}),
    Client ! {error,timeout},
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
terminate(Reason, _StateName, _State=#state{req_id=ReqID}) ->
    riak_eventer:notify(riak_put_fsm, put_fsm_end,
                        {ReqID, Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

update_metadata(RObj) ->
    MD0 = riak_object:get_update_metadata(RObj),
    NewMD = case dict:is_key("no_update", MD0) of
        true -> dict:erase("no_update", MD0);
        false -> dict:store(<<"X-Riak-VTag">>,
                       make_vtag(RObj),
                       dict:store(<<"X-Riak-Last-Modified">>,
                                  httpd_util:rfc1123_date(),
                                  MD0))
    end,
    riak_object:apply_updates(riak_object:update_metadata(RObj, NewMD)).

prune_vclock(RObj,BucketProps) ->
    % This function is a little bit evil, as it relies on the
    % internal structure of vclocks.
    % That structure being [{Id, {Vsn, Timestamp}}]
    V = riak_object:vclock(RObj),
    SortV = lists:sort(fun({_,{_,A}},{_,{_,B}}) -> A < B end, V),
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    case prune_vclock1(Now,SortV,BucketProps,no_change) of
        {no_change, _} -> RObj;
        {pruned, NewV} -> riak_object:set_vclock(RObj,NewV)
    end.

prune_vclock1(Now,V,BProps,Changed) ->
    case length(V) =< proplists:get_value(small_vclock,BProps) of
        true -> {Changed, V};
        false ->
            {_,{_,HeadTime}} = hd(V),
            case (Now - HeadTime) < proplists:get_value(young_vclock,BProps) of
                true -> {Changed, V};
                false -> prune_vclock1(Now,V,BProps,Changed,HeadTime)
            end
    end.
prune_vclock1(Now,V,BProps,Changed,HeadTime) ->
    % has a precondition that V is longer than small and older than young
    case length(V) > proplists:get_value(big_vclock,BProps) of
        true -> prune_vclock1(Now,tl(V),BProps,pruned);
        false ->
            case (Now - HeadTime) > proplists:get_value(old_vclock,BProps) of
                true -> prune_vclock1(Now,tl(V),BProps,pruned);
                false -> {Changed, V}
            end
    end.

make_vtag(RObj) ->
    <<HashAsNum:128/integer>> = crypto:md5(iolist_to_binary(io_lib:format("~p",
                                                 [riak_object:vclock(RObj)]))),
    riak_util:integer_to_list(HashAsNum,62).

% following two are just utility functions for test assist
vc_obj(VC) -> riak_object:set_vclock(riak_object:new(<<"b">>,<<"k">>,<<"v">>), VC).
obj_vc(OB) -> riak_object:vclock(OB).

prune_small_vclock_test() ->
    % vclock with less entries than small_vclock will be untouched
    OldTime = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
               - 32000000,
    SmallVC = [{<<"1">>, {1, OldTime}},
               {<<"2">>, {2, OldTime}},
               {<<"3">>, {3, OldTime}}],
    Props = [{small_vclock,4}],
    ?assertEqual(SmallVC, obj_vc(prune_vclock(vc_obj(SmallVC), Props))).

prune_young_vclock_test() ->
    % vclock with all entries younger than young_vclock will be untouched
    NewTime = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
               - 1,
    VC = [{<<"1">>, {1, NewTime}},
          {<<"2">>, {2, NewTime}},
          {<<"3">>, {3, NewTime}}],
    Props = [{small_vclock,1},{young_vclock,1000}],
    ?assertEqual(VC, obj_vc(prune_vclock(vc_obj(VC), Props))).

prune_big_vclock_test() ->
    % vclock not preserved by small or young will be pruned down to
    % no larger than big_vclock entries
    NewTime = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
               - 1000,
    VC = [{<<"1">>, {1, NewTime}},
          {<<"2">>, {2, NewTime}},
          {<<"3">>, {3, NewTime}}],
    Props = [{small_vclock,1},{young_vclock,1},
             {big_vclock,2},{old_vclock,100000}],
    ?assert(length(obj_vc(prune_vclock(vc_obj(VC), Props))) =:= 2).

prune_old_vclock_test() ->
    % vclock not preserved by small or young will be pruned down to
    % no larger than big_vclock and no entries more than old_vclock ago
    NewTime = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
               - 1000,
    OldTime = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
               - 100000,    
    VC = [{<<"1">>, {1, NewTime}},
          {<<"2">>, {2, OldTime}},
          {<<"3">>, {3, OldTime}}],
    Props = [{small_vclock,1},{young_vclock,1},
             {big_vclock,2},{old_vclock,10000}],
    ?assert(length(obj_vc(prune_vclock(vc_obj(VC), Props))) =:= 1).

make_vtag_test() ->
    Obj = riak_object:new(<<"b">>,<<"k">>,<<"v1">>),
    ?assertNot(make_vtag(Obj) =:= 
               make_vtag(riak_object:increment_vclock(Obj,<<"client_id">>))).
