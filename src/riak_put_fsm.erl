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

-export([start/6]).
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

start(ReqId,RObj,W,DW,Timeout,From) ->
    gen_fsm:start(?MODULE, [ReqId,RObj,W,DW,Timeout,From], []).

%% @private
init([ReqId,RObj0,W,DW,Timeout,Client]) ->
    {ok,Ring} = riak_ring_manager:get_my_ring(),
    StateData = #state{robj=RObj0, client=Client, w=W, dw=DW,
                       req_id=ReqId, timeout=Timeout, ring=Ring},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{robj=RObj0, req_id=ReqId,
                                      timeout=Timeout, ring=Ring}) ->
    RObj = update_metadata(RObj0),
    RealStartTime = riak_util:moment(),
    Bucket = riak_object:bucket(RObj),
    BucketProps = riak_bucket:get_bucket(Bucket, Ring),
    Key = riak_object:key(RObj),
    riak_eventer:notify(riak_put_fsm, put_fsm_start,
                        {ReqId, RealStartTime, Bucket, Key}),
    DocIdx = riak_util:chash_key({Bucket, Key}),
    Msg = {self(), {Bucket,Key}, RObj, ReqId, RealStartTime},
    N = proplists:get_value(n_val,BucketProps),
    Preflist = riak_ring:filtered_preflist(DocIdx, Ring, N),
    {Targets, Fallbacks} = lists:split(N, Preflist),
    {Sent1, Pangs1} = riak_util:try_cast(vnode_put, Msg, nodes(), Targets),
    Sent = case length(Sent1) =:= N of   % Sent is [{Index,TargetNode,SentNode}]
        true -> Sent1;
        false -> Sent1 ++ riak_util:fallback(vnode_put,Msg,Pangs1,Fallbacks)
    end,
    riak_eventer:notify(riak_put_fsm, put_fsm_sent,
                                {ReqId, [{T,S} || {_I,T,S} <- Sent]}),
    StateData = StateData0#state{
                  robj=RObj, n=N, preflist=Preflist, bkey={Bucket,Key},
                  waiting_for=Sent, starttime=riak_util:moment(),
                  replied_w=[], replied_dw=[], replied_fail=[],
                  endtime=Timeout+riak_util:moment()},
    {next_state,waiting_vnode_w,StateData,Timeout}.

waiting_vnode_w({w, Idx, ReqId},
                  StateData=#state{w=W,dw=DW,req_id=ReqId,client=Client, bkey={Bucket, Key},
                                   replied_w=Replied0, endtime=End}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= W of
        true ->
            case DW of
                0 ->
                    Client ! {ReqId, ok},
                    riak_eventer:notify(riak_put_fsm, put_fsm_reply_ok,
                                        {ReqId, ok, {Bucket, Key}}),
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
waiting_vnode_w({dw, Idx, _ReqId},
                  StateData=#state{replied_dw=Replied0, endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_dw=Replied},
    {next_state,waiting_vnode_w,NewStateData,End-riak_util:moment()};
waiting_vnode_w({fail, Idx, ReqId},
                  StateData=#state{n=N,w=W,client=Client,
                                   replied_fail=Replied0,endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= W of
        true ->
            {next_state,waiting_vnode_w,NewStateData,End-riak_util:moment()};
        false ->
            riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                                {ReqId, {error,too_many_fails,Replied}}),
            Client ! {ReqId, {error,too_many_fails}},
            {stop,normal,NewStateData}
    end;
waiting_vnode_w(timeout, StateData=#state{client=Client,req_id=ReqId}) ->
    riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                        {ReqId, {error,timeout}}),
    Client ! {ReqId, {error,timeout}},
    {stop,normal,StateData}.

waiting_vnode_dw({w, _Idx, ReqId},
          StateData=#state{req_id=ReqId, endtime=End}) ->
    {next_state,waiting_vnode_dw,StateData,End-riak_util:moment()};
waiting_vnode_dw({dw, Idx, ReqId},
                 StateData=#state{dw=DW, client=Client, bkey={Bucket, Key},
                                   replied_dw=Replied0, endtime=End}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= DW of
        true ->
            riak_eventer:notify(riak_put_fsm, put_fsm_reply_ok,
                                {ReqId, ok, {Bucket, Key}}),
            Client ! {ReqId, ok},
            {stop,normal,StateData};
        false ->
            NewStateData = StateData#state{replied_dw=Replied},
            {next_state,waiting_vnode_dw,NewStateData,End-riak_util:moment()}
    end;
waiting_vnode_dw({fail, Idx, ReqId},
                  StateData=#state{n=N,dw=DW,client=Client,
                                   replied_fail=Replied0,endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= DW of
        true ->
            {next_state,waiting_vnode_dw,NewStateData,End-riak_util:moment()};
        false ->
            riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                                {ReqId, {error,too_many_fails,Replied}}),
            Client ! {ReqId, {error,too_many_fails}},
            {stop,normal,NewStateData}
    end;
waiting_vnode_dw(timeout, StateData=#state{client=Client,req_id=ReqId}) ->
    riak_eventer:notify(riak_put_fsm, put_fsm_reply,
                        {ReqId, {error,timeout}}),
    Client ! {ReqId, {error,timeout}},
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
terminate(Reason, _StateName, _State=#state{req_id=ReqId}) ->
    riak_eventer:notify(riak_put_fsm, put_fsm_end,
                        {ReqId, Reason}),
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
                                  erlang:now(),
                                  MD0))
    end,
    riak_object:apply_updates(riak_object:update_metadata(RObj, NewMD)).

make_vtag(RObj) ->
    <<HashAsNum:128/integer>> = crypto:md5(term_to_binary(riak_object:vclock(RObj))),
    riak_util:integer_to_list(HashAsNum,62).

make_vtag_test() ->
    Obj = riak_object:new(<<"b">>,<<"k">>,<<"v1">>),
    ?assertNot(make_vtag(Obj) =:= 
               make_vtag(riak_object:increment_vclock(Obj,<<"client_id">>))).
