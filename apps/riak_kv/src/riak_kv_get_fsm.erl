%% -------------------------------------------------------------------
%%
%% riak_get_fsm: coordination of Riak GET requests
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

-module(riak_kv_get_fsm).
-behaviour(gen_fsm).

-export([start/6]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([initialize/2,waiting_vnode_r/2,waiting_read_repair/2]).

-record(state, {client :: {pid(), reference()},
                n :: pos_integer(), 
                r :: pos_integer(), 
                allowmult :: boolean(), 
                preflist :: [{pos_integer(), atom()}], 
                waiting_for :: [{pos_integer(), atom(), atom()}],
                req_id :: pos_integer(), 
                starttime :: pos_integer(), 
                replied_r :: list(), 
                replied_notfound :: list(),
                replied_fail :: list(),
                repair_sent :: list(), 
                final_obj :: undefined|riak_object:riak_object(),
                timeout :: pos_integer(),
                tref    :: reference(),
                bkey :: {riak_object:bucket(), riak_object:key()},
                ring :: riak_core_ring:riak_core_ring(),
                startnow :: {pos_integer(), pos_integer(), pos_integer()}
               }).

start(ReqId,Bucket,Key,R,Timeout,From) ->
    gen_fsm:start(?MODULE, [ReqId,Bucket,Key,R,Timeout,From], []).

%% @private
init([ReqId,Bucket,Key,R,Timeout,Client]) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    StateData = #state{client=Client,r=R, timeout=Timeout,
                req_id=ReqId, bkey={Bucket,Key}, ring=Ring},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{timeout=Timeout, req_id=ReqId,
                                      bkey={Bucket,Key}, ring=Ring}) ->
    StartNow = now(),
    TRef = erlang:send_after(Timeout, self(), timeout),
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    Msg = {self(), {Bucket,Key}, ReqId},
    BucketProps = riak_core_bucket:get_bucket(Bucket, Ring),
    N = proplists:get_value(n_val,BucketProps),
    AllowMult = proplists:get_value(allow_mult,BucketProps),
    Preflist = riak_core_ring:preflist(DocIdx, Ring),
    {Targets, Fallbacks} = lists:split(N, Preflist),
    {Sent1, Pangs1} = riak_kv_util:try_cast(vnode_get, Msg, nodes(), Targets),
    Sent = case length(Sent1) =:= N of   % Sent is [{Index,TargetNode,SentNode}]
        true -> Sent1;
        false -> Sent1 ++ riak_kv_util:fallback(vnode_get,Msg,Pangs1,Fallbacks)
    end,
    StateData = StateData0#state{n=N,allowmult=AllowMult,repair_sent=[],
                       preflist=Preflist,final_obj=undefined,
                       replied_r=[],replied_fail=[],
                       replied_notfound=[],starttime=riak_core_util:moment(),
                       waiting_for=Sent,tref=TRef,startnow=StartNow},
    {next_state,waiting_vnode_r,StateData}.

waiting_vnode_r({r, {ok, RObj}, Idx, ReqId},
                  StateData=#state{r=R,allowmult=AllowMult,
                                   req_id=ReqId,client=Client,
                                   replied_r=Replied0}) ->
    Replied = [{RObj,Idx}|Replied0],
    case length(Replied) >= R of
        true ->
            Final = respond(Client,Replied,AllowMult,ReqId),
            update_stats(StateData),
            NewStateData = StateData#state{replied_r=Replied,final_obj=Final},
            finalize(NewStateData);
        false ->
            NewStateData = StateData#state{replied_r=Replied},
            {next_state,waiting_vnode_r,NewStateData}
    end;
waiting_vnode_r({r, {error, notfound}, Idx, ReqId},
                  StateData=#state{r=R,allowmult=AllowMult,replied_fail=Fails,
                                   req_id=ReqId,client=Client,n=N,
                                   replied_r=Replied,
                                   replied_notfound=NotFound0}) ->
    NotFound = [Idx|NotFound0],
    NewStateData = StateData#state{replied_notfound=NotFound},
    FailThreshold = erlang:min(trunc((N/2.0)+1), % basic quorum, or
                               (N-R+1)), % cannot ever get R 'ok' replies
    %% FailThreshold = N-R+1,
    case (length(NotFound) + length(Fails)) >= FailThreshold of
        false ->
            {next_state,waiting_vnode_r,NewStateData};
        true ->
            update_stats(StateData),
            Client ! {ReqId, {error,notfound}},
            Final = merge(Replied,AllowMult),
            finalize(NewStateData#state{final_obj=Final})
    end;
waiting_vnode_r({r, {error, Err}, Idx, ReqId},
                  StateData=#state{r=R,client=Client,n=N,allowmult=AllowMult,
                                   replied_r=Replied,
                                   replied_fail=Failed0,req_id=ReqId,
                                   replied_notfound=NotFound}) ->
    Failed = [{Err,Idx}|Failed0],
    NewStateData = StateData#state{replied_fail=Failed},
    FailThreshold = erlang:min(trunc((N/2.0)+1), % basic quorum, or
                               (N-R+1)), % cannot ever get R 'ok' replies
    %% FailThreshold = N-R+1,
    case (length(Failed) + length(NotFound)) >= FailThreshold of
        false ->
            {next_state,waiting_vnode_r,NewStateData};
        true ->
            case length(NotFound) of
                0 ->
                    FullErr = [E || {E,_I} <- Failed],
                    update_stats(StateData),
                    Client ! {ReqId, {error,FullErr}};
                _ ->
                    update_stats(StateData),
                    Client ! {ReqId, {error,notfound}}
            end,
            Final = merge(Replied,AllowMult),
            finalize(NewStateData#state{final_obj=Final})
    end;
waiting_vnode_r(timeout, StateData=#state{client=Client,req_id=ReqId,replied_r=Replied,allowmult=AllowMult}) ->
    update_stats(StateData),
    Client ! {ReqId, {error,timeout}},
    really_finalize(StateData#state{final_obj=merge(Replied, AllowMult)}).

waiting_read_repair({r, {ok, RObj}, Idx, ReqId},
                  StateData=#state{req_id=ReqId,allowmult=AllowMult,replied_r=Replied0}) ->
    Replied = [{RObj,Idx}|Replied0],
    Final = merge(Replied,AllowMult),
    finalize(StateData#state{replied_r=Replied,final_obj=Final});
waiting_read_repair({r, {error, notfound}, Idx, ReqId},
                  StateData=#state{req_id=ReqId,replied_notfound=Replied0}) ->
    finalize(StateData#state{replied_notfound=[Idx|Replied0]});
waiting_read_repair({r, {error, Err}, Idx, ReqId},
                  StateData=#state{req_id=ReqId,replied_fail=Replied0}) ->
    finalize(StateData#state{replied_fail=[{Err,Idx}|Replied0]});
waiting_read_repair(timeout, StateData) ->
    really_finalize(StateData).

has_all_replies(#state{replied_r=R,replied_fail=F,replied_notfound=NF, n=N}) ->
    length(R) + length(F) + length(NF) >= N.

finalize(StateData=#state{replied_r=[]}) ->
    case has_all_replies(StateData) of
        true -> {stop,normal,StateData};
        false -> {next_state,waiting_read_repair,StateData}
    end;    
finalize(StateData) ->
    case has_all_replies(StateData) of
        true -> really_finalize(StateData);
        false -> {next_state,waiting_read_repair,StateData}
    end.

really_finalize(StateData=#state{final_obj=Final,
                          waiting_for=Sent,
                          replied_r=RepliedR,
                          bkey=BKey,
                          req_id=ReqId,
                          replied_notfound=NotFound,
                          starttime=StartTime}) ->
    case Final of
        tombstone ->
            maybe_finalize_delete(StateData);
        {ok,_} ->
            maybe_do_read_repair(Sent,Final,RepliedR,NotFound,BKey,
                                 ReqId,StartTime);
        _ -> nop
    end,
    {stop,normal,StateData}.

maybe_finalize_delete(_StateData=#state{replied_notfound=NotFound,n=N,
                                        replied_r=RepliedR,
                                        waiting_for=Sent,req_id=ReqId,
                                        bkey=BKey}) ->
    spawn(fun() ->
    IdealNodes = [{I,Node} || {I,Node,Node} <- Sent],
    case length(IdealNodes) of
        N -> % this means we sent to a perfect preflist
            case (length(RepliedR) + length(NotFound)) of
                N -> % and we heard back from all nodes with non-failure
                    case lists:all(fun(X) -> riak_kv_util:is_x_deleted(X) end,
                                   [O || {O,_I} <- RepliedR]) of
                        true -> % and every response was X-Deleted, go!
                            [gen_server2:call({riak_kv_vnode_master, Node},
                                             {vnode_del, {Idx,Node},
                                              {BKey,ReqId}}) ||
                                {Idx,Node} <- IdealNodes];
                        _ -> nop
                    end;
                _ -> nop
            end;
        _ -> nop
    end
    end).

maybe_do_read_repair(Sent,Final,RepliedR,NotFound,BKey,ReqId,StartTime) ->
    Targets = ancestor_indices(Final, RepliedR) ++ NotFound,
    {ok, FinalRObj} = Final,
    Msg = {self(), BKey, FinalRObj, ReqId, StartTime, [{returnbody, false}]},
    case Targets of
        [] -> nop;
        _ ->
            [begin 
                {Idx,Node,Fallback} = lists:keyfind(Target, 1, Sent),
                gen_server:cast({riak_kv_vnode_master, Fallback},
                                {vnode_put, {Idx,Node}, Msg})
             end || Target <- Targets]
    end.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(timeout, StateName, StateData) ->
    ?MODULE:StateName(timeout, StateData);
%% @private
handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
terminate(Reason, _StateName, _State) ->
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

merge(VResponses, AllowMult) ->
   merge_robjs([R || {R,_I} <- VResponses],AllowMult).

respond(Client,VResponses,AllowMult,ReqId) ->
    Merged = merge(VResponses, AllowMult),
    case Merged of
        tombstone ->
            Reply = {error,notfound};
        {ok, Obj} ->
            case riak_kv_util:is_x_deleted(Obj) of
                true ->
                    Reply = {error, notfound};
                false ->
                    Reply = {ok, Obj}
            end;
        X ->
            Reply = X
    end,
    Client ! {ReqId, Reply},
    Merged.

merge_robjs([], _) ->
    {error, notfound};
merge_robjs(RObjs0,AllowMult) ->
    RObjs1 = [X || X <- [riak_kv_util:obj_not_deleted(O) ||
                            O <- RObjs0], X /= undefined],
    case RObjs1 of
        [] -> tombstone;
        _ ->
            RObj = riak_object:reconcile(RObjs0,AllowMult),
            {ok, RObj}
    end.

strict_descendant(O1, O2) ->
    vclock:descends(riak_object:vclock(O1),riak_object:vclock(O2)) andalso
    not vclock:descends(riak_object:vclock(O2),riak_object:vclock(O1)).

ancestor_indices({ok, Final},AnnoObjects) ->
    [Idx || {O,Idx} <- AnnoObjects, strict_descendant(Final, O)].


update_stats(#state{startnow=StartNow}) ->
    EndNow = now(),
    riak_kv_stat:update({get_fsm_time, timer:now_diff(EndNow, StartNow)}).
