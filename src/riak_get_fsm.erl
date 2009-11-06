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

-module(riak_get_fsm).
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
                endtime :: pos_integer(),
                bkey :: {riak_object:bucket(), riak_object:key()},
                ring :: riak_ring:riak_ring()
               }).

start(ReqId,Bucket,Key,R,Timeout,From) ->
    gen_fsm:start(?MODULE, [ReqId,Bucket,Key,R,Timeout,From], []).

%% @private
init([ReqId,Bucket,Key,R,Timeout,Client]) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    StateData = #state{client=Client,r=R, timeout=Timeout,
                req_id=ReqId, bkey={Bucket,Key}, ring=Ring},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{timeout=Timeout, req_id=ReqId,
                                      bkey={Bucket,Key}, ring=Ring}) ->
    RealStartTime = riak_util:moment(),
    DocIdx = riak_util:chash_key({Bucket, Key}),
    riak_eventer:notify(riak_get_fsm, get_fsm_start,
                        {ReqId, RealStartTime, Bucket, Key}),
    Msg = {self(), {Bucket,Key}, ReqId},
    BucketProps = riak_bucket:get_bucket(Bucket, Ring),
    N = proplists:get_value(n_val,BucketProps),
    AllowMult = proplists:get_value(allow_mult,BucketProps),
    Preflist = riak_ring:filtered_preflist(DocIdx, Ring, N),
    {Targets, Fallbacks} = lists:split(N, Preflist),
    {Sent1, Pangs1} = riak_util:try_cast(vnode_get, Msg, Targets),
    Sent = case length(Sent1) =:= N of   % Sent is [{Index,TargetNode,SentNode}]
        true -> Sent1;
        false -> Sent1 ++ riak_util:fallback(vnode_get,Msg,Pangs1,Fallbacks)
    end,
    riak_eventer:notify(riak_get_fsm, get_fsm_sent,
                                {ReqId, [{T,S} || {_I,T,S} <- Sent]}),
    StateData = StateData0#state{n=N,allowmult=AllowMult,repair_sent=[],
                       preflist=Preflist,final_obj=undefined,
                       replied_r=[],replied_fail=[],
                       replied_notfound=[],starttime=riak_util:moment(),
                       waiting_for=Sent,endtime=Timeout+riak_util:moment()},
    {next_state,waiting_vnode_r,StateData,Timeout}.

waiting_vnode_r({r, {ok, RObj}, Idx, ReqId},
                  StateData=#state{r=R,allowmult=AllowMult,
                                   req_id=ReqId,client=Client,
                                   replied_r=Replied0, endtime=End}) ->
    Replied = [{RObj,Idx}|Replied0],
    case length(Replied) >= R of
        true ->
            Final = respond(Client,Replied,AllowMult,ReqId),
            case Final of
                {error, notfound} ->
                    riak_eventer:notify(riak_get_fsm, get_fsm_reply,
                                        {ReqId, notfound});
                {ok, _} ->
                    riak_eventer:notify(riak_get_fsm, get_fsm_reply,
                                        {ReqId, ok})
            end,
            NewStateData = StateData#state{replied_r=Replied,final_obj=Final},
            finalize(NewStateData, End);
        false ->
            NewStateData = StateData#state{replied_r=Replied},
            {next_state,waiting_vnode_r,NewStateData,End-riak_util:moment()}
    end;
waiting_vnode_r({r, {error, notfound}, Idx, ReqId},
                  StateData=#state{r=R,replied_fail=Fails,
                                   req_id=ReqId,client=Client,n=N,
                                   replied_notfound=Replied0,endtime=End}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_notfound=Replied},
    case (N - length(Replied) - length(Fails)) >= R of
        true ->
            {next_state,waiting_vnode_r,NewStateData,End-riak_util:moment()};
        false ->
            riak_eventer:notify(riak_get_fsm, get_fsm_reply,
                                {ReqId, notfound}),
            Client ! {ReqId, {error,notfound}},
            {stop,normal,NewStateData}
    end;
waiting_vnode_r({r, {error, Err}, Idx, ReqId},
                  StateData=#state{r=R,client=Client,n=N,
                                   replied_fail=Replied0,req_id=ReqId,
                                   replied_notfound=NotFound,endtime=End}) ->
    Replied = [{Err,Idx}|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied) - length(NotFound)) >= R of
        true ->
            {next_state,waiting_vnode_r,NewStateData,End-riak_util:moment()};
        false ->
            case length(NotFound) of
                0 ->
                    FullErr = [E || {E,_I} <- Replied],
                    riak_eventer:notify(riak_get_fsm, get_fsm_reply,
                                        {ReqId, {error,FullErr}}),
                    Client ! {ReqId, {error,FullErr}},
                    {stop,normal,NewStateData};
                _ ->
                    riak_eventer:notify(riak_get_fsm, get_fsm_reply,
                                        {ReqId, notfound}),
                    Client ! {ReqId, {error,notfound}},
                    {stop,normal,NewStateData}
            end
    end;
waiting_vnode_r(timeout, StateData=#state{client=Client,req_id=ReqId}) ->
    riak_eventer:notify(riak_get_fsm, get_fsm_reply,
                        {ReqId, timeout}),
    Client ! {ReqId, {error,timeout}},
    {stop,normal,StateData}.

waiting_read_repair({r, {ok, RObj}, Idx, ReqId},
                  StateData=#state{req_id=ReqId,replied_r=Replied0,
                                   endtime=End}) ->
    finalize(StateData#state{replied_r=[{RObj,Idx}|Replied0]}, End);
waiting_read_repair({r, {error, notfound}, Idx, ReqId},
                  StateData=#state{req_id=ReqId,replied_notfound=Replied0,
                                   endtime=End}) ->
    finalize(StateData#state{replied_notfound=[Idx|Replied0]}, End);
waiting_read_repair({r, {error, Err}, Idx, ReqId},
                  StateData=#state{req_id=ReqId,replied_fail=Replied0,
                                   endtime=End}) ->
    finalize(StateData#state{replied_fail=[{Err,Idx}|Replied0]}, End);
waiting_read_repair(timeout, StateData) ->
    finalize(StateData).

finalize(StateData=#state{replied_r=R,replied_fail=F,replied_notfound=NF, n=N},
         End) ->
    case (length(R) + length(F) + length(NF)) >= N of
        true -> finalize(StateData);
        false -> {next_state,waiting_read_repair,
                  StateData,End-riak_util:moment()}
    end.
finalize(StateData=#state{final_obj=Final,
                          replied_r=RepliedR,
                          bkey=BKey,
                          req_id=ReqId,
                          replied_notfound=NotFound,
                          ring=Ring,
                          starttime=StartTime}) ->
    case Final of
        {error, notfound} ->
            maybe_finalize_delete(StateData);
        {ok,_} ->
            maybe_do_read_repair(Ring,Final,RepliedR,NotFound,BKey,
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
                    case lists:all(fun(X) -> riak_util:is_x_deleted(X) end,
                                   [O || {O,_I} <- RepliedR]) of
                        true -> % and every response was X-Deleted, go!
                            riak_eventer:notify(riak_get_fsm,
                                                delete_finalize_start,
                                                {ReqId, BKey}),
                            [gen_server2:call({riak_vnode_master, Node},
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

maybe_do_read_repair(Ring,Final,RepliedR,NotFound,BKey,ReqId,StartTime) ->
    Targets0 = ancestor_indices(Final, RepliedR) ++ NotFound,
    Targets = [{Idx,riak_ring:index_owner(Ring,Idx)} || Idx <- Targets0],
    {ok, FinalRObj} = Final,
    Msg = {self(), BKey, FinalRObj, ReqId, StartTime},
    case Targets of
        [] -> nop;
        _ ->
            riak_eventer:notify(riak_get_fsm, read_repair,
                                {ReqId, Targets}),
            [gen_server:cast({riak_vnode_master, Node},
                             {vnode_put, {Idx,Node}, Msg}) ||
                {Idx,Node} <- Targets]
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
terminate(Reason, _StateName, _State=#state{req_id=ReqId}) ->
    riak_eventer:notify(riak_get_fsm, get_fsm_end,
                        {ReqId, Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

respond(Client,VResponses,AllowMult,ReqId) ->
    Reply = merge_robjs([R || {R,_I} <- VResponses],AllowMult),
    Client ! {ReqId, Reply},
    Reply.

merge_robjs(RObjs0,AllowMult) ->
    RObjs1 = [X || X <- [riak_util:obj_not_deleted(O) ||
                            O <- RObjs0], X /= undefined],
    case RObjs1 of
        [] -> {error, notfound};
        _ ->
            RObj = riak_object:reconcile(RObjs1,AllowMult),
            {ok, RObj}
    end.

ancestor_indices(_,AnnoObjects) ->
    ToRemove = [[Idx || {O2,Idx} <- AnnoObjects,
     vclock:descends(riak_object:vclock(O1),riak_object:vclock(O2)),
     (vclock:descends(riak_object:vclock(O2),riak_object:vclock(O1)) == false)]
		|| {O1,_} <- AnnoObjects],
    lists:flatten(ToRemove).
