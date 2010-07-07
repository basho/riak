%% Riak EnterpriseDS
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
-module(riak_repl_put_fsm).
-include_lib("eunit/include/eunit.hrl").
-include_lib("riak_kv/include/riak_kv_vnode.hrl").
-behaviour(gen_fsm).
-define(DEFAULT_OPTS, [{returnbody, false}]).
-export([start/6,start/7]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([initialize/2,waiting_vnode_w/2,waiting_vnode_dw/2]).

-record(state, {robj :: riak_object:riak_object(),
                client :: {pid(), reference()},
                rclient :: riak_client:riak_client(),
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
                tref    :: reference(),
                ring :: riak_core_ring:riak_core_ring(),
                startnow :: {pos_integer(), pos_integer(), pos_integer()},
                options :: list(),
                is_delete = false :: boolean()
               }).

start(ReqId,RObj,W,DW,Timeout,From) ->
    start(ReqId,RObj,W,DW,Timeout,From,?DEFAULT_OPTS).

start(ReqId,RObj,W,DW,Timeout,From,Options) ->
    gen_fsm:start(?MODULE, [ReqId,RObj,W,DW,Timeout,From,Options], []).

%% @private
init([ReqId,RObj0,W,DW,Timeout,Client,Options0]) ->
    Options = case Options0 of [] -> ?DEFAULT_OPTS; _ -> Options0 end,
    {ok,Ring} = riak_core_ring_manager:get_my_ring(),
    Bucket = riak_object:bucket(RObj0),
    Key = riak_object:key(RObj0),
    StateData = #state{robj=RObj0, client=Client, w=W, dw=DW, 
                       bkey={Bucket, Key}, 
                       is_delete=riak_kv_util:is_x_deleted(RObj0),
                       req_id=ReqId, timeout=Timeout, ring=Ring,
                       options=proplists:unfold(Options)},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{robj=RObj0, req_id=ReqId,
                                      timeout=Timeout, ring=Ring, 
                                      bkey={Bucket,Key}=BKey,
                                      options=Options}) ->
    StartNow = now(),
    TRef = erlang:send_after(Timeout, self(), timeout),
    RealStartTime = riak_core_util:moment(),
    BucketProps = riak_core_bucket:get_bucket(Bucket, Ring),
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    Req = ?KV_PUT_REQ{
             bkey = BKey,
             object = RObj0,
             req_id = ReqId,
             start_time = RealStartTime,
             options = Options},
    N = proplists:get_value(n_val,BucketProps),
    Preflist = riak_core_ring:preflist(DocIdx, Ring),
    {Targets, Fallbacks} = lists:split(N, Preflist),
    UpNodes = riak_core_node_watcher:nodes(riak_kv),
    {Sent1, Pangs1} = riak_kv_util:try_cast(Req,UpNodes,Targets),
    Sent = case length(Sent1) =:= N of   % Sent is [{Index,TargetNode,SentNode}]
               true -> Sent1;
               false -> Sent1 ++ riak_kv_util:fallback(Req,UpNodes,Pangs1,Fallbacks)
           end,
    StateData = StateData0#state{
                  robj=RObj0, n=N, preflist=Preflist,
                  waiting_for=Sent, starttime=riak_core_util:moment(),
                  replied_w=[], replied_dw=[], replied_fail=[],
                  tref=TRef,startnow=StartNow},
    {next_state,waiting_vnode_w,StateData}.

waiting_vnode_w({w, Idx, ReqId},
                StateData=#state{w=W,dw=DW,req_id=ReqId,client=Client,replied_w=Replied0, is_delete=IsDelete}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= W of
        true ->
            case DW of
                0 ->
                    Client ! {ReqId, ok},
                    case IsDelete of 
                        true -> try_reap(StateData);
                        false -> ignore
                    end,
                    {stop,normal,StateData};
                _ ->
                    NewStateData = StateData#state{replied_w=Replied},
                    {next_state,waiting_vnode_dw,NewStateData}
            end;
        false ->
            NewStateData = StateData#state{replied_w=Replied},
            {next_state,waiting_vnode_w,NewStateData}
    end;
waiting_vnode_w({dw, Idx, _ReqId},
                  StateData=#state{replied_dw=Replied0}) ->
    {next_state,waiting_vnode_w, StateData#state{replied_dw=[Idx|Replied0]}};
waiting_vnode_w({fail, Idx, ReqId},
                  StateData=#state{n=N,w=W,client=Client,
                                   replied_fail=Replied0}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= W of
        true ->
            {next_state,waiting_vnode_w,NewStateData};
        false ->
            Client ! {ReqId, {error,too_many_fails}},
            {stop,normal,NewStateData}
    end;
waiting_vnode_w(timeout, StateData=#state{client=Client,req_id=ReqId}) ->
    Client ! {ReqId, {error,timeout}},
    {stop,normal,StateData}.

waiting_vnode_dw({w, _Idx, ReqId},
          StateData=#state{req_id=ReqId}) ->
    {next_state,waiting_vnode_dw,StateData};
waiting_vnode_dw({dw, Idx, ReqId},
                 StateData=#state{dw=DW, 
                                  client=Client, 
                                  replied_dw=Replied0,
                                  is_delete=IsDelete}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= DW of
        true ->
            Client ! {ReqId, ok},
            case IsDelete of 
                true -> try_reap(StateData);
                false -> ignore
            end,
            {stop,normal,StateData};
        false ->
            NewStateData = StateData#state{replied_dw=Replied},
            {next_state,waiting_vnode_dw,NewStateData}
    end;
waiting_vnode_dw({fail, Idx, ReqId},
                  StateData=#state{n=N,dw=DW,client=Client,
                                   replied_fail=Replied0}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= DW of
        true ->
            {next_state,waiting_vnode_dw,NewStateData};
        false ->
            Client ! {ReqId, {error,too_many_fails}},
            {stop,normal,NewStateData}
    end;
waiting_vnode_dw(timeout, StateData=#state{client=Client,req_id=ReqId}) ->
    Client ! {ReqId, {error,timeout}},
    {stop,normal,StateData}.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private

handle_info(timeout, StateName, StateData) ->
    ?MODULE:StateName(timeout, StateData);
handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
terminate(Reason, _StateName, _State) ->
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

try_reap(#state{bkey={Bucket, Key}, timeout=Timeout, w=W}) ->
    ReqId = erlang:phash2(erlang:now()),
    riak_kv_get_fsm:start(ReqId,Bucket,Key,W,Timeout,self()).
