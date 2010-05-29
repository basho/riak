%% -------------------------------------------------------------------
%%
%% riak_put_fsm: coordination of Riak PUT requests
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

%% @doc coordination of Riak PUT requests

-module(riak_kv_put_fsm).
-include_lib("eunit/include/eunit.hrl").
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
                returnbody :: boolean(),
                resobjs :: list(),
                allowmult :: boolean(),
                reply_arity :: 1 | 2
               }).

start(ReqId,RObj,W,DW,Timeout,From) ->
    start(ReqId,RObj,W,DW,Timeout,From,?DEFAULT_OPTS).

start(ReqId,RObj,W,DW,Timeout,From,Options) ->
    gen_fsm:start(?MODULE, [ReqId,RObj,W,DW,Timeout,From,Options], []).

%% @private
init([ReqId,RObj0,W,DW,Timeout,Client,Options0]) ->
    Options = case Options0 of [] -> ?DEFAULT_OPTS; _ -> Options0 end,
    {ok,Ring} = riak_core_ring_manager:get_my_ring(),
    BucketProps = riak_core_bucket:get_bucket(riak_object:bucket(RObj0), Ring),
    AllowMult = proplists:get_value(allow_mult,BucketProps),
    {ok, RClient} = riak:local_client(),
    Bucket = riak_object:bucket(RObj0),
    Key = riak_object:key(RObj0),
    StateData0 = #state{robj=RObj0, client=Client, w=W, dw=DW, bkey={Bucket, Key},
                        req_id=ReqId, timeout=Timeout, ring=Ring,
                        rclient=RClient, options=proplists:unfold(Options),
                        resobjs=[], allowmult=AllowMult, reply_arity=1},
    StateData = handle_options(StateData0),
    {ok,initialize,StateData,0}.

%% @private
handle_options(State=#state{options=Options}) ->
    handle_options(Options, State).
%% @private
handle_options([], State) ->
    State;
handle_options([{returnbody, true}|T], State=#state{w=W}) ->
    handle_options(T, State#state{returnbody=true,dw=W, reply_arity=2});
handle_options([{returnbody, false}|T], State=#state{w=W}) ->
    case has_postcommit_hooks(element(1,State#state.bkey)) of
        true ->
            Options = [{returnbody, true}],
            handle_options(T, State#state{options=Options,
                                          returnbody=true,
                                          dw=W,
                                          reply_arity=1});
        false ->
            handle_options(T, State#state{returnbody=false})
    end;
handle_options([{_,_}|T], State) -> handle_options(T, State).

%% @private
initialize(timeout, StateData0=#state{robj=RObj0, req_id=ReqId, client=Client,
                                      timeout=Timeout, ring=Ring, bkey={Bucket,Key},
                                      rclient=RClient, options=Options}) ->
    case invoke_hook(precommit, RClient, update_metadata(RObj0)) of
        fail ->
            Client ! {ReqId, {error, precommit_fail}},
            {stop, normal, StateData0};
        {fail, Reason} ->
            Client ! {ReqId, {error, {precommit_fail, Reason}}},
            {stop, normal, StateData0};
        RObj1 ->
            StartNow = now(),
            TRef = erlang:send_after(Timeout, self(), timeout),
            RealStartTime = riak_core_util:moment(),
            BucketProps = riak_core_bucket:get_bucket(Bucket, Ring),
            DocIdx = riak_core_util:chash_key({Bucket, Key}),
            Msg = {self(), {Bucket,Key}, RObj1, ReqId, RealStartTime, Options},
            N = proplists:get_value(n_val,BucketProps),
            Preflist = riak_core_ring:preflist(DocIdx, Ring),
            {Targets, Fallbacks} = lists:split(N, Preflist),
            {Sent1, Pangs1} = riak_kv_util:try_cast(vnode_put, Msg, nodes(), Targets),
            Sent = case length(Sent1) =:= N of   % Sent is [{Index,TargetNode,SentNode}]
                       true -> Sent1;
                       false -> Sent1 ++ riak_kv_util:fallback(vnode_put,Msg,Pangs1,Fallbacks)
                   end,
            StateData = StateData0#state{
                          robj=RObj1, n=N, preflist=Preflist,
                          waiting_for=Sent, starttime=riak_core_util:moment(),
                          replied_w=[], replied_dw=[], replied_fail=[],
                          tref=TRef,startnow=StartNow},
            {next_state,waiting_vnode_w,StateData}
    end.

waiting_vnode_w({w, Idx, ReqId},
                StateData=#state{w=W,dw=DW,req_id=ReqId,client=Client,replied_w=Replied0}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= W of
        true ->
            case DW of
                0 ->
                    Client ! {ReqId, ok},
                    update_stats(StateData),
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
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_dw=Replied},
    {next_state,waiting_vnode_w,NewStateData};
waiting_vnode_w({dw, Idx, ResObj, _ReqId},
                  StateData=#state{replied_dw=Replied0, resobjs=ResObjs0}) ->
    Replied = [Idx|Replied0],
    ResObjs = [ResObj|ResObjs0],
    NewStateData = StateData#state{replied_dw=Replied, resobjs=ResObjs},
    {next_state,waiting_vnode_w,NewStateData};
waiting_vnode_w({fail, Idx, ReqId},
                  StateData=#state{n=N,w=W,client=Client,
                                   replied_fail=Replied0}) ->
    Replied = [Idx|Replied0],
    NewStateData = StateData#state{replied_fail=Replied},
    case (N - length(Replied)) >= W of
        true ->
            {next_state,waiting_vnode_w,NewStateData};
        false ->
            update_stats(StateData),
            Client ! {ReqId, {error,too_many_fails}},
            {stop,normal,NewStateData}
    end;
waiting_vnode_w(timeout, StateData=#state{client=Client,req_id=ReqId}) ->
    update_stats(StateData),
    Client ! {ReqId, {error,timeout}},
    {stop,normal,StateData}.

waiting_vnode_dw({w, _Idx, ReqId},
          StateData=#state{req_id=ReqId}) ->
    {next_state,waiting_vnode_dw,StateData};
waiting_vnode_dw({dw, Idx, ReqId},
                 StateData=#state{dw=DW, client=Client, replied_dw=Replied0}) ->
    Replied = [Idx|Replied0],
    case length(Replied) >= DW of
        true ->
            Client ! {ReqId, ok},
            update_stats(StateData),
            {stop,normal,StateData};
        false ->
            NewStateData = StateData#state{replied_dw=Replied},
            {next_state,waiting_vnode_dw,NewStateData}
    end;
waiting_vnode_dw({dw, Idx, ResObj, ReqId},
                 StateData=#state{dw=DW, client=Client, replied_dw=Replied0,
                                  allowmult=AllowMult, reply_arity=ReplyArity,
                                  rclient=RClient, resobjs=ResObjs0}) ->
    Replied = [Idx|Replied0],
    ResObjs = [ResObj|ResObjs0],
    case length(Replied) >= DW of
        true ->
            ReplyObj = merge_robjs(ResObjs, AllowMult),
            Reply = case ReplyArity of 1 -> ok; 2 -> {ok, ReplyObj} end,
            Client ! {ReqId, Reply},
            invoke_hook(postcommit, RClient, ReplyObj),
            update_stats(StateData),
            {stop,normal,StateData};
        false ->
            NewStateData = StateData#state{replied_dw=Replied,resobjs=ResObjs},
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
    update_stats(StateData),
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
    riak_core_util:integer_to_list(HashAsNum,62).

make_vtag_test() ->
    Obj = riak_object:new(<<"b">>,<<"k">>,<<"v1">>),
    ?assertNot(make_vtag(Obj) =:=
               make_vtag(riak_object:increment_vclock(Obj,<<"client_id">>))).

update_stats(#state{startnow=StartNow}) ->
    EndNow = now(),
    riak_kv_stat:update({put_fsm_time, timer:now_diff(EndNow, StartNow)}).

%% Internal functions
invoke_hook(HookType, RClient, RObj) ->
    Bucket = riak_object:bucket(RObj),
    BucketProps = RClient:get_bucket(Bucket),
    R = proplists:get_value(HookType, BucketProps, []),
    case R of
        <<"none">> ->
            RObj;
        [] ->
            RObj;
        Hooks when is_list(Hooks) ->
            run_hooks(HookType, RObj, Hooks)
    end.

run_hooks(_HookType, RObj, []) ->
    RObj;
run_hooks(HookType, RObj, [{struct, Hook}|T]) ->
    Mod = proplists:get_value(<<"mod">>, Hook),
    Fun = proplists:get_value(<<"fun">>, Hook),
    JSName = proplists:get_value(<<"name">>, Hook),
    Result = invoke_hook(HookType, Mod, Fun, JSName, RObj),
    case HookType of
        precommit ->
            case Result of
                fail ->
                    Result;
                _ ->
                    run_hooks(HookType, Result, T)
            end;
        postcommit ->
            run_hooks(HookType, RObj, T)
    end.


invoke_hook(precommit, Mod0, Fun0, undefined, RObj) ->
    Mod = binary_to_atom(Mod0, utf8),
    Fun = binary_to_atom(Fun0, utf8),
    Mod:Fun(RObj);
invoke_hook(precommit, undefined, undefined, JSName, RObj) ->
    case riak_kv_js_manager:blocking_dispatch({{jsfun, JSName}, RObj}) of
        {ok, <<"fail">>} ->
            fail;
        {ok, [{<<"fail">>, Message}]} ->
            {fail, Message};
        {ok, NewObj} ->
            riak_object:from_json(NewObj);
        {error, Error} ->
            error_logger:error_msg("Error executing pre-commit hook: ~s",
                                   [Error]),
            fail
    end;
invoke_hook(postcommit, Mod0, Fun0, undefined, Obj) ->
    Mod = binary_to_atom(Mod0, utf8),
    Fun = binary_to_atom(Fun0, utf8),
    proc_lib:spawn(fun() -> Mod:Fun(Obj) end);

invoke_hook(postcommit, undefined, undefined, _JSName, _Obj) ->
    error_logger:warning_msg("Javascript post-commit hooks aren't implemented");
%% NOP to handle all other cases
invoke_hook(_, _, _, _, RObj) ->
    RObj.

merge_robjs(RObjs0,AllowMult) ->
    RObjs1 = [X || X <- RObjs0,
                   X /= undefined],
    case RObjs1 of
        [] -> {error, notfound};
        _ -> riak_object:reconcile(RObjs1,AllowMult)
    end.

has_postcommit_hooks(Bucket) ->
    lists:flatten(proplists:get_all_values(postcommit, riak_core_bucket:get_bucket(Bucket))) /= [].
