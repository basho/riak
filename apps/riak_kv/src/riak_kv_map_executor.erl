%% -------------------------------------------------------------------
%%
%% riak_map_executor: perform work for map-phase jobs
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

%% @doc perform work for map-phase jobs

-module(riak_kv_map_executor).
-behaviour(gen_fsm).

-export([start_link/5]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait/2]).

-record(state, {bkey,qterm,phase_pid,vnodes,keydata,ring,timeout,vnode_timer,vnode_timeout}).

% {link, Bucket, Tag, Acc}
% {map, FunTerm, Arg, Acc}

% where FunTerm is one of:
% {modfun, Mod, Fun} : Mod and Fun are both atoms -> Mod:Fun(Obj,Keydata,Arg)
% {qfun, Fun} : Fun is an actual fun -> Fun(Obj,Keydata,Arg)

% all map funs (and link funs) must return a list of values,
% but that is not enforced at this layer

start_link(Ring, {{_, _}, _}=Input, QTerm, Timeout, PhasePid) ->
    gen_fsm:start_link(?MODULE, [Ring,Input,QTerm,Timeout,PhasePid], []);
start_link(_Ring, _BadInput, _QTerm, _Timeout, _PhasePid) ->
    {error, bad_input}.
%% @private
init([Ring,{{Bucket,Key},KeyData},QTerm0,Timeout,PhasePid]) ->
    VNodeTimeout = app_helper:get_env(riak_kv, vnode_mr_timeout, 1000),
    DocIdx = riak_core_util:chash_key({Bucket,Key}),
    BucketProps = riak_core_bucket:get_bucket(Bucket, Ring),
    LinkFun = case QTerm0 of
                  {erlang, {link,_,_,_}} -> proplists:get_value(linkfun, BucketProps);
                  _ -> nop
    end,
    case LinkFun of
        linkfun_unset ->
            riak_kv_phase_proto:mapexec_error(PhasePid,
                                            io_lib:format("linkfun unset for ~s",[Bucket])),
            {stop,no_linkfun};
        _ ->
            QTerm = case QTerm0 of
                        {_, {map, _, _, _}} -> QTerm0;
                        {Lang, {link, LB, LT, LAcc}} -> {Lang, {map, LinkFun, {LB, LT}, LAcc}}
                    end,
            N = proplists:get_value(n_val,BucketProps),
            Preflist = riak_core_ring:preflist(DocIdx, Ring),
            {Targets, _} = lists:split(N, Preflist),
            State = #state{bkey={Bucket,Key},qterm=QTerm,phase_pid=PhasePid,
                           vnodes=Targets,keydata=KeyData,ring=Ring,timeout=VNodeTimeout,
                           vnode_timeout=VNodeTimeout},
            case try_vnode(State) of
                {error, no_vnodes} ->
                    {stop, no_vnodes};
                NewState ->
                    {ok, wait, NewState, Timeout}
            end
    end.

try_vnode(#state{vnodes=[], keydata=KD, bkey=BKey, phase_pid=PhasePid}) ->
    riak_kv_phase_proto:mapexec_result(PhasePid, [{not_found, BKey, KD}]),
    {error, no_vnodes};
try_vnode(#state{qterm=QTerm, bkey=BKey, keydata=KeyData, vnodes=[{P, VN}|VNs],
                 vnode_timeout=VNodeTimeout}=StateData) ->
    UpNodes = riak_core_node_watcher:nodes(riak_kv),
    case lists:member(VN, UpNodes) of
        false ->
            try_vnode(StateData#state{vnodes=VNs});
        true ->
            riak_kv_vnode:map({P,VN},self(),QTerm,BKey,KeyData),
            {ok, TRef} = timer:send_after(VNodeTimeout, self(), timeout),
            StateData#state{vnodes=VNs, vnode_timer=TRef}
    end.

wait(timeout, StateData=#state{bkey=BKey, keydata=KD, phase_pid=PhasePid,vnodes=[]}) ->
    riak_kv_phase_proto:mapexec_result(PhasePid, [{not_found, BKey, KD}]),
    {stop,normal,StateData};
wait(timeout, #state{timeout=Timeout}=StateData) ->
    case try_vnode(StateData) of
        {error, no_vnodes} ->
           {stop, normal, StateData};
        NewState ->
            {next_state, wait, NewState, Timeout}
    end;
wait({mapexec_error, _VN, _ErrMsg},
     StateData=#state{bkey=BKey, keydata=KD, phase_pid=PhasePid,vnodes=[], vnode_timer=TRef}) ->
    timer:cancel(TRef),
    riak_kv_phase_proto:mapexec_result(PhasePid, [{not_found, BKey, KD}]),
    {stop,normal,StateData};
wait({mapexec_error_noretry, _VN, ErrMsg}, #state{phase_pid=PhasePid, vnode_timer=TRef}=StateData) ->
    timer:cancel(TRef),
    riak_kv_phase_proto:mapexec_error(PhasePid, ErrMsg),
    {stop, normal, StateData};
wait({mapexec_error, _VN, _ErrMsg}, #state{timeout=Timeout, vnode_timer=TRef}=StateData) ->
    timer:cancel(TRef),
    case try_vnode(StateData) of
        {error, no_vnodes} ->
            {stop, normal, StateData};
        NewState ->
            {next_state, wait, NewState, Timeout}
    end;
wait({mapexec_reply, executing, _}, #state{timeout=Timeout}=StateData) ->
    {next_state, wait, StateData, Timeout};
wait({mapexec_reply, RetVal, _VN}, StateData=#state{phase_pid=PhasePid, vnode_timer=TRef}) ->
    timer:cancel(TRef),
    riak_kv_phase_proto:mapexec_result(PhasePid, RetVal),
    {stop,normal,StateData}.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(timeout, StateName, StateData) ->
    gen_fsm:send_event(self(), timeout),
    {next_state, StateName, StateData};

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
terminate(Reason, _StateName, _State) ->
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
