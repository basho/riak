%% -------------------------------------------------------------------
%%
%% riak_vnode: a single node's management of a single partition of data
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

%% @doc a single node's management of a single partition of data

-module(riak_kv_vnode).
-behaviour(gen_fsm).

-export([start_link/1,
         get_vnode_index/1]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([active/2,active/3]).

-define(TIMEOUT, 60000).
-define(LOCK_RETRY_TIMEOUT, 10000).

-record(state, {idx,mapcache,mod,modstate,handoff_q,handoff_token}).
-record(putargs, {returnbody,lww,bkey,robj,reqid,bprops,prunetime,fsm_pid}).

start_link(Idx) -> gen_fsm:start_link(?MODULE, [Idx], []).

get_vnode_index(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_vnode_index).

init([VNodeIndex]) ->
    Mod = app_helper:get_env(riak_kv, storage_backend),
    Configuration = app_helper:get_env(riak_kv),
    {ok, ModState} = Mod:start(VNodeIndex, Configuration),
    StateData0 = #state{idx=VNodeIndex,mod=Mod,modstate=ModState,
                        handoff_q=not_in_handoff},
    case hometest(StateData0) of
        {next_state, StateName, StateData, Timeout} ->
            {ok, StateName, StateData, Timeout};
        {stop,normal,StateData} ->
            {ok, ModState1} = Mod:start(VNodeIndex, Configuration),
            {ok, active, StateData#state{mod=Mod, modstate=ModState1}, ?TIMEOUT}
    end.

%% @private
hometest(StateData0=#state{idx=Idx,handoff_q=HQ}) ->
    StateData = StateData0#state{mapcache=orddict:new()},
    {ok, MyRing} = riak_core_ring_manager:get_my_ring(),
    Me = node(),
    case riak_core_ring:index_owner(MyRing, Idx) of
        Me ->
            {next_state,active,StateData#state{handoff_q=not_in_handoff},
             ?TIMEOUT};
        TargetNode ->
            case net_adm:ping(TargetNode) of
                pang ->
                    {next_state,active,StateData,?TIMEOUT};
                pong ->
                    case HQ of
                        not_in_handoff ->
                            do_handoff(TargetNode, StateData);
                        _ ->
                            do_list_handoff(TargetNode, HQ, StateData)
                    end
            end
    end.

%% @private
do_handoff(TargetNode, StateData=#state{idx=Idx, mod=Mod, modstate=ModState}) ->
    case Mod:is_empty(ModState) of
        true ->
            delete_and_exit(StateData);
        false ->
            {HQ,TO,HT} = case riak_kv_handoff_sender:start_link(TargetNode, Idx, all) of
                {ok, _Pid, HandoffToken} -> {[], ?TIMEOUT, HandoffToken};
                {error, locked} -> {not_in_handoff, ?LOCK_RETRY_TIMEOUT, undefined}
            end,
            {next_state,active,StateData#state{handoff_q=HQ,handoff_token=HT},TO}
    end.

%% @private
do_list_handoff(TargetNode, BKeyList, StateData=#state{idx=Idx}) ->
    case BKeyList of
        [] ->
            delete_and_exit(StateData);
        _ ->
            {HQ,TO,HT} = case riak_kv_handoff_sender:start_link(TargetNode, Idx, BKeyList) of
                {ok, _Pid, HandoffToken} -> {[], ?TIMEOUT, HandoffToken};
                {error, locked} -> {not_in_handoff, ?LOCK_RETRY_TIMEOUT, undefined}
            end,
            {next_state,active,StateData#state{handoff_q=HQ,handoff_token=HT},TO}
    end.

%% @private
delete_and_exit(StateData=#state{idx=Idx, mod=Mod, modstate=ModState}) ->
    ok = Mod:drop(ModState),
    gen_server:cast(riak_kv_vnode_master, {add_exclusion, Idx}),
    {stop, normal, StateData}.

%%%%%%%%%% in active state, we process normal client requests
active({get_binary,BKey}, _From, StateData=#state{mod=Mod,modstate=ModState}) ->
    {reply,Mod:get(ModState,BKey),active,StateData,?TIMEOUT};
active(list, _From, StateData=#state{mod=Mod,modstate=ModState}) ->
    {reply,{ok, Mod:list(ModState)},active,StateData,?TIMEOUT}.
active(timeout, StateData) ->
    hometest(StateData);
active({diffobj,{BKey,BinObj}}, StateData) ->
    case do_diffobj_put(BKey, binary_to_term(BinObj), StateData) of
        ok ->
            nop;
        {error, Err} ->
            error_logger:error_msg("Error storing handoff obj: ~p~n", [Err])
    end,
    {next_state,active,StateData,?TIMEOUT};
active({map, ClientPid, QTerm, BKey, KeyData}, StateData) ->
    NewState = do_map(ClientPid,QTerm,BKey,KeyData,StateData,self()),
    {next_state,active,NewState,?TIMEOUT};
active(handoff_complete, StateData=#state{idx=Idx,handoff_token=HT}) ->
    global:del_lock({HT, {node(), Idx}}, [node()]),
    hometest(StateData);
active({put, FSM_pid, BKey, RObj, ReqID, FSMTime, Options},
       StateData=#state{idx=Idx,mapcache=Cache,handoff_q=HQ0}) ->
    HQ = case HQ0 of
        not_in_handoff -> not_in_handoff;
        _ -> [BKey|HQ0]
    end,
    gen_fsm:send_event(FSM_pid, {w, Idx, ReqID}),
    do_put(FSM_pid, BKey, RObj, ReqID, FSMTime, Options, StateData),
    {next_state,
     active,StateData#state{mapcache=orddict:erase(BKey,Cache),
                            handoff_q=HQ},?TIMEOUT};
active({get, FSM_pid, BKey, ReqID}, StateData) ->
    do_get(FSM_pid, BKey, ReqID, StateData),
    {next_state,active,StateData,?TIMEOUT};
active({list_bucket, FSM_pid, Bucket, ReqID},
       StateData=#state{mod=Mod,modstate=ModState,idx=Idx}) ->
    do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx),
    {next_state,active,StateData,?TIMEOUT};
active({delete, From, BKey, ReqID}, StateData=#state{mapcache=Cache}) ->
    do_delete(From, BKey, ReqID, StateData),
    {next_state,
     active,StateData#state{mapcache=orddict:erase(BKey,Cache)},?TIMEOUT};
active({mapcache, BKey,{FunName,Arg,KeyData},MF_Res},
       StateData=#state{mapcache=Cache}) ->
    KeyCache0 = case orddict:find(BKey, Cache) of
        error -> orddict:new();
        {ok,CDict} -> CDict
    end,
    KeyCache = orddict:store({FunName,Arg,KeyData},MF_Res,KeyCache0),
    {next_state,active,
     StateData#state{mapcache=orddict:store(BKey,KeyCache,Cache)},?TIMEOUT};
active(purge_mapcache, StateData) ->
    {next_state, active, StateData#state{mapcache=orddict:new()}};
active({mapcache, BKey,{M,F,Arg,KeyData},MF_Res},
       StateData=#state{mapcache=Cache}) ->
    KeyCache0 = case orddict:find(BKey, Cache) of
        error -> orddict:new();
        {ok,CDict} -> CDict
    end,
    KeyCache = orddict:store({M,F,Arg,KeyData},MF_Res,KeyCache0),
    {next_state,active,
     StateData#state{mapcache=orddict:store(BKey,KeyCache,Cache)},?TIMEOUT}.

%% @private
do_get(FSM_pid, BKey, ReqID,
       _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    RetVal = case do_get_binary(BKey, Mod, ModState) of
        {ok, Binary} -> {ok, binary_to_term(Binary)};
        X -> X
    end,
    riak_kv_stat:update(vnode_get),
    gen_fsm:send_event(FSM_pid, {r, RetVal, Idx, ReqID}).

%% @private
do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx) ->
    RetVal = Mod:list_bucket(ModState,Bucket),
    gen_fsm:send_event(FSM_pid, {kl, RetVal,Idx,ReqID}).

%% @private
do_get_binary(BKey, Mod, ModState) ->
    Mod:get(ModState,BKey).

%% @private
do_delete(Client, BKey, ReqID,
          _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    case Mod:delete(ModState, BKey) of
        ok ->
            gen_server2:reply(Client, {del, Idx, ReqID});
        {error, _Reason} ->
            gen_server2:reply(Client, {fail, Idx, ReqID})
    end.

%% @private
% upon receipt of a handoff datum, there is no client FSM
do_diffobj_put(BKey={Bucket,_}, DiffObj,
       _StateData=#state{mod=Mod,modstate=ModState}) ->
    ReqID = erlang:phash2(erlang:now()),
    case syntactic_put_merge(Mod, ModState, BKey, DiffObj, ReqID) of
        {newobj, NewObj} ->
            AMObj = enforce_allow_mult(NewObj, riak_core_bucket:get_bucket(Bucket)),
            Val = term_to_binary(AMObj),
            Res = Mod:put(ModState, BKey, Val),
            case Res of
                ok -> riak_kv_stat:update(vnode_put);
                _ -> nop
            end,
            Res;
        _ -> ok
    end.

%% @private
% upon receipt of a client-initiated put
do_put(FSM_pid, {Bucket,_Key}=BKey, RObj, ReqID, PruneTime, Options, State) ->
    {ok,Ring} = riak_core_ring_manager:get_my_ring(),
    BProps = riak_core_bucket:get_bucket(Bucket, Ring),
    PutArgs = #putargs{returnbody=proplists:get_value(returnbody, Options, false),
                       lww=proplists:get_value(last_write_wins, BProps, false),
                       bkey=BKey,
                       robj=RObj,
                       reqid=ReqID,
                       bprops=BProps,
                       prunetime=PruneTime},
    gen_fsm:send_event(FSM_pid, perform_put(prepare_put(State, PutArgs), State, PutArgs)),
    riak_kv_stat:update(vnode_put).

prepare_put(#state{}, #putargs{lww=true, robj=RObj}) -> 
    {true, RObj};
prepare_put(#state{mod=Mod,modstate=ModState}, 
            #putargs{bkey=BKey,robj=RObj,reqid=ReqID,bprops=BProps,prunetime=PruneTime}) ->
    case syntactic_put_merge(Mod, ModState, BKey, RObj, ReqID) of
        {oldobj, OldObj} ->
            {false, OldObj};
        {newobj, NewObj} ->
            VC = riak_object:vclock(NewObj),
            AMObj = enforce_allow_mult(NewObj, BProps),
            ObjToStore = riak_object:set_vclock(AMObj,
                                                vclock:prune(VC,PruneTime,BProps)),
            {true, ObjToStore}
    end.

perform_put({false, Obj}, #state{idx=Idx}, #putargs{returnbody=true,reqid=ReqID}) ->
    {dw, Idx, Obj, ReqID};
perform_put({false, _Obj}, #state{idx=Idx}, #putargs{returnbody=false,reqid=ReqId}) ->
    {dw, Idx, ReqId};
perform_put({true, Obj}, #state{idx=Idx,mod=Mod,modstate=ModState},
            #putargs{returnbody=RB, bkey=BKey, reqid=ReqID}) ->
    Val = term_to_binary(Obj),
    case Mod:put(ModState, BKey, Val) of
        ok ->
            case RB of
                true -> {dw, Idx, Obj, ReqID};
                false -> {dw, Idx, ReqID}
            end;
        {error, _Reason} ->
            {fail, Idx, ReqID}
    end.

%% @private
%% enforce allow_mult bucket property so that no backend ever stores
%% an object with multiple contents if allow_mult=false for that bucket
enforce_allow_mult(Obj, BProps) ->
    case proplists:get_value(allow_mult, BProps) of
        true -> Obj;
        _ ->
            case riak_object:get_contents(Obj) of
                [_] -> Obj;
                Mult ->
                    {MD, V} = select_newest_content(Mult),
                    riak_object:set_contents(Obj, [{MD, V}])
            end
    end.

%% @private
%% choose the latest content to store for the allow_mult=false case
select_newest_content(Mult) ->
    hd(lists:sort(
         fun({MD0, _}, {MD1, _}) ->
                 riak_core_util:compare_dates(
                   dict:fetch(<<"X-Riak-Last-Modified">>, MD0),
                   dict:fetch(<<"X-Riak-Last-Modified">>, MD1))
         end,
         Mult)).

%% @private
do_map(ClientPid, QTerm, BKey, KeyData, #state{mod=Mod, modstate=ModState, mapcache=Cache}=State, VNode) ->
    {Reply, NewState} = case do_map(QTerm, BKey, Mod, ModState, KeyData, Cache, VNode, ClientPid) of
                            map_executing ->
                                {{mapexec_reply, executing, self()}, State};
                            {ok, Retval} ->
                                {{mapexec_reply, Retval, self()}, State};
                            {error, Error} ->
                                {{mapexec_error, self(), Error}, State}
                        end,
    gen_fsm:send_event(ClientPid, Reply),
    NewState.

%% @private
syntactic_put_merge(Mod, ModState, BKey, Obj1, ReqId) ->
    case Mod:get(ModState, BKey) of
        {error, notfound} -> {newobj, Obj1};
        {ok, Val0} ->
            Obj0 = binary_to_term(Val0),
            ResObj = riak_object:syntactic_merge(
                       Obj0,Obj1,term_to_binary(ReqId)),
            case riak_object:vclock(ResObj) =:= riak_object:vclock(Obj0) of
                true -> {oldobj, ResObj};
                false -> {newobj, ResObj}
            end
    end.

%% @private
get_merkle(State=#state{}) ->
    F = fun(K,V,Acc) -> merkerl:insert({K, erlang:phash2(V)}, Acc) end,
    InitAcc = merkerl:build_tree([]),
    case do_fold(F, InitAcc, State) of
        undefined ->
            {ok, term_to_binary(merkerl:build_tree([]))};
        M when is_tuple(M) ->
            {ok, term_to_binary(M, [compressed])}
    end.

%% @private
get_vclocks(KeyList,_State=#state{mod=Mod,modstate=ModState}) ->
    [{BKey, get_vclock(BKey,Mod,ModState)} || BKey <- KeyList].
%% @private
get_vclock(BKey,Mod,ModState) ->
    case Mod:get(ModState, BKey) of
        {error, notfound} -> vclock:fresh();
        {ok, Val} -> riak_object:vclock(binary_to_term(Val))
    end.

%% @private
do_fold(Fun, Acc0, _State=#state{mod=Mod, modstate=ModState}) ->
    Mod:fold(ModState, Fun, Acc0).

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% @private
handle_event({get_merkle, From}, StateName, State) ->
    gen_server2:reply(From, get_merkle(State)),
    {next_state, StateName, State, ?TIMEOUT};
handle_event({get_vclocks, From, KeyList}, StateName, State) ->
    gen_server2:reply(From, get_vclocks(KeyList, State)),
    {next_state, StateName, State, ?TIMEOUT};
handle_event({fold, {Fun, Acc0, From}}, StateName, State) ->
    gen_server2:reply(From, do_fold(Fun, Acc0, State)),
    {next_state, StateName, State, ?TIMEOUT}.

%% @private
handle_sync_event({diffobj,{BKey,BinObj}}, _From, StateName, StateData) ->
    case do_diffobj_put(BKey, binary_to_term(BinObj), StateData) of
        ok ->
            {reply, ok, StateName, StateData, ?TIMEOUT};
        {error, Err} ->
            error_logger:error_msg("Error storing handoff obj: ~p~n", [Err]),
            {reply, {error, Err}, StateName, StateData, ?TIMEOUT}
    end;
handle_sync_event(get_vnode_index, _From, StateName, StateData) ->
    {reply, StateData#state.idx, StateName, StateData, ?TIMEOUT};
handle_sync_event(_Even, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(vnode_shutdown, _StateName, StateData) ->
    {stop,normal,StateData};
handle_info(ok, StateName, StateData) ->
    {next_state, StateName, StateData, ?TIMEOUT};
handle_info({Mod, Msg}, StateName, #state { mod = Mod } = StateData) ->
    Mod:handle_info(StateData#state.modstate, Msg),
    {next_state, StateName, StateData, ?TIMEOUT}.

%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

do_map({erlang, {map, FunTerm, Arg, _Acc}}, BKey, Mod, ModState, KeyData, Cache, VNode, _ClientPid) ->
    CacheKey = build_key(FunTerm, Arg, KeyData),
    CacheVal = cache_fetch(BKey, CacheKey, Cache),
    case CacheVal of
        not_cached ->
            uncached_map(BKey, Mod, ModState, FunTerm, Arg, KeyData, VNode);
        CV ->
            {ok, CV}
    end;
do_map({javascript, {map, FunTerm, Arg, _}=QTerm}, BKey, Mod, ModState, KeyData, Cache, _VNode, ClientPid) ->
    CacheKey = build_key(FunTerm, Arg, KeyData),
    CacheVal = cache_fetch(BKey, CacheKey, Cache),
    case CacheVal of
        not_cached ->
            case Mod:get(ModState, BKey) of
                {ok, Binary} ->
                    V = binary_to_term(Binary),
                    riak_kv_js_manager:dispatch({ClientPid, QTerm, V, KeyData, BKey}),
                    map_executing;
                {error, notfound} ->
                    {error, notfound}
            end;
        CV ->
            {ok, CV}
    end.

build_key({modfun, CMod, CFun}, Arg, KeyData) ->
    {CMod, CFun, Arg, KeyData};
build_key({jsfun, FunName}, Arg, KeyData) ->
    {FunName, Arg, KeyData};
build_key(_, _, _) ->
    no_key.

cache_fetch(_BKey, no_key, _Cache) ->
    not_cached;
cache_fetch(BKey, CacheKey, Cache) ->
    case orddict:find(BKey, Cache) of
        error -> not_cached;
        {ok,CDict} ->
            case orddict:find(CacheKey,CDict) of
                error -> not_cached;
                {ok,CVal} -> CVal
            end
    end.

uncached_map(BKey, Mod, ModState, FunTerm, Arg, KeyData, VNode) ->
    case Mod:get(ModState, BKey) of
        {ok, Binary} ->
            V = binary_to_term(Binary),
            exec_map(V, FunTerm, Arg, BKey, KeyData, VNode);
        {error, notfound} ->
            exec_map({error, notfound}, FunTerm, Arg, BKey, KeyData, VNode);
        X ->
            {error, X}
    end.

exec_map(V, FunTerm, Arg, BKey, KeyData, VNode) ->
    try
        case FunTerm of
            {qfun, F} -> {ok, (F)(V,KeyData,Arg)};
            {modfun, M, F} ->
                MF_Res = M:F(V,KeyData,Arg),
                gen_fsm:send_event(VNode,
                                   {mapcache, BKey,{M,F,Arg,KeyData},MF_Res}),
                {ok, MF_Res}
        end
    catch C:R ->
            Reason = {C, R, erlang:get_stacktrace()},
            {error, Reason}
    end.
