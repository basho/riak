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

%% @doc gen_server process per partition-store

-module(riak_vnode).

-behaviour(gen_server2).
-export([start_link/1,start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-record(state, {idx,sidekick,mapcache,active,mod,modstate}).

%% @spec start_link(integer()) -> {ok, Pid}
%% @doc Start the vnode with index VNodeIndex
start_link(VNodeIndex) ->
    gen_server2:start_link(?MODULE, [VNodeIndex], []).

%% @spec start(integer()) -> {ok, Pid}
%% @doc Start the vnode with index VNodeIndex
start(VNodeIndex) ->
    gen_server2:start(?MODULE, [VNodeIndex], []).

%% @private
init([VNodeIndex]) ->
    riak_eventer:notify(riak_vnode, init, VNodeIndex),
    Mod = riak:get_app_env(storage_backend),
    {ok, ModState} = Mod:start(VNodeIndex),
    {ok, Sidekick} = riak_vnode_sidekick:start(self(), VNodeIndex),
    Cache = dict:new(),
    {ok, #state{idx=VNodeIndex,active=true,
                sidekick=Sidekick,mapcache=Cache,mod=Mod,modstate=ModState}}.

%% @private
handle_cast({get_merkle,From},State) ->
    gen_server2:reply(From, get_merkle(State)),
    {noreply, State};
handle_cast({get_vclocks, From, KeyList},State) ->
    gen_server2:reply(From, get_vclocks(KeyList,State)),
    {noreply, State};
handle_cast({mapcache,BKey,{M,F,Arg,KeyData},MF_Res},
            State=#state{mapcache=Cache}) ->
    KeyCache0 = case dict:find(BKey, Cache) of
        error -> dict:new();
        {ok,CDict} -> CDict
    end,
    KeyCache = dict:store({M,F,Arg,KeyData},MF_Res,KeyCache0),
    {noreply, State#state{mapcache=dict:store(BKey,KeyCache,Cache)}};
handle_cast(cache_purge, State=#state{idx=Idx}) ->
    riak_eventer:notify(riak_vnode, cache_purge, Idx),
    {noreply, State#state{mapcache=dict:new()}};
handle_cast(activate, State=#state{idx=Idx}) ->
    riak_eventer:notify(riak_vnode, activate, Idx),
    {noreply, State#state{active=true}};
handle_cast(deactivate, State=#state{idx=Idx}) ->  % when transferring to home
    riak_eventer:notify(riak_vnode, deactivate, Idx),
    {noreply, State#state{active=false}};
handle_cast({rexit, From}, State=#state{idx=Idx,sidekick=Sidekick}) ->
    riak_eventer:notify(riak_vnode, shutdown, {Idx,From}),
    Sidekick ! vnode_shutdown,
    {stop, normal, State};
handle_cast({vnode_merkle, {RemoteVN,Merkle}}, State) ->
    spawn(fun() -> do_merkle(RemoteVN,Merkle,State) end),
    {noreply, State};
handle_cast(_, State=#state{active=false}) -> % below here requires "active"
    {noreply, State};
handle_cast({map, ClientPid, QTerm, BKey, KeyData},
            State=#state{mapcache=Cache,mod=Mod,modstate=ModState}) ->
    riak_eventer:notify(riak_vnode, map, QTerm),
    VNode = self(),
    do_map(ClientPid,QTerm,BKey,KeyData,Cache,Mod,ModState,VNode),
    {noreply, State};
handle_cast({put, FSM_pid, BKey, RObj, ReqID, FSMTime},
            State=#state{mapcache=Cache,idx=Idx}) ->
    riak_eventer:notify(riak_vnode, put, {ReqID, Idx}),
    gen_fsm:send_event(FSM_pid, {w, Idx, ReqID}),
    do_put(FSM_pid, BKey, RObj, ReqID, FSMTime, State),
    {noreply, State#state{mapcache=dict:erase(BKey,Cache)}};
handle_cast({get, FSM_pid, BKey, ReqID}, State=#state{idx=Idx}) ->
    riak_eventer:notify(riak_vnode, get, {ReqID, Idx}),
    do_get(FSM_pid, BKey, ReqID, State),
    {noreply, State};
handle_cast({delete, Client, BKey, ReqID}, State=#state{idx=Idx}) ->
    riak_eventer:notify(riak_vnode, delete, {ReqID, Idx}),
    do_delete(Client, BKey, ReqID, State),
    {noreply, State};
handle_cast({list_bucket, Client, Bucket, ReqID},
            State=#state{mod=Mod,modstate=ModState,idx=Idx}) ->
    riak_eventer:notify(riak_vnode, list_bucket, {ReqID, Idx}),
    do_list_bucket(Client,ReqID,Bucket,Mod,ModState,Idx),
    {noreply, State}.

%% @private
handle_call(is_backup_node,_From,State) ->
    {reply, riak:get_app_env(backup, false), State};
handle_call({get_binary,BKey},
            From,State=#state{mod=Mod,modstate=ModState}) ->
    async_get_binary(From,BKey,Mod,ModState),
    {noreply, State};
handle_call(list,From,State=#state{mod=Mod,modstate=ModState}) ->
    async_do_list(From,Mod,ModState),
    {noreply, State}.

do_get(FSM_pid, BKey, ReqID,
       _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    RetVal = case do_get_binary(BKey, Mod, ModState) of
        {ok, Binary} -> {ok, binary_to_term(Binary)};
        X -> X
    end,
    riak_eventer:notify(riak_vnode, get_reply, ReqID),
    gen_fsm:send_event(FSM_pid, {r, RetVal, Idx, ReqID}).

async_get_binary(From,BKey,Mod,ModState) ->
    spawn(fun() ->
                  RetVal = do_get_binary(BKey,Mod,ModState),
                  gen_server2:reply(From, RetVal)
          end).

async_do_list(From,Mod,ModState) ->
    spawn(fun() ->
                  RetVal = Mod:list(ModState),
                  gen_server2:reply(From, RetVal)
          end).

do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx) ->
    RetVal = Mod:list_bucket(ModState,Bucket),
    riak_eventer:notify(riak_vnode, keys_reply, {ReqID, FSM_pid}),
    gen_fsm:send_event(FSM_pid, {kl, RetVal,Idx,ReqID}).

do_get_binary(BKey, Mod, ModState) ->
    Mod:get(ModState,BKey).

do_delete(Client, BKey, ReqID,
          _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    case Mod:delete(ModState, BKey) of
        ok ->
            riak_eventer:notify(riak_vnode,delete_reply,ReqID),
            gen_server2:reply(Client, {del, Idx, ReqID});
        {error, Reason} ->
            riak_eventer:notify(riak_vnode,delete_fail,{ReqID,Reason}),
            gen_server2:reply(Client, {fail, Idx, ReqID})
    end.

simple_binary_put(BKey, Val, Mod, ModState) ->
    Mod:put(ModState, BKey, Val).

do_put(FSM_pid, BKey, RObj, ReqID, PruneTime, 
       _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    {ok,Ring} = riak_ring_manager:get_my_ring(),    
    {Bucket,_Key} = BKey,
    BProps = riak_bucket:get_bucket(Bucket, Ring),
    case syntactic_put_merge(Mod, ModState, BKey, RObj, ReqID) of
        oldobj -> 
            riak_eventer:notify(riak_vnode,put_reply,ReqID),
            gen_fsm:send_event(FSM_pid, {dw, Idx, ReqID});
        {newobj, NewObj} ->
            VC = riak_object:vclock(NewObj),
            ObjToStore = riak_object:set_vclock(NewObj,
                                           vclock:prune(VC,PruneTime,BProps)),
            Val = term_to_binary(ObjToStore),
            case simple_binary_put(BKey, Val, Mod, ModState) of
                ok ->
                    riak_eventer:notify(riak_vnode,put_reply,ReqID),
                    gen_fsm:send_event(FSM_pid, {dw, Idx, ReqID});
                {error, Reason} ->
                    riak_eventer:notify(riak_vnode,put_fail,{ReqID,Reason}),
                    gen_fsm:send_event(FSM_pid, {fail, Idx, ReqID})
            end
    end.

do_map(ClientPid,{map,FunTerm,Arg,_Acc},
       BKey,KeyData,Cache,Mod,ModState,VNode) ->
    riak_eventer:notify(riak_vnode, map_start, {FunTerm,Arg,BKey}),
    CacheVal = case FunTerm of
        {qfun,_} -> not_cached; % live funs are not cached
        {modfun,CMod,CFun} ->
            case dict:find(BKey, Cache) of
                error -> not_cached;
                {ok,CDict} ->
                    case dict:find({CMod,CFun,Arg,KeyData},CDict) of
                        error -> not_cached;
                        {ok,CVal} -> CVal
                    end
            end
    end,
    RetVal = case CacheVal of
        not_cached ->
             uncached_map(BKey,Mod,ModState,FunTerm,Arg,KeyData,VNode);
        CV ->
             riak_eventer:notify(riak_vnode,cached_map,{FunTerm,Arg,BKey}),
             {mapexec_reply, CV, self()}
    end,
    riak_eventer:notify(riak_vnode, map_reply, {FunTerm,Arg,BKey}),
    gen_fsm:send_event(ClientPid, RetVal).

uncached_map(BKey,Mod,ModState,FunTerm,Arg,KeyData,VNode) ->
    riak_eventer:notify(riak_vnode, uncached_map, {FunTerm,Arg,BKey}),
    case do_get_binary(BKey, Mod, ModState) of
        {ok, Binary} ->
            V = binary_to_term(Binary),
            uncached_map1(V,FunTerm,Arg,BKey,KeyData,VNode);
        {error, notfound} ->
            uncached_map1({error, notfound},FunTerm,Arg,BKey,KeyData,VNode);
        X -> {mapexec_error, self(), X}
    end.

uncached_map1(V,FunTerm,Arg,BKey,KeyData,VNode) ->
    try
        MapVal = case FunTerm of
            {qfun,F} -> (F)(V,KeyData,Arg);
            {modfun,M,F} ->
                MF_Res = M:F(V,KeyData,Arg),
                gen_server2:cast(VNode,
                                 {mapcache, BKey,{M,F,Arg,KeyData},MF_Res}),
                MF_Res
        end,
        {mapexec_reply, MapVal, self()}
    catch C:R ->
         Reason = {C, R, erlang:get_stacktrace()},
         {mapexec_error, self(), Reason}
    end.

do_merkle(RemoteVN,RemoteMerkle,
          _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    % called upon receipt of merkle tree from RemoteVN
    % this function is quite computationally intensive if either side has
    %  a significant volume of data
    % note that the hashed values are of term {ok,BinVal} not BinVal
    %  (this is intentional)
    riak_eventer:notify(riak_vnode, merkle_start, RemoteVN),
    MyMerkle = merkerl:build_tree([{K,crypto:sha(V)} || {K,{ok,V}} <- 
                [{K,Mod:get(ModState,K)} || K <- Mod:list(ModState)]]),
    case merkerl:diff(MyMerkle,RemoteMerkle) of
        [] -> nop;
        MerkDiff ->
            RemoteResults = [{K,gen_server2:call(RemoteVN, {get_binary,K})} ||
                                K <- MerkDiff],
            RemoteObjs = [{K,binary_to_term(V)} || {K,{ok,V}} <- RemoteResults],
            IsBackup = gen_server2:call(RemoteVN,is_backup_node) ,
            [local_reconcile(K,V,Mod,ModState,IsBackup) || {K,V} <- RemoteObjs]
    end,
    riak_eventer:notify(riak_vnode, merkle_end, RemoteVN),
    % the following cast is because the remote side has now finished handoff
    gen_server2:cast(RemoteVN, {rexit, {peer, Idx}}).

local_reconcile(K,RemObj,Mod,ModState,IsBackup) ->
    FinalObj = case IsBackup of
        true -> RemObj;
        false ->
            case Mod:get(ModState,K) of
                {ok,V} -> riak_object:reconcile([binary_to_term(V),RemObj],false);
                _ -> RemObj
            end
    end,
    case Mod:put(ModState, K, term_to_binary(FinalObj, [compressed])) of
        ok ->
            riak_eventer:notify(riak_vnode, stored_handoff, K);
        {error, Reason} ->
            riak_eventer:notify(riak_vnode, stored_handoff_fail, {K,Reason})
    end.

%% @private
get_merkle(_State=#state{mod=Mod,modstate=ModState}) ->
    KeyList = Mod:list(ModState),
    Merk0 = merkerl:build_tree([]),
    get_merk(Mod,ModState,KeyList,Merk0).
%% @private
get_merk(_Mod,_ModState,[],Merk) -> Merk;
get_merk(Mod,ModState,[BKey|KeyList],Merk) ->
    V = Mod:get(ModState,BKey), % normally, V = {ok,BinObj}
    get_merk(Mod,ModState,KeyList,merkerl:insert({BKey,erlang:phash2(V)},Merk)).

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
handle_info(_Msg, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
syntactic_put_merge(Mod, ModState, BKey, Obj1, ReqId) ->
    case Mod:get(ModState, BKey) of
        {error, notfound} -> {newobj, Obj1};
        {ok, Val0} ->
            Obj0 = binary_to_term(Val0),
            ResObj = riak_object:syntactic_merge(
                       Obj0,Obj1,term_to_binary(ReqId)),
            case riak_object:vclock(ResObj) =:= riak_object:vclock(Obj0) of
                true -> oldobj;
                false -> {newobj, ResObj}
            end    
    end.

