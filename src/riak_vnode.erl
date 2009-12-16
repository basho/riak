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

-module(riak_vnode).
-behaviour(gen_fsm).

-export([start/1]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([active/2,merk_waiting/2,waiting_diffobjs/2]).
-export([active/3,merk_waiting/3,waiting_diffobjs/3]).

-define(TIMEOUT, 60000).

-record(state, {idx,mapcache,mod,modstate,waiting_diffobjs}).

start(Idx) ->
    gen_fsm:start(?MODULE, [Idx], []).
init([VNodeIndex]) ->
    Mod = riak:get_app_env(storage_backend),
    {ok, ModState} = Mod:start(VNodeIndex),
    StateData0 = #state{idx=VNodeIndex,mod=Mod,modstate=ModState},
    {next_state, StateName, StateData, Timeout} = hometest(StateData0),
    {ok, StateName, StateData, Timeout}.

%% @private
hometest(StateData0=#state{idx=Idx}) ->
    StateData = StateData0#state{mapcache=orddict:new()},
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    Me = node(),
    case riak_ring:index_owner(MyRing, Idx) of
        Me ->
            {next_state,active,StateData,?TIMEOUT};
        TargetNode ->
            case net_adm:ping(TargetNode) of
                pang -> {next_state,active,StateData,?TIMEOUT};
                pong -> build_and_send_merkle(TargetNode, StateData)
            end
    end.

%% @private
build_and_send_merkle(TargetNode,
                      StateData=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    ObjList = Mod:list(ModState),
    Merk = make_merk(StateData, ObjList),
    gen_server:cast({riak_vnode_master, TargetNode},
                    {vnode_merkle, {self(),Idx,Merk,ObjList}}),
    {next_state,merk_waiting,
     StateData#state{waiting_diffobjs=ObjList},?TIMEOUT}.

%% @private
make_merk(StateData,ObjList) ->
    Merk = merkerl:build_tree([]),
    make_merk(StateData,ObjList,Merk).
make_merk(_StateData,[],Merk) -> Merk;
make_merk(StateData=#state{mod=Mod,modstate=ModState},
          [BKey|Objlist],Merk) ->
    V = Mod:get(ModState,BKey), % normally, V = {ok,BinObj}
    make_merk(StateData,Objlist,merkerl:insert({BKey,erlang:phash2(V)},Merk)).

%% @private
send_diff_objs(TargetNode,DiffList,
               StateData=#state{mod=Mod,modstate=ModState}) ->
    % send each obj (BKey) in difflist to targetnode
    % return a state with waiting_diffobjs populated
    Sent = [K || K <- [send_diff_obj(TargetNode,BKey,Mod,ModState) 
                       || BKey <- DiffList], K /= nop],
    StateData#state{waiting_diffobjs=Sent}.
send_diff_obj(TargetNode,BKey,Mod,ModState) ->
    case Mod:get(ModState,BKey) of
        {ok,BinObj} ->
            gen_fsm:send_event(TargetNode, {diffobj,{BKey,BinObj,self()}}),
            BKey;
        _ ->
            nop
    end.

%%%%%%%%%% in merk_waiting state, we have sent a merkle tree to the
%%%%%%%%%% home vnode, and are waiting for a list of different objects
merk_waiting({get_binary,_BKey}, _From, StateData) ->
    {reply,{error, wrong_state},active,StateData,?TIMEOUT};
merk_waiting(list, _From, StateData) ->
    {reply,{error, wrong_state},active,StateData,?TIMEOUT}.
merk_waiting(timeout, StateData) ->
    % didn't get a response to our merkle tree, switch back to active mode
    {next_state,active,StateData#state{waiting_diffobjs=[]},?TIMEOUT};
merk_waiting(merk_nodiff, StateData0=#state{waiting_diffobjs=WD,
                                            mod=Mod,modstate=ModState}) ->
    % the far side is home and has all of the objects, cleanup time
    StateData = StateData0#state{waiting_diffobjs=[]},
    [Mod:delete(ModState, BKey) || BKey <- WD],
    case Mod:list(ModState) of
        [] -> 
            Mod:stop(ModState),
            {stop,normal,StateData};
        _ ->
            hometest(StateData)
    end;
merk_waiting({merk_diff,TargetVNode,DiffList}, StateData0) ->
    StateData = send_diff_objs(TargetVNode,DiffList,StateData0),
    {next_state,waiting_diffobjs,StateData,?TIMEOUT};
merk_waiting({diffobj,{_BKey,_BinObj,_RemNode}}, StateData) ->
    hometest(StateData);
merk_waiting({map, ClientPid, QTerm, BKey, KeyData},
             StateData=#state{mapcache=Cache,mod=Mod,modstate=ModState}) ->
    do_map(ClientPid,QTerm,BKey,KeyData,Cache,Mod,ModState,self()),
    {next_state,merk_waiting,StateData,?TIMEOUT};
merk_waiting({put, FSM_pid, _BKey, _RObj, ReqID, _FSMTime},
             StateData=#state{idx=Idx}) ->
    gen_fsm:send_event(FSM_pid, {fail, Idx, ReqID}),
    {next_state,merk_waiting,StateData,?TIMEOUT};
merk_waiting({get, FSM_pid, BKey, ReqID}, StateData) ->
    do_get(FSM_pid, BKey, ReqID, StateData),
    {next_state,merk_waiting,StateData,?TIMEOUT};
merk_waiting({vnode_merkle, {_RemoteVN,_Merkle,_ObjList}}, StateData) ->
    hometest(StateData);
merk_waiting({list_bucket, FSM_pid, Bucket, ReqID},
             StateData=#state{mod=Mod,modstate=ModState,idx=Idx}) ->
    do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx),
    {next_state,merk_waiting,StateData,?TIMEOUT};
merk_waiting({delete, From, BKey, ReqID}, StateData=#state{mapcache=Cache}) ->
    do_delete(From, BKey, ReqID, StateData),
    {next_state,
     merk_waiting,StateData#state{mapcache=orddict:erase(BKey,Cache)},?TIMEOUT};
merk_waiting(_OtherMessage,StateData) ->
    {next_state,merk_waiting,StateData,?TIMEOUT}.

%%%%%%%%%% in waiting_diffobjs state, we have sent a list of diff objs to the
%%%%%%%%%% home vnode, and are waiting to hear that they've been handled
waiting_diffobjs({get_binary,_BKey}, _From, StateData) ->
    {reply,{error, wrong_state},active,StateData,?TIMEOUT};
waiting_diffobjs(list, _From, StateData) ->
    {reply,{error, wrong_state},active,StateData,?TIMEOUT}.
waiting_diffobjs(timeout, StateData) ->
    {next_state,active,StateData#state{waiting_diffobjs=[]},?TIMEOUT};
waiting_diffobjs({resolved_diffobj,K},
         StateData0=#state{waiting_diffobjs=WD0,mod=Mod,modstate=ModState})->
    WD = lists:delete(K,WD0),
    Mod:delete(ModState, K),
    StateData = StateData0#state{waiting_diffobjs=WD},
    case WD of
        [] -> % resolved all the intended diff objects
            hometest(StateData);
        _ -> % some left, keep waiting
            {next_state,waiting_diffobjs,StateData,?TIMEOUT}
    end;
waiting_diffobjs(merk_nodiff, StateData) ->
    % got merkle reply at a very strange time
    % jump into active mode to handle some requests before trying again
    {next_state,active,StateData#state{waiting_diffobjs=[]},?TIMEOUT};
waiting_diffobjs({merk_diff,_TargetNode,_DiffList}, StateData) ->
    % got merkle reply at a very strange time
    % jump into active mode to handle some requests before trying again
    {next_state,active,StateData#state{waiting_diffobjs=[]},?TIMEOUT};
waiting_diffobjs({diffobj,{_BKey,_BinObj,_RemNode}}, StateData) ->
    hometest(StateData);
waiting_diffobjs({map, ClientPid, QTerm, BKey, KeyData},
                 StateData=#state{mapcache=Cache,mod=Mod,modstate=ModState}) ->
    do_map(ClientPid,QTerm,BKey,KeyData,Cache,Mod,ModState,self()),
    {next_state,waiting_diffobjs,StateData,?TIMEOUT};
waiting_diffobjs({put, FSM_pid, _BKey, _RObj, ReqID, _FSMTime},
                 StateData=#state{idx=Idx}) ->
    gen_fsm:send_event(FSM_pid, {fail, Idx, ReqID}),
    {next_state,waiting_diffobjs,StateData,?TIMEOUT};
waiting_diffobjs({get, FSM_pid, BKey, ReqID}, StateData) ->
    do_get(FSM_pid, BKey, ReqID, StateData),
    {next_state,waiting_diffobjs,StateData,?TIMEOUT};
waiting_diffobjs({vnode_merkle, {_RemoteVN,_Merkle,_ObjList}}, StateData) ->
    hometest(StateData);
waiting_diffobjs({list_bucket, FSM_pid, Bucket, ReqID},
                 StateData=#state{mod=Mod,modstate=ModState,idx=Idx}) ->
    do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx),
    {next_state,waiting_diffobjs,StateData,?TIMEOUT};
waiting_diffobjs({delete, From, BKey, ReqID},
                 StateData=#state{mapcache=Cache}) ->
    do_delete(From, BKey, ReqID, StateData),
    {next_state,waiting_diffobjs,
     StateData#state{mapcache=orddict:erase(BKey,Cache)},?TIMEOUT};
waiting_diffobjs(_OtherMessage,StateData) ->
    {next_state,waiting_diffobjs,StateData,?TIMEOUT}.

%%%%%%%%%% in active state, we process normal client requests
active({get_binary,BKey}, _From, StateData=#state{mod=Mod,modstate=ModState}) ->
    {reply,Mod:get(ModState,BKey),active,StateData,?TIMEOUT};
active(list, _From, StateData=#state{mod=Mod,modstate=ModState}) ->
    {reply,{ok, Mod:list(ModState)},active,StateData,?TIMEOUT}.
active(timeout, StateData) ->
    hometest(StateData);
active({diffobj,{BKey,BinObj,FromVN}}, StateData) ->
    do_diffobj_put(BKey, binary_to_term(BinObj), StateData),
    gen_fsm:send_event(FromVN,{resolved_diffobj,BKey}),
    {next_state,active,StateData,?TIMEOUT};
active({map, ClientPid, QTerm, BKey, KeyData},
       StateData=#state{mapcache=Cache,mod=Mod,modstate=ModState}) ->
    do_map(ClientPid,QTerm,BKey,KeyData,Cache,Mod,ModState,self()),
    {next_state,active,StateData,?TIMEOUT};
active({put, FSM_pid, BKey, RObj, ReqID, FSMTime},
       StateData=#state{idx=Idx,mapcache=Cache}) ->
    gen_fsm:send_event(FSM_pid, {w, Idx, ReqID}),
    do_put(FSM_pid, BKey, RObj, ReqID, FSMTime, StateData),
    {next_state,
     active,StateData#state{mapcache=orddict:erase(BKey,Cache)},?TIMEOUT};
active({get, FSM_pid, BKey, ReqID}, StateData) ->
    do_get(FSM_pid, BKey, ReqID, StateData),
    {next_state,active,StateData,?TIMEOUT};
active({vnode_merkle, {RemoteVN,Merkle,ObjList}}, StateData) ->
    Me = self(),
    spawn(fun() -> do_merkle(Me,RemoteVN,Merkle,ObjList,StateData) end),
    {next_state,active,StateData,?TIMEOUT};
active({list_bucket, FSM_pid, Bucket, ReqID},
       StateData=#state{mod=Mod,modstate=ModState,idx=Idx}) ->
    do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx),
    {next_state,active,StateData,?TIMEOUT};
active({delete, From, BKey, ReqID}, StateData=#state{mapcache=Cache}) ->
    do_delete(From, BKey, ReqID, StateData),
    {next_state,
     active,StateData#state{mapcache=orddict:erase(BKey,Cache)},?TIMEOUT};
active({mapcache, BKey,{M,F,Arg,KeyData},MF_Res},
       StateData=#state{mapcache=Cache}) ->
    KeyCache0 = case orddict:find(BKey, Cache) of
        error -> orddict:new();
        {ok,CDict} -> CDict
    end,
    KeyCache = orddict:store({M,F,Arg,KeyData},MF_Res,KeyCache0),
    {next_state,active,
     StateData#state{mapcache=orddict:store(BKey,KeyCache,Cache)},?TIMEOUT};
active(merk_nodiff, StateData) ->
    hometest(StateData);
active({merk_diff,_TargetNode,_DiffList}, StateData) ->
    hometest(StateData);
active({resolved_diffobj,_K}, StateData) ->
    hometest(StateData).

%% @private
do_get(FSM_pid, BKey, ReqID,
       _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    RetVal = case do_get_binary(BKey, Mod, ModState) of
        {ok, Binary} -> {ok, binary_to_term(Binary)};
        X -> X
    end,
    riak_stat:update(vnode_get),
    gen_fsm:send_event(FSM_pid, {r, RetVal, Idx, ReqID}).

%% @private
do_list_bucket(FSM_pid,ReqID,Bucket,Mod,ModState,Idx) ->
    RetVal = Mod:list_bucket(ModState,Bucket),
    riak_eventer:notify(riak_vnode, keys_reply, {ReqID, FSM_pid}),
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
            AMObj = enforce_allow_mult(NewObj, riak_bucket:get_bucket(Bucket)),
            Val = term_to_binary(AMObj),
            Mod:put(ModState, BKey, Val),
            riak_stat:update(vnode_put);
        _ -> nop
    end.

%% @private
% upon receipt of a client-initiated put
do_put(FSM_pid, BKey, RObj, ReqID, PruneTime, 
       _State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    {ok,Ring} = riak_ring_manager:get_my_ring(),    
    {Bucket,_Key} = BKey,
    BProps = riak_bucket:get_bucket(Bucket, Ring),
    case syntactic_put_merge(Mod, ModState, BKey, RObj, ReqID) of
        oldobj -> 
            gen_fsm:send_event(FSM_pid, {dw, Idx, ReqID});
        {newobj, NewObj} ->
            VC = riak_object:vclock(NewObj),
            AMObj = enforce_allow_mult(NewObj, BProps),
            ObjToStore = riak_object:set_vclock(AMObj,
                                           vclock:prune(VC,PruneTime,BProps)),
            Val = term_to_binary(ObjToStore),
            case Mod:put(ModState, BKey, Val) of
                ok ->
                    gen_fsm:send_event(FSM_pid, {dw, Idx, ReqID});
                {error, _Reason} ->
                    gen_fsm:send_event(FSM_pid, {fail, Idx, ReqID})
            end,
            riak_stat:update(vnode_put)
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
                 riak_util:compare_dates(
                   dict:fetch(<<"X-Riak-Last-Modified">>, MD0),
                   dict:fetch(<<"X-Riak-Last-Modified">>, MD1))
         end,
         Mult)).

%% @private
do_map(ClientPid,{map,FunTerm,Arg,_Acc},
       BKey,KeyData,Cache,Mod,ModState,VNode) ->
    CacheVal = case FunTerm of
        {qfun,_} -> not_cached; % live funs are not cached
        {modfun,CMod,CFun} ->
            case orddict:find(BKey, Cache) of
                error -> not_cached;
                {ok,CDict} ->
                    case orddict:find({CMod,CFun,Arg,KeyData},CDict) of
                        error -> not_cached;
                        {ok,CVal} -> CVal
                    end
            end
    end,
    RetVal = case CacheVal of
        not_cached ->
             uncached_map(BKey,Mod,ModState,FunTerm,Arg,KeyData,VNode);
        CV ->
             {mapexec_reply, CV, self()}
    end,
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
                gen_fsm:send_event(VNode,
                                   {mapcache, BKey,{M,F,Arg,KeyData},MF_Res}),
                MF_Res
        end,
        {mapexec_reply, MapVal, self()}
    catch C:R ->
         Reason = {C, R, erlang:get_stacktrace()},
         {mapexec_error, self(), Reason}
    end.

%% @private
do_merkle(Me,RemoteVN,RemoteMerkle,ObjList,StateData) ->
    % given a RemoteMerkle over the ObjList from RemoteVN
    % determine which elements in ObjList we differ on
    MyMerkle = make_merk(StateData,ObjList),
    case merkerl:diff(MyMerkle,RemoteMerkle) of
        [] -> gen_fsm:send_event(RemoteVN,merk_nodiff);
        DiffList -> gen_fsm:send_event(RemoteVN,{merk_diff,Me,DiffList})
    end.

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
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% @private
handle_event({get_merkle, From}, StateName, State) ->
    gen_server2:reply(From, get_merkle(State)),
    {next_state, StateName, State, ?TIMEOUT};
handle_event({get_vclocks, From, KeyList}, StateName, State) ->
    gen_server2:reply(From, get_vclocks(KeyList, State)),
    {next_state, StateName, State, ?TIMEOUT}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(vnode_shutdown, _StateName, StateData) ->
    {stop,normal,StateData}.

%% @private
terminate(_Reason, _StateName, _State) -> ok.

