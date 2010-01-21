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

-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([active/2,active/3]).

-define(TIMEOUT, 60000).

-record(state, {idx,mapcache,mod,modstate,handoff_q}).

start_link(Idx) -> gen_fsm:start_link(?MODULE, [Idx], []).

init([VNodeIndex]) ->
    io:format("~p init~n",[VNodeIndex]),
    Mod = riak:get_app_env(storage_backend),
    Configuration = riak:get_app_env(),
    {ok, ModState} = Mod:start(VNodeIndex, Configuration),
    StateData0 = #state{idx=VNodeIndex,mod=Mod,modstate=ModState,
                        handoff_q=not_in_handoff},
    case hometest(StateData0) of
        {next_state, StateName, StateData, Timeout} ->
            {ok, StateName, StateData, Timeout};
        {stop,normal,StateData} ->
            % always stick around for one timeout cycle
            {ok, active, StateData, ?TIMEOUT}
    end.

%% @private
hometest(StateData0=#state{idx=Idx,handoff_q=HQ}) ->
    StateData = StateData0#state{mapcache=orddict:new()},
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    Me = node(),
    case riak_ring:index_owner(MyRing, Idx) of
        Me ->
            io:format("~p hometest home~n",[Idx]),
            {next_state,active,StateData#state{handoff_q=not_in_handoff},
             ?TIMEOUT};
        TargetNode ->
            case net_adm:ping(TargetNode) of
                pang -> 
                    io:format("~p hometest pang~n",[Idx]),
                    {next_state,active,StateData,?TIMEOUT};
                pong -> 
                    case HQ of
                        not_in_handoff ->
                            io:format("~p hometest dh~n",[Idx]),
                            do_handoff(TargetNode, StateData);
                        _ ->
                            io:format("~p hometest dLh~n",[Idx]),
                            do_list_handoff(TargetNode, HQ, StateData)
                    end
            end
    end.

%% @private
do_handoff(TargetNode, StateData=#state{idx=Idx, mod=Mod, modstate=ModState}) ->
    case Mod:is_empty(ModState) of
        true ->
            io:format("~p dh empty~n",[Idx]),
            delete_and_exit(StateData);
        false ->
            io:format("~p dh nonempty~n",[Idx]),
            riak_handoff_sender:start_link(TargetNode, Idx, all),
            {next_state,active,StateData#state{handoff_q=[]},?TIMEOUT}
    end.

%% @private
do_list_handoff(TargetNode, BKeyList, StateData=#state{idx=Idx}) ->
    case BKeyList of
        [] ->
            io:format("~p dLh empty~n",[Idx]),
            delete_and_exit(StateData);
        _ ->
            io:format("~p dLh nonempty~n",[Idx]),
            riak_handoff_sender:start_link(TargetNode, Idx, BKeyList),
            {next_state,active,StateData#state{handoff_q=[]},?TIMEOUT}
    end.

%% @private
delete_and_exit(StateData=#state{idx=Idx,mod=Mod, modstate=ModState}) ->
    io:format("~p del and exit~n",[Idx]),
    ok = Mod:drop(ModState),
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
active({map, ClientPid, QTerm, BKey, KeyData},
       StateData=#state{mapcache=Cache,mod=Mod,modstate=ModState}) ->
    do_map(ClientPid,QTerm,BKey,KeyData,Cache,Mod,ModState,self()),
    {next_state,active,StateData,?TIMEOUT};
active(handoff_complete, StateData) ->
    hometest(StateData);
active({put, FSM_pid, BKey, RObj, ReqID, FSMTime},
       StateData=#state{idx=Idx,mapcache=Cache,handoff_q=HQ0}) ->
    HQ = case HQ0 of
        not_in_handoff -> not_in_handoff;
        _ -> [BKey|HQ0]
    end,
    gen_fsm:send_event(FSM_pid, {w, Idx, ReqID}),
    do_put(FSM_pid, BKey, RObj, ReqID, FSMTime, StateData),
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
            Res = Mod:put(ModState, BKey, Val),
            case Res of
                ok -> riak_stat:update(vnode_put);
                _ -> nop
            end,
            Res;
        _ -> ok
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
get_vclocks(KeyList,_State=#state{mod=Mod,modstate=ModState}) ->
    [{BKey, get_vclock(BKey,Mod,ModState)} || BKey <- KeyList].
%% @private
get_vclock(BKey,Mod,ModState) ->
    case Mod:get(ModState, BKey) of
        {error, notfound} -> vclock:fresh();
        {ok, Val} -> riak_object:vclock(binary_to_term(Val))
    end.

%% @private
do_fold(Fun, Acc0, _State=#state{idx=Idx,mod=Mod, modstate=ModState}) ->
    io:format("~p do_fold~n",[Idx]),
    Mod:fold(ModState, Fun, Acc0).

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% @private
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
handle_sync_event(_Even, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(vnode_shutdown, _StateName, StateData) ->
    {stop,normal,StateData}.

%% @private
terminate(_Reason, _StateName, _State) -> ok.

