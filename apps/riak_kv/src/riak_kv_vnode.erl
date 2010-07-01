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

%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.

-module(riak_kv_vnode).
-export([start_vnode/1, del/3, put/6, readrepair/6, list_keys/3,map/5, fold/3]).
-export([purge_mapcaches/0,mapcache/4,terminate/2]).
-export([is_empty/1, delete/1]).
-export([handoff_starting/2, handoff_cancelled/1, handoff_finished/2, handle_handoff_data/3]).
-export([init/1, handle_command/3, handle_handoff_command/3]).
-include_lib("riak_kv/include/riak_kv_vnode.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([map_test/3]).
-endif.
-record(state, {idx :: partition(), 
                mod :: module(),
                modstate :: term(),
                mapcache :: term(),
                in_handoff = false :: boolean()}).

-record(putargs, {returnbody :: boolean(),
                  lww :: boolean(),
                  bkey :: {binary(), binary()},
                  robj :: term(),
                  reqid :: non_neg_integer(), 
                  bprops :: maybe_improper_list(),
                  prunetime :: non_neg_integer()}).
-define(CLEAR_MAPCACHE_INTERVAL, 60000).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, riak_kv_vnode).

del(Preflist, BKey, ReqId) ->
    riak_core_vnode_master:sync_command(Preflist,
                                        ?KV_DELETE_REQ{bkey=BKey,
                                                       req_id=ReqId},
                                        riak_kv_vnode_master).

%% Issue a put for the object to the preflist, expecting a reply
%% to an FSM.
put(Preflist, BKey, Obj, ReqId, StartTime, Options) ->
    put(Preflist, BKey, Obj, ReqId, StartTime, Options, {fsm, undefined, self()}).

put(Preflist, BKey, Obj, ReqId, StartTime, Options, Sender) ->   
    riak_core_vnode_master:command(Preflist,
                                   ?KV_PUT_REQ{
                                      bkey = BKey,
                                      object = Obj,
                                      req_id = ReqId,
                                      start_time = StartTime,
                                      options = Options},
                                   Sender,
                                   riak_kv_vnode_master).

%% Do a put without sending any replies
readrepair(Preflist, BKey, Obj, ReqId, StartTime, Options) ->   
    put(Preflist, BKey, Obj, ReqId, StartTime, Options, ignore).

list_keys(Preflist, Bucket, ReqId) ->
    riak_core_vnode_master:command(Preflist,
                                   ?KV_LISTKEYS_REQ{
                                      bucket=Bucket,
                                      req_id=ReqId},
                                   {fsm, undefined, self()},
                                   riak_kv_vnode_master).

map(Preflist, ClientPid, QTerm, BKey, KeyData) ->
    riak_core_vnode_master:command(Preflist,
                                   ?KV_MAP_REQ{
                                      qterm=QTerm,
                                      bkey=BKey,
                                      keydata=KeyData},
                                   {fsm, undefined, ClientPid},
                                   riak_kv_vnode_master).

fold(Preflist, Fun, Acc0) ->
    riak_core_vnode_master:sync_spawn_command(Preflist,
                                              ?FOLD_REQ{
                                                 foldfun=Fun,
                                                 acc0=Acc0},
                                              riak_kv_vnode_master).

purge_mapcaches() ->
    VNodes = riak_core_vnode_master:all_nodes(?MODULE),
    lists:foreach(fun(VNode) -> riak_core_vnode:send_command(VNode, purge_mapcache) end, VNodes).
    
mapcache(Pid, BKey, What, R) ->
    riak_core_vnode:send_command(Pid, {mapcache, BKey, What, R}).
    

%% VNode callbacks

init([Index]) ->
    Mod = app_helper:get_env(riak_kv, storage_backend),
    Configuration = app_helper:get_env(riak_kv),
    {ok, ModState} = Mod:start(Index, Configuration),
    schedule_clear_mapcache(),
    {ok, #state{idx=Index, mod=Mod, modstate=ModState, mapcache=orddict:new()}}.

handle_command(?KV_PUT_REQ{bkey=BKey,
                           object=Object,
                           req_id=ReqId,
                           start_time=StartTime,
                           options=Options},
               Sender, State=#state{idx=Idx,mapcache=Cache}) ->
    riak_core_vnode:reply(Sender, {w, Idx, ReqId}),
    do_put(Sender, BKey,  Object, ReqId, StartTime, Options, State),
    {noreply, State#state{mapcache=orddict:erase(BKey,Cache)}};

handle_command(?KV_GET_REQ{bkey=BKey,req_id=ReqId},Sender,State) ->
    do_get(Sender, BKey, ReqId, State);
handle_command(?KV_LISTKEYS_REQ{bucket=Bucket, req_id=ReqId}, _Sender, 
               State=#state{mod=Mod, modstate=ModState, idx=Idx}) ->
    do_list_bucket(ReqId,Bucket,Mod,ModState,Idx,State);
handle_command(?KV_DELETE_REQ{bkey=BKey, req_id=ReqId}, _Sender, 
               State=#state{mod=Mod, modstate=ModState, 
                            idx=Idx, mapcache=Cache}) ->
    NewState = State#state{mapcache=orddict:erase(BKey,Cache)},
    case Mod:delete(ModState, BKey) of
        ok ->
            {reply, {del, Idx, ReqId}, NewState};
        {error, _Reason} ->
            {reply, {fail, Idx, ReqId}, NewState}
    end;
handle_command(?KV_MAP_REQ{bkey=BKey,qterm=QTerm,keydata=KeyData},
               Sender, State) ->
    do_map(Sender,QTerm,BKey,KeyData,State,self());
handle_command(?FOLD_REQ{foldfun=Fun, acc0=Acc},_Sender,State) ->
    Reply = do_fold(Fun, Acc, State),
    {reply, Reply, State};
%% Commands originating from inside this vnode
handle_command({mapcache, BKey,{FunName,Arg,KeyData}, MF_Res}, _Sender,
               State=#state{mapcache=Cache}) ->
    KeyCache0 = case orddict:find(BKey, Cache) of
        error -> orddict:new();
        {ok,CDict} -> CDict
    end,
    KeyCache = orddict:store({FunName,Arg,KeyData},MF_Res,KeyCache0),
    {noreply, State#state{mapcache=orddict:store(BKey,KeyCache,Cache)}};
handle_command({mapcache, BKey,{M,F,Arg,KeyData},MF_Res}, _Sender, 
               State=#state{mapcache=Cache}) ->
    KeyCache0 = case orddict:find(BKey, Cache) of
        error -> orddict:new();
        {ok,CDict} -> CDict
    end,
    KeyCache = orddict:store({M,F,Arg,KeyData},MF_Res,KeyCache0),
    {noreply, State#state{mapcache=orddict:store(BKey,KeyCache,Cache)}};
handle_command(purge_mapcache, _Sender, State) ->
    {noreply, State#state{mapcache=orddict:new()}};
handle_command(clear_mapcache, _Sender, State) ->
    schedule_clear_mapcache(),
    {noreply, State#state{mapcache=orddict:new()}}.

handle_handoff_command(Req=?FOLD_REQ{}, Sender, State) -> 
    handle_command(Req, Sender, State);
handle_handoff_command(purge_mapcache, Sender, State) ->
    handle_command(purge_mapcache, Sender, State);
handle_handoff_command(clear_mapcache, Sender, State) ->
    handle_command(clear_mapcache, Sender, State);
handle_handoff_command(_Req, _Sender, State) -> {forward, State}.


handoff_starting(_TargetNode, State) ->
    {true, State#state{in_handoff=true}}.

handoff_cancelled(State) ->
    {ok, State#state{in_handoff=false}}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(BKey, Obj, State) ->
    case do_diffobj_put(BKey, Obj, State) of
        ok ->
            {reply, ok, State};
        Err ->
            {reply, {error, Err}, State}
    end.

is_empty(State=#state{mod=Mod, modstate=ModState}) ->
    {Mod:is_empty(ModState), State}.

delete(State=#state{mod=Mod, modstate=ModState}) ->
    ok = Mod:drop(ModState),
    {ok, State}.

terminate(_Reason, #state{mod=Mod, modstate=ModState}) ->
    Mod:stop(ModState),
    ok.

%% old vnode helper functions


%store_call(State=#state{mod=Mod, modstate=ModState}, Msg) ->
%    Mod:call(ModState, Msg).

%% @private
% upon receipt of a client-initiated put
do_put(Sender, {Bucket,_Key}=BKey, RObj, ReqID, PruneTime, Options, State) ->
    {ok,Ring} = riak_core_ring_manager:get_my_ring(),
    BProps = riak_core_bucket:get_bucket(Bucket, Ring),
    PutArgs = #putargs{returnbody=proplists:get_value(returnbody,Options,false),
                       lww=proplists:get_value(last_write_wins, BProps, false),
                       bkey=BKey,
                       robj=RObj,
                       reqid=ReqID,
                       bprops=BProps,
                       prunetime=PruneTime},
    Reply = perform_put(prepare_put(State, PutArgs), State, PutArgs),
    riak_core_vnode:reply(Sender, Reply),
    riak_kv_stat:update(vnode_put).

prepare_put(#state{}, #putargs{lww=true, robj=RObj}) -> 
    {true, RObj};
prepare_put(#state{mod=Mod,modstate=ModState}, #putargs{bkey=BKey,
                                                        robj=RObj,
                                                        reqid=ReqID,
                                                        bprops=BProps,
                                                        prunetime=PruneTime}) ->
    case syntactic_put_merge(Mod, ModState, BKey, RObj, ReqID) of
        {oldobj, OldObj} ->
            {false, OldObj};
        {newobj, NewObj} ->
            VC = riak_object:vclock(NewObj),
            AMObj = enforce_allow_mult(NewObj, BProps),
            ObjToStore = riak_object:set_vclock(
                           AMObj,
                           vclock:prune(VC,PruneTime,BProps)
                           ),
            {true, ObjToStore}
    end.

perform_put({false, Obj},#state{idx=Idx},#putargs{returnbody=true,reqid=ReqID}) ->
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
do_get(_Sender, BKey, ReqID,
       State=#state{idx=Idx,mod=Mod,modstate=ModState}) ->
    RetVal = case do_get_binary(BKey, Mod, ModState) of
        {ok, Binary} -> {ok, binary_to_term(Binary)};
        X -> X
    end,
    riak_kv_stat:update(vnode_get),
    {reply, {r, RetVal, Idx, ReqID}, State}.

%% @private
do_get_binary(BKey, Mod, ModState) ->
    Mod:get(ModState,BKey).


%% @private
do_list_bucket(ReqID,Bucket,Mod,ModState,Idx,State) ->
    RetVal = Mod:list_bucket(ModState,Bucket),
    {reply, {kl, RetVal, Idx, ReqID}, State}.

%% @private
do_fold(Fun, Acc0, _State=#state{mod=Mod, modstate=ModState}) ->
    Mod:fold(ModState, Fun, Acc0).

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
do_map(Sender, QTerm, BKey, KeyData, #state{mod=Mod, modstate=ModState, mapcache=Cache}=State, VNode) ->
    {Reply, NewState} = case do_map(QTerm, BKey, Mod, ModState, KeyData, Cache, VNode, Sender) of
                            map_executing ->
                                {{mapexec_reply, executing, self()}, State};
                            {ok, Retval} ->
                                {{mapexec_reply, Retval, self()}, State};
                            {error, Error} ->
                                {{mapexec_error, self(), Error}, State}
                        end,
    {reply, Reply, NewState}.

do_map({erlang, {map, FunTerm, Arg, _Acc}}, BKey, Mod, ModState, KeyData, Cache, VNode, _Sender) ->
    CacheKey = build_key(FunTerm, Arg, KeyData),
    CacheVal = cache_fetch(BKey, CacheKey, Cache),
    case CacheVal of
        not_cached ->
            uncached_map(BKey, Mod, ModState, FunTerm, Arg, KeyData, VNode);
        CV ->
            {ok, CV}
    end;
do_map({javascript, {map, FunTerm, Arg, _}=QTerm}, BKey, Mod, ModState, KeyData, Cache, _VNode, Sender) ->
    CacheKey = build_key(FunTerm, Arg, KeyData),
    CacheVal = cache_fetch(BKey, CacheKey, Cache),
    case CacheVal of
        not_cached ->
            case Mod:get(ModState, BKey) of
                {ok, Binary} ->
                    V = binary_to_term(Binary),
                    riak_kv_js_manager:dispatch({Sender, QTerm, V, KeyData, BKey}),
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

exec_map(V, FunTerm, Arg, BKey, KeyData, _VNode) ->
    try
        case FunTerm of
            {qfun, F} -> {ok, (F)(V,KeyData,Arg)};
            {modfun, M, F} ->
                MF_Res = M:F(V,KeyData,Arg),
                mapcache(self(), BKey,{M,F,Arg,KeyData},MF_Res),
                {ok, MF_Res}
        end
    catch C:R ->
            Reason = {C, R, erlang:get_stacktrace()},
            {error, Reason}
    end.

schedule_clear_mapcache() ->
    riak_core_vnode:send_command_after(?CLEAR_MAPCACHE_INTERVAL, clear_mapcache).


-ifdef(TEST).

dummy_backend() ->
    Ring = riak_core_ring:fresh(16,node()),
    riak_core_ring_manager:set_ring_global(Ring),
    application:set_env(riak_kv, storage_backend, riak_kv_ets_backend),
    application:set_env(riak_core, default_bucket_props, []).
   

%% Make sure the mapcache gets cleared when the bkey is updated
mapcache_put_test() ->
    dummy_backend(),
    BKey = {<<"b">>,<<"k">>},
    CacheKey = {mod,func,arg,keydata},
    {ok, S1} = init([0]),
    ?assertEqual(not_cached, cache_fetch(BKey, CacheKey, S1#state.mapcache)),
    {noreply, S2} = handle_command({mapcache, BKey, CacheKey, result},
                                   noreply, S1),
    ?assertEqual(result, cache_fetch(BKey, CacheKey, S2#state.mapcache)),

    O = riak_object:new(<<"b">>,<<"k">>,<<"v">>),
    {noreply, S3} = handle_command(?KV_PUT_REQ{bkey=BKey,
                                               object=O,
                                               req_id=123,
                                               start_time=now(),
                                               options=[]},
                                   {raw, 456, self()}, S2),
    ?assertEqual(not_cached, cache_fetch(BKey, CacheKey, S3#state.mapcache)),
    %% The put request generates a {w,...} and {dw,...} event
    flush_msgs().

mapcache_delete_test() ->
    dummy_backend(),

    BKey = {<<"b">>,<<"k">>},
    CacheKey = {mod,func,arg,keydata},
    {ok, S1} = init([0]),
    ?assertEqual(not_cached, cache_fetch(BKey, CacheKey, S1#state.mapcache)),
    {noreply, S2} = handle_command({mapcache, BKey, CacheKey, result},
                                   noreply, S1),
    ?assertEqual(result, cache_fetch(BKey, CacheKey, S2#state.mapcache)),

    {reply, {del, 0, 123}, S3} = handle_command(?KV_DELETE_REQ{bkey=BKey,
                                                  req_id=123},
                                   {raw, 456, self()}, S2),
    ?assertEqual(not_cached, cache_fetch(BKey, CacheKey, S3#state.mapcache)),
    %% The put request generates a {w,...} and {dw,...} event
    flush_msgs().

purge_mapcaches_test() ->
    dummy_backend(),

    %%
    %% Start up 3 vnodes
    %%

    %% make sure we create the registered processes - no test hangovers
    cleanup_servers(),
    {ok, _Sup} = riak_core_vnode_sup:start_link(),
    {ok, _VMaster} = riak_core_vnode_master:start_link(riak_kv_vnode),
    application:load(riak_core),
    {ok, _RingEvents} = riak_core_ring_events:start_link(),
    {ok, _NodeEvent} = riak_core_node_watcher_events:start_link(),
    {ok, _NodeWatcher} = riak_core_node_watcher:start_link(),
    riak_core_node_watcher:service_up(riak_kv, self()),

    %% Get the first three partitions and start up vnodes
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    Partitions = lists:sublist([I || {I,_N} <- riak_core_ring:all_owners(Ring)], 3),
    [start_vnode(Index) || Index <- Partitions],
    Pids = [Pid || {_,Pid,_,_} <- supervisor:which_children(riak_core_vnode_sup)],
    ?assertEqual(length(Partitions), length(Pids)),

    %% Prove nothing there
    FunTerm = {modfun, ?MODULE, map_test},
    Arg = arg, 
    QTerm = {erlang, {map, FunTerm, Arg, acc}},
    KeyData = keydata,
    CacheKey = build_key(FunTerm, Arg, KeyData),
    BKey = {<<"b">>,<<"k">>},
    [check_mapcache(I, QTerm, BKey, KeyData, {error, notfound}) || I <- Partitions],

    %% Send them each something to cache
    [mapcache(Pid, BKey, CacheKey, some_val) || Pid <- Pids],

    %% Check it is there by issuing a map request
    flush_msgs(),
    [check_mapcache(I, QTerm, BKey, KeyData, some_val) || I <- Partitions],

    %% Purge all nodes
    purge_mapcaches(),

    %% Check it is gone
    [check_mapcache(I, QTerm, BKey, KeyData, {error, notfound}) || I <- Partitions],

    riak_core_node_watcher:service_down(riak_kv),
    cleanup_servers().
     
cleanup_servers() ->
    riak_kv_test_util:stop_process(riak_core_node_watcher),
    riak_kv_test_util:stop_process(riak_core_node_watcher_events),
    riak_kv_test_util:stop_process(riak_core_ring_events),
    riak_kv_test_util:stop_process(riak_core_vnode_sup),
    riak_kv_test_util:stop_process(riak_kv_vnode_master).
    

check_mapcache(Index, QTerm, BKey, KeyData, Expect) ->
    map({Index,node()}, self(), QTerm, BKey, KeyData),
    receive
        Msg ->
            {'$gen_event',{mapexec_reply,Result,_Pid}} = Msg,
            ?assertEqual(Expect, Result)
    after
        100 ->
            ?assert(false)
    end.
         
%% Map identity function - returns what you give it   
map_test(Obj, _KeyData, _Arg) ->
    Obj.

flush_msgs() ->                              
    receive
        _Msg ->
            flush_msgs()
    after
        0 ->
            ok
    end.
    
    

-endif. % TEST
