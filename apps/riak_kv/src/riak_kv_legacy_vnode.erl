%% -------------------------------------------------------------------
%%
%% riak_kv_legacy_vnode: Message translation for legacy vnode requests
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

%% @doc  QuickCheck tests for riak_core_vnode code

-module(riak_kv_legacy_vnode).
-export([rewrite_cast/1,
         rewrite_call/2]).
-include_lib("riak_kv/include/riak_kv_vnode.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @private

rewrite_cast({vnode_map, {Partition,_Node},
             {ClientPid,QTerm,BKey,KeyData}}) ->
    Req = riak_core_vnode_master:make_request(
            ?KV_MAP_REQ{
               qterm = QTerm,
               bkey = BKey,
               keydata = KeyData},
            {fsm, undefined, ClientPid},
            Partition),
    {ok, Req};
rewrite_cast({vnode_put, {Partition,_Node},
              {FSM_pid,BKey,RObj,ReqID,FSMTime,Options}}) ->
    Req = riak_core_vnode_master:make_request(
            ?KV_PUT_REQ{
               bkey = BKey,
               object = RObj,
               req_id = ReqID,
               start_time = FSMTime,
               options = Options},
            {fsm, undefined, FSM_pid},
            Partition),
    {ok, Req};
rewrite_cast({vnode_get, {Partition,_Node},
              {FSM_pid,BKey,ReqId}}) ->
    Req = riak_core_vnode_master:make_request(
            ?KV_GET_REQ{
               bkey = BKey,
               req_id = ReqId
              },
            {fsm, undefined, FSM_pid},
            Partition),
    {ok, Req};
%% rewrite_cast({vnode_merkle, {RemoteVN,Partition,Merkle,ObjList}}, State) ->
%%     Pid = get_vnode(Partition, State),
%%     gen_fsm:send_event(Pid, {vnode_merkle, {RemoteVN,Merkle,ObjList}}),
%%     {noreply, State};
rewrite_cast({vnode_list_bucket, {Partition,_Node},
              {FSM_pid, Bucket, ReqID}}) ->
    Req = riak_core_vnode_master:make_request(
            ?KV_LISTKEYS_REQ{
               bucket=Bucket, 
               req_id=ReqID},
            {fsm, undefined, FSM_pid},
            Partition),
    {ok, Req}.
%% rewrite_cast({add_exclusion, Partition}, State=#state{excl=Excl}) ->
%%     {ok, Ring} = riak_core_ring_manager:get_my_ring(),
%%     riak_core_ring_events:ring_update(Ring),
%%     {noreply, State#state{excl=ordsets:add_element(Partition, Excl)}}.

%% @private
%% rewrite_call(all_possible_vnodes, _From, State) ->
%%     {reply, make_all_active(State), State};
%% rewrite_call(all_vnodes, _From, State) ->
%%     {reply, all_vnodes(State), State};
rewrite_call({vnode_del, {Partition,_Node},
              {BKey,ReqID}}, _From) ->
    Req = riak_core_vnode_master:make_request(
            ?KV_DELETE_REQ{bkey=BKey,
                           req_id=ReqID},
            noreply,
            Partition),
    {ok, Req}.
%% rewrite_call({get_merkle, Partition}, From, State) ->
%%     Pid = get_vnode(Partition, State),
%%     spawn(fun() -> gen_fsm:send_all_state_event(Pid, {get_merkle, From}) end),
%%     {noreply, State};
%% rewrite_call({get_vclocks,Partition,KeyList},From,State) ->
%%     Pid = get_vnode(Partition, State),
%%     spawn(fun() -> gen_fsm:send_all_state_event(
%%                      Pid,{get_vclocks,From,KeyList}) end),
%%     {noreply, State};
%% rewrite_call({fold, {Partition, Fun, Acc0}}, From, State) ->
%%     Pid = get_vnode(Partition, State),
%%     spawn(
%%       fun() -> gen_fsm:send_all_state_event(Pid, {fold, {Fun,Acc0,From}}) end),
%%     {noreply, State};
%% rewrite_call({get_vnode, Partition}, _From, State) ->
%%     {reply, {ok, get_vnode(Partition, State)}, State};
%% rewrite_call(get_exclusions, _From, State=#state{excl=Excl}) ->
%%     {reply, {ok, ordsets:to_list(Excl)}, State}.

-ifdef(TEST).

-define(RING_KEY, riak_ring).

start_servers() ->
    %% dbgh:start(),
    %% dbgh:trace(riak_core_vnode_master),
    %% dbgh:trace(riak_core_vnode),
    %% dbgh:trace(riak_kv_vnode),
    %% dbgh:trace(?MODULE),
    application:set_env(riak_kv, storage_backend, riak_kv_gb_trees_backend),
    application:set_env(riak_core, default_bucket_props, []),
    Ring = riak_core_ring:fresh(16, node()),
    mochiglobal:put(?RING_KEY, Ring),
    riak_kv_test_util:stop_process(riak_kv_vnode_master),
    riak_kv_test_util:stop_process(riak_core_vnode_sup),
    {ok, _Sup} = riak_core_vnode_sup:start_link(),
    {ok, _Vmaster} = riak_core_vnode_master:start_link(riak_kv_vnode, ?MODULE),
    ok.

stop_servers(_R) ->
    riak_kv_test_util:stop_process(riak_kv_vnode_master),
    riak_kv_test_util:stop_process(riak_core_vnode_sup).

legacy_kv_test_() ->
    {spawn,
     {setup, fun start_servers/0, fun stop_servers/1,
      [{"get", ?_test(
        begin
            send_0_11_0_cmd(vnode_get, {self(), {<<"bucket">>,<<"key">>}, 123}),
            receive
                Msg ->
                    ?assertEqual({'$gen_event',{r,{error,notfound},0,123}}, Msg)
            after
                100 ->
                    ?assert(false)
            end
        end)},
       {"put", ?_test(
        begin
            Bucket = <<"bucket">>,
            Key = <<"key">>,
            RObj1 = riak_object:new(Bucket, Key, <<"val">>),
            ReqId = 456,
            RealStartTime = {0,0,0},
            Options = [],
            send_0_11_0_cmd(vnode_put, 
                            {self(), {Bucket,Key}, RObj1, ReqId, RealStartTime, Options}),
            receive
                Msg ->
                    ?assertEqual({'$gen_event',{w,0,456}}, Msg),
                    receive
                        Msg2 ->
                            ?assertEqual({'$gen_event',{dw,0,456}}, Msg2)
                    after
                        100 ->
                            ?assert(false)
                    end
            after
                100 ->
                    ?assert(false)
            end
        end)},
       {"list bucket", ?_test(
        begin
            Bucket = <<"listbucket">>,
            ReqID = 789,
            send_0_11_0_cmd(vnode_list_bucket,{self(), Bucket, ReqID}),
            receive
                Msg ->
                    ?assertEqual({'$gen_event',{kl,[],0,789}}, Msg)
            after
                100 ->
                    ?assert(false)
            end
        end)},
       {"map", ?_test(
        begin
            FunTerm = {qfun, fun(Obj, _KeyData, _Arg) -> Obj end},
            Arg = arg, 
            QTerm = {erlang, {map, FunTerm, Arg, acc}},
            KeyData = keydata,

            BKey = {<<"bucket">>,<<"notakey">>},
            send_0_11_0_cmd(vnode_map, {self(),QTerm,BKey,KeyData}),
            receive
                Msg ->
                    {'$gen_event',{mapexec_reply, Result,_Pid}} = Msg,
                    ?assertEqual({error, notfound}, Result)
            after
                100 ->
                    ?assert(false)
            end
        end)}]
     }}.

send_0_11_0_cmd(Cmd, Msg) ->
    gen_server:cast({riak_kv_vnode_master, node()},
                    {Cmd, {0, node()}, Msg}).
                                  

-endif.
      
    
