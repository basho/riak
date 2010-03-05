%% -------------------------------------------------------------------
%%
%% riak_keys_fsm: listing of bucket keys
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

%% @doc listing of bucket keys

-module(riak_keys_fsm).
-behaviour(gen_fsm).

-export([start/6]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([initialize/2,waiting_kl/2]).

-record(state, {client :: {pid(), reference()},
                client_type :: atom(),
                bloom :: term(),
                vnodes_left :: [node()],
                bucket :: riak_object:bucket(),
                timeout :: pos_integer(),
                req_id :: pos_integer(),
                ring :: riak_ring:riak_ring()
               }).

start(ReqId,Bucket,Timeout,ClientType,ErrorTolerance,From) ->
    gen_fsm:start(?MODULE,
                  [ReqId,Bucket,Timeout,ClientType,ErrorTolerance,From], []).

%% @private
init([ReqId,Bucket,Timeout,ClientType,ErrorTolerance,Client]) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    Bloom = bloom:sbf(10000000,ErrorTolerance),
    StateData = #state{client=Client, client_type=ClientType, timeout=Timeout,
                       bloom=Bloom, req_id=ReqId, bucket=Bucket, ring=Ring},
    {ok,initialize,StateData,0}.

ask_vn({Index,Node},ReqId,Msg) ->
    gen_server:cast({riak_vnode_master, Node},
                    {vnode_list_bucket,{Index,ReqId},Msg}).

%% @private
initialize(timeout, StateData0=#state{timeout=Timeout, req_id=ReqId,
                                      bucket=Bucket, ring=Ring}) ->
    [FirstVN|VNodes] = riak_ring:all_owners(Ring),
    ask_vn(FirstVN,ReqId,{self(), Bucket, ReqId}),
    StateData = StateData0#state{vnodes_left=VNodes},
    {next_state, waiting_kl, StateData, Timeout}.

waiting_kl({kl, Keys, _Idx, ReqId},
           StateData=#state{vnodes_left=[],bucket=Bucket,
            bloom=Bloom,req_id=ReqId,client=Client,client_type=ClientType}) ->
    process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client),
    case ClientType of
        mapred -> luke_flow:finish_inputs(Client);
        plain -> Client ! {ReqId, done}
    end,
    {stop,normal,StateData};
waiting_kl({kl, Keys, _Idx, ReqId},
           StateData=#state{vnodes_left=[FirstVN|VNodes],bloom=Bloom,
                   req_id=ReqId,client=Client,timeout=Timeout,
                            bucket=Bucket,client_type=ClientType}) ->
    ask_vn(FirstVN,ReqId,{self(), Bucket, ReqId}),
    {next_state, waiting_kl,
     StateData#state{
       bloom=process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client),
       vnodes_left=VNodes},
     Timeout}.

%% @private
process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client) ->
    process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client,[]).
%% @private
process_keys([],Bucket,ClientType,Bloom,ReqId,Client,Acc) ->
    case ClientType of
        mapred -> luke_flow:add_inputs(Client, [{Bucket,K} || K <- Acc]);
        plain -> Client ! {ReqId, {keys, Acc}}
    end,
    lists:foldl(fun(E,A) -> bloom:add(E,A) end, Bloom, Acc);
process_keys([K|Rest],Bucket,ClientType,Bloom,ReqId,Client,Acc) ->
    case bloom:member(K,Bloom) of
        true -> process_keys(Rest,Bucket,ClientType,Bloom,ReqId,Client,Acc);
        false -> process_keys(Rest,Bucket,ClientType,bloom:add(K,Bloom),ReqId,Client,[K|Acc])
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
    riak_eventer:notify(riak_keys_fsm, key_fsm_end,
                        {ReqId, Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
