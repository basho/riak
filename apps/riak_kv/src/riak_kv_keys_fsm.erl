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

-module(riak_kv_keys_fsm).
-behaviour(gen_fsm).

-export([start/6]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([initialize/2,waiting_kl/2]).

-record(state, {client :: {pid(), reference()},
                client_type :: atom(),
                bloom :: term(),
                pls :: [list()],
                wait_pls :: [term()],
                simul_pls :: integer(),
                vns :: term(),
                bucket :: riak_object:bucket(),
                timeout :: pos_integer(),
                req_id :: pos_integer(),
                ring :: riak_core_ring:riak_core_ring()
               }).

start(ReqId,Bucket,Timeout,ClientType,ErrorTolerance,From) ->
    gen_fsm:start(?MODULE,
                  [ReqId,Bucket,Timeout,ClientType,ErrorTolerance,From], []).

%% @private
init([ReqId,Bucket,Timeout,ClientType,ErrorTolerance,Client]) ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    {ok, Bloom} = ebloom:new(10000000,ErrorTolerance,ReqId),
    StateData = #state{client=Client, client_type=ClientType, timeout=Timeout,
                       bloom=Bloom, req_id=ReqId, bucket=Bucket, ring=Ring},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{bucket=Bucket, ring=Ring}) ->
    BucketProps = riak_core_bucket:get_bucket(Bucket, Ring),
    N = proplists:get_value(n_val,BucketProps),
    PLS0 = riak_core_ring:all_preflists(Ring,N),
    {LA1, LA2} = lists:partition(fun({A,_B}) -> A rem N == 0 end,
                              lists:zip(lists:seq(0,(length(PLS0)-1)), PLS0)),
    {_, PLS} = lists:unzip(lists:append(LA1,LA2)),
    Simul_PLS = trunc(length(PLS) / N),
    StateData = StateData0#state{pls=PLS,simul_pls=Simul_PLS,
                                 wait_pls=[],vns=sets:from_list([])},
    reduce_pls(StateData).

waiting_kl({kl, Keys, Idx, ReqId},
           StateData0=#state{pls=PLS,vns=VNS0,wait_pls=WPL0,bloom=Bloom,
                            req_id=ReqId,client=Client,timeout=Timeout,
                            bucket=Bucket,client_type=ClientType}) ->
    process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client),
    WPL = [{W_Idx,W_Node,W_PL} || {W_Idx,W_Node,W_PL} <- WPL0, W_Idx /= Idx],
    WNs = [W_Node || {W_Idx,W_Node,_W_PL} <- WPL0, W_Idx =:= Idx],
    Node = case WNs of
        [WN] -> WN;
        _ -> undefined
    end,
    VNS = sets:add_element({Idx,Node},VNS0),
    StateData = StateData0#state{wait_pls=WPL,vns=VNS},
    case PLS of
        [] ->
            case WPL of
                [] -> finish(StateData);
                _ -> {next_state, waiting_kl, StateData, Timeout}
            end;
        _ -> reduce_pls(StateData)
    end;

waiting_kl(timeout, StateData=#state{pls=PLS,wait_pls=WPL}) ->
    NewPLS = lists:append(PLS, [W_PL || {_W_Idx,_W_Node,W_PL} <- WPL]),
    reduce_pls(StateData#state{pls=NewPLS,wait_pls=[]}).

finish(StateData=#state{req_id=ReqId,client=Client,client_type=ClientType}) ->
    case ClientType of
        mapred -> luke_flow:finish_inputs(Client);
        plain -> Client ! {ReqId, done}
    end,
    {stop,normal,StateData}.
                                             
ask_vn({Index,Node},ReqId,Msg) ->
    gen_server:cast({riak_kv_vnode_master, Node},
                    {vnode_list_bucket,{Index,ReqId},Msg}).

reduce_pls(StateData0=#state{timeout=Timeout, req_id=ReqId,wait_pls=WPL,
                             simul_pls=Simul_PLS, bucket=Bucket}) ->
    case find_free_pl(StateData0) of
        {none_free,NewPLS} ->
            StateData = StateData0#state{pls=NewPLS},
            case NewPLS =:= [] andalso WPL =:= [] of
                true -> finish(StateData);
                false -> {next_state, waiting_kl, StateData, Timeout}
            end;
        {[{Idx,Node}|RestPL],PLS} ->
            case net_adm:ping(Node) of
                pong ->
                    ask_vn({Idx,Node},ReqId,{self(), Bucket, ReqId}),
                    WaitPLS = [{Idx,Node,RestPL}|WPL],
                    StateData = StateData0#state{pls=PLS, wait_pls=WaitPLS},
                    case length(WaitPLS) > Simul_PLS of
                        true ->
                            {next_state, waiting_kl, StateData, Timeout};
                        false ->
                            reduce_pls(StateData)
                    end;
                pang ->
                    reduce_pls(StateData0#state{pls=[RestPL|PLS]})
            end                        
    end.

find_free_pl(StateData) -> find_free_pl1(StateData, []).
find_free_pl1(_StateData=#state{pls=[]}, NotFree) -> {none_free,NotFree};
find_free_pl1(StateData=#state{wait_pls=WPL,pls=[PL|PLS],vns=VNS}, NotFree) ->
    case PL of
        [] -> find_free_pl1(StateData#state{pls=PLS}, NotFree);
        _ ->
            case check_pl(PL,VNS,WPL) of
                redundant -> find_free_pl1(StateData#state{pls=PLS},NotFree);
                notfree -> find_free_pl1(StateData#state{pls=PLS},[PL|NotFree]);
                free -> {PL,lists:append(PLS,NotFree)}
            end
    end.

check_pl(PL,VNS,WPL) ->
    case sets:is_disjoint(sets:from_list(PL),VNS) of
        false -> redundant;
        true ->
            PL_Nodes = sets:from_list([Node || {_Idx,Node} <- PL]),
            WaitNodes = sets:from_list([Node || {_Idx,Node,_RestPL} <- WPL]),
            case sets:is_disjoint(PL_Nodes,WaitNodes) of
                false -> notfree;
                true -> free
            end
    end.

%% @private
process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client) ->
    process_keys(Keys,Bucket,ClientType,Bloom,ReqId,Client,[]).
%% @private
process_keys([],Bucket,ClientType,_Bloom,ReqId,Client,Acc) ->
    case ClientType of
        mapred -> luke_flow:add_inputs(Client, [{Bucket,K} || K <- Acc]);
        plain -> Client ! {ReqId, {keys, Acc}}
    end,
    ok;
process_keys([K|Rest],Bucket,ClientType,Bloom,ReqId,Client,Acc) ->
    case ebloom:contains(Bloom,K) of
        true -> 
            process_keys(Rest,Bucket,ClientType,
                         Bloom,ReqId,Client,Acc);
        false ->
            ebloom:insert(Bloom,K),
            process_keys(Rest,Bucket,ClientType,
                         Bloom,ReqId,Client,[K|Acc])
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
terminate(Reason, _StateName, _State) ->
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
