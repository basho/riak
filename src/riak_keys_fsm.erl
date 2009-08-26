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

-module(riak_keys_fsm).
-behaviour(gen_fsm).

-export([start/4]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
-export([initialize/2,waiting_kl/2]).

-record(state, {client :: {pid(), reference()},
                keys :: [set()],
                waiting :: [node()],
                bucket :: riak_object:bucket(),
                timeout :: pos_integer(),
                endtime :: pos_integer(),
                req_id :: pos_integer(),
                ring :: riak_ring:riak_ring()
               }).

start(Ring,Bucket,Timeout,From) ->
    gen_fsm:start(?MODULE, [Ring,Bucket,Timeout,From], []).

%% @private
init([Ring,Bucket,Timeout,Client]) ->
    StateData = #state{client=Client, timeout=Timeout,
                       bucket=Bucket, ring=Ring},
    {ok,initialize,StateData,0}.

%% @private
initialize(timeout, StateData0=#state{timeout=Timeout,
                                      bucket=Bucket, ring=Ring}) ->
    RealStartTime = riak_util:moment(),
    ReqID = erlang:phash2({random:uniform(), self(), Bucket, RealStartTime}),
    riak_eventer:notify(riak_keys_fsm, keys_fsm_start,
                        {ReqID, RealStartTime, Bucket}),
    Msg = {self(), Bucket, ReqID},
    NodeList = riak_ring:all_owners(Ring),
    Asked = lists:foldl(
              fun({Index, Node}, Acc) ->
                      case net_adm:ping(Node) of
                          pang -> Acc;
                          pong ->
                              gen_server:cast(
                                {riak_vnode_master, Node},
                                {vnode_list_bucket,{Index,ReqID},Msg}),
                              [Index|Acc]
                      end
              end,
              [],
              NodeList),
    StateData = StateData0#state{waiting=Asked, keys=[],
                       endtime=Timeout+riak_util:moment(),
                       req_id=ReqID},
    {next_state, waiting_kl, StateData, Timeout}.

waiting_kl({kl, Keys, Idx, ReqID},
           StateData=#state{keys=Acc,waiting=Waiting,endtime=End}) ->
    NewAcc = [sets:from_list(Keys)|Acc],
    case lists:delete(Idx, Waiting) of
        [] ->
            riak_eventer:notify(riak_keys_fsm, finish, {ReqID, normal}),
            respond(StateData#state.client,NewAcc),
            {stop, normal, StateData};
        StillWaiting ->
            {next_state, waiting_kl,
             StateData#state{keys=NewAcc,
                             waiting=StillWaiting},
             End-riak_util:moment()}
    end;
waiting_kl(timeout, StateData=#state{keys=Acc,client=Client,req_id=ReqID}) ->
    riak_eventer:notify(riak_keys_fsm, finish, {ReqID, timeout}),
    respond(Client, Acc),
    {stop, normal, StateData}.

%% @private
respond(Client, KeyLists) ->
    Reply = sets:to_list(sets:union(KeyLists)),
    gen_server2:reply(Client, {ok, Reply}),
    Reply.

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
terminate(Reason, _StateName, _State=#state{req_id=ReqID}) ->
    riak_eventer:notify(riak_keys_fsm, key_fsm_end,
                        {ReqID, Reason}),
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
