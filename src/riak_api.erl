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

%% @doc The gen_server entry point for riak client objects.
%%      See riak_client for usage.

-module(riak_api).

-behaviour(gen_server2).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {ring}).

%% @private
start_link() -> gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
handle_call({reload_all, Mod}, _From, State) ->
    {reply, riak_util:reload_all(Mod), State};
handle_call({remove_from_cluster,ExitingNode}, _From, State) ->
    {reply, riak_ring_gossiper:remove_from_cluster(ExitingNode), State};
handle_call({mapred,Inputs,Query,Timeout}, From, State) ->
    NewState = ensure_ring(State),
    riak_mapreduce_fsm:start(NewState#state.ring,Inputs,Query,Timeout,From),
    {noreply, NewState};
handle_call({put,RObj,W,DW,Timeout}, From, State) ->
    NewState = ensure_ring(State),
    riak_put_fsm:start(NewState#state.ring, RObj,W,DW,Timeout,From),
    {noreply, NewState};
handle_call({get,Bucket,Key,R,Timeout}, From, State) ->
    NewState = ensure_ring(State),
    riak_get_fsm:start(NewState#state.ring, Bucket,Key,R,Timeout,From),
    {noreply, NewState};
handle_call({delete,Bucket,Key,RW,Timeout}, From, State) ->
    spawn(fun() -> riak_delete:delete(Bucket,Key,RW,Timeout,From) end),
    {noreply, State};
handle_call({set_bucket,BucketName,BucketProps}, From, State) ->
    spawn(fun() ->
          gen_server2:reply(From,
                           riak_bucket:set_bucket(BucketName, BucketProps))
          end),
    {noreply, State};
handle_call({get_bucket,BucketName}, From, State) ->
    spawn(fun() ->
          gen_server2:reply(From,
                           riak_bucket:get_bucket(BucketName))
          end),
    {noreply, State};
handle_call({list_keys,Bucket}, From, State) ->
    spawn(fun() ->
        gen_server2:reply(From, riak_bucketkeys:get_keys(Bucket))
          end),
    {noreply, State}.

%% @private
init([]) -> {ok, #state{ring=undefined}}.

%% @private
handle_cast({send_event, ClientId, EventName, EventDetail}, State) ->
    riak_eventer:notify(client_event, EventName, {ClientId, EventDetail}),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply,State}.

%% @private
handle_info({set_ring, Ring}, State) ->
    {noreply, State#state{ring=Ring}};

handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% @private
ensure_ring(State=#state{ring=undefined}) ->
    riak_ring_manager:subscribe(self()),
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    State#state{ring=Ring};
ensure_ring(State) -> State.
    
