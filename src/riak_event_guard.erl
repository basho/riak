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

%% @doc A wrapper server for connecting riak_eventer handlers to the gen_event.

-module(riak_event_guard).

-export([start_link/0]).
-export([add_handler/3]).
-export([init/1, handle_info/2]).
-export([handle_call/3,handle_cast/2,code_change/3,terminate/2]).

-behavior(gen_server).

%% @spec start_link() -> {ok, pid()}
%% @doc The usual gen_server start_link mechanism.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @private
init([]) ->
    case gen_event:start_link({local,riak_event}) of
        {ok, Pid} -> {ok, Pid};
        {error,{already_started,Pid}} -> {ok, Pid};
        X -> {stop, {error_in_init, X}}
    end.

%% @spec add_handler(HandlerMod :: atom(), Arg :: term(), 
%%                   MatchSpec :: string()) ->
%%       ok | {error, Error :: term()}
%% @doc Attach a new HandlerMod to riak events, started with Arg.
add_handler(HandlerMod, Arg, MatchSpec) ->
    gen_server:call(?MODULE, {add_handler, HandlerMod, Arg, MatchSpec}).

%% @private
handle_call({add_handler, HandlerMod, Arg, MatchSpec},_From,State) -> 
    {ok, MyRing} = riak_ring_manager:get_my_ring(),
    Eventers0 = case riak_ring:get_meta(eventers, MyRing) of
        undefined -> [];
        {ok, X} -> sets:to_list(X)
    end,
    
    Eventers = sets:add_element({node(),MatchSpec}, sets:from_list(
                 [{N,MS} || {N,MS} <- Eventers0,
                       net_adm:ping(N) =:= pong,
                       N /= node()])),
    NewRing = riak_ring:update_meta(eventers, Eventers, MyRing),
    riak_ring_manager:set_my_ring(NewRing),
    riak_ring_manager:write_ringfile(),
    Reply = gen_event:swap_sup_handler(riak_event,
                                       {{HandlerMod,{node(),now()}}, swap}, 
                                       {{HandlerMod,{node(),now()}}, Arg}),
    riak_ring_gossiper:gossip_to(
      riak_ring:index_owner(NewRing,riak_ring:random_other_index(NewRing))),
    {reply, Reply, State}.

%% @private
handle_info({gen_event_EXIT, HandlerMod, Reason},State) ->
    %% gen_event manager sends this message if a handler was added using
    %% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
    riak_eventer:notify(riak_event_guard, gen_event_exit,{HandlerMod, Reason}),
    {noreply,State}.

%% @private
handle_cast(_,State) -> {noreply,State}.
%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%% @private
terminate(_Reason,_State)  -> ok.
