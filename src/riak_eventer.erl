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

-module(riak_eventer).
-behaviour(gen_server2).
-export([start_link/0,start_link/1,stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([notify/1,notify/3,eventer_config/1,do_eventer/1]).

-include_lib("eunit/include/eunit.hrl").

%% @private
start_link() -> gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(test) -> % when started this way, run a mock server (nop)
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [test], []).

%% @private
init([]) -> {ok, stateless_server};
init([test]) -> {ok, test}.

notify(Event) ->
    gen_server2:cast(riak_local_logger, {event, Event}),
    gen_server2:cast(?MODULE, {event, Event}).

notify(Module, EventName, EventDetail) ->
    notify({Module, EventName, node(), EventDetail}).

%% @private (only used for test instances)
stop() -> gen_server2:cast(?MODULE, stop).

%% @private
handle_cast(stop, State) -> {stop,normal,State};

handle_cast({event, _Event}, test) -> {noreply,test};
handle_cast({event, Event}, State) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),    %%%% TEST EVENTS!
    Eventers = match_eventers(get_eventers(Ring), Event, []),
    [gen_event:notify({riak_event,Node},Event) || Node <- Eventers],
    {noreply, State}.
  

eventer_config([Cluster, CookieStr]) ->
    RipConf = [{no_config, true}, {cluster_name, Cluster},
       {riak_cookie, list_to_atom(CookieStr)}, {ring_state_dir, "<nostore>"},
       {ring_creation_size, 12}, {gossip_interval,1000000},
       {wants_claim_fun, {riak_claim, never_wants_claim}},
       {riak_web_ip, "undefined"},
       {doorbell_port, 7000 + random:uniform(1000)},
       {storage_backend, undefined}],
    application:stop(sasl),
    application:unload(sasl),
    ok = application:load({application,sasl,[{errlog_type,error}]}),
    ok = application:start(sasl),
    [application:set_env(riak,K,V) || {K,V} <- RipConf].

parse_matchspec(MatchSpec) when is_list(MatchSpec) ->
    [NM,MM,TM] = string:tokens(MatchSpec, ":"),
    {list_to_atom(NM),list_to_atom(MM),list_to_atom(TM)}.

do_eventer([IP, PortStr, HandlerName, HandlerArg]) ->
    do_eventer([IP, PortStr, HandlerName, HandlerArg, "_:_:_"]);
do_eventer([IP, PortStr, HandlerName, HandlerArg, MatchSpec]) ->
    MS = parse_matchspec(MatchSpec),
    riak_startup:join_cluster([IP, PortStr]),
    timer:sleep(random:uniform(1000)), % let some gossip happen
    riak_event_guard:add_handler(list_to_atom(HandlerName),HandlerArg, MS),
    ok.

get_eventers(Ring) ->
    case riak_ring:get_meta(eventers, Ring) of
        undefined -> [];
        {ok, X} -> sets:to_list(X)
    end.        

match_eventers([], _, Acc) ->
    Acc;
match_eventers([{Eventer,MS}|Rest], Event, Acc) ->
    case match_event(MS,Event) of
        true ->
            match_eventers(Rest, Event, [Eventer|Acc]);
        false ->
            match_eventers(Rest, Event, Acc)
    end.

% Match an event to the event filter.
% The idea is that the filter values (in the first tuple)
% must either be a wildcard ('_'), or must match 
% the value within the event tuple.
% Return true if the filter matches the event.
match_event({Node1, Module1, Type1}, {Module2, Type2, Node2, _}) when
    (Node1 == '_' orelse Node1 == Node2) andalso
    (Module1 == '_' orelse Module1 == Module2) andalso
    (Type1 == '_' orelse Type1 == Type2) -> true;
match_event(_, _) -> false.
    

handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% @private
handle_call(_, _From, State) -> {reply, no_call_support, State}.

match_eventer_test() ->
    Event1 = {some_mod, some_type, some_node, {some_detail}},
    Event2 = {some_mod, some_type, other_node, {some_detail}},
    Event3 = {some_mod, other_type, other_node, {some_detail}},
    Event4 = {other_mod, other_type, other_node, {some_detail}},
    MS1 = {'_', '_', '_'},
    MS2 = {some_node, '_', '_'},
    MS3 = {some_node, some_mod, '_'},
    MS4 = {some_node, some_mod, some_type},
    ?assertEqual(match_event(MS1, Event1), true),
    ?assertEqual(match_event(MS2, Event1), true),
    ?assertEqual(match_event(MS2, Event2), false),
    ?assertEqual(match_event(MS3, Event1), true),
    ?assertEqual(match_event(MS3, Event3), false),
    ?assertEqual(match_event(MS4, Event1), true),
    ?assertEqual(match_event(MS4, Event4), false).
