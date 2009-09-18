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


%% @doc
%% riak_eventer allows you to attach event handlers to a running Riak cluster to
%% receive event notifications (in the form of Erlang messages) about what is happening
%% in the application. 
%%
%% Events are generated using notify/1 or notify/3. Each event consists
%% of a Module, an EventName, the Node on which the event is generated,
%% and additional detail about the event.
%%
%% This is stored in a tuple of the form {event, {Module, EventName, Node, EventDetail}}.
%% For example, a 'put' operation will generate an event such as 
%% {event,{riak_vnode,put, 'node@hostname', ...}}.
%%
%% Event handlers are added via add_handler(Pid, Description, MatchHead, MatchGuard),
%% and can be removed via remove_handler(Pid, MatchHead, MatchGuard).
%%
%% Full MatchSpec style matching is allowed (see http://erlang.org/doc/apps/erts/match_spec.html)
%% to filter events at the server level, and the system fully supports registering 
%% a single process for multiple events.
%%
%% Riak monitors running handlers, and automatically removes 
%% handlers of dead processors.


-module(riak_eventer).
-behaviour(gen_server2).
-export([start_link/0,start_link/1,stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([notify/1, notify/3]).
-export ([add_handler/4]).
-export ([remove_handler/1, remove_handler/3]).
-export ([remove_dead_handlers/0]).

-define(REMOVE_INTERVAL, 5 * 1000).

-include_lib("eunit/include/eunit.hrl").

-record (handler, {
    id,         % The id of this handler. Made from a combination 
                % of pid, matchhead, and matchguard, allowing for
                % multiple handlers from the same pid.
    desc,       % Human readable description
    pid,        % Pid of the remote process
    matchhead,  % MatchHead applied against {Module, EventName, Node, EventDetail}
    matchguard  % MatchGuard, defaults to []
}).

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
    
add_handler(Pid, Desc, MatchHead, MatchSpec) ->
    gen_server:call(?MODULE, {add_handler, Pid, Desc, MatchHead, MatchSpec}).

remove_handler(Pid, MatchHead, MatchSpec) ->
    HandlerID = get_handler_id(Pid, MatchHead, MatchSpec),
    remove_handler(HandlerID).

remove_handler(HandlerID) ->
    gen_server:call(?MODULE, {remove_handler, HandlerID}).
    
remove_dead_handlers() ->
    gen_server:call(?MODULE, {remove_dead_handlers, false}).

%% @private (only used for test instances)
stop() -> gen_server2:cast(?MODULE, stop).

%% @private
handle_call({add_handler, Pid, Desc, MatchHead, MatchSpec},_From,State) -> 
    % Monitor the pid, we want to know when to remove it...
    erlang:monitor(process, Pid),

    % Add the handler...
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    Handler = make_handler(Pid, Desc, MatchHead, MatchSpec),
    Ring1 = add_handler_to_ring(Handler, Ring),
    
    % Set and save the new ring...
    riak_ring_manager:set_my_ring(Ring1),
    riak_ring_manager:write_ringfile(),
    
    % Gossip the new ring...
    RandomNode = riak_ring:index_owner(Ring1,riak_ring:random_other_index(Ring1)),
    riak_connect:send_ring(RandomNode),
    {reply, ok, State};
    
handle_call({remove_handler, HandlerID},_From,State) -> 
    % Remove the handler...
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    Ring1 = remove_handler_from_ring(HandlerID, Ring),
    
    % Set and save the new ring...
    riak_ring_manager:set_my_ring(Ring1),
    riak_ring_manager:write_ringfile(),
    
    % Gossip the new ring...
    RandomNode = riak_ring:index_owner(Ring1,riak_ring:random_other_index(Ring1)),
    riak_connect:send_ring(RandomNode),
    {reply, ok, State};
    
    
handle_call(_, _From, State) -> {reply, no_call_support, State}.

%% @private
handle_cast(stop, State) -> {stop,normal,State};

handle_cast({event, _Event}, test) -> {noreply,test};

handle_cast({event, Event}, State) ->
    % Get the handlers...
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    Handlers = get_handlers(Ring),
    MatchingHandlers = get_matching_handlers(Event, Handlers),
    
    % Send the message to all handlers...
    [begin
        Pid = X#handler.pid,
        Pid ! {event, Event}
    end || X <- MatchingHandlers],
    {noreply, State};
    
handle_cast(_, State) -> {noreply, State}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
    % Get a 'DOWN' message, so remove any handlers from this Pid...
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    OldHandlers = get_handlers(Ring),
    
    % Filter out any dead handlers...
    F = fun(Handler) -> Handler#handler.pid /= Pid end,
    NewHandlers = lists:filter(F, OldHandlers),
    
    % Write and gossip the ring if it has changed...
    RingHasChanged = OldHandlers /= NewHandlers,
    case RingHasChanged of
        true ->
            % Set and save the new ring...
            Ring1 = set_handlers(NewHandlers, Ring),
            riak_ring_manager:set_my_ring(Ring1),
            riak_ring_manager:write_ringfile(),
    
            % Gossip the new ring...
            RandomNode = riak_ring:index_owner(Ring1,riak_ring:random_other_index(Ring1)),
            riak_connect:send_ring(RandomNode);
        false -> ignore
    end,
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% make_handler/4 -
%% Create an handler record from the supplied params.
make_handler(Pid, Desc, MatchHead, MatchGuard) ->
    ID = get_handler_id(Pid, MatchHead, MatchGuard),
    #handler {
        id = ID,
        pid = Pid,
        desc = Desc,
        matchhead = MatchHead,
        matchguard = MatchGuard
    }.
    
%% add_handler_to_ring/5 -
%% Given an handler and a ring, add the handler to
%% the ring.
add_handler_to_ring(Handler, Ring) ->
    Handlers = get_handlers(Ring),
    Handlers1 = lists:keystore(Handler#handler.id, 2, Handlers, Handler),
    _Ring1 = set_handlers(Handlers1, Ring).

%% remove_handler_from_ring/4 -
%% Given part of an handler definition and a Ring, remove
%% the matching handler from the ring.
remove_handler_from_ring(Pid, MatchHead, MatchGuard, Ring) -> 
    HandlerID = get_handler_id(Pid, MatchHead, MatchGuard),
    remove_handler_from_ring(HandlerID, Ring).

%% remove_handler_from_ring/2 -
%% Given an HandlerID and a Ring, remove
%% the matching handler from the ring.
remove_handler_from_ring(HandlerID, Ring) -> 
    % Remove the handler from the ring...
    Handlers = get_handlers(Ring),
    Handlers1 = lists:keydelete(HandlerID, 2, Handlers),
    _Ring1 = set_handlers(Handlers1, Ring).
  
%% get_matching_handlers/2 -
%% Given an event and a list of #handlers, look 
%% through the handlers for all handlers that 
%% should receive the event based on their matchspec.
get_matching_handlers(Event, Handlers) ->
    F = fun(H = #handler { matchhead=MatchHead, matchguard=MatchGuard }, Matches) ->
        % NOTE: Compiled match_specs cannot be transfered across nodes,
        % so we have to recompile each time. Don't worry, it's fast.
        MS = ets:match_spec_compile([{MatchHead, MatchGuard, ['$$']}]),
        case ets:match_spec_run([Event], MS) of
            [_] -> [H|Matches];
            _ -> Matches
        end
    end,
    lists:foldl(F, [], Handlers).
    
%% Return the handlers in a ring...        
get_handlers(Ring) ->
    case riak_ring:get_meta(handlers, Ring) of
        undefined -> [];
        {ok, X} -> X
    end.
    
%% Update a ring with a new set of handlers...
set_handlers(Handlers, Ring) ->
    riak_ring:update_meta(handlers, Handlers, Ring).
    
get_handler_id(Pid, MatchHead, MatchGuard) ->
    erlang:md5(term_to_binary({Pid, MatchHead, MatchGuard})).
    
%% TESTS %%%
    
add_handler_to_ring_test() ->
    application:set_env(riak, ring_creation_size, 16),
    
    % The bare ring...
    Ring = riak_ring:fresh(),
    [] = get_handlers(Ring),
    
    % Add an handler...
    Handler1 = make_handler(self(), "Test description", {'_', '_', '_', '_'}, []),
    Ring1 = add_handler_to_ring(Handler1, Ring),
    [Handler1] = get_handlers(Ring1),
    
    % Add another handler...
    Handler2 = make_handler(self(), "Test description 1", {riak_vnode, '_', '_', '_'}, []),
    Ring2 = add_handler_to_ring(Handler2, Ring1),
    ?assert(lists:sort([Handler1, Handler2]) == lists:sort(get_handlers(Ring2))),
    
    % Remove Handler2, only Handler1 should be left...
    Ring3 = remove_handler_from_ring(Handler2#handler.pid, Handler2#handler.matchhead, Handler2#handler.matchguard, Ring2),
    [Handler1] = get_handlers(Ring3),
    
    % Remove Handler1, no handlers should be left...
    Ring4 = remove_handler_from_ring(Handler1#handler.id, Ring3),
    [] = get_handlers(Ring4),
    ok.

get_matching_handlers_test() ->
    Handlers = [
        make_handler(self(), "All 1", '_', []),
        make_handler(self(), "All 2", {'_', '_', '_', '_'}, []),
        make_handler(self(), "Only riak_vnode 1", {riak_vnode, '_', '_', '_'}, []),
        make_handler(self(), "Only riak_vnode 2", {'$1', '_', '_', '_'}, [{'==', '$1', riak_vnode}]),
        make_handler(self(), "Only riak_vnode delete", {riak_vnode, delete, '_', '_'}, []),
        make_handler(self(), "Only riak_vnode put, get, or delete", {'$1', '$2', '_', '_'}, [
            {'andalso', {'==', '$1', riak_vnode}, {'orelse', {'==', '$2', get}, {'==', '$2', put}, {'==', '$2', delete}}}
        ])
    ],
    ?assert(length(get_matching_handlers({test, ignored, ignored, ignored}, Handlers)) == 2),    
    ?assert(length(get_matching_handlers({riak_vnode, ignored, ignored, ignored}, Handlers)) == 4),    
    ?assert(length(get_matching_handlers({riak_vnode, delete, ignored, ignored}, Handlers)) == 6).
    
    