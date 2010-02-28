%% -------------------------------------------------------------------
%%
%% riak_cache_backend: buckets as in-memory caches
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

%% @doc riak_cache_backend is a backend that turns a bucket into a 
%% memcached-type memory cache. Ejects the least recently used
%% objects either when cache gets full or the object's lease
%% expires.
%%
%% 
%% === Config Settings ===
%% 
%% * 'riak_cache_backend_memory' - Specifies the amount of maximum 
%%   amount of memory allocated to cache, in MB. 
%% * 'riak_cache_backend_ttl' - When an object is accessed, renew the
%%   lease for this many seconds.
%% * 'riak_cache_backend_max_ttl' - Don't allow the object's lease to 
%%   be renewed after this many seconds.

-module (riak_cache_backend).
-export([start/2, stop/1, get/2, put/3, list/1, list_bucket/2, delete/2]).
-export([drop/1, is_empty/1, fold/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).


-include_lib("eunit/include/eunit.hrl").

-define (DEFAULT_TTL,     timer:minutes(10)).
-define (DEFAULT_MAX_TTL, timer:hours(1)).
-define (DEFAULT_MEMORY,  100).  % 100MB

% obj_tree two levels of gb_tree
%   - first level is buckets
%   - second level is keys
-record (state, {ttl, max_ttl, max_memory, used_memory, obj_tree, time_tree}).
-record (entry, {bucket, key, value, tref, created, now}).

%%% RIAK BACKEND INTERFACE %%%

% @spec start(Partition :: integer(), Config :: proplist()) ->
%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(_Partition, Config) ->
    gen_server:start_link(?MODULE, [Config], []).

get(State, BKey) -> 
    gen_server:call(State, {get, BKey}).
    
put(State, BKey, Val) -> 
    gen_server:call(State, {put, BKey, Val}).
    
list(State) -> 
    gen_server:call(State, list).

list_bucket(State, Bucket) ->
    gen_server:call(State, {list_bucket, Bucket}).

delete(State, BKey) -> 
    gen_server:call(State, {delete, BKey}).

drop(State) ->
    gen_server:call(State, drop).

is_empty(State) ->
    gen_server:call(State, is_empty).

fold(State, Fun0, Acc) ->
    gen_server:call(State, {fold, Fun0, Acc}).

stop(State) -> 
    gen_server:call(State, stop).


%%% GEN_SERVER %%%

%% @private
init([Config]) ->
    MaxTTL = proplists:get_value(riak_cache_backend_max_ttl, Config, ?DEFAULT_MAX_TTL),
    TTL = proplists:get_value(riak_cache_backend_ttl, Config, ?DEFAULT_TTL),
    Memory = proplists:get_value(riak_cache_backend_memory, Config, ?DEFAULT_MEMORY),
    {ok, #state { 
        ttl=TTL, max_ttl=MaxTTL, max_memory=Memory * 1024 * 1024, used_memory=0,
        obj_tree=gb_trees:empty(), time_tree=gb_trees:empty()
    }}.


%% @private
handle_call({get, {Bucket, Key}}, _From, State) -> 
    % Read using the BKey...
    case obj_find(Bucket, Key, State) of
        {value, Entry} ->
            % If we have exceeded the max_ttl for this object, then 
            % delete the object and return {error, notfound}.
            % Otherwise, renew the lease and return the object.
            case exceeds_max_ttl(Entry, State#state.max_ttl) of
                true -> 
                    {_, ok, NewState} = handle_call({delete, {Bucket, Key}}, _From, State),
                    {reply, {error, notfound}, NewState};
                false ->
                    NewEntry = extend_lease(Entry, State#state.ttl),
                    NewState = obj_store(NewEntry, State),
                    {reply, {ok, NewEntry#entry.value}, NewState}
            end;
        none ->
            {reply, {error, notfound}, State}
    end;
    
handle_call({put, {Bucket, Key}, Value}, _From, State) ->
    % Delete the old entry if it exists...
    {_, ok, State1} = handle_call({delete, {Bucket, Key}}, _From, State),
    
    % Make an entry in obj_tree and time_tree
    % Create a timer ref to remove.
    Now = {now(), make_ref()},
    Entry = #entry { bucket=Bucket, key=Key, value=Value, created=now(), now=Now },
    NewEntry = extend_lease(Entry, State#state.ttl),
    State2 = obj_store(NewEntry, State1),
    State3 = time_store(NewEntry, State2),
    
    % Update space used...
    Size = size(Bucket) + size(Key) + size(Value),
    UsedMemory = State3#state.used_memory + Size,
    State4 = State3#state { used_memory=UsedMemory },
    
    % Get us under the max_memory setting...
    State5 = trim_while_too_big(State4),
    
    {reply, ok, State5};
    
handle_call({delete, {Bucket, Key}}, _From, State) -> 
    % Remove the object from th obj_tree and time_tree...
    case obj_find(Bucket, Key, State) of
        {value, Entry} ->
            % Remove the timer ref...
            cancel_timer(Entry),

            % Remove entries from obj_tree and time_tree...
            State1 = obj_delete(Entry, State),
            State2 = time_delete(Entry, State1),
            
            % Update space used...
            Size = size(Bucket) + size(Key) + size(Entry#entry.value),
            UsedMemory = State2#state.used_memory - Size,
            State3 = State2#state { used_memory=UsedMemory },
            {reply, ok, State3};
        none ->
            {reply, ok, State}
    end;
    
handle_call({eject_from_cache, {Bucket, Key}, UniqueRef}, _From, State) ->
    % Check if the object exists.
    case obj_find(Bucket, Key, State) of
        {value, Entry} ->
            % Check if the Uniquerefs match.
            case Entry#entry.tref of
                {_, UniqueRef} -> 
                    % Delete the object.
                    handle_call({delete, {Bucket, Key}}, self(), State);
                _ ->
                    % Refs don't match, so ignore.
                    {reply, ok, State}
            end;
        none ->
            % Missing object, so ignore.
            {reply, ok, State}
    end;
    
handle_call(list, _From, State) ->
    % Fold through the gb_trees, gathering keys...
    F = fun(Bucket, Tree, Acc) ->
        %io:format("Bucket: ~p~n", [Bucket]),
        %io:format("Tree: ~p~n", [Tree]),
        %io:format("Acc: ~p~n", [Acc]),

        Keys = [{Bucket, Key} || Key <- gb_trees:keys(Tree)],
        Acc ++ Keys        
    end,
    AllKeys = gb_trees_fold(F, [], State#state.obj_tree),
    {reply, AllKeys, State};
    
handle_call({list_bucket, '_'}, _From, State) ->
    % List all buckets...
    Buckets = gb_trees:keys(State#state.obj_tree),
    {reply, Buckets, State};
    
handle_call({list_bucket, {filter, Bucket, Fun}}, _From, State) ->
    case gb_trees:lookup(Bucket, State#state.obj_tree) of
        {value, Tree} ->
            Keys = gb_trees:keys(Tree),
            FilteredKeys = lists:filter(Fun, Keys),
            {reply, FilteredKeys, State};
            
        none -> 
            {reply, [], State}
    end;
    
handle_call({list_bucket, Bucket}, _From, State) ->
    case gb_trees:lookup(Bucket, State#state.obj_tree) of
        {value, Tree} ->
            Keys = gb_trees:keys(Tree),
            {reply, Keys, State};
            
        none -> 
            {reply, [], State}
    end;
handle_call(drop, _From, State=#state{time_tree=TT}) ->
    [timer:cancel(E#entry.tref) || {_, E} <- gb_trees:to_list(TT)],
    {reply, ok, State#state{obj_tree=gb_trees:empty(), 
                            time_tree=gb_trees:empty(),
                            used_memory=0}};
handle_call(is_empty, _From, State) ->
    {reply, AllKeys, State} = handle_call(list, _From, State),
    case AllKeys of
        [] ->
            {reply, true, State};
        _ ->
            {reply, false, State}
    end;
handle_call({fold, Fun0, Acc}, _From, State) ->
    {reply, Keys, _} = handle_call(list, _From, State),
    Reply = do_fold(State, Keys, Fun0, Acc),
    {reply, Reply, State};
handle_call(stop, _From, State) -> 
    {reply, ok, State}.

%% @private
handle_cast(_, State) -> {noreply, State}.

%% @private
handle_info(_Msg, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private Functions
%%

do_fold(State=#state{obj_tree=_OT}, Keys, Fun0, Acc) ->
    Objs = [element(2, obj_find(B, K, State)) || {B,K} <- Keys],
    Fun = fun(E, AccIn) ->
                  Fun0({E#entry.bucket, E#entry.key}, E#entry.value, AccIn)
          end,
    lists:foldl(Fun, Acc, Objs).
    
% Lookup the #entry record specified by Bucket/Key.    
% Returns {value, Entry} or none.
obj_find(Bucket, Key, State) -> 
    Level1 = State#state.obj_tree,
    case gb_trees:lookup(Bucket, Level1) of
        {value, Level2} -> gb_trees:lookup(Key, Level2);
        none -> none
    end.

% Stores the #entry record in the obj_tree.    
% Returns NewState.
obj_store(Entry, State) -> 
    Bucket = Entry#entry.bucket,
    Key = Entry#entry.key,
    Level1 = State#state.obj_tree,
    Level2 = case gb_trees:lookup(Bucket, Level1) of
        {value, X} -> X;
        none -> gb_trees:empty()
    end,
    
    NewLevel2 = gb_trees:enter(Key, Entry, Level2),
    NewLevel1 = gb_trees:enter(Bucket, NewLevel2, Level1),
    State#state { obj_tree=NewLevel1 }.
    
% Remove the #entry record in the obj_tree.
% Returns NewState.
obj_delete(Entry, State) ->
    Bucket = Entry#entry.bucket,
    Key = Entry#entry.key,
    Level1 = State#state.obj_tree,
    {value, Level2} = gb_trees:lookup(Bucket, Level1),
    NewLevel2 = gb_trees:delete(Key, Level2),
    NewLevel1 = gb_trees:update(Bucket, NewLevel2, Level1),
    State#state { obj_tree=NewLevel1 }.
    
    
% Get the oldest object in the cache. Assumes that 
% the cache has at least one entry.
% Returns Entry.
time_oldest(State) ->
    {_, Entry} = gb_trees:smallest(State#state.time_tree),
    Entry.
    
% Store the #entry record in the time_tree.
% Assumes the entry does not yet exist.
% Returns NewState.
time_store(Entry, State) ->
    Now = Entry#entry.now,
    TimeTree = State#state.time_tree,
    NewTimeTree = gb_trees:insert(Now, Entry, TimeTree),
    State#state { time_tree = NewTimeTree }.
    
% Remove the #entry record in the time_tree.
% Assumes the entry exists.
% Returns NewState.
time_delete(Entry, State) ->
    Now = Entry#entry.now,
    TimeTree = State#state.time_tree,
    NewTimeTree = gb_trees:delete(Now, TimeTree),
    State#state { time_tree = NewTimeTree }.

% Check if this object is past the max_ttl setting.
% Returns 'true' or 'false'.
exceeds_max_ttl(Entry, MaxTTL) ->
    Created = Entry#entry.created,
    Diff = (timer:now_diff(now(), Created) / 1000 / 1000),
    Diff > MaxTTL.


% Extend the lease of an object in cache. Remove
% the old timer, create a new timer, and update the
% object.
% Returns the new #entry record.
extend_lease(Entry, TTL) ->
    % Cancel the old timer...
    cancel_timer(Entry),

    % Create the new timer...
    MS = trunc(timer:seconds(TTL)),
    UniqueRef = make_ref(),
    Bucket = Entry#entry.bucket,
    Key = Entry#entry.key,
    Request = {eject_from_cache, {Bucket, Key}, UniqueRef},
    {ok, TRef} = timer:apply_after(MS, gen_server, call, [self(), Request]),
    
    % Update the entry...
    Entry#entry { tref={TRef, UniqueRef} }.
    
% Cancel the timeout of an event in cache.
% Called before removing.
cancel_timer(Entry) ->
    case Entry#entry.tref of
        {OldTRef, _} -> timer:cancel(OldTRef);
        undefined    -> ok
    end.

% If we are above our max memory, then trim out 
% the oldest objects...
trim_while_too_big(State) when 
    State#state.used_memory =< State#state.max_memory ->
    State;
trim_while_too_big(State) ->
    % Delete the oldest object...
    Entry = time_oldest(State),
    Bucket = Entry#entry.bucket,
    Key = Entry#entry.key,
    {_, ok, NewState} = handle_call({delete, {Bucket, Key}}, self(), State),

    % Loop...
    trim_while_too_big(NewState).

gb_trees_fold(Fun, Acc, Tree) ->
    Iterator = gb_trees:iterator(Tree),
    gb_trees_fold_inner(Fun, Acc, gb_trees:next(Iterator)).

gb_trees_fold_inner(_, Acc, none) -> Acc;
gb_trees_fold_inner(Fun, Acc, {Key, Val, Iterator}) ->
    NewAcc = Fun(Key, Val, Acc),
    gb_trees_fold_inner(Fun, NewAcc, gb_trees:next(Iterator)).


%%
%% Test
%%

% @private
simple_test() ->
    riak_test_util:standard_backend_test(riak_cache_backend, []).
    
% @private
ttl_test() ->
    % Set TTL to 0.02 seconds...
    Config = [{riak_cache_backend_ttl, 0.02}],
    {ok, State} = start(42, Config),

    Bucket = <<"Bucket">>, 
    Key = <<"Key">>,
    Value = <<"Value">>,

    % Put an object...
    put(State, {Bucket, Key}, Value),
    
    % Get it immediately...
    {ok, Value} = get(State, {Bucket, Key}),
    {ok, Value} = get(State, {Bucket, Key}),
    {ok, Value} = get(State, {Bucket, Key}),
    
    % Wait 0.05 seconds, object should be cleared from cache...
    timer:sleep(trunc(timer:seconds(0.05))),
    
    % Get the object again, it should be missing...
    {error, notfound} = get(State, {Bucket, Key}),
    
    ok.
    
max_ttl_test() ->
    % Set TTL to 0.04 seconds...
    % Set Max TTL to 0.9 seconds...
    Config = [{riak_cache_backend_ttl, 0.04}, {riak_cache_backend_max_ttl, 0.09}],
    {ok, State} = start(42, Config),

    Bucket = <<"Bucket">>, 
    Key = <<"Key">>,
    Value = <<"Value">>,

    % Put an object...
    put(State, {Bucket, Key}, Value),

    % Wait 0.03 seconds, access it...
    timer:sleep(trunc(timer:seconds(0.03))),
    {ok, Value} = get(State, {Bucket, Key}),
    
    % Wait 0.03 seconds, access it...
    timer:sleep(trunc(timer:seconds(0.03))),
    {ok, Value} = get(State, {Bucket, Key}),
    
    % Wait 0.05 seconds, it should expire...
    timer:sleep(trunc(timer:seconds(0.05))),
    
    % This time it should be gone...
    {error, notfound} = get(State, {Bucket, Key}),
    
    ok.
    
% @private
max_memory_test() ->
    % Set max size to 1.5kb...
    Config = [{riak_cache_backend_memory, 1.5 * (1 / 1024)}],
    {ok, State} = start(42, Config),

    Bucket = <<"Bucket">>, 
    Key1 = <<"Key1">>,
    Value1 = list_to_binary(string:copies("1", 1024)),
    Key2 = <<"Key2">>,
    Value2 = list_to_binary(string:copies("2", 1024)),

    % Write Key1 to the datastore...
    put(State, {Bucket, Key1}, Value1),
    
    % Fetch it...
    {ok, Value1} = get(State, {Bucket, Key1}),
    
    % Pause for a second to let clocks increment...
    timer:sleep(timer:seconds(1)),
    
    % Write Key2 to the datastore...
    put(State, {Bucket, Key2}, Value2),
    
    % Key1 should be kicked out...
    {error, notfound} = get(State, {Bucket, Key1}),

    % Key2 should still be present...
    {ok, Value2} = get(State, {Bucket, Key2}),
    
    ok.
