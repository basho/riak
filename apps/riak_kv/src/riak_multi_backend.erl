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

%% @doc riak_gb_trees_backend is a Riak storage backend using Erlang gb_trees.

-module (riak_multi_backend).
-export([start/2, stop/1,get/2,put/3,list/1,list_bucket/2,delete/2,is_empty/1,drop/1,fold/3]).

-include_lib("eunit/include/eunit.hrl").

-record (state, {backends, default_backend}).

%% @doc
%% riak_multi_backend allows you to run multiple backends within a 
%% single Riak instance. The 'backend' property of a bucket specifies
%% the backend in which the object should be stored. If no 'backend'
%% is specified, then the 'multi_backend_default' setting is used.
%% If this is unset, then the first defined backend is used.
%% 
%% === Configuration ===
%% 
%%     {storage_backend, riak_multi_backend},
%%     {multi_backend_default, first_backend},
%%     {multi_backend, [ 
%%       % format: {name, module, [Configs]}
%%       {first_backend, riak_xxx_backend, [
%%         {config1, ConfigValue1},
%%         {config2, ConfigValue2}
%%       ]},
%%       {second_backend, riak_yyy_backend, [
%%         {config1, ConfigValue1},
%%         {config2, ConfigValue2}
%%       ]}
%%     ]}
%%
%%
%% Then, tell a bucket which one to use...
%%
%%     riak_bucket:set_bucket(&lt;&lt;"MY_BUCKET"&gt;&gt;, [{backend, second_backend}])
%%
%%


% @spec start(Partition :: integer(), Config :: integer()) ->
%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(Partition, Config) ->
    % Sanity checking...
    Defs = proplists:get_value(multi_backend, Config),
    assert(is_list(Defs), {invalid_config_setting, multi_backend, list_expected}),
    assert(length(Defs) > 0, {invalid_config_setting, multi_backend, list_is_empty}),
    {First, _, _} = hd(Defs),
    
    % Get the default...
    DefaultBackend = proplists:get_value(multi_backend_default, Config, First),
    assert(lists:keymember(DefaultBackend, 1, Defs), {invalid_config_setting, multi_backend_default, backend_not_found}),
    
    % Start the backends...
    Backends = [begin
        {ok, State} = Module:start(Partition, SubConfig),
        {Name, Module, State}
    end || {Name, Module, SubConfig} <- Defs],

    {ok, #state { backends=Backends, default_backend=DefaultBackend}}.

% @spec stop(state()) -> ok | {error, Reason :: term()}
stop(State) -> 
    Backends = State#state.backends,
    Results = [Module:stop(SubState) || {_, Module, SubState} <- Backends],
    ErrorResults = [X || X <- Results, X /= ok],
    case ErrorResults of
        [] -> ok;
        _ -> {error, ErrorResults}
    end.

% get(state(), Key :: binary()) ->
%   {ok, Val :: binary()} | {error, Reason :: term()}
get(State, {Bucket, Key}) ->
    {_Name, Module, SubState} = get_backend(Bucket, State),
    Module:get(SubState, {Bucket, Key}).

% put(state(), Key :: binary(), Val :: binary()) ->
%   ok | {error, Reason :: term()}
put(State, {Bucket, Key}, Value) -> 
    {_Name, Module, SubState} = get_backend(Bucket, State),
    Module:put(SubState, {Bucket, Key}, Value).

% delete(state(), Key :: binary()) ->
%   ok | {error, Reason :: term()}
delete(State, {Bucket, Key}) -> 
    {_Name, Module, SubState} = get_backend(Bucket, State),
    Module:delete(SubState, {Bucket, Key}).

% list(state()) -> [Key :: binary()]
list(State) ->
    F = fun({_, Module, SubState}, Acc) ->
        Module:list(SubState) ++ Acc
    end,
    lists:foldl(F, [], State#state.backends).

% list_bucket(state(), '_') -> [Bucket :: binary()]
list_bucket(State, '_') -> 
    F = fun({_, Module, SubState}, Acc) ->
        Module:list_bucket(SubState, '_') ++ Acc
    end,
    lists:foldl(F, [], State#state.backends);
    
% list_bucket(state(), {filter, Bucket :: binary(), F :: function()}) -> [Key :: binary()]   
list_bucket(State, {filter, Bucket, FilterFun}) ->
    F = fun({_, Module, SubState}, Acc) ->
        Module:list_bucket(SubState, {filter, Bucket, FilterFun}) ++ Acc
    end,
    lists:foldl(F, [], State#state.backends);
    
list_bucket(State, Bucket) ->
    {_Name, Module, SubState} = get_backend(Bucket, State),
    Module:list_bucket(SubState, Bucket).

is_empty(State) ->
    F = fun({_, Module, SubState}, Acc) ->
                [Module:is_empty(SubState)|Acc]
        end,
    lists:all(fun(I) -> I end, lists:foldl(F, [], State#state.backends)).

drop(State) ->
    F = fun({_, Module, SubState}, Acc) ->
                [Module:drop(SubState)|Acc]
        end,
    lists:foldl(F, [], State#state.backends),
    ok.

fold(State, Fun0, Acc) ->    
    F = fun({_, Module, SubState}, AccIn) ->
                [Module:fold(SubState, Fun0, AccIn)|AccIn]
        end,
    lists:flatten(lists:foldl(F, Acc, State#state.backends)).


% Given a Bucket name and the State, return the
% backend definition. (ie: {Name, Module, SubState})
get_backend(Bucket, State) ->
    % Get the name of the backend...
    DefaultBackend = State#state.default_backend,
    BucketProps = riak_bucket:get_bucket(Bucket),
    BackendName = proplists:get_value(backend, BucketProps, DefaultBackend),

    % Ensure that a backend by that name exists...
    Backends = State#state.backends,
    case lists:keyfind(BackendName, 1, Backends) of
        false -> throw({?MODULE, undefined_backend, BackendName});
        Backend -> Backend
    end.

assert(true, _) -> ok;
assert(false, Error) -> throw({?MODULE, Error}).    
    
% @private
simple_test() ->    
    % Start the ring manager...
    crypto:start(),
    riak_ring_manager:start_link(test),
    
    % Set some buckets...
    riak_bucket:set_bucket(<<"b1">>, [{backend, first_backend}]),
    riak_bucket:set_bucket(<<"b2">>, [{backend, second_backend}]),
    
    % Run the standard backend test...
    Config = sample_config(),
    riak_test_util:standard_backend_test(riak_multi_backend, Config).

get_backend_test() ->
    % Start the ring manager...
    crypto:start(),
    riak_ring_manager:start_link(test),
    
    % Set some buckets...
    riak_bucket:set_bucket(<<"b1">>, [{backend, first_backend}]),
    riak_bucket:set_bucket(<<"b2">>, [{backend, second_backend}]),
    
    % Start the backend...    
    {ok, State} = start(42, sample_config()),

    % Check our buckets...
    {first_backend, riak_gb_trees_backend, _} = get_backend(<<"b1">>, State),
    {second_backend, riak_ets_backend, _} = get_backend(<<"b2">>, State),
    
    % Check the default...
    {second_backend, riak_ets_backend, _} = get_backend(<<"b3">>, State),
    
    ok.


sample_config() ->
    [
        {storage_backend, riak_multi_backend},
        {multi_backend_default, second_backend},
        {multi_backend, [
            {first_backend, riak_gb_trees_backend, []},
            {second_backend, riak_ets_backend, []}
        ]}
    ].
