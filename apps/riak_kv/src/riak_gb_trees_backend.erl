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

-module(riak_gb_trees_backend).

-include_lib("eunit/include/eunit.hrl").
-export([start/2, stop/1,get/2,put/3,list/1,list_bucket/2,delete/2,is_empty/1,fold/3,drop/1]).

% @type state() = term().
-record(state, {pid}).


% @spec start(Partition :: integer(), Config :: integer()) ->
%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(_Partition, _Config) ->
    Pid = spawn(fun() -> 
        {A1,A2,A3} = now(),
        random:seed(A1, A2, A3),
        tree_loop(gb_trees:empty()) 
    end),
    {ok, #state { pid=Pid }}.

% @spec stop(state()) -> ok | {error, Reason :: term()}
stop(#state { pid=Pid }) -> 
    Ref = make_ref(),
    Pid ! {stop, self(), Ref},
    receive {stop_response, Result, Ref} -> Result end.

% get(state(), Key :: binary()) ->
%   {ok, Val :: binary()} | {error, Reason :: term()}
get(#state { pid=Pid }, BKey) ->
    Ref = make_ref(),
    Pid ! {get, BKey, self(), Ref},
    receive {get_response, Result, Ref} -> Result end.

% put(state(), Key :: binary(), Val :: binary()) ->
%   ok | {error, Reason :: term()}
put(#state { pid=Pid }, BKey, Value) -> 
    Ref = make_ref(),
    Pid ! {put, BKey, Value, self(), Ref},
    receive {put_response, Result, Ref} -> Result end.

% delete(state(), Key :: binary()) ->
%   ok | {error, Reason :: term()}
delete(#state { pid=Pid }, BKey) -> 
    Ref = make_ref(),
    Pid ! {delete, BKey, self(), Ref},
    receive {delete_response, Result, Ref} -> Result end.

% list(state()) -> [Key :: binary()]
list(#state { pid=Pid }) ->
    Ref = make_ref(),
    Pid ! {list, self(), Ref},
    receive {list_response, Result, Ref} -> Result end.

list_bucket(#state { pid=Pid }, Bucket) ->
    Ref = make_ref(),
    Pid ! {list_bucket, Bucket, self(), Ref},
    receive {list_bucket_response, Result, Ref} -> Result end.

is_empty(#state { pid=Pid }) ->
    Ref = make_ref(),
    Pid ! {is_empty, self(), Ref},
    receive {is_empty_response, Result, Ref} -> Result end.

fold(#state{ pid=Pid }, Fun0, Acc) ->
    Ref = make_ref(),
    Pid ! {fold, Fun0, Acc, self(), Ref},
    receive {fold_response, Result, Ref} -> Result end.

drop(#state{ pid=Pid }) ->
    Ref = make_ref(),
    Pid ! {drop, self(), Ref},
    receive {drop_response, Result, Ref} -> Result end.
    
tree_loop(Tree) ->
    receive
        {get, BKey, Pid, Ref} ->
            case gb_trees:lookup(BKey, Tree) of
                {value, Value} -> Pid ! {get_response, {ok, Value}, Ref};
                none           -> Pid ! {get_response, {error, notfound}, Ref}
            end,
            tree_loop(Tree);
            
        {put, BKey, Value, Pid, Ref} ->
            Tree1 = case gb_trees:is_defined(BKey, Tree) of
                true  -> gb_trees:update(BKey, Value, Tree);
                false -> gb_trees:insert(BKey, Value, Tree)
            end,
            Pid ! {put_response, ok, Ref},
            tree_loop(Tree1);
        
           
        {delete, BKey, Pid, Ref} -> 
            Tree1 = case gb_trees:is_defined(BKey, Tree) of
                true -> gb_trees:delete(BKey, Tree);
                false -> Tree
            end,
            Pid ! {delete_response, ok, Ref},
            tree_loop(Tree1);

        {list, Pid, Ref} ->
            Pid ! {list_response, gb_trees:keys(Tree), Ref},
            tree_loop(Tree);

        {list_bucket, Bucket, Pid, Ref} ->
            Pid ! {list_bucket_response,
                   srv_list_bucket(Tree, Bucket), Ref},
            tree_loop(Tree);

        {is_empty, Pid, Ref} ->
            Pid ! {is_empty_response, gb_trees:is_empty(Tree), Ref},
            tree_loop(Tree);

        {drop, Pid, Ref} ->
            Pid ! {drop_response, ok, Ref},
            tree_loop(gb_trees:empty());

        {fold, Fun0, Acc, Pid, Ref} ->
            Pid ! {fold_response,
                   srv_fold(Tree, Fun0, Acc), Ref},
            tree_loop(Tree);

        {stop, Pid, Ref} ->
            Pid ! {stop_response, ok, Ref}
    end.

srv_list_bucket(Tree, {filter, Bucket, Fun}) ->
     lists:append(
       lists:filter(
         Fun,
         [ [Key] || {B, Key} <- gb_trees:keys(Tree), B == Bucket ]));
srv_list_bucket(Tree, '_') ->
     sets:to_list(
       sets:from_list(
         [ Bucket || {Bucket, _} <- gb_trees:keys(Tree) ]));
srv_list_bucket(Tree, Bucket) ->
    [ Key || {B, Key} <- gb_trees:keys(Tree), B == Bucket ].

srv_fold(Tree, Fun0, Acc) ->
    Iter0 = gb_trees:iterator(Tree),
    srv_fold1(gb_trees:next(Iter0), Fun0, Acc).

srv_fold1(none, _Fun0, Acc) ->
    Acc;
srv_fold1({K,V,Iter}, Fun0, Acc) ->
    srv_fold1(gb_trees:next(Iter), Fun0, Fun0(K,V,Acc)).


%%
%% Test
%%

% @private
simple_test() ->
    riak_test_util:standard_backend_test(riak_gb_trees_backend, []).

