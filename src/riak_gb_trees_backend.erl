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
-export([start/1,stop/1,get/2,put/3,list/1,list_bucket/2,delete/2]).

% @type state() = term().
-record(state, {pid}).


% @private
simple_test() ->
    application:set_env(riak, riak_gb_trees_backend_root,
                        "test/gb_trees-backend"),
    ?assertCmd("rm -rf test/gb_trees-backend"),
    riak_test_util:standard_backend_test(riak_gb_trees_backend).

% @spec start(Partition :: integer()) ->
%                        {ok, state()} | {{error, Reason :: term()}, state()}
start(_Partition) ->
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

list_bucket(_, _) ->
    throw(unsupported).
    
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
                true -> gb_trees:deltee(BKey, Tree);
                false -> Tree
            end,
            Pid ! {delete_response, ok, Ref},
            tree_loop(Tree1);

        {list, Pid, Ref} ->
            Pid ! {list_response, gb_trees:keys(Tree), Ref},
            tree_loop(Tree);
            
        {stop, Pid, Ref} ->
            Pid ! {stop_response, ok, Ref}
    end.

% riak_gb_trees_backend does not currently serialize. But if it did,
% it could use the functions below.
%
% read_tree(Filename) ->
%     case file:read_file(Filename) of
%         {ok, B} -> binary_to_term(zlib:unzip(B));
%         _ -> gb_trees:empty()
%     end.        
%     
% write_tree(Tree, Filename) ->
%     B = term_to_binary(Tree),
%     BC = zlib:zip(B),
%     ok = file:write_file(Filename, BC).
