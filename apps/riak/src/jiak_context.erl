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

%% @doc An instance of this parameterized module will be passed as the
%%      Context parameter to functions in Jiak bucket modules.  Those
%%      moduels can use the set_prop and get_prop to store state in
%%      this context, allowing threading of state between check_write,
%%      affect_write, etc.
-module(jiak_context,[Diff,DataProps]).

-export([diff/0, set_diff/1,
         set_prop/2, get_prop/1,
         set_props/1]).

%% @spec diff() -> jiak_resource:diff()
%% @doc Get the computed modifications to the object being stored,
%%      as computed by jiak_resource.
diff() -> Diff.

%% @spec set_diff(jiak_resource:diff()) -> jiak_context()
%% @doc Set the list of modifications.
set_diff(NewDiff) ->
    jiak_context:new(NewDiff,DataProps).

%% @spec set_prop(Key :: term(), Value :: term()) -> jiak_context()
%% @doc Associate Value with the name Key in this context.  A
%%      subesquent call of get_prop(Key) on the resulting jiak_context
%%      will return Value.
set_prop(Key,Value) ->
    jiak_context:new(Diff,
                     [{Key, Value}|proplists:delete(Key, DataProps)]).

%% @spec set_props([{Key :: term(), Value :: term()}]) -> jiak_context()
%% @doc Associate each Value with each Key in a new jiak_context.
%% @equiv lists:foldl(fun({K,V},C) -> C:set_prop(K,V) end,
%%                    Context, List)
set_props(List) ->
    jiak_context:new(Diff,
                     lists:foldl(fun({K, V}, Acc) ->
                                         [{K,V}|proplists:delete(K, Acc)]
                                 end,
                                 DataProps,
                                 List)).

%% @spec get_prop(term()) -> term()|undefined
%% @doc Get the Value that was previously associated with Key in a
%%      call to set_prop/2.  If no value is associated with Key,
%%      the atom 'undefined' is returned.
get_prop(Key) -> proplists:get_value(Key,DataProps).
