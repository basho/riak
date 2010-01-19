%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Convenience module for interacting with Javascript from Erlang.
%% The functions provided by this module marshal bindings and function
%% args into JSON before sending them to Javascript. While this does
%% incur a certain amount of overhead it has the benefit of (mostly)
%% preserving types as they roundtrip between Erlang and Javascript.
%% Of course, this also means all Erlang values MUST BE convertable
%% into JSON. In practice, this is less restricting than it sounds.
-module(js).

-export([define/2, define/3, eval/2, call/3, call/4]).

%% @spec define(port(), binary()) -> ok | {error, any()}
%% @doc Define one or more Javascript expressions.
define(Ctx, Js) ->
    define(Ctx, Js, []).

%% @spec define(port(), binary(), list(any())) -> ok | {error, any()}
%% @doc Define one or more Javascript expressions using a set of bindings. Bindings
%% are useful when the expressions use closures.
define(Ctx, Js, Bindings) ->
    JsBindings = list_to_binary(build_bindings(Bindings, [])),
    FinalJs = iolist_to_binary([JsBindings, Js]),
    js_driver:define_js(Ctx, FinalJs).

%% @spec eval(port(), binary()) -> {ok, any()} | {error, any()}
%% @doc Evaluate one or more Javascript expressions and return the results
eval(Ctx, Js) ->
    js_driver:eval_js(Ctx, Js).

%% @spec call(port(), binary(), list(any())) -> {ok, Result} | {error, any()}
%% @doc Call a function by name with a list of arguments. This is roughly the
%% same as apply in most other languages.
call(Ctx, FunctionName, Args) ->
    call(Ctx, FunctionName, Args, []).

%% @spec call(port(), binary(), list(any()), list(any())) -> {ok, Result} | {error, any()}
%% @doc Call a function by name with a list of arguments and environmental bindings. Bindings
%% behave just like define/3.
call(Ctx, FunctionName, Args, Bindings) ->
    JsBindings = list_to_binary(build_bindings(Bindings, [])),
    ArgList = build_arg_list(Args, []),
    Js = iolist_to_binary([<<"function() {">>, JsBindings, <<"return ">>, FunctionName, <<"(">>, ArgList, <<");">>, <<"}();">>]),
    js_driver:eval_js(Ctx, Js).

%% Internal functions
build_bindings([], Accum) ->
    Accum;
build_bindings([{VarName, Value}|T], Accum) ->
    FinalVarName = case is_atom(VarName) of
                       true ->
                           atom_to_list(VarName);
                       false ->
                           VarName
                   end,
    build_bindings(T, [[FinalVarName, "=", js_mochijson2:encode(Value), ";"]|Accum]).

build_arg_list([], Accum) ->
    lists:reverse(Accum);
build_arg_list([H|[]], Accum) ->
    build_arg_list([], [js_mochijson2:encode(H)|Accum]);
build_arg_list([H|T], Accum) ->
    build_arg_list(T, [[js_mochijson2:encode(H), ","]|Accum]).
