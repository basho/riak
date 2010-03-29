%% -------------------------------------------------------------------
%%
%% riak_core: Core Riak Application
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
-module(json_pp).

-define(SPACE, 32).
-define(is_quote(C), (C == $\") orelse (C == $\')).
-define(is_indent(C), (C == 91) orelse (C == 123)). % [, {
-define(is_undent(C), (C == 93) orelse (C == 125)). % ], }
-export([print/1,
         test/0]).

print(Str) when is_list(Str) -> json_pp(Str, 0, undefined, []).

json_pp([$\\, C| Rest], I, C, Acc) -> % in quote
    json_pp(Rest, I, C, [C, $\\| Acc]);
json_pp([C| Rest], I, undefined, Acc) when ?is_quote(C) ->
    json_pp(Rest, I, C, [C| Acc]);
json_pp([C| Rest], I, C, Acc) -> % in quote
    json_pp(Rest, I, undefined, [C| Acc]);
json_pp([C| Rest], I, undefined, Acc) when ?is_indent(C) ->
    json_pp(Rest, I+1, undefined, [pp_indent(I+1), $\n, C| Acc]);
json_pp([C| Rest], I, undefined, Acc) when ?is_undent(C) ->
    json_pp(Rest, I-1, undefined, [C, pp_indent(I-1), $\n| Acc]);
json_pp([$,| Rest], I, undefined, Acc) ->
    json_pp(Rest, I, undefined, [pp_indent(I), $\n, $,| Acc]);
json_pp([$:| Rest], I, undefined, Acc) ->
    json_pp(Rest, I, undefined, [?SPACE, $:| Acc]);
json_pp([C|Rest], I, Q, Acc) ->
    json_pp(Rest, I, Q, [C| Acc]);
json_pp([], _I, _Q, Acc) -> % done
    lists:reverse(Acc).

pp_indent(I) -> lists:duplicate(I*4, ?SPACE).

%% testing

test_data() ->
    {struct, [{foo, true},
              {bar, false},
              {baz, {array, [1, 2, 3, 4]}},
              {'fiz:f', null},
              {"fozzer\"", 5}]}.

listify(IoList) -> binary_to_list(list_to_binary(IoList)).

test() ->
    J1 = listify(mochijson:encode(test_data())),
    io:format("~s~n", [listify(print(J1))]).
    
    
