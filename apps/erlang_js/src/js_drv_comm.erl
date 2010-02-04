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

-module(js_drv_comm).

-export([pack/2]).

pack(Cmd, Terms) when length(Cmd) == 2 ->
    NewTerms = lists:foldr(fun(T, Acc) -> marshal_data(T, Acc) end, [], Terms),
    list_to_binary(lists:flatten([[Cmd], [NewTerms]])).

%% Internal functions
marshal_data(Term, Acc) when is_integer(Term) ->
    [<<Term:32>>|Acc];
marshal_data(Term, Acc) when is_list(Term) ->
    marshal_data(list_to_binary(Term), Acc);
marshal_data(Term, Acc) when is_binary(Term) ->
    S = size(Term),
    [[<<S:32>>, Term]|Acc].
