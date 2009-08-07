%% @author Robert Ahrens <rahrens@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2007-2009 Basho Technologies
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

%% @doc Module for URL-dispatch by pattern matching.

-module(webmachine_dispatcher).
-author('Robert Ahrens <rahrens@basho.com>').
-author('Justin Sheehy <justin@basho.com>').

-export([dispatch/2]).

-define(SEPARATOR, $\/).
-define(MATCH_ALL, '*').

%% @spec dispatch(Path::string(), DispatchList::[matchterm()]) ->
%%                                            dispterm() | dispfail()
%% @doc Interface for URL dispatching.
%% See also http://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration
dispatch(PathAsString, DispatchList) ->
    Path = string:tokens(PathAsString, [?SEPARATOR]),
    % URIs that end with a trailing slash are implicitly one token
    % "deeper" than we otherwise might think as we are "inside"
    % a directory named by the last token.
    ExtraDepth = case lists:last(PathAsString) == ?SEPARATOR of
		     true -> 1;
		     _ -> 0
		 end,
    try_binding(DispatchList, Path, ExtraDepth).

%% @type matchterm() = {[pathterm()], matchmod(), matchopts()}.
% The dispatch configuration is a list of these terms, and the
% first one whose list of pathterms matches the input path is used.

%% @type pathterm() = '*' | string() | atom().
% A list of pathterms is matched against a '/'-separated input path.
% The '*' pathterm matches all remaining tokens.
% A string pathterm will match a token of exactly the same string.
% Any atom pathterm other than '*' will match any token and will
% create a binding in the result if a complete match occurs.

%% @type matchmod() = atom().
% This atom, if present in a successful matchterm, will appear in
% the resulting dispterm.  In Webmachine this is used to name the
% resource module that will handle the matching request.

%% @type matchopts() = [term()].
% This term, if present in a successful matchterm, will appear in
% the resulting dispterm.  In Webmachine this is used to provide
% arguments to the resource module handling the matching request.

%% @type dispterm() = {matchmod(), matchopts(), pathtokens(),
%%                bindings(), approot(), stringpath()}.

%% @type pathtokens() = [pathtoken()].
% This is the list of tokens matched by a trailing '*' pathterm.

%% @type pathtoken() = string().

%% @type bindings() = [{bindingterm(),pathtoken()}].
% This is a proplist of bindings indicated by atom terms in the
% matching spec, bound to the matching tokens in the request path.

%% @type approot() = string().

%% @type stringpath() = string().
% This is the path portion matched by a trailing '*' pathterm.

%% @type dispfail() = {no_dispatch_match, pathtokens()}.

try_binding([], PathTokens, _) ->
    {no_dispatch_match, PathTokens};
try_binding([{PathSchema, Mod, Props}|Rest], PathTokens, ExtraDepth) ->
    case bind_path(PathSchema, PathTokens, [], 0) of
        {ok, Remainder, Bindings, Depth} ->
            {Mod, Props, Remainder, Bindings,
             calculate_app_root(Depth + ExtraDepth), reconstitute(Remainder)};
        fail -> 
            try_binding(Rest, PathTokens, ExtraDepth)
    end.

bind_path([], [], Bindings, Depth) ->
    {ok, [], Bindings, Depth};
bind_path([?MATCH_ALL], PathRest, Bindings, Depth) when is_list(PathRest) ->
    {ok, PathRest, Bindings, Depth + length(PathRest)};
bind_path(_, [], _, _) ->
    fail;
bind_path([Token|Rest],[Match|PathRest],Bindings,Depth) when is_atom(Token) ->
    bind_path(Rest, PathRest, [{Token, Match}|Bindings], Depth + 1);
bind_path([Token|Rest], [Token|PathRest], Bindings, Depth) ->
    bind_path(Rest, PathRest, Bindings, Depth + 1);
bind_path(_, _, _, _) ->
    fail.

reconstitute([]) -> "";
reconstitute(UnmatchedTokens) -> string:join(UnmatchedTokens, [?SEPARATOR]).

calculate_app_root(1) -> ".";
calculate_app_root(N) when N > 1 ->
    string:join(lists:duplicate(N, ".."), [?SEPARATOR]).
