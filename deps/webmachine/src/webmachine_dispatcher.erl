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
-author('Bryan Fink <bryan@basho.com>').

-export([dispatch/2, dispatch/3]).

-define(SEPARATOR, $\/).
-define(MATCH_ALL, '*').

%% @spec dispatch(Path::string(), DispatchList::[matchterm()]) ->
%%                                            dispterm() | dispfail()
%% @doc Interface for URL dispatching.
%% See also http://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration
dispatch(PathAsString, DispatchList) ->
    dispatch([], PathAsString, DispatchList).

%% @spec dispatch(Host::string(), Path::string(),
%%                DispatchList::[matchterm()]) ->
%%         dispterm() | dispfail()
%% @doc Interface for URL dispatching.
%% See also http://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration
dispatch(HostAsString, PathAsString, DispatchList) ->
    Path = string:tokens(PathAsString, [?SEPARATOR]),
    % URIs that end with a trailing slash are implicitly one token
    % "deeper" than we otherwise might think as we are "inside"
    % a directory named by the last token.
    ExtraDepth = case lists:last(PathAsString) == ?SEPARATOR of
		     true -> 1;
		     _ -> 0
		 end,
    {Host, Port} = split_host_port(HostAsString),
    try_host_binding(DispatchList, lists:reverse(Host), Port,
                     Path, ExtraDepth).

split_host_port(HostAsString) ->
    case string:tokens(HostAsString, ":") of
        [HostPart, PortPart] ->
            {split_host(HostPart), list_to_integer(PortPart)};
        [HostPart] ->
            {split_host(HostPart), 80};
        [] ->
            %% no host header
            {[], 80}
    end.

split_host(HostAsString) ->
    string:tokens(HostAsString, ".").

%% @type matchterm() = hostmatchterm() | pathmatchterm().
% The dispatch configuration is a list of these terms, and the
% first one whose host and path terms match the input is used.
% Using a pathmatchterm() here is equivalent to using a hostmatchterm()
% of the form {{['*'],'*'}, [pathmatchterm()]}.

%% @type hostmatchterm() = {hostmatch(), [pathmatchterm()]}.
% The dispatch configuration contains a list of these terms, and the
% first one whose host and one pathmatchterm match is used.

%% @type hostmatch() = [hostterm()] | {[hostterm()], portterm()}.
% A host header (Host, X-Forwarded-For, etc.) will be matched against
% this term.  Using a raws [hostterm()] list is equivalent to using
% {[hostterm()], '*'}.

%% @type hostterm() = '*' | string() | atom().
% A list of hostterms is matched against a '.'-separated hostname.
% The '*' hosterm matches all remaining tokens, and is only allowed at
% the head of the list.
% A string hostterm will match a token of exactly the same string.
% Any atom hostterm other than '*' will match any token and will
% create a binding in the result if a complete match occurs.

%% @type portterm() = '*' | integer() | atom().
% A portterm is matched against the integer port after any ':' in
% the hostname, or 80 if no port is found.
% The '*' portterm patches any port
% An integer portterm will match a port of exactly the same integer.
% Any atom portterm other than '*' will match any port and will
% create a binding in the result if a complete match occurs.

%% @type pathmatchterm() = {[pathterm()], matchmod(), matchopts()}.
% The dispatch configuration contains a list of these terms, and the
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

try_host_binding([], Host, Port, Path, _Depth) ->
    {no_dispatch_match, {Host, Port}, Path};
try_host_binding([Dispatch|Rest], Host, Port, Path, Depth) ->
    {{HostSpec,PortSpec},PathSpec} =
        case Dispatch of
            {{H,P},S} -> {{H,P},S};
            {H,S}     -> {{H,?MATCH_ALL},S};
            S         -> {{[?MATCH_ALL],?MATCH_ALL},[S]}
        end,
    case bind_port(PortSpec, Port, []) of
        {ok, PortBindings} ->
            case bind(lists:reverse(HostSpec), Host, PortBindings, 0) of
                {ok, HostRemainder, HostBindings, _} ->
                    case try_path_binding(PathSpec, Path, HostBindings, Depth) of
                        {Mod, Props, PathRemainder, PathBindings,
                         AppRoot, StringPath} ->
                            {Mod, Props, HostRemainder, Port, PathRemainder,
                             PathBindings, AppRoot, StringPath};
                        {no_dispatch_match, _} ->
                            try_host_binding(Rest, Host, Port, Path, Depth)
                    end;
                fail ->
                    try_host_binding(Rest, Host, Port, Path, Depth)
            end;
        fail ->
            try_host_binding(Rest, Host, Port, Path, Depth)
    end.

bind_port(Port, Port, Bindings) -> {ok, Bindings};
bind_port(?MATCH_ALL, _Port, Bindings) -> {ok, Bindings};
bind_port(PortAtom, Port, Bindings) when is_atom(PortAtom) ->
    {ok, [{PortAtom, Port}|Bindings]};
bind_port(_, _, _) -> fail.

try_path_binding([], PathTokens, _, _) ->
    {no_dispatch_match, PathTokens};
try_path_binding([{PathSchema, Mod, Props}|Rest], PathTokens,
                 Bindings, ExtraDepth) ->
    case bind(PathSchema, PathTokens, Bindings, 0) of
        {ok, Remainder, NewBindings, Depth} ->
            {Mod, Props, Remainder, NewBindings,
             calculate_app_root(Depth + ExtraDepth), reconstitute(Remainder)};
        fail -> 
            try_path_binding(Rest, PathTokens, Bindings, ExtraDepth)
    end.

bind([], [], Bindings, Depth) ->
    {ok, [], Bindings, Depth};
bind([?MATCH_ALL], Rest, Bindings, Depth) when is_list(Rest) ->
    {ok, Rest, Bindings, Depth + length(Rest)};
bind(_, [], _, _) ->
    fail;
bind([Token|RestToken],[Match|RestMatch],Bindings,Depth) when is_atom(Token) ->
    bind(RestToken, RestMatch, [{Token, Match}|Bindings], Depth + 1);
bind([Token|RestToken], [Token|RestMatch], Bindings, Depth) ->
    bind(RestToken, RestMatch, Bindings, Depth + 1);
bind(_, _, _, _) ->
    fail.

reconstitute([]) -> "";
reconstitute(UnmatchedTokens) -> string:join(UnmatchedTokens, [?SEPARATOR]).

calculate_app_root(1) -> ".";
calculate_app_root(N) when N > 1 ->
    string:join(lists:duplicate(N, ".."), [?SEPARATOR]).
