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
-include_lib("eunit/include/eunit.hrl").

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

%%
%% TEST
%%

app_root_test() ->
    ?assertEqual(".",           calculate_app_root(1)),
    ?assertEqual("../..",       calculate_app_root(2)),
    ?assertEqual("../../..",    calculate_app_root(3)),
    ?assertEqual("../../../..", calculate_app_root(4)).

reconstitute_test() ->
    ?assertEqual("",            reconstitute([])),
    ?assertEqual("foo",         reconstitute(["foo"])),
    ?assertEqual("foo/bar",     reconstitute(["foo","bar"])),
    ?assertEqual("foo/bar/baz", reconstitute(["foo","bar","baz"])).

split_host_test() ->
    ?assertEqual(["foo","bar","baz"], split_host("foo.bar.baz")).

split_host_port_test() ->
    ?assertEqual({[], 80}, split_host_port("")),
    ?assertEqual({["foo","bar","baz"], 80},
                 split_host_port("foo.bar.baz:80")),
    ?assertEqual({["foo","bar","baz"], 1234},
                 split_host_port("foo.bar.baz:1234")).

%% port binding
bind_port_simple_match_test() ->
    ?assertEqual({ok, []}, bind_port(80, 80, [])),
    ?assertEqual({ok, [{foo, bar}]},
                 bind_port(1234, 1234, [{foo, bar}])).

bind_port_matchall_test() ->
    ?assertEqual({ok, []}, bind_port('*', 80, [])),
    ?assertEqual({ok, [{foo, bar}]},
                 bind_port('*', 1234, [{foo, bar}])).

bind_port_match_test() ->
    ?assertEqual({ok, [{foo, 80}]}, bind_port(foo, 80, [])),
    {ok, WholeBinding} = bind_port(foo, 1234, [{bar, baz}]),
    ?assertEqual(2, length(WholeBinding)),
    ?assertEqual(1234, proplists:get_value(foo, WholeBinding)),
    ?assertEqual(baz, proplists:get_value(bar, WholeBinding)).

ind_port_fail_test() ->
    ?assertEqual(fail, bind_port(80, 1234, [])).

%% path binding

bind_path_empty_test() ->
    ?assertEqual({ok, [], [], 0}, bind([], [], [], 0)),
    ?assertEqual({ok, [], [{x,"a"}], 1},
                 bind([], [], [{x,"a"}], 1)).

bind_path_matchall_test() ->
    ?assertEqual({ok, [], [], 1},
                 bind(['*'], [], [], 1)),
    ?assertEqual({ok, ["a","b"], [], 2},
                 bind(['*'], ["a","b"], [], 0)).

bind_path_fail_longer_match_test() ->
    ?assertEqual(fail, bind(["x"], [], [], 0)),
    ?assertEqual(fail, bind([foo], [], [], 0)).

bind_path_with_binding_test() ->
    ?assertEqual({ok, [], [{foo, "a"}], 1},
                 bind([foo], ["a"], [], 0)),
    {ok, Rest, Bind, Depth} = bind([foo,'*'], ["a","b"], [{bar, baz}], 1),
    ?assertEqual(["b"], Rest),
    ?assertEqual(3, Depth),
    ?assertEqual(2, length(Bind)),
    ?assertEqual("a", proplists:get_value(foo, Bind)),
    ?assertEqual(baz, proplists:get_value(bar, Bind)).

bind_path_string_match_test() ->
    ?assertEqual({ok, [], [], 1},
                 bind(["a"], ["a"], [], 0)),
    ?assertEqual({ok, [], [{foo, bar}], 4},
                 bind(["a","b","c"], ["a","b","c"], [{foo, bar}], 1)).

bind_path_string_fail_test() ->
    ?assertEqual(fail, bind(["a"], ["b"], [], 0)),
    ?assertEqual(fail, bind(["a","b"], ["a","c"], [], 0)).

try_path_matching_test() ->
    ?assertEqual({bar, baz, [], [], ".", ""},
                 try_path_binding([{["foo"], bar, baz}], ["foo"], [], 0)),
    Dispatch = [{["a", x], foo, bar},
                {["b", y], baz, quux},
                {["b", y, '*'], baz2, quux2}],
    ?assertEqual({foo, bar, [], [{x, "c"}], "../..", []},
                 try_path_binding(Dispatch, ["a","c"], [], 0)),
    ?assertEqual({baz, quux, [], [{y, "c"}], "../..", []},
                 try_path_binding(Dispatch, ["b","c"], [], 0)),
    ?assertEqual({baz2, quux2, ["z"], [{y, "c"}], "../../..", "z"},
                 try_path_binding(Dispatch, ["b","c","z"], [], 0)),
    ?assertEqual({baz2, quux2, ["z","v"], [{y, "c"}], "../../../..", "z/v"},
                 try_path_binding(Dispatch, ["b","c","z","v"], [], 0)).

try_path_failing_test() ->
    ?assertEqual({no_dispatch_match, ["a"]},
                 try_path_binding([{["b"], x, y}], ["a"], [], 0)).

%% host binding

try_host_binding_nohosts_test() ->
    PathDispatches = [{["a"], foo, bar},
                      {["b"], baz, quux}],
    ?assertEqual(try_host_binding([{{['*'],'*'},PathDispatches}],
                                  ["quux","baz"], 80, ["a"], 0),
                 try_host_binding(PathDispatches,
                                  ["quux","baz"], 80, ["a"], 0)),
    ?assertEqual(try_host_binding([{{['*'],'*'},PathDispatches}],
                                  ["quux","baz"], 80, ["b"], 0),
                 try_host_binding(PathDispatches,
                                  ["quux","baz"], 80, ["b"], 0)),
    ?assertEqual(try_host_binding([ {{['*'],'*'},[D]} || D <- PathDispatches],
                                  ["quux","baz"], 1234, ["a"], 0),
                 try_host_binding(PathDispatches,
                                  ["quux","baz"], 1234, ["a"], 0)),
    ?assertEqual(try_host_binding([ {{['*'],'*'},[D]} || D <- PathDispatches],
                                  ["quux","baz"], 1234, ["b"], 0),
                 try_host_binding(PathDispatches,
                                  ["quux","baz"], 1234, ["b"], 0)).

try_host_binding_noport_test() ->
    Dispatch = [{["foo","bar"], [{["a"],x,y}]},
                {["baz","quux"],[{["b"],z,q}]},
                {[m,"quux"],    [{["c"],r,s}]},
                {['*',"quux"],  [{["d"],t,u}]}],
    ExplicitWildPort = [ {{H, '*'},P} || {H, P} <- Dispatch ],
    ?assertEqual(try_host_binding(ExplicitWildPort,
                                  ["bar","foo"], 80, ["a"], 0),
                 try_host_binding(Dispatch,
                                  ["bar","foo"], 80, ["a"], 0)),
    ?assertEqual(try_host_binding(ExplicitWildPort,
                                  ["quux","baz"], 1234, ["b"], 0),
                 try_host_binding(Dispatch,
                                  ["quux","baz"], 1234, ["b"], 0)),
    ?assertEqual(try_host_binding(ExplicitWildPort,
                                  ["quux","yes"], 81, ["c"], 0),
                 try_host_binding(Dispatch,
                                  ["quux","yes"], 81, ["c"], 0)),
    ?assertEqual(try_host_binding(ExplicitWildPort,
                                  ["quux","no"], 82, ["d"], 0),
                 try_host_binding(Dispatch,
                                  ["quux","no"], 82, ["d"], 0)).

try_host_binding_fullmatch_test() ->
    Dispatch = [{{["foo","bar"],80},[{["a"],x,y}]},
                {{[foo,"bar"],80},  [{["b"],z,q}]},
                {{[foo,"bar"],baz}, [{["c"],r,s}]},
                {{['*',"bar"],'*'}, [{["d"],t,u}]}],
    ?assertEqual({x, y, [], 80, [], [], ".", ""},
                 try_host_binding(Dispatch,
                                  ["bar","foo"], 80, ["a"], 0)),
    ?assertEqual({z, q, [], 80, [], [{foo,"baz"}], ".", ""},
                 try_host_binding(Dispatch,
                                  ["bar","baz"], 80, ["b"], 0)),
    {Mod, Props, HostRemainder, Port, PathRemainder,
     PathBindings, AppRoot, StringPath}=
        try_host_binding(Dispatch, ["bar","quux"], 1234, ["c"], 0),
    ?assertEqual(r, Mod),
    ?assertEqual(s, Props),
    ?assertEqual("", HostRemainder),
    ?assertEqual(1234, Port),
    ?assertEqual([], PathRemainder),
    ?assertEqual(2, length(PathBindings)),
    ?assertEqual("quux", proplists:get_value(foo, PathBindings)),
    ?assertEqual(1234, proplists:get_value(baz, PathBindings)),
    ?assertEqual(".", AppRoot),
    ?assertEqual("", StringPath),
    ?assertEqual({t, u, ["quux","foo"], 80, [], [], ".", ""},
                 try_host_binding(Dispatch, ["bar","quux","foo"],80,["d"],0)).

try_host_binding_fail_test() ->
    ?assertEqual({no_dispatch_match, {["bar","foo"], 1234}, ["x","y","z"]},
                 try_host_binding([], ["bar","foo"], 1234, ["x","y","z"], 0)).

dispatch_test() ->
    ?assertEqual({x, y, [], 80, [], [], "../../..", ""},
                 dispatch("a/b/c",[{["a","b","c"],x,y}])),
    ?assertEqual({x, y, [], 80, [], [], "../../..", ""},
                 dispatch("foo.bar", "a/b/c",
                          [{{["foo","bar"],80},[{["a","b","c"],x,y}]}])),
    ?assertEqual({x, y, [], 1234, [], [], "../../..", ""},
                 dispatch("foo.bar:1234", "a/b/",
                          [{{["foo","bar"],1234},[{["a","b"],x,y}]}])),
    ?assertEqual({no_dispatch_match, {["bar","baz"],8000}, ["q","r"]},
                 dispatch("baz.bar:8000", "q/r",
                          [{{["foo","bar"],80},[{["a","b","c"],x,y}]}])).
