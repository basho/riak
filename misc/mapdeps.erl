#!/usr/bin/env escript

%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

% Map a project by its rebar configs.
%
% Usage: mapdeps [directory]
%
% Output can be rendered by graphviz:
%    ./mapdeps /Users/basho/riak | dot -Tpng -oriak.png
%
% It's fairly simple-minded at the moment:
%  - assumes all deps are in [directory]/deps
%  - assumes all deps definitions are {atom(), _, _}
%  - assumes the name of the toplevel is the basename of the directory
%  - showing the app name, not the repo name

-module(mapdeps).

-export([main/1]).

usage() ->
    io:format(standard_error,
              "Map a project by rebar configs~n~n"
              "Usage: mapdeps [directory]~n~n"
              "Output can be rendered by graphviz:~n"
              "   ./mapdeps /Users/basho/riak | dot -Tpng -oriak.png",
              []).

main([]) ->
    {ok, CWD} = file:get_cwd(),
    map_dir(CWD);
main([Dir]) ->
    map_dir(Dir);
main(_wat) ->
    usage(),
    exit(-1).

map_dir(BaseDir) ->
    RebarPath = filename:join([BaseDir, "rebar.config"]),
    case filelib:is_regular(RebarPath) of
        true ->
            file_start(),
            Map = map_rebar(BaseDir, RebarPath, ordsets:new()),
            [ file_edge(From, To) || {From, To} <- Map ],
            file_end();
        false ->
            io:format(standard_error, "~p not found.", [RebarPath]),
            usage(),
            exit(-1)
    end.

%% Read a rebar file. Find any `deps' option. Accumulate tuples of the
%% form `{App, Dep}' for each element in this deps list.  Recurse and
%% attempt to read the rebar.config for each dep.
map_rebar(BaseDir, Path, Acc) ->
    case file:consult(Path) of
        {ok, Opts} ->
            Deps = proplists:get_value(deps, Opts, []),
            lists:foldl(
              fun({DepName, _, _}, A) ->
                      From = app_name(Path),
                      To = atom_to_list(DepName),
                      case ordsets:is_element({To, From}, A) of
                          true ->
                              %% we've already seen the other side,
                              %% recursing would just put us in a loop
                              %% TODO: warning color
                              ordsets:add_element({From, To}, A);
                          false ->
                              NA = ordsets:add_element({From, To}, A),
                              DepPath = filename:join(
                                          [BaseDir, "deps",
                                           atom_to_list(DepName),
                                           "rebar.config"]),
                              map_rebar(BaseDir, DepPath, NA)
                      end
              end,
              Acc,
              Deps);
        _ ->
            Acc
    end.

app_name(Path) ->
    %% assumes Path ends in rebar.config
    filename:basename(filename:dirname(Path)).

file_start() ->
    io:format(standard_io, "digraph {~n", []).

file_end() ->
    io:format(standard_io, "}~n", []).

file_edge(From, To) ->
    io:format(standard_io, " \"~s\" -> ~s;~n", [From, To]).
