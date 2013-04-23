%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.
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

%% @doc Wrapper for the tests in riak_search/tests/riak_search

-module(verify_search).
-include_lib("eunit/include/eunit.hrl").

-export([confirm/0]).
%% To run in the possibly remote node
-export([test_dirs/1]).

-define(SEARCH_REPO, "git://github.com/basho/riak_search").

confirm() ->
    Config = [{riak_search, [{enabled, true}]}],
    [Node0 | _RestNodes] = Nodes = rt:build_cluster(3, Config),
    rt:wait_until_ring_converged(Nodes),

    Path = rt_config:get(rt_scratch_dir),
    lager:info("Creating scratch dir if necessary at ~s", [Path]),
    ?assertMatch({0, _}, rt:cmd("mkdir -p " ++ Path)),
    SearchRepoDir = filename:join(Path, "riak_search"),
    lager:info("Deleting any previous riak_search repo ~s", [SearchRepoDir]),
    ?assertMatch({0, _}, rt:cmd("rm -rf " ++ SearchRepoDir)),
    lager:info("Cloning riak_search repo within scratch dir"),
    ?assertMatch({0, _}, rt:cmd("git clone --depth 1 "++?SEARCH_REPO,
                                 [{cd, Path}])),
    BaseDir = filename:join([Path, "riak_search", "tests", "riak_search"]),

    rt:load_modules_on_nodes([?MODULE], [Node0]),
    TestDirs = rpc:call(Node0, ?MODULE, test_dirs, [BaseDir]),
    ?assert(is_list(TestDirs)),
    Run =
        fun(Dir) ->
            lager:info("Running test in directory ~s", [Dir]),
            ?assertMatch(ok,
                         rpc:call(Node0, riak_search_test, test, [Dir]))
        end,
    lists:foreach(Run, TestDirs),
    pass.


test_dirs(BaseDir) ->
    {ok, SubDirs} = file:list_dir(BaseDir),
    [filename:join([BaseDir, SubDir]) ||
       SubDir <- SubDirs,
       %% @todo Figure out why this one is not run by run_all.sh
       %% It does fail in a weird way if included
       SubDir /= "replication_test",
       filelib:is_file(filename:join([BaseDir, SubDir, "script.def"]))].
