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

%% @private
-module(rtdev).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(DEVS(N), lists:concat(["dev", N, "@127.0.0.1"])).
-define(DEV(N), list_to_atom(?DEVS(N))).
-define(PATH, (rt:config(rtdev_path))).

get_deps() ->
    lists:flatten(io_lib:format("~s/dev/dev1/lib", [relpath(current)])).

riakcmd(Path, N, Cmd) ->
    io_lib:format("~s/dev/dev~b/bin/riak ~s", [Path, N, Cmd]).

gitcmd(Path, Cmd) ->
    io_lib:format("git --git-dir=\"~s/.git\" --work-tree=\"~s/\" ~s",
                  [Path, Path, Cmd]).

riak_admin_cmd(Path, N, Args) ->
    Quoted =
        lists:map(fun(Arg) when is_list(Arg) ->
                          lists:flatten([$", Arg, $"]);
                     (_) ->
                          erlang:error(badarg)
                  end, Args),
    ArgStr = string:join(Quoted, " "),
    io_lib:format("~s/dev/dev~b/bin/riak-admin ~s", [Path, N, ArgStr]).

run_git(Path, Cmd) ->
    lager:info("Running: ~s", [gitcmd(Path, Cmd)]),
    os:cmd(gitcmd(Path, Cmd)).

run_riak(N, Path, Cmd) ->
    lager:info("Running: ~s", [riakcmd(Path, N, Cmd)]),
    R = os:cmd(riakcmd(Path, N, Cmd)),
    case Cmd of
        "start" ->
            case rt_intercept:are_intercepts_loaded(?DEV(N)) of
                false ->
                    ok = rt_intercept:load_intercepts([?DEV(N)]);
                true ->
                    ok
            end,
            R;
        _ ->
            R
    end.

setup_harness(_Test, _Args) ->
    Path = relpath(root),
    %% Stop all discoverable nodes, not just nodes we'll be using for this test.
    rt:pmap(fun(X) -> stop_all(X ++ "/dev") end, devpaths()),

    %% Reset nodes to base state
    lager:info("Resetting nodes to fresh state"),
    run_git(Path, "reset HEAD --hard"),
    run_git(Path, "clean -fd"),

    lager:info("Cleaning up lingering pipe directories"),
    rt:pmap(fun(Dir) ->
                    %% when joining two absolute paths, filename:join intentionally
                    %% throws away the first one. ++ gets us around that, while
                    %% keeping some of the security of filename:join.
                    %% the extra slashes will be pruned by filename:join, but this
                    %% ensures that there will be at least one between "/tmp" and Dir
                    PipeDir = filename:join(["/tmp//" ++ Dir, "dev"]),
                    %% when using filelib:wildcard/2, there must be a wildchar char
                    %% before the first '/'.
                    Files = filelib:wildcard("dev?/*.{r,w}", PipeDir),
                    [ file:delete(filename:join(PipeDir, File)) || File <- Files],
                    file:del_dir(PipeDir)
            end, devpaths()),
    ok.

relpath(Vsn) ->
    Path = ?PATH,
    relpath(Vsn, Path).

relpath(Vsn, Paths=[{_,_}|_]) ->
    orddict:fetch(Vsn, orddict:from_list(Paths));
relpath(current, Path) ->
    Path;
relpath(root, Path) ->
    Path;
relpath(_, _) ->
    throw("Version requested but only one path provided").

upgrade(Node, NewVersion) ->
    N = node_id(Node),
    Version = node_version(N),
    lager:info("Upgrading ~p : ~p -> ~p", [Node, Version, NewVersion]),
    stop(Node),
    rt:wait_until_unpingable(Node),
    OldPath = relpath(Version),
    NewPath = relpath(NewVersion),

    Commands = [
        io_lib:format("cp -p -P -R \"~s/dev/dev~b/data\" \"~s/dev/dev~b\"",
                       [OldPath, N, NewPath, N]),
        io_lib:format("rm -rf ~s/dev/dev~b/data/*",
                       [OldPath, N]),
        io_lib:format("cp -p -P -R \"~s/dev/dev~b/etc\" \"~s/dev/dev~b\"",
                       [OldPath, N, NewPath, N])
    ],
    [ begin
        lager:info("Running: ~s", [Cmd]),
        os:cmd(Cmd)
    end || Cmd <- Commands],
    VersionMap = orddict:store(N, NewVersion, rt:config(rt_versions)),
    rt:set_config(rt_versions, VersionMap),
    start(Node),
    rt:wait_until_pingable(Node),
    ok.

all_the_app_configs(DevPath) ->
    case filelib:is_dir(DevPath) of
        true ->
            Devs = filelib:wildcard(DevPath ++ "/dev/dev*"),
            [ Dev ++ "/etc/app.config" || Dev <- Devs];
        _ ->
            lager:debug("~s is not a directory.", [DevPath]),
            []
    end.

update_app_config(all, Config) ->
    lager:info("rtdev:update_app_config(all, ~p)", [Config]),
    [ update_app_config(DevPath, Config) || DevPath <- devpaths()];
update_app_config(Node, Config) when is_atom(Node) ->
    N = node_id(Node),
    Path = relpath(node_version(N)),
    ConfigFile = io_lib:format("~s/dev/dev~b/etc/app.config", [Path, N]),
    update_app_config_file(ConfigFile, Config);
update_app_config(DevPath, Config) ->
    [update_app_config_file(AppConfig, Config) || AppConfig <- all_the_app_configs(DevPath)].

update_app_config_file(ConfigFile, Config) ->
    lager:info("rtdev:update_app_config_file(~s, ~p)", [ConfigFile, Config]),
    {ok, [BaseConfig]} = file:consult(ConfigFile),
    MergeA = orddict:from_list(Config),
    MergeB = orddict:from_list(BaseConfig),
    NewConfig =
        orddict:merge(fun(_, VarsA, VarsB) ->
                              MergeC = orddict:from_list(VarsA),
                              MergeD = orddict:from_list(VarsB),
                              orddict:merge(fun(_, ValA, _ValB) ->
                                                    ValA
                                            end, MergeC, MergeD)
                      end, MergeA, MergeB),
    NewConfigOut = io_lib:format("~p.", [NewConfig]),
    ?assertEqual(ok, file:write_file(ConfigFile, NewConfigOut)),
    ok.

get_backends() ->
    Backends = lists:usort(
        lists:flatten([ get_backends(DevPath) || DevPath <- devpaths()])),
    case Backends of
        [riak_kv_bitcask_backend] -> bitcask;
        [riak_kv_eleveldb_backend] -> eleveldb;
        [riak_kv_memory_backend] -> memory;
        [Other] -> Other;
        MoreThanOne -> MoreThanOne
    end.

get_backends(DevPath) ->
    [get_backend(AppConfig) || AppConfig <- all_the_app_configs(DevPath)].

get_backend(AppConfig) ->
    {ok, [Config]} = file:consult(AppConfig),
    kvc:path(riak_kv.storage_backend, Config).

node_path(Node) ->
    N = node_id(Node),
    Path = relpath(node_version(N)),
    lists:flatten(io_lib:format("~s/dev/dev~b", [Path, N])).

create_dirs(Nodes) ->
    Snmp = [node_path(Node) ++ "/data/snmp/agent/db" || Node <- Nodes],
    [?assertCmd("mkdir -p " ++ Dir) || Dir <- Snmp].

clean_data_dir(Nodes, SubDir) when is_list(Nodes) ->
    DataDirs = [node_path(Node) ++ "/data/" ++ SubDir || Node <- Nodes],
    lists:foreach(fun rm_dir/1, DataDirs).

rm_dir(Dir) ->
    lager:info("Removing directory ~s", [Dir]), 
    ?assertCmd("rm -rf " ++ Dir),
    ?assertEqual(false, filelib:is_dir(Dir)).

add_default_node_config(Nodes) ->
    case rt:config(rt_default_config, undefined) of
        undefined -> ok;
        Defaults when is_list(Defaults) ->
            rt:pmap(fun(Node) ->
                            update_app_config(Node, Defaults)
                    end, Nodes),
            ok;
        BadValue ->
            lager:error("Invalid value for rt_default_config : ~p", [BadValue]),
            throw({invalid_config, {rt_default_config, BadValue}})
    end.

deploy_nodes(NodeConfig) ->
    Path = relpath(root),
    lager:info("Riak path: ~p", [Path]),
    NumNodes = length(NodeConfig),
    NodesN = lists:seq(1, NumNodes),
    Nodes = [?DEV(N) || N <- NodesN],
    NodeMap = orddict:from_list(lists:zip(Nodes, NodesN)),
    {Versions, Configs} = lists:unzip(NodeConfig),
    VersionMap = lists:zip(NodesN, Versions),

    %% Check that you have the right versions available
    [ check_node(Version) || Version <- VersionMap ],
    rt:set_config(rt_nodes, NodeMap),
    rt:set_config(rt_versions, VersionMap),

    create_dirs(Nodes),

    %% Set initial config
    add_default_node_config(Nodes),
    rt:pmap(fun({_, default}) ->
                    ok;
               ({Node, Config}) ->
                    update_app_config(Node, Config)
            end,
            lists:zip(Nodes, Configs)),

    %% create snmp dirs, for EE
    create_dirs(Nodes),

    %% Start nodes
    %%[run_riak(N, relpath(node_version(N)), "start") || N <- Nodes],
    rt:pmap(fun(N) -> run_riak(N, relpath(node_version(N)), "start") end, NodesN),

    %% Ensure nodes started
    [ok = rt:wait_until_pingable(N) || N <- Nodes],

    %% %% Enable debug logging
    %% [rpc:call(N, lager, set_loglevel, [lager_console_backend, debug]) || N <- Nodes],

    %% We have to make sure that riak_core_ring_manager is running before we can go on.
    [ok = rt:wait_until_registered(N, riak_core_ring_manager) || N <- Nodes],

    %% Ensure nodes are singleton clusters
    [ok = rt:check_singleton_node(?DEV(N)) || {N, Version} <- VersionMap,
                                              Version /= "0.14.2"],

    lager:info("Deployed nodes: ~p", [Nodes]),
    Nodes.

stop_all(DevPath) ->
    case filelib:is_dir(DevPath) of
        true ->
            Devs = filelib:wildcard(DevPath ++ "/dev*"),
            %% Works, but I'd like it to brag a little more about it.
            Stop = fun(C) ->
                Cmd = C ++ "/bin/riak stop",
                [Output | _Tail] = string:tokens(os:cmd(Cmd), "\n"),
                Status = case Output of
                    "ok" -> "ok";
                    _ -> "wasn't running"
                end,
                lager:info("Stopping Node... ~s ~~ ~s.", [Cmd, Status])
            end,
            [Stop(D) || D <- Devs];
        _ -> lager:info("~s is not a directory.", [DevPath])
    end,
    ok.

stop(Node) ->
    RiakPid = rpc:call(Node, os, getpid, []),
    N = node_id(Node),
    run_riak(N, relpath(node_version(N)), "stop"),
    F = fun(_N) ->
            os:cmd("kill -0 " ++ RiakPid) =/= []
    end,
    ?assertEqual(ok, rt:wait_until(Node, F)),
    ok.

start(Node) ->
    N = node_id(Node),
    run_riak(N, relpath(node_version(N)), "start"),
    ok.

attach(Node, Expected) ->
    interactive(Node, "attach", Expected).

console(Node, Expected) ->
    interactive(Node, "console", Expected).

interactive(Node, Command, Exp) ->
    N = node_id(Node),
    Path = relpath(node_version(N)),
    Cmd = riakcmd(Path, N, Command),
    lager:info("Opening a port for riak ~s.", [Command]),
    P = open_port({spawn, binary_to_list(iolist_to_binary(Cmd))},
                  [stream, use_stdio, exit_status, binary, stderr_to_stdout]),
    interactive_loop(P, Exp).

interactive_loop(Port, Expected) ->
    receive
        {Port, {data, Data}} ->
            %% We've gotten some data, so the port isn't done executing
            %% Let's break it up by newline and display it.
            Tokens = string:tokens(binary_to_list(Data), "\n"),
            [lager:debug("~s", [Text]) || Text <- Tokens],

            %% Now we're going to take hd(Expected) which is either {expect, X}
            %% or {send, X}. If it's {expect, X}, we foldl through the Tokenized
            %% data looking for a partial match via rt:str/2. If we find one,
            %% we pop hd off the stack and continue iterating through the list
            %% with the next hd until we run out of input. Once hd is a tuple
            %% {send, X}, we send that test to the port. The assumption is that
            %% once we send data, anything else we still have in the buffer is
            %% meaningless, so we skip it. That's what that {sent, sent} thing
            %% is about. If there were a way to abort mid-foldl, I'd have done
            %% that. {sent, _} -> is just a pass through to get out of the fold.

            NewExpected = lists:foldl(fun(X, Expect) ->
                    [{Type, Text}|RemainingExpect] = case Expect of
                        [] -> [{done, "done"}|[]];
                        E -> E
                    end,
                    case {Type, rt:str(X, Text)} of
                        {expect, true} ->
                            RemainingExpect;
                        {expect, false} ->
                            [{Type, Text}|RemainingExpect];
                        {send, _} ->
                            port_command(Port, list_to_binary(Text ++ "\n")),
                            [{sent, "sent"}|RemainingExpect];
                        {sent, _} ->
                            Expect;
                        {done, _} ->
                            []
                    end
                end, Expected, Tokens),
            %% Now that the fold is over, we should remove {sent, sent} if it's there.
            %% The fold might have ended not matching anything or not sending anything
            %% so it's possible we don't have to remove {sent, sent}. This will be passed
            %% to interactive_loop's next iteration.
            NewerExpected = case NewExpected of
                [{sent, "sent"}|E] -> E;
                E -> E
            end,
            %% If NewerExpected is empty, we've met all expected criteria and in order to boot
            %% Otherwise, loop.
            case NewerExpected of
                [] -> ?assert(true);
                _ -> interactive_loop(Port, NewerExpected)
            end;
        {Port, {exit_status,_}} ->
            %% This port has exited. Maybe the last thing we did was {send, [4]} which
            %% as Ctrl-D would have exited the console. If Expected is empty, then
            %% We've met every expectation. Yay! If not, it means we've exited before
            %% something expected happened.
            ?assertEqual([], Expected)
        after rt:config(rt_max_wait_time) ->
            %% interactive_loop is going to wait until it matches expected behavior
            %% If it doesn't, the test should fail; however, without a timeout it
            %% will just hang forever in search of expected behavior. See also: Parenting
            ?assertEqual([], Expected)
    end.

admin(Node, Args) ->
    N = node_id(Node),
    Path = relpath(node_version(N)),
    Cmd = riak_admin_cmd(Path, N, Args),
    lager:info("Running: ~s", [Cmd]),
    Result = os:cmd(Cmd),
    lager:info("~s", [Result]),
    {ok, Result}.

riak(Node, Args) ->
    N = node_id(Node),
    Path = relpath(node_version(N)),
    Result = run_riak(N, Path, Args),
    lager:info("~s", [Result]),
    {ok, Result}.

node_id(Node) ->
    NodeMap = rt:config(rt_nodes),
    orddict:fetch(Node, NodeMap).

node_version(N) ->
    VersionMap = rt:config(rt_versions),
    orddict:fetch(N, VersionMap).

spawn_cmd(Cmd) ->
    spawn_cmd(Cmd, []).
spawn_cmd(Cmd, Opts) ->
    Port = open_port({spawn, Cmd}, [stream, in, exit_status] ++ Opts),
    Port.

wait_for_cmd(Port) ->
    rt:wait_until(node(),
                  fun(_) ->
                          receive
                              {Port, Msg={data, _}} ->
                                  self() ! {Port, Msg},
                                  false;
                              {Port, Msg={exit_status, _}} ->
                                  catch port_close(Port),
                                  self() ! {Port, Msg},
                                  true
                          after 0 ->
                                  false
                          end
                  end),
    get_cmd_result(Port, []).

cmd(Cmd) ->
    cmd(Cmd, []).

cmd(Cmd, Opts) ->
    wait_for_cmd(spawn_cmd(Cmd, Opts)).

get_cmd_result(Port, Acc) ->
    receive
        {Port, {data, Bytes}} ->
            get_cmd_result(Port, [Bytes|Acc]);
        {Port, {exit_status, Status}} ->
            Output = lists:flatten(lists:reverse(Acc)),
            {Status, Output}
    after 0 ->
            timeout
    end.

check_node({_N, Version}) ->
    case proplists:is_defined(Version, rt:config(rtdev_path)) of
        true -> ok;
        _ ->
            lager:error("You don't have Riak ~s installed or configured", [Version]),
            erlang:error("You don't have Riak " ++ atom_to_list(Version) ++ " installed or configured")
    end.

set_backend(Backend) ->
    lager:info("rtdev:set_backend(~p)", [Backend]),
    update_app_config(all, [{riak_kv, [{storage_backend, Backend}]}]),
    get_backends().

get_version() ->
    case file:read_file(relpath(current) ++ "/VERSION") of
        {error, enoent} -> unknown;
        {ok, Version} -> Version
    end.

teardown() ->
    %% Stop all discoverable nodes, not just nodes we'll be using for this test.
    [stop_all(X ++ "/dev") || X <- devpaths()].

whats_up() ->
    io:format("Here's what's running...~n"),

    Up = [rpc:call(Node, os, cmd, ["pwd"]) || Node <- nodes()],
    [io:format("  ~s~n",[string:substr(Dir, 1, length(Dir)-1)]) || Dir <- Up].

devpaths() ->
    lists:usort([ DevPath || {_Name, DevPath} <- proplists:delete(root, rt:config(rtdev_path))]).

versions() ->
    proplists:get_keys(rt:config(rtdev_path)) -- [root].

get_node_logs() ->
    Root = proplists:get_value(root, ?PATH),
    [ begin
          {ok, Data} = file:read_file(Filename),
          {Filename, Data}
      end || Filename <- filelib:wildcard(Root ++ "/*/dev/dev*/log/*") ].
