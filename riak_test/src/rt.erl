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

%% @doc
%% Implements the base `riak_test' API, providing the ability to control
%% nodes in a Riak cluster as well as perform commonly reused operations.
%% Please extend this module with new functions that prove useful between
%% multiple independent tests.
-module(rt).
-include_lib("eunit/include/eunit.hrl").

-export([
         admin/2,
         assert_nodes_agree_about_ownership/1,
         assert_which/1,
         async_start/1,
         attach/2,
         build_cluster/1,
         build_cluster/2,
         build_cluster/3,
         capability/2,
         check_singleton_node/1,
         claimant_according_to/1,
         clean_cluster/1,
         clean_data_dir/1,
         clean_data_dir/2,
         cmd/1,
         cmd/2,
         config/1,
         config/2,
         config_or_os_env/1,
         config_or_os_env/2,
         connection_info/1,
         console/2,
         deploy_nodes/1,
         deploy_nodes/2,
         down/2,
         download/1,
         enable_search_hook/2,
         get_deps/0,
         get_node_logs/0,
         get_os_env/1,
         get_os_env/2,
         get_ring/1,
         get_version/0,
         heal/1,
         home_dir/0,
         http_url/1,
         httpc/1,
         httpc_read/3,
         httpc_write/4,
         install_on_absence/2,
         is_mixed_cluster/1,
         is_pingable/1,
         join/2,
         leave/1,
         load_config/1,
         load_modules_on_nodes/2,
         log_to_nodes/2,
         log_to_nodes/3,
         members_according_to/1,
         owners_according_to/1,
         partition/2,
         pbc/1,
         pbc_read/3,
         pbc_set_bucket_prop/3,
         pbc_write/4,
         pbc_put_dir/3,
         pbc_put_file/4,
         pmap/2,
         remove/2,
         riak/2,
         rpc_get_env/2,
         set_backend/1,
         set_config/2,
         setup_harness/2,
         slow_upgrade/3,
         spawn_cmd/1,
         spawn_cmd/2,
         search_cmd/2,
         start/1,
         start_and_wait/1,
         status_of_according_to/2,
         stop/1,
         stop_and_wait/1,
         str/2,
         stream_cmd/1,
         stream_cmd/2,
         systest_read/2,
         systest_read/3,
         systest_read/5,
         systest_write/2,
         systest_write/3,
         systest_write/5,
         teardown/0,
         update_app_config/2,
         upgrade/2,
         url_to_filename/1,
         versions/0,
         wait_for_cluster_service/2,
         wait_for_cmd/1,
         wait_for_service/2,
         wait_until/2,
         wait_until_all_members/1,
         wait_until_all_members/2,
         wait_until_capability/3,
         wait_until_connected/1,
         wait_until_legacy_ringready/1,
         wait_until_owners_according_to/2,
         wait_until_no_pending_changes/1,
         wait_until_nodes_agree_about_ownership/1,
         wait_until_nodes_ready/1,
         wait_until_pingable/1,
         wait_until_ready/1,
         wait_until_registered/2,
         wait_until_ring_converged/1,
         wait_until_status_ready/1,
         wait_until_transfers_complete/1,
         wait_until_unpingable/1,
         whats_up/0,
         which/1,
         brutal_kill/1
        ]).

-define(HARNESS, (rt:config(rt_harness))).

%% @doc Return the home directory of the riak_test script.
-spec home_dir() -> file:filename().
home_dir() ->
    filename:dirname(filename:absname(escript:script_name())).

%% @doc gets riak deps from the appropriate harness
-spec get_deps() -> list().
get_deps() -> ?HARNESS:get_deps().

%% @doc if String contains Substr, return true.
-spec str(string(), string()) -> boolean().
str(String, Substr) ->
    case string:str(String, Substr) of
        0 -> false;
        _ -> true
    end.

%% @doc Get the value of an OS Environment variable. The arity 1 version of
%%      this function will fail the test if it is undefined.
get_os_env(Var) ->
    case get_os_env(Var, undefined) of
        undefined ->
            lager:error("ENV['~s'] is not defined", [Var]),
            ?assert(false);
        Value -> Value
    end.

%% @doc Get the value of an OS Evironment variable. The arity 2 version of
%%      this function will return the Default if the OS var is undefined.
get_os_env(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Value
    end.

%% @doc Wrap 'which' to give a good output if something is not installed
which(Command) ->
    lager:info("Checking for presence of ~s", [Command]),
    Cmd = lists:flatten(io_lib:format("which ~s; echo $?", [Command])),
    case rt:str(os:cmd(Cmd), "0") of
        false ->
            lager:warning("`~s` is not installed", [Command]),
            false;
        true ->
            true
    end.

download(Url) ->
    lager:info("Downloading ~s", [Url]),
    Filename = url_to_filename(Url),
    case filelib:is_file(filename:join(rt:config(rt_scratch_dir), Filename))  of
        true ->
            lager:info("Got it ~p", [Filename]),
            ok;
        _ ->
            lager:info("Getting it ~p", [Filename]),
            rt:stream_cmd("curl  -O -L " ++ Url, [{cd, rt:config(rt_scratch_dir)}])
    end.

url_to_filename(Url) ->
    lists:last(string:tokens(Url, "/")).
%% @doc like rt:which, but asserts on failure
assert_which(Command) ->
    ?assert(rt:which(Command)).

%% @doc checks if Command is installed and runs InstallCommand if not
%% ex:  rt:install_on_absence("bundler", "gem install bundler --no-rdoc --no-ri"),
install_on_absence(Command, InstallCommand) ->
    case rt:which(Command) of
        false ->
            lager:info("Attempting to install `~s` with command `~s`", [Command, InstallCommand]),
            ?assertCmd(InstallCommand);
        _True ->
            ok
    end.


%% @doc Rewrite the given node's app.config file, overriding the varialbes
%%      in the existing app.config with those in `Config'.
update_app_config(all, Config) ->
    ?HARNESS:update_app_config(all, Config);
update_app_config(Node, Config) ->
    stop(Node),
    ?assertEqual(ok, rt:wait_until_unpingable(Node)),
    ?HARNESS:update_app_config(Node, Config),
    start(Node).

%% @doc Helper that returns first successful application get_env result,
%%      used when different versions of Riak use different app vars for
%%      the same setting.
rpc_get_env(_, []) ->
    undefined;
rpc_get_env(Node, [{App,Var}|Others]) ->
    case rpc:call(Node, application, get_env, [App, Var]) of
        {ok, Value} ->
            {ok, Value};
        _ ->
            rpc_get_env(Node, Others)
    end.

-type interface() :: {http, tuple()} | {pb, tuple()}.
-type interfaces() :: [interface()].
-type conn_info() :: [{node(), interfaces()}].

-spec connection_info([node()]) -> conn_info().
connection_info(Nodes) ->
    [begin
         {ok, [{PB_IP, PB_Port}]} = get_pb_conn_info(Node),
         {ok, [{HTTP_IP, HTTP_Port}]} =
             rpc:call(Node, application, get_env, [riak_core, http]),
         {Node, [{http, {HTTP_IP, HTTP_Port}}, {pb, {PB_IP, PB_Port}}]}
     end || Node <- Nodes].

-spec get_pb_conn_info(node()) -> [{inet:ip_address(), pos_integer()}].
get_pb_conn_info(Node) ->
    case rpc_get_env(Node, [{riak_api, pb},
                            {riak_api, pb_ip},
                            {riak_kv, pb_ip}]) of
        {ok, [{NewIP, NewPort}|_]} ->
            {ok, [{NewIP, NewPort}]};
        {ok, PB_IP} ->
            {ok, PB_Port} = rpc_get_env(Node, [{riak_api, pb_port},
                                               {riak_kv, pb_port}]),
            {ok, [{PB_IP, PB_Port}]};
        _ ->
            undefined
    end.

%% @doc Deploy a set of freshly installed Riak nodes, returning a list of the
%%      nodes deployed.
%% @todo Re-add -spec after adding multi-version support
deploy_nodes(Versions) when is_list(Versions) ->
    deploy_nodes(Versions, [riak_kv]);
deploy_nodes(NumNodes) when is_integer(NumNodes) ->
    deploy_nodes([ current || _ <- lists:seq(1, NumNodes)]).

%% @doc Deploy a set of freshly installed Riak nodes with the given
%%      `InitialConfig', returning a list of the nodes deployed.
-spec deploy_nodes(NumNodes :: integer(), any()) -> [node()].
deploy_nodes(NumNodes, InitialConfig) when is_integer(NumNodes) ->
    NodeConfig = [{current, InitialConfig} || _ <- lists:seq(1,NumNodes)],
    deploy_nodes(NodeConfig);
deploy_nodes(Versions, Services) ->
    NodeConfig = [ version_to_config(Version) || Version <- Versions ],
    Nodes = ?HARNESS:deploy_nodes(NodeConfig),
    lager:info("Waiting for services ~p to start on ~p.", [Services, Nodes]),
    [ ok = wait_for_service(Node, Service) || Node <- Nodes, Service <- Services ],
    Nodes.

version_to_config({_, _}=Config) -> Config;
version_to_config(Version) -> {Version, default}.

%% @doc Start the specified Riak node
start(Node) ->
    ?HARNESS:start(Node).

%% @doc Start the specified Riak `Node' and wait for it to be pingable
start_and_wait(Node) ->
    start(Node),
    ?assertEqual(ok, wait_until_pingable(Node)).

async_start(Node) ->
    spawn(fun() -> start(Node) end).

%% @doc Stop the specified Riak `Node'.
stop(Node) ->
    lager:info("Stopping riak on ~p", [Node]),
    timer:sleep(10000), %% I know, I know!
    ?HARNESS:stop(Node).
    %%rpc:call(Node, init, stop, []).

%% @doc Stop the specified Riak `Node' and wait until it is not pingable
stop_and_wait(Node) ->
    stop(Node),
    ?assertEqual(ok, wait_until_unpingable(Node)).

%% @doc Upgrade a Riak `Node' to a specific version
upgrade(Node, NewVersion) ->
    ?HARNESS:upgrade(Node, NewVersion).

%% @doc Upgrade a Riak node to a specific version using the alternate
%%      leave/upgrade/rejoin approach
slow_upgrade(Node, NewVersion, Nodes) ->
    lager:info("Perform leave/upgrade/join upgrade on ~p", [Node]),
    lager:info("Leaving ~p", [Node]),
    leave(Node),
    ?assertEqual(ok, rt:wait_until_unpingable(Node)),
    upgrade(Node, NewVersion),
    lager:info("Rejoin ~p", [Node]),
    join(Node, hd(Nodes -- [Node])),
    lager:info("Wait until all nodes are ready and there are no pending changes"),
    ?assertEqual(ok, wait_until_nodes_ready(Nodes)),
    ?assertEqual(ok, wait_until_no_pending_changes(Nodes)),
    ok.

%% @doc Have `Node' send a join request to `PNode'
join(Node, PNode) ->
    R = try_join(Node, PNode),
    lager:info("[join] ~p to (~p): ~p", [Node, PNode, R]),
    ?assertEqual(ok, R),
    ok.

%% @doc try_join tries different rpc:calls to join a node, because the module changed
%%      in riak 1.2.0
try_join(Node, PNode) ->
    case rpc:call(Node, riak_core, join, [PNode]) of
        {badrpc, _} ->
            rpc:call(Node, riak, join, [PNode]);
        Result ->
            Result
    end.

%% @doc Have the `Node' leave the cluster
leave(Node) ->
    R = try_leave(Node),
    lager:info("[leave] ~p: ~p", [Node, R]),
    ?assertEqual(ok, R),
    ok.

%% @doc try_leave tries different rpc:calls to leave a `Node', because the module changed
%%      in riak 1.2.0
try_leave(Node) ->
    case rpc:call(Node, riak_core, leave, []) of
        {badrpc, _} ->
            rpc:call(Node, riak_kv_console, leave, [[]]),
            ok;
        Result ->
            Result
    end.

%% @doc Have `Node' remove `OtherNode' from the cluster
remove(Node, OtherNode) ->
    ?assertEqual(ok,
                 rpc:call(Node, riak_kv_console, remove, [[atom_to_list(OtherNode)]])).

%% @doc Have `Node' mark `OtherNode' as down
down(Node, OtherNode) ->
    rpc:call(Node, riak_kv_console, down, [[atom_to_list(OtherNode)]]).

%% @doc partition the `P1' from `P2' nodes
%%      note: the nodes remained connected to riak_test@local,
%%      which is how `heal/1' can still work.
partition(P1, P2) ->
    OldCookie = rpc:call(hd(P1), erlang, get_cookie, []),
    NewCookie = list_to_atom(lists:reverse(atom_to_list(OldCookie))),
    [true = rpc:call(N, erlang, set_cookie, [N, NewCookie]) || N <- P1],
    [[true = rpc:call(N, erlang, disconnect_node, [P2N]) || N <- P1] || P2N <- P2],
    {NewCookie, OldCookie, P1, P2}.

%% @doc heal the partition created by call to `partition/2'
%%      `OldCookie' is the original shared cookie
heal({_NewCookie, OldCookie, P1, P2}) ->
    Cluster = P1 ++ P2,
    % set OldCookie on P1 Nodes
    [true = rpc:call(N, erlang, set_cookie, [N, OldCookie]) || N <- P1],
    wait_until_connected(Cluster),
    {_GN, []} = rpc:sbcast(Cluster, riak_core_node_watcher, broadcast),
    ok.

%% @doc Spawn `Cmd' on the machine running the test harness
spawn_cmd(Cmd) ->
    ?HARNESS:spawn_cmd(Cmd).

%% @doc Spawn `Cmd' on the machine running the test harness
spawn_cmd(Cmd, Opts) ->
    ?HARNESS:spawn_cmd(Cmd, Opts).

%% @doc Wait for a command spawned by `spawn_cmd', returning
%%      the exit status and result
wait_for_cmd(CmdHandle) ->
    ?HARNESS:wait_for_cmd(CmdHandle).

%% @doc Spawn `Cmd' on the machine running the test harness, returning
%%      the exit status and result
cmd(Cmd) ->
    ?HARNESS:cmd(Cmd).

%% @doc Spawn `Cmd' on the machine running the test harness, returning
%%      the exit status and result
cmd(Cmd, Opts) ->
    ?HARNESS:cmd(Cmd, Opts).

%% @doc pretty much the same as os:cmd/1 but it will stream the output to lager.
%%      If you're running a long running command, it will dump the output
%%      once per second, as to not create the impression that nothing is happening.
-spec stream_cmd(string()) -> {integer(), string()}.
stream_cmd(Cmd) ->
    Port = open_port({spawn, binary_to_list(iolist_to_binary(Cmd))}, [stream, stderr_to_stdout, exit_status]),
    stream_cmd_loop(Port, "", "", now()).

%% @doc same as rt:stream_cmd/1, but with options, like open_port/2
-spec stream_cmd(string(), string()) -> {integer(), string()}.
stream_cmd(Cmd, Opts) ->
    Port = open_port({spawn, binary_to_list(iolist_to_binary(Cmd))}, [stream, stderr_to_stdout, exit_status] ++ Opts),
    stream_cmd_loop(Port, "", "", now()).

stream_cmd_loop(Port, Buffer, NewLineBuffer, Time={_MegaSecs, Secs, _MicroSecs}) ->
    receive
        {Port, {data, Data}} ->
            {_, Now, _} = now(),
            NewNewLineBuffer = case Now > Secs of
                true ->
                    lager:info(NewLineBuffer),
                    "";
                _ ->
                    NewLineBuffer
            end,
            case rt:str(Data, "\n") of
                true ->
                    lager:info(NewNewLineBuffer),
                    Tokens = string:tokens(Data, "\n"),
                    [ lager:info(Token) || Token <- Tokens ],
                    stream_cmd_loop(Port, Buffer ++ NewNewLineBuffer ++ Data, "", Time);
                _ ->
                    stream_cmd_loop(Port, Buffer, NewNewLineBuffer ++ Data, now())
            end;
        {Port, {exit_status, Status}} ->
            catch port_close(Port),
            {Status, Buffer}
    after rt:config(rt_max_wait_time) ->
            {-1, Buffer}
    end.
%%%===================================================================
%%% Remote code management
%%%===================================================================
load_modules_on_nodes([], Nodes)
  when is_list(Nodes) ->
    ok;
load_modules_on_nodes([Module | MoreModules], Nodes)
  when is_list(Nodes) ->
    case code:get_object_code(Module) of
        {Module, Bin, File} ->
            {_, []} = rpc:multicall(Nodes, code, load_binary, [Module, File, Bin]);
        error ->
            error(lists:flatten(io_lib:format("unable to get_object_code(~s)", [Module])))
    end,
    load_modules_on_nodes(MoreModules, Nodes).


%%%===================================================================
%%% Status / Wait Functions
%%%===================================================================

%% @doc Is the `Node' up according to net_adm:ping
is_pingable(Node) ->
    net_adm:ping(Node) =:= pong.

is_mixed_cluster(Nodes) when is_list(Nodes) ->
    %% If the nodes are bad, we don't care what version they are
    {Versions, _BadNodes} = rpc:multicall(Nodes, init, script_id, [], rt:config(rt_max_wait_time)),
    length(lists:usort(Versions)) > 1;
is_mixed_cluster(Node) ->
    Nodes = rpc:call(Node, erlang, nodes, []),
    is_mixed_cluster(Nodes).

%% @private
is_ready(Node) ->
    case rpc:call(Node, riak_core_ring_manager, get_raw_ring, []) of
        {ok, Ring} ->
            lists:member(Node, riak_core_ring:ready_members(Ring));
        _ ->
            false
    end.

%% @private
is_ring_ready(Node) ->
    case rpc:call(Node, riak_core_ring_manager, get_raw_ring, []) of
        {ok, Ring} ->
            riak_core_ring:ring_ready(Ring);
        _ ->
            false
    end.

%% @doc Utility function used to construct test predicates. Retries the
%%      function `Fun' until it returns `true', or until the maximum
%%      number of retries is reached. The retry limit is based on the
%%      provided `rt_max_wait_time' and `rt_retry_delay' parameters in
%%      specified `riak_test' config file.
wait_until(Node, Fun) ->
    wait_until(Node, Fun, fun(_N) -> fail end).

wait_until(Node, Fun, TimeoutFun) ->
    MaxTime = rt:config(rt_max_wait_time),
    Delay = rt:config(rt_retry_delay),
    Retry = MaxTime div Delay,
    wait_until(Node, Fun, Retry, Delay, TimeoutFun).

%% @deprecated Use {@link wait_until/2} instead.
wait_until(Node, Fun, Retry, Delay, TimeoutFun) ->
    Pass = Fun(Node),
    case {Retry, Pass} of
        {_, true} ->
            ok;
        {0, _} ->
            TimeoutFun(Node);
        _ ->
            timer:sleep(Delay),
            wait_until(Node, Fun, Retry-1, Delay, TimeoutFun)
    end.

%% @doc Wait until the specified node is considered ready by `riak_core'.
%%      As of Riak 1.0, a node is ready if it is in the `valid' or `leaving'
%%      states. A ready node is guaranteed to have current preflist/ownership
%%      information.
wait_until_ready(Node) ->
    lager:info("Wait until ~p ready", [Node]),
    ?assertEqual(ok, wait_until(Node, fun is_ready/1)),
    ok.

%% @doc Wait until status can be read from riak_kv_console
wait_until_status_ready(Node) ->
    lager:info("Wait until status ready in ~p", [Node]),
    ?assertEqual(ok, wait_until(Node,
                                fun(_) ->
                                        case rpc:call(Node, riak_kv_console, status, [[]]) of
                                            ok ->
                                                true;
                                            _ ->
                                                false
                                        end
                                end)).

%% @doc Given a list of nodes, wait until all nodes believe there are no
%% on-going or pending ownership transfers.
-spec wait_until_no_pending_changes([node()]) -> ok | fail.
wait_until_no_pending_changes(Nodes0) ->
    lager:info("Wait until no pending changes on ~p", [Nodes0]),
    F = fun(Nodes) ->
                rpc:multicall(Nodes, riak_core_vnode_manager, force_handoffs, []),
                {Rings, BadNodes} = rpc:multicall(Nodes, riak_core_ring_manager, get_raw_ring, []),
                Changes = [ riak_core_ring:pending_changes(Ring) =:= [] || {ok, Ring} <- Rings ],
                BadNodes =:= [] andalso length(Changes) =:= length(Nodes) andalso lists:all(fun(T) -> T end, Changes)
        end,
    ?assertEqual(ok, wait_until(Nodes0, F)),
    ok.

%% @doc Waits until no transfers are in-flight or pending, checked by
%% riak_core_status:transfers().
-spec wait_until_transfers_complete([node()]) -> ok | fail.
wait_until_transfers_complete([Node0|_]) ->
    lager:info("Wait until transfers complete ~p", [Node0]),
    F = fun(Node) ->
                {DownNodes, Transfers} = rpc:call(Node, riak_core_status, transfers, []),
                DownNodes =:= [] andalso Transfers =:= []
        end,
    ?assertEqual(ok, wait_until(Node0, F)),
    ok.

wait_for_service(Node, Services) when is_list(Services) ->
    F = fun(N) ->
                case rpc:call(N, riak_core_node_watcher, services, [N]) of
                    {badrpc, _Error} ->
                        false;
                    CurrServices when is_list(CurrServices) ->
                        lists:all(fun(Service) -> lists:member(Service, CurrServices) end, Services);
                    _ ->
                        false
                end
        end,
    ?assertEqual(ok, wait_until(Node, F)),
    ok;
wait_for_service(Node, Service) ->
    wait_for_service(Node, [Service]).

wait_for_cluster_service(Nodes, Service) ->
    lager:info("Wait for cluster service ~p in ~p", [Service, Nodes]),
    F = fun(N) ->
                UpNodes = rpc:call(N, riak_core_node_watcher, nodes, [Service]),
                (Nodes -- UpNodes) == []
        end,
    [?assertEqual(ok, wait_until(Node, F)) || Node <- Nodes],
    ok.

%% @doc Given a list of nodes, wait until all nodes are considered ready.
%%      See {@link wait_until_ready/1} for definition of ready.
wait_until_nodes_ready(Nodes) ->
    lager:info("Wait until nodes are ready : ~p", [Nodes]),
    [?assertEqual(ok, wait_until(Node, fun is_ready/1)) || Node <- Nodes],
    ok.

%% @doc Wait until all nodes in the list `Nodes' believe each other to be
%%      members of the cluster.
wait_until_all_members(Nodes) ->
    wait_until_all_members(Nodes, Nodes).

%% @doc Wait until all nodes in the list `Nodes' believes all nodes in the
%%      list `Members' are members of the cluster.
wait_until_all_members(Nodes, Members) ->
    lager:info("Wait until all members ~p ~p", [Nodes, Members]),
    S1 = ordsets:from_list(Members),
    F = fun(Node) ->
                S2 = ordsets:from_list(members_according_to(Node)),
                ordsets:is_subset(S1, S2)
        end,
    [?assertEqual(ok, wait_until(Node, F)) || Node <- Nodes],
    ok.

%% @doc Given a list of nodes, wait until all nodes believe the ring has
%%      converged (ie. `riak_core_ring:is_ready' returns `true').
wait_until_ring_converged(Nodes) ->
    lager:info("Wait until ring converged on ~p", [Nodes]),
    [?assertEqual(ok, wait_until(Node, fun is_ring_ready/1)) || Node <- Nodes],
    ok.

wait_until_legacy_ringready(Node) ->
    lager:info("Wait until legacy ring ready on ~p", [Node]),
    rt:wait_until(Node,
                  fun(_) ->
                          case rpc:call(Node, riak_kv_status, ringready, []) of
                              {ok, _Nodes} ->
                                  true;
                              _ ->
                                  false
                          end
                  end).

%% @doc wait until each node in Nodes is disterl connected to each.
wait_until_connected(Nodes) ->
    lager:info("Wait until connected ~p", [Nodes]),
    F = fun(Node) ->
                Connected = rpc:call(Node, erlang, nodes, []),
                lists:sort(Nodes) == lists:sort([Node]++Connected)--[node()]
        end,
    [?assertEqual(ok, wait_until(Node, F)) || Node <- Nodes],
    ok.

%% @doc Wait until the specified node is pingable
wait_until_pingable(Node) ->
    lager:info("Wait until ~p is pingable", [Node]),
    F = fun(N) ->
                net_adm:ping(N) =:= pong
        end,
    ?assertEqual(ok, wait_until(Node, F)),
    ok.

%% @doc Wait until the specified node is no longer pingable
wait_until_unpingable(Node) ->
    lager:info("Wait until ~p is not pingable", [Node]),
    _OSPidToKill = rpc:call(Node, os, getpid, []),
    F = fun(N) ->
                net_adm:ping(N) =:= pang
        end,
    TimeoutFun = fun(N) ->
                         lager:info("We tried it the easy way, but ~s wouldn't listen, so now it's 'kill -9' time", [N]),
                         lager:info("Not actually killing anything... long live `riak stop`"),
                         %%rpc:cast(N, os, cmd, [io_lib:format("kill -9 ~s", [OSPidToKill])]),
                         fail
        end,
    %% Hard coding a 6 minute timeout on this wait only. This function is called to see that
    %% riak has stopped. Riak stop should only take about 5 minutes before its timeouts kill
    %% the process. This wait should at least wait that long.
    Delay = rt:config(rt_retry_delay),
    Retry = 360000 div Delay,
    ?assertEqual(ok, wait_until(Node, F, Retry, Delay, TimeoutFun)),
    ok.


% Waits untill a certain regiestered name pops up on the remote node.
wait_until_registered(Node, Name) ->
    lager:info("Wait until the ring manager is up on ~p", [Node]),

    F = fun(_) ->
                Registered = rpc:call(Node, erlang, registered, []),
                lists:member(riak_core_ring_manager, Registered)
        end,
    TimeoutFun = fun(_) ->
                lager:info("The server with the namee ~p on ~p is not coming up.", [Name, Node]),
                fail
        end,
    Delay = rt:config(rt_retry_delay),
    Retry = 360000 div Delay,
    ?assertEqual(ok, wait_until(Node, F, Retry, Delay, TimeoutFun)),
    ok.

% when you just can't wait
brutal_kill(Node) ->
   lager:info("Killing node ~p", [Node]),
   OSPidToKill = rpc:call(Node, os, getpid, []),
   rpc:cast(Node, os, cmd, [io_lib:format("kill -9 ~s", [OSPidToKill])]),
   ok.

capability(Node, all) ->
    rpc:call(Node, riak_core_capability, all, []);
capability(Node, Capability) ->
    rpc:call(Node, riak_core_capability, get, [Capability]).

wait_until_capability(Node, Capability, Value) ->
    rt:wait_until(Node,
                  fun(_) ->
                          Value == capability(Node, Capability)
                  end).

wait_until_owners_according_to(Node, Nodes) ->
    SortedNodes = lists:usort(Nodes),
    F = fun(N) ->
        owners_according_to(N) =:= SortedNodes
    end,
    ?assertEqual(ok, wait_until(Node, F)),
    ok.

wait_until_nodes_agree_about_ownership(Nodes) ->
    lager:info("Wait until nodes agree about ownership ~p", [Nodes]),
    Results = [ wait_until_owners_according_to(Node, Nodes) || Node <- Nodes ],
    ?assert(lists:all(fun(X) -> ok =:= X end, Results)).

%%%===================================================================
%%% Ring Functions
%%%===================================================================

%% @doc Ensure that the specified node is a singleton node/cluster -- a node
%%      that owns 100% of the ring.
check_singleton_node(Node) ->
    lager:info("Check ~p is a singleton", [Node]),
    {ok, Ring} = rpc:call(Node, riak_core_ring_manager, get_raw_ring, []),
    Owners = lists:usort([Owner || {_Idx, Owner} <- riak_core_ring:all_owners(Ring)]),
    ?assertEqual([Node], Owners),
    ok.

%% @doc Get the raw ring for `Node'.
get_ring(Node) ->
    {ok, Ring} = rpc:call(Node, riak_core_ring_manager, get_raw_ring, []),
    Ring.

assert_nodes_agree_about_ownership(Nodes) ->
    ?assertEqual(ok, wait_until_ring_converged(Nodes)),
    ?assertEqual(ok, wait_until_all_members(Nodes)),
    [ ?assertEqual({Node, Nodes}, {Node, owners_according_to(Node)}) || Node <- Nodes].

%% @doc Return a list of nodes that own partitions according to the ring
%%      retrieved from the specified node.
owners_according_to(Node) ->
    {ok, Ring} = rpc:call(Node, riak_core_ring_manager, get_raw_ring, []),
    Owners = [Owner || {_Idx, Owner} <- riak_core_ring:all_owners(Ring)],
    lists:usort(Owners).

%% @doc Return a list of cluster members according to the ring retrieved from
%%      the specified node.
members_according_to(Node) ->
    {ok, Ring} = rpc:call(Node, riak_core_ring_manager, get_raw_ring, []),
    Members = riak_core_ring:all_members(Ring),
    Members.

%% @doc Return the cluster status of `Member' according to the ring
%%      retrieved from `Node'.
status_of_according_to(Member, Node) ->
    {ok, Ring} = rpc:call(Node, riak_core_ring_manager, get_raw_ring, []),
    Status = riak_core_ring:member_status(Ring, Member),
    Status.

%% @doc Return a list of nodes that own partitions according to the ring
%%      retrieved from the specified node.
claimant_according_to(Node) ->
    {ok, Ring} = rpc:call(Node, riak_core_ring_manager, get_raw_ring, []),
    Claimant = riak_core_ring:claimant(Ring),
    Claimant.

%%%===================================================================
%%% Cluster Utility Functions
%%%===================================================================

%% @doc Safely construct a new cluster and return a list of the deployed nodes
%% @todo Add -spec and update doc to reflect mult-version changes
build_cluster(Versions) when is_list(Versions) ->
    build_cluster(length(Versions), Versions, default);
build_cluster(NumNodes) ->
    build_cluster(NumNodes, default).

%% @doc Safely construct a `NumNode' size cluster using
%%      `InitialConfig'. Return a list of the deployed nodes.
build_cluster(NumNodes, InitialConfig) ->
    build_cluster(NumNodes, [], InitialConfig).

build_cluster(NumNodes, Versions, InitialConfig) ->
    %% Deploy a set of new nodes
    Nodes =
        case Versions of
            [] ->
                deploy_nodes(NumNodes, InitialConfig);
            _ ->
                deploy_nodes(Versions)
        end,

    %% Ensure each node owns 100% of it's own ring
    [?assertEqual([Node], owners_according_to(Node)) || Node <- Nodes],

    %% Join nodes
    [Node1|OtherNodes] = Nodes,
    [join(Node, Node1) || Node <- OtherNodes],

    ?assertEqual(ok, wait_until_nodes_ready(Nodes)),

    %% Ensure each node owns a portion of the ring
    wait_until_nodes_agree_about_ownership(Nodes),
    ?assertEqual(ok, wait_until_no_pending_changes(Nodes)),

    lager:info("Cluster built: ~p", [Nodes]),
    Nodes.

%% @doc Stop nodes and wipe out their data directories
clean_cluster(Nodes) when is_list(Nodes) ->
    [stop_and_wait(Node) || Node <- Nodes],
    clean_data_dir(Nodes).

clean_data_dir(Nodes) ->
    clean_data_dir(Nodes, "").

clean_data_dir(Nodes, SubDir) when not is_list(Nodes) ->
    clean_data_dir([Nodes], SubDir);
clean_data_dir(Nodes, SubDir) when is_list(Nodes) ->
    ?HARNESS:clean_data_dir(Nodes, SubDir).

%% @doc Shutdown every node, this is for after a test run is complete.
teardown() ->
    %% stop all connected nodes, 'cause it'll be faster that
    %%lager:info("RPC stopping these nodes ~p", [nodes()]),
    %%[ rt:stop(Node) || Node <- nodes()],
    %% Then do the more exhaustive harness thing, in case something was up
    %% but not connected.
    ?HARNESS:teardown().

versions() ->
    ?HARNESS:versions().
%%%===================================================================
%%% Basic Read/Write Functions
%%%===================================================================

systest_write(Node, Size) ->
    systest_write(Node, Size, 2).

systest_write(Node, Size, W) ->
    systest_write(Node, 1, Size, <<"systest">>, W).

%% @doc Write (End-Start)+1 objects to Node. Objects keys will be
%% `Start', `Start+1' ... `End', each encoded as a 32-bit binary
%% (`<<Key:32/integer>>'). Object values are the same as their keys.
%%
%% The return value of this function is a list of errors
%% encountered. If all writes were successful, return value is an
%% empty list. Each error has the form `{N :: integer(), Error :: term()}',
%% where N is the unencoded key of the object that failed to store.
systest_write(Node, Start, End, Bucket, W) ->
    rt:wait_for_service(Node, riak_kv),
    {ok, C} = riak:client_connect(Node),
    F = fun(N, Acc) ->
                Obj = riak_object:new(Bucket, <<N:32/integer>>, <<N:32/integer>>),
                try C:put(Obj, W) of
                    ok ->
                        Acc;
                    Other ->
                        [{N, Other} | Acc]
                catch
                    What:Why ->
                        [{N, {What, Why}} | Acc]
                end
        end,
    lists:foldl(F, [], lists:seq(Start, End)).

systest_read(Node, Size) ->
    systest_read(Node, Size, 2).

systest_read(Node, Size, R) ->
    systest_read(Node, 1, Size, <<"systest">>, R).

systest_read(Node, Start, End, Bucket, R) ->
    rt:wait_for_service(Node, riak_kv),
    {ok, C} = riak:client_connect(Node),
    F = fun(N, Acc) ->
                case C:get(Bucket, <<N:32/integer>>, R) of
                    {ok, Obj} ->
                        case riak_object:get_value(Obj) of
                            <<N:32/integer>> ->
                                Acc;
                            WrongVal ->
                                [{N, {wrong_val, WrongVal}} | Acc]
                        end;
                    Other ->
                        [{N, Other} | Acc]
                end
        end,
    lists:foldl(F, [], lists:seq(Start, End)).

%%%===================================================================
%%% PBC & HTTPC Functions
%%%===================================================================

%% @doc get me a protobuf client process and hold the mayo!
-spec pbc(node()) -> pid().
pbc(Node) ->
    rt:wait_for_service(Node, riak_kv),
    ConnInfo = proplists:get_value(Node, connection_info([Node])),
    {IP, PBPort} = proplists:get_value(pb, ConnInfo),
    {ok, Pid} = riakc_pb_socket:start_link(IP, PBPort, [{auto_reconnect, true}]),
    Pid.

%% @doc does a read via the erlang protobuf client
-spec pbc_read(pid(), binary(), binary()) -> binary().
pbc_read(Pid, Bucket, Key) ->
    {ok, Value} = riakc_pb_socket:get(Pid, Bucket, Key),
    Value.

%% @doc does a write via the erlang protobuf client
-spec pbc_write(pid(), binary(), binary(), binary()) -> atom().
pbc_write(Pid, Bucket, Key, Value) ->
    Object = riakc_obj:new(Bucket, Key, Value),
    riakc_pb_socket:put(Pid, Object).

%% @doc sets a bucket property/properties via the erlang protobuf client
-spec pbc_set_bucket_prop(pid(), binary(), [proplists:property()]) -> atom().
pbc_set_bucket_prop(Pid, Bucket, PropList) ->
    riakc_pb_socket:set_bucket(Pid, Bucket, PropList).

%% @doc Puts the contents of the given file into the given bucket using the
%% filename as a key and assuming a plain text content type.
pbc_put_file(Pid, Bucket, Key, Filename) ->
    {ok, Contents} = file:read_file(Filename),
    riakc_pb_socket:put(Pid, riakc_obj:new(Bucket, Key, Contents, "text/plain")).

%% @doc Puts all files in the given directory into the given bucket using the
%% filename as a key and assuming a plain text content type.
pbc_put_dir(Pid, Bucket, Dir) ->
    lager:info("Putting files from dir ~p into bucket ~p", [Dir, Bucket]),
    {ok, Files} = file:list_dir(Dir),
    [pbc_put_file(Pid, Bucket, list_to_binary(F), filename:join([Dir, F]))
     || F <- Files].

%% @doc Returns HTTP URL information for a list of Nodes
http_url(Nodes) when is_list(Nodes) ->
    [begin
         {Host, Port} = orddict:fetch(http, Connections),
         lists:flatten(io_lib:format("http://~s:~b", [Host, Port]))
     end || {_Node, Connections} <- connection_info(Nodes)];
http_url(Node) ->
    hd(http_url([Node])).

%% @doc get me an http client.
-spec httpc(node()) -> term().
httpc(Node) ->
    rt:wait_for_service(Node, riak_kv),
    {ok, [{IP, Port}|_]} = rpc:call(Node, application, get_env, [riak_core, http]),
    rhc:create(IP, Port, "riak", []).

%% @doc does a read via the http erlang client.
-spec httpc_read(term(), binary(), binary()) -> binary().
httpc_read(C, Bucket, Key) ->
    {_, Value} = rhc:get(C, Bucket, Key),
    Value.

%% @doc does a write via the http erlang client.
-spec httpc_write(term(), binary(), binary(), binary()) -> atom().
httpc_write(C, Bucket, Key, Value) ->
    Object = riakc_obj:new(Bucket, Key, Value),
    rhc:put(C, Object).

%%%===================================================================
%%% Command Line Functions
%%%===================================================================

%% @doc Call 'bin/riak-admin' command on `Node' with arguments `Args'
admin(Node, Args) ->
    ?HARNESS:admin(Node, Args).

%% @doc Call 'bin/riak' command on `Node' with arguments `Args'
riak(Node, Args) ->
    ?HARNESS:riak(Node, Args).

search_cmd(Node, Args) ->
    {ok, Cwd} = file:get_cwd(),
    rpc:call(Node, riak_search_cmd, command, [[Cwd | Args]]).

%% @doc Runs `riak attach' on a specific node, and tests for the expected behavoir.
%%      Here's an example: ```
%%      rt:attach(Node, [{expect, "erlang.pipe.1 \(^D to exit\)"},
%%                       {send, "riak_core_ring_manager:get_my_ring()."},
%%                       {expect, "dict,"},
%%                       {send, [4]}]), %% 4 = Ctrl + D'''
%%      `{expect, String}' scans the output for the existance of the String.
%%         These tuples are processed in order.
%%
%%      `{send, String}' sends the string to the console.
%%         Once a send is encountered, the buffer is discarded, and the next
%%         expect will process based on the output following the sent data.
%%
attach(Node, Expected) ->
    ?HARNESS:attach(Node, Expected).

%% @doc Runs `riak console' on a specific node
%% @see rt:attach/2
console(Node, Expected) ->
    ?HARNESS:console(Node, Expected).

%%%===================================================================
%%% Search
%%%===================================================================

%% doc Enable the search KV hook for the given `Bucket'.  Any `Node'
%%     in the cluster may be used as the change is propagated via the
%%     Ring.
enable_search_hook(Node, Bucket) when is_binary(Bucket) ->
    lager:info("Installing search hook for bucket ~p", [Bucket]),
    ?assertEqual(ok, rpc:call(Node, riak_search_kv_hook, install, [Bucket])).

%%%===================================================================
%%% Test harness setup, configuration, and internal utilities
%%%===================================================================

%% @doc Sets the backend of ALL nodes that could be available to riak_test.
%%      this is not limited to the nodes under test, but any node that
%%      riak_test is able to find. It then queries each available node
%%      for it's backend, and returns it if they're all equal. If different
%%      nodes have different backends, it returns a list of backends.
%%      Currently, there is no way to request multiple backends, so the
%%      list return type should be considered an error.
-spec set_backend(atom()) -> atom()|[atom()].
set_backend(bitcask) ->
    set_backend(riak_kv_bitcask_backend);
set_backend(eleveldb) ->
    set_backend(riak_kv_eleveldb_backend);
set_backend(memory) ->
    set_backend(riak_kv_memory_backend);
set_backend(Backend) when Backend == riak_kv_bitcask_backend; Backend == riak_kv_eleveldb_backend; Backend == riak_kv_memory_backend ->
    lager:info("rt:set_backend(~p)", [Backend]),
    ?HARNESS:set_backend(Backend);
set_backend(Other) ->
    lager:warning("rt:set_backend doesn't recognize ~p as a legit backend, using the default.", [Other]),
    ?HARNESS:get_backends().

%% @doc Gets the current version under test. In the case of an upgrade test
%%      or something like that, it's the version you're upgrading to.
-spec get_version() -> binary().
get_version() ->
    ?HARNESS:get_version().

%% @doc outputs some useful information about nodes that are up
whats_up() ->
    ?HARNESS:whats_up().

%% @doc Log a message to the console of the specified test nodes.
%%      Messages are prefixed by the string "---riak_test--- "
%%      Uses lager:info/1 'Fmt' semantics
log_to_nodes(Nodes, Fmt) ->
    log_to_nodes(Nodes, Fmt, []).

%% @doc Log a message to the console of the specified test nodes.
%%      Messages are prefixed by the string "---riak_test--- "
%%      Uses lager:info/2 'LFmt' and 'LArgs' semantics
log_to_nodes(Nodes, LFmt, LArgs) ->
    Module = lager,
    Function = log,
    Meta = [],
    Args = case LArgs of
               [] -> [info, Meta, "---riak_test--- " ++ LFmt];
               _  -> [info, Meta, "---riak_test--- " ++ LFmt, LArgs]
           end,
    [rpc:call(Node, Module, Function, Args) || Node <- Nodes].

%% @private utility function
pmap(F, L) ->
    Parent = self(),
    lists:foldl(
      fun(X, N) ->
              spawn(fun() ->
                            Parent ! {pmap, N, F(X)}
                    end),
              N+1
      end, 0, L),
    L2 = [receive {pmap, N, R} -> {N,R} end || _ <- L],
    {_, L3} = lists:unzip(lists:keysort(1, L2)),
    L3.

%% @private
setup_harness(Test, Args) ->
    ?HARNESS:setup_harness(Test, Args).

%% @private
load_config(undefined) ->
    load_dot_config("default");
load_config(ConfigName) ->
    case load_config_file(ConfigName) of
        ok -> ok;
        {error, enoent} -> load_dot_config(ConfigName)
    end.

%% @private
load_dot_config(ConfigName) ->
    case file:consult(filename:join([os:getenv("HOME"), ".riak_test.config"])) of
        {ok, Terms} ->
            %% First, set up the defaults
            case proplists:get_value(default, Terms) of
                undefined -> meh; %% No defaults set, move on.
                Default -> [set_config(Key, Value) || {Key, Value} <- Default]
            end,
            %% Now, overlay the specific project
            Config = proplists:get_value(list_to_atom(ConfigName), Terms),
            [set_config(Key, Value) || {Key, Value} <- Config],
            ok;
        {error, Reason} ->
            erlang:error("Failed to parse config file", ["~/.riak_test.config", Reason])
 end.

%% @private
load_config_file(File) ->
    case file:read_file_info(File) of
        {ok, _} ->
            io:format("*********************************************************************************~n"),
            io:format("WARNING! Use of config files is now deprecated, use ~~/.riak_test.config instead.~n"),
            io:format("*********************************************************************************~n"),
            io:format("Please acknowledge that you're aware that this functionality will be gone soon.~n"),
            Input = io:get_chars("[y/N] ", 1),
            case Input of
                "y" -> ok;
                "Y" -> ok;
                _ -> exit(1)
            end;
        _ -> meh
    end,
    case file:consult(File) of
        {ok, Terms} ->
            [set_config(Key, Value) || {Key, Value} <- Terms],
            ok;
        {error, enoent} ->
            {error, enoent};
        {error, Reason} ->
            erlang:error("Failed to parse config file", [File, Reason])
    end.

%% @private
set_config(Key, Value) ->
    ok = application:set_env(riak_test, Key, Value).

%% @private
config(Key) ->
    case kvc:path(Key, application:get_all_env(riak_test)) of
        [] -> erlang:error("Missing configuration key", [Key]);
        Value -> Value
    end.

%% @private
config(Key, Default) ->
    case kvc:path(Key, application:get_all_env(riak_test)) of
        [] -> Default;
        Value -> Value
    end.

-spec config_or_os_env(atom()) -> term().
config_or_os_env(Config) ->
    OSEnvVar = to_upper(atom_to_list(Config)),
    case {get_os_env(OSEnvVar, undefined), config(Config, undefined)} of
        {undefined, undefined} ->
            MSG = io_lib:format("Neither riak_test.~p nor ENV['~p'] are defined", [Config, OSEnvVar]),
            erlang:error(binary_to_list(iolist_to_binary(MSG)));
        {undefined, V} ->
            lager:info("Found riak_test.~s: ~s", [Config, V]),
            V;
        {V, _} ->
            lager:info("Found ENV[~s]: ~s", [OSEnvVar, V]),
            rt:set_config(Config, V),
            V
    end.

-spec config_or_os_env(atom(), term()) -> term().
config_or_os_env(Config, Default) ->
    OSEnvVar = to_upper(atom_to_list(Config)),
    case {get_os_env(OSEnvVar, undefined), config(Config, undefined)} of
        {undefined, undefined} -> Default;
        {undefined, V} ->
            lager:info("Found riak_test.~s: ~s", [Config, V]),
            V;
        {V, _} ->
            lager:info("Found ENV[~s]: ~s", [OSEnvVar, V]),
            rt:set_config(Config, V),
            V
    end.

to_upper(S) -> lists:map(fun char_to_upper/1, S).
char_to_upper(C) when C >= $a, C =< $z -> C bxor $\s;
char_to_upper(C) -> C.

%% @doc Downloads any extant log files from the harness's running
%%   nodes.
get_node_logs() ->
    ?HARNESS:get_node_logs().
