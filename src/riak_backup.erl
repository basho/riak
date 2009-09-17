%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.    

%% @doc Utilities for backup and restore of a riak cluster.
%%      Note that if you want to restore to exactly the contents of
%%      a dump, you should restore to an empty cluster.  Otherwise,
%%      restore will reconcile values with the existing data.

-module(riak_backup).

-export([dump_config/1,do_dump/1,restore_config/1,do_restore/1]).
-export([do_restore_mdbinary/1]).

%% @type dump_config_params() = list()
%% @spec dump_config(dump_config_params()) -> term()
%% @doc Configure/prep a node to perform backup for Cluster, using CookieStr.
%%      The argument is a list of the form
%%      [Cluster :atom(), CookieStr :: list()].
dump_config([Cluster, CookieStr]) ->
    RipConf = [{no_config, true}, {cluster_name, Cluster},
       {riak_cookie, list_to_atom(CookieStr)}, {ring_state_dir, "<nostore>"},
       {ring_creation_size, 12}, {gossip_interval, 1000},
       {wants_claim_fun, {riak_claim, never_wants_claim}},
       {doorbell_port, undefined}, {storage_backend, undefined}],
    backup_config(RipConf).

%% @type restore_config_params() = list()
%% @spec restore_config(restore_config_params()) -> term()
%% @doc Configure/prep a node to perform restore for Cluster, using CookieStr.
%%      The argument is a list of the form
%%      [Cluster :: atom(), CookieStr :: list()].
restore_config([Cluster, CookieStr]) ->
    TempDir = make_tmp_dir(),
    RipConf = [{no_config, true}, {cluster_name, Cluster},
       {riak_cookie, list_to_atom(CookieStr)}, {ring_state_dir, "<nostore>"},
       {ring_creation_size, 12}, {gossip_interval, 60000},
       {wants_claim_fun, {riak_claim, never_wants_claim}},
       {riak_web_ip, "undefined"}, {doorbell_port, undefined},{backup, true},
       {riak_fs_backend_root, filename:join(TempDir, "storage")},
       {storage_backend, undefined}],
    backup_config(RipConf).

%% @private
backup_config(RipConf) ->
    application:stop(sasl),
    application:unload(sasl),
    ok = application:load({application,sasl,[{errlog_type,error}]}),
    ok = application:start(sasl),
    [application:set_env(riak,K,V) || {K,V} <- RipConf].

%% @type dump_params() = list()
%% @spec do_dump(dump_params()) -> ok
%% @doc Connect to the cluster via IP:PortStr, and make a dumpfile at Filename.
%%      The argument is a list of the form
%%      [IP :: list(), PortStr :: list(), Filename :: list()].
do_dump([IP, PortStr, Filename]) ->
    ReqID = erlang:phash2({random:uniform(), self()}),
    io:format("starting dump ID ~p~n", [ReqID]),
    riak_startup:join_cluster([IP, PortStr]),
    All_I_VN = lists:flatten(
          [gen_server:call({riak_vnode_master, Node},all_possible_vnodes) ||
                  Node <- nodes()]),
    IV_Lists = [{I, VN, gen_server2:call(VN,list)} || {I,VN} <- All_I_VN],
    {ok, dumptable} = dets:open_file(dumptable, [{file, Filename}]),
    dump_records(IV_Lists),
    ok = dets:sync(dumptable),
    ok = dets:close(dumptable),
    io:format("dump ID ~p stored to ~p~n", [ReqID,Filename]),
    ok.

%% @private
dump_records([]) -> ok;
dump_records([{_I,VN,List}|IVL_Tail]) ->
    dump_records1(VN,List),
    dump_records(IVL_Tail).

%% @private
dump_records1(_VN,[]) -> ok;
dump_records1(VN,[K|K_Tail]) ->
    {ok, V} = gen_server2:call(VN, {get_binary, K}),
    Obj = binary_to_term(V),
    Bucket = riak_object:bucket(Obj),
    Key = riak_object:key(Obj),
    ok = dets:insert(dumptable, [{{Bucket,Key}, V}]),
    dump_records1(VN,K_Tail).

%% @type restore_params() = list()
%% @spec do_restore(restore_params()) -> ok
%% @doc Connect via IP:PortStr / Cookie, and restore using dump at Filename.
%%      Note that this reconciles instead of blindly overwriting.
%%      The argument is a list of the form
%%      [IP :: list(), PortStr :: list(), Cookie :: list(), Filename :: list()].
do_restore([IP, PortStr, Cookie, Filename]) ->
    ReqID = erlang:phash2({random:uniform(), self()}),
    io:format("starting restore ID ~p~n", [ReqID]),
    {ok, r_table} = dets:open_file(r_table, [{file, Filename}]),
    {ok, Client} = riak:client_connect(IP,list_to_integer(PortStr),list_to_atom(Cookie)),
    Trav = dets:traverse(r_table,
      fun({{Bucket0,Key},V}) ->
              RObj00 = binary_to_term(V),
              {Bucket, RObj0} =
                  if is_binary(Bucket0) -> {Bucket0, RObj00};
                     is_atom(Bucket0) ->
                          BN = list_to_binary(atom_to_list(Bucket0)),
                          {BN,
                           riak_object:set_vclock(
                             riak_object:set_contents(
                               riak_object:new(BN, Key, backup_placeholder),
                               riak_object:get_contents(RObj00)),
                             riak_object:vclock(RObj00))}
                  end,
              RObj = riak_object:update_metadata(RObj0,
                       dict:store("no_update",no_update,
                         riak_object:get_update_metadata(RObj0))),
              PutRes = Client:put(RObj,1,1,900000),
              {continue, {Bucket,Key,PutRes}}
      end),
    ok = dets:close(r_table),
    io:format("restore ID ~p completed with ~p objects.~n",
              [ReqID,length(Trav)]),
    ok.

%% @private
make_tmp_dir() ->
    TmpId = io_lib:format("riptemp.~p",
                          [erlang:phash2({random:uniform(),self()})]),
    TempDir = filename:join("/tmp", TmpId),
    case filelib:is_dir(TempDir) of
        true -> make_tmp_dir();
        false ->
            ok = file:make_dir(TempDir),
            TempDir
    end.

do_restore_mdbinary(Filename) ->
    {ok, r_table} = dets:open_file(r_table, [{file, Filename}]),
    {ok, Client} = riak:local_client(),
    Trav = dets:traverse(r_table,
      fun({{Bucket0,Key},V}) ->
              RObj00 = binary_to_term(V),
              {Bucket, RObj0} =
                  if is_binary(Bucket0) -> {Bucket0, RObj00};
                     is_atom(Bucket0) ->
                          BN = list_to_binary(atom_to_list(Bucket0)),
                          {BN,
                           riak_object:set_vclock(
                             riak_object:set_contents(
                               riak_object:new(BN, Key, backup_placeholder),
                               riak_object:get_contents(RObj00)),
                             riak_object:vclock(RObj00))}
                  end,
              MD0 = dict:store("no_update",no_update, 
                               riak_object:get_update_metadata(RObj0)),
              {ObjMD,_} = hd(riak_object:get_contents(RObj0)),
              MD1 = case dict:find("X-Riak-VTag", ObjMD) of
                  {ok,VTag} -> dict:store(<<"X-Riak-VTag">>,VTag,MD0);
                  error -> MD0
              end,
              MD2 = case dict:find("X-Riak-Last-Modified", ObjMD) of
                  {ok,LM} -> dict:store(<<"X-Riak-Last-Modified">>,LM,MD1);
                  error -> MD1
              end,
              RObj = riak_object:update_metadata(RObj0,MD2),
              PutRes = Client:put(RObj,1,1,900000),
              {continue, {Bucket,Key,PutRes}}
      end),
    ok = dets:close(r_table),
    length(Trav).
