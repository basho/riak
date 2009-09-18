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

-export ([backup/2, restore/2]).

-define (TABLE, riak_backup_table).

%% @doc 
%% Connect to the cluster of which EntryNode is a member, 
%% read data from the cluster, and save the data in the specified file.
backup(EntryNode, Filename) -> throw(not_yet_supported).
    % Make sure we can reach the node...
    ensure_connected(EntryNode),

    % Get a list of nodes...
    Ring = rpc:call(EntryNode, riak_ring_manager, get_my_ring, []),
    Members = riak_ring:all_members(Ring),

    % Make sure all nodes in the cluster agree on the ring...
    ensure_synchronized(Ring, Members),
    
    % Backup the data...
    {ok, backup_table} = dets:open_file(?TABLE, [{file, Filename}]),
    [backup_node(Node) || Node <- Nodes],
    ok = dets:sync(?TABLE),
    ok = dets:close(?TABLE),
    
    % Make sure the nodes are still synchronized...
    ensure_synchronized(Ring, Members),
    ok.
    
backup_node(Node) ->
    VNodes = gen_server:call({riak_vnode_master, Node}, all_possible_vnodes),
    [backup_vnode(VNode) ||  VNode <- VNodes].
    
backup_vnode(_VNode = {_Index, VNodePid}) ->
    Keys = gen_server2:call(VN,list),
    [backup_key(VNodePid, Key) || Key <- Keys].

backup_key(VNodePid, Key) ->
    {ok, V} = gen_server2:call(VNodePid, {get_binary, K}),
    Obj = binary_to_term(V),
    Bucket = riak_object:bucket(Obj),
    Key = riak_object:key(Obj),
    ok = dets:insert(?TABLE, [{{Bucket,Key}, V}]).




%% @doc
%% Read data from the specified file created by backup/2,
%% and write it to the cluster of which EntryNode is a member.
restore(EntryNode, Filename) ->
    % Make sure we can reach the node...
    ensure_connected(EntryNode),
    
    {ok, r_table} = dets:open_file(r_table, [{file, Filename}]),
    {ok, Client} = riak:client_connect(Node),
    Trav = dets:traverse(r_table,
      fun({{Bucket,Key},V}) ->
              RObj0 = binary_to_term(V),
              RObj = riak_object:update_metadata(RObj0,
                       dict:store("no_update",no_update,
                         riak_object:get_update_metadata(RObj0))),
              PutRes = Client:put(RObj,1,1,900000),
              {continue, {Bucket,Key,PutRes}}
      end),
    ok = dets:close(r_table),
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
      fun({{Bucket,Key},V}) ->
              RObj0 = binary_to_term(V),
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





    
    
    










ensure_connected(Node) ->
    case net_adm:ping(Node) of
        pang -> throw({could_not_reach_node, Node}),
        pong -> ok
    end.

ensure_synchronized(Ring, Members) ->
    F = fun(Node) ->
        Ring2 = rpc:call(Node, riak_ring_manager, get_my_ring, []),
        (Ring#chstate.vclock == Ring2#chstate.vclock) andalso
        (Ring#chstate.chring == Ring2#chstate.chring) andalso
        (Ring#chstate.meta == Ring2#chstate.meta)
    end,
    case lists:all(F, Members) of
        true -> ok;
        false -> throw({nodes_not_synchronized, Members})
    end.

pmap(Fun, List) ->
    Workers = [spawn_worker(self(), Pred, Data) || X <- List],
    [wait_result(Worker) || Worker <- Workers].

spawn_worker(Parent, Fun, Data) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), Fun(Data)} end).

wait_result({Pid,Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid,Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.
