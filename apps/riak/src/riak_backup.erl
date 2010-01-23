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

%%% BACKUP %%%

%% @doc 
%% Connect to the cluster of which EntryNode is a member, 
%% read data from the cluster, and save the data in the specified file.
backup(EntryNode, Filename) -> 
    % Make sure we can reach the node...
    ensure_connected(EntryNode),

    % Get a list of nodes...
    {ok, Ring} = rpc:call(EntryNode, riak_ring_manager, get_my_ring, []),
    Members = riak_ring:all_members(Ring),

    % Print status...
    io:format("Backing up to '~s'.~n", [Filename]),
    io:format("...from ~p~n", [Members]),

    % Make sure all nodes in the cluster agree on the ring...
    ensure_synchronized(Ring, Members),

    % Backup the data...
    {ok, ?TABLE} = disk_log:open([{name, ?TABLE},
                                  {file, Filename},
                                  {mode, read_write},
                                  {type, halt}]),

    [backup_node(Node) || Node <- Members],
    ok = disk_log:sync(?TABLE),
    ok = disk_log:close(?TABLE),
    
    % Make sure the nodes are still synchronized...
    ensure_synchronized(Ring, Members),
    ok.
    
backup_node(Node) ->
    VNodes = gen_server:call({riak_vnode_master, Node}, all_possible_vnodes),
    [backup_vnode(VNode) ||  VNode <- VNodes].
    
backup_vnode(_VNode = {_Index, VNodePid}) ->
    {ok, List} = gen_fsm:sync_send_event(VNodePid, list, infinity),
    [backup_key(VNodePid, Bucket, Key) || {Bucket, Key} <- List].

backup_key(VNodePid, Bucket, Key) ->
    {ok, B} = gen_fsm:sync_send_event(VNodePid, {get_binary, {Bucket, Key}}, infinity),
    ok = disk_log:log(?TABLE, B).


%%% RESTORE %%%

%% @doc
%% Read data from the specified file created by backup/2,
%% and write it to the cluster of which EntryNode is a member.
restore(EntryNode, Filename) ->
    io:format("Restoring from '~s' to cluster to which '~s' belongs.~n", [Filename, EntryNode]),
    
    % Connect to the node...
    {ok, Client} = riak:client_connect(EntryNode),
    
    % Open the table, write it out, close the table...
    {ok, ?TABLE} = disk_log:open([{name, ?TABLE},
                                  {file, Filename},
                                  {mode, read_only},
                                   {type, halt}]),
    Count = traverse_backup(
                disk_log:chunk(?TABLE, start), 
                fun(Entry) -> read_and_restore_function(Client, Entry) end, 0),
    ok = disk_log:close(?TABLE),
    io:format("Restored ~p records.~n", [Count]),
    ok.

traverse_backup(eof, _VisitorFun, Count) ->
    Count;
traverse_backup({Cont, Terms}, VisitorFun, Count) when is_list(Terms) ->
    [VisitorFun(T) || T <- Terms],
    traverse_backup(disk_log:chunk(?TABLE, Cont), 
                    VisitorFun, Count+length(Terms)).
    

read_and_restore_function(Client, BinTerm) ->
    Obj = binary_to_term(BinTerm),
    Bucket = riak_object:bucket(Obj),
    Key = riak_object:key(Obj),
    % Data Cleaning...
    Obj1 = make_binary_bucket(Bucket, Key, Obj),

    % Use the existing metadata, and tell Riak not to 
    % update the X-Riak-VTag or X-Riak-Last-Modified values.
    MetaData = hd(riak_object:get_metadatas(Obj1)), 
    MetaData1 = dict:store("no_update", no_update, MetaData),
    Obj2 = riak_object:update_metadata(Obj1, MetaData1),
    
    % Store the object...
    Response = Client:put(Obj2,1,1,1200000),
    {continue, Response}.
   
%%% DATA CLEANING %%% 
    
%% If the bucket name is an atom, convert it to a binary...
make_binary_bucket(Bucket, Key, OriginalObj) when is_atom(Bucket) ->
    Bucket1 = list_to_binary(atom_to_list(Bucket)),
    OriginalContents = riak_object:get_contents(OriginalObj),
    OriginalVClock = riak_object:vclock(OriginalObj),

    % We can't change the bucket name without creating a new object...
    NewObj = riak_object:new(Bucket1, Key, placeholder),
    NewObj1 = riak_object:set_contents(NewObj, OriginalContents),
    _NewObj2 = riak_object:set_vclock(NewObj1, OriginalVClock);
    
%% If the bucket name is a binary, just pass it on through...
make_binary_bucket(Bucket, _Key, Obj) when is_binary(Bucket) -> Obj.

%% @private
%% Try to reach the specified node, throw exception on failure.
ensure_connected(Node) ->
    case net_adm:ping(Node) of
        pang -> throw({could_not_reach_node, Node});
        pong -> ok
    end.

%% @private
%% Make sure that rings of all members are synchronized, 
%% throw exception on failure.
ensure_synchronized(Ring, Members) ->
    F = fun(Node) ->
        {ok, Ring2} = rpc:call(Node, riak_ring_manager, get_my_ring, []),
        riak_ring:equal_rings(Ring, Ring2)
    end,
    case lists:all(F, Members) of
        true -> ok;
        false -> throw({nodes_not_synchronized, Members})
    end.

% pmap(Fun, List) ->
%     Workers = [spawn_worker(self(), Pred, Data) || X <- List],
%     [wait_result(Worker) || Worker <- Workers].
% 
% spawn_worker(Parent, Fun, Data) ->
%     erlang:spawn_monitor(fun() -> Parent ! {self(), Fun(Data)} end).
% 
% wait_result({Pid,Ref}) ->
%     receive
%         {'DOWN', Ref, _, _, normal} -> receive {Pid,Result} -> Result end;
%         {'DOWN', Ref, _, _, Reason} -> exit(Reason)
%     end.
