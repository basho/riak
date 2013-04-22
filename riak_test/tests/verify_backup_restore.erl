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

%% @doc Verifies the functionality of the riak-admin backup and restore
%% commands.  Restore re-puts the data store by backup.  Notice that this does
%% not mean the data is restored to what it was.  Newer data may prevail
%% depending on the configuration (last write wins, vector clocks used, etc).

-module(verify_backup_restore).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("riakc/include/riakc.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NUM_NODES, 4).
-define(NUM_KEYS, 1000).
-define(NUM_DEL, 100).
-define(NUM_MOD, 100).
-define(SEARCH_BUCKET, <<"search_bucket">>).

confirm() ->
    lager:info("Building cluster of ~p nodes", [?NUM_NODES]),
    SpamDir = rt:config_or_os_env(spam_dir),
    Config = [{riak_search, [{enabled, true}]}],
    [Node0 | _RestNodes] = Nodes = rt:build_cluster(?NUM_NODES, Config),
    rt:enable_search_hook(Node0, ?SEARCH_BUCKET),
    rt:wait_until_ring_converged(Nodes),
    PbcPid = rt:pbc(Node0),
    Searches =
        [
          {<<"ZiaSun">>, 1},
          {<<"headaches">>, 4},
          {<<"YALSP">>, 3},
          {<<"mister">>, 0},
          {<<"prohibiting">>, 5},
          {<<"mx.example.net">>, 187}
         ],
    EmptySearches = [ {Term, 0} || {Term, _Count} <- Searches],
    ConcatBin = fun({T, _},Acc) -> <<T/binary, " ", Acc/binary>> end,
    AllTerms = lists:foldl(ConcatBin, <<"">>, Searches),

    lager:info("Indexing data for search from ~p", [SpamDir]),
    rt:pbc_put_dir(PbcPid, ?SEARCH_BUCKET, SpamDir),
    ExtraKey = <<"Extra1">>, 
    riakc_pb_socket:put(PbcPid, 
                        riakc_obj:new(?SEARCH_BUCKET, 
                                      ExtraKey, 
                                      AllTerms)),

    lager:info("Writing some data to the cluster"),
    write_some(Node0, [{last, ?NUM_KEYS}]),

    lager:info("Verifying data made it in"),
    rt:wait_until_no_pending_changes(Nodes),
    verify_searches(PbcPid, Searches, 1),
    [?assertEqual([], read_some(Node, [{last, ?NUM_KEYS}])) || Node <- Nodes],

    BackupFile = filename:join([rt:config(rt_scratch_dir), "TestBackup.bak"]),
    case filelib:is_regular(BackupFile) of
        true ->
            lager:info("Deleting current backup file at ~p", [BackupFile]),
            ?assertMatch(ok, file:delete(BackupFile));
        _ -> ok
    end,

    lager:info("Backing up the data to ~p", [BackupFile]),
    Cookie = "riak",
    rt:admin(Node0, ["backup", atom_to_list(Node0), Cookie, BackupFile, "all"]),

    lager:info("Modifying data on cluster"),
    ModF = fun(N) ->
                   <<"MOD_V_", (i2b(N))/binary>>
           end,
    lager:info("Modifying another ~p keys (mods will persist after backup)",
               [?NUM_MOD]),
    write_some(Node0, [{last, ?NUM_MOD},
                       {vfun, ModF}]),
    lager:info("Deleting ~p keys", [?NUM_DEL+1]),
    ?assertEqual([], delete_some(Node0, [{first, ?NUM_MOD+1}, 
                                         {last, ?NUM_MOD+?NUM_DEL}])),
    lager:info("Deleting extra search doc"),
    riakc_pb_socket:delete(PbcPid, ?SEARCH_BUCKET, ExtraKey),
    rt:wait_until_no_pending_changes(Nodes),

    lager:info("Verifying data has changed"),
    [?assertEqual([], read_some(Node, [{last, ?NUM_MOD},
                                       {vfun, ModF}]))
     || Node <- Nodes],
    [?assertEqual([], read_some(Node, [{first, ?NUM_MOD+1},
                                       {last, ?NUM_MOD+?NUM_DEL},
                                       {expect, deleted}]))
     || Node <- Nodes],
    verify_searches(PbcPid, Searches, 0),

    lager:info("Restoring from backup ~p", [BackupFile]),
    rt:admin(Node0, ["restore", atom_to_list(Node0), Cookie, BackupFile]),
    rt:wait_until_no_pending_changes(Nodes),

    lager:info("Verifying that deleted data is back, mods are still in"),
    [?assertEqual([], read_some(Node, [{last, ?NUM_MOD},
                                       {vfun, ModF}]))
     || Node <- Nodes],
    [?assertEqual([], read_some(Node, [{first, ?NUM_MOD+1},
                                       {last, ?NUM_KEYS}]))
     || Node <- Nodes],

    lager:info("Verifying deleted search results are back"),
    verify_searches(PbcPid, Searches, 1),

    lager:info("Wipe out entire cluster and start fresh"),
    riakc_pb_socket:stop(PbcPid),
    rt:clean_cluster(Nodes),
    lager:info("Rebuilding the cluster"),
    rt:build_cluster(?NUM_NODES, Config),
    rt:enable_search_hook(Node0, ?SEARCH_BUCKET),
    rt:wait_until_ring_converged(Nodes),
    rt:wait_until_no_pending_changes(Nodes),
    PbcPid2 = rt:pbc(Node0),

    lager:info("Verify no data in cluster"),
    [?assertEqual([], read_some(Node, [{last, ?NUM_KEYS},
                                       {expect, deleted}]))
     || Node <- Nodes],
    verify_searches(PbcPid2, EmptySearches, 0),

    lager:info("Restoring from backup ~p again", [BackupFile]),
    rt:admin(Node0, ["restore", atom_to_list(Node0), Cookie, BackupFile]),
    rt:enable_search_hook(Node0, ?SEARCH_BUCKET),

    lager:info("Verifying data is back to original backup"),
    rt:wait_until_no_pending_changes(Nodes),
    verify_searches(PbcPid2, Searches, 1),
    [?assertEqual([], read_some(Node, [{last, ?NUM_KEYS}])) || Node <- Nodes],

    lager:info("C'est tout mon ami"),
    riakc_pb_socket:stop(PbcPid2),
    pass.


verify_searches(PbcPid, Searches, Offset) ->
    [verify_search_count(PbcPid, T, C + Offset) 
        || {T, C} <- Searches ].

i2b(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N)).

default_kfun(N) when is_integer(N) ->
    <<"K_",(i2b(N))/binary>>.

default_vfun(N) when is_integer(N) ->
    <<"V_",(i2b(N))/binary>>.

% @todo Maybe replace systest_write
write_some(Node, Props) ->
    Bucket = proplists:get_value(bucket, Props, <<"test_bucket">>),
    W = proplists:get_value(w, Props, 2),
    Start = proplists:get_value(first, Props, 0),
    End = proplists:get_value(last, Props, 1000),
    Del = proplists:get_value(delete, Props, false),
    KFun = proplists:get_value(kfun, Props, fun default_kfun/1),
    VFun = proplists:get_value(vfun, Props, fun default_vfun/1),

    {ok, C} = riak:client_connect(Node),
    F = fun(N, Acc) ->
                {K, V} = {KFun(N), VFun(N)},
                case Del of 
                    true -> C:delete(Bucket, K);
                    _ -> ok
                end,
                Obj = riak_object:new(Bucket, K, V),
                case C:put(Obj, W) of
                    ok ->
                        Acc;
                    Other ->
                        [{N, Other} | Acc]
                end
        end,
    lists:foldl(F, [], lists:seq(Start, End)).

% @todo Maybe replace systest_read
read_some(Node, Props) ->
    Bucket = proplists:get_value(bucket, Props, <<"test_bucket">>),
    R = proplists:get_value(r, Props, 2),
    Start = proplists:get_value(first, Props, 0),
    End = proplists:get_value(last, Props, 1000),
    Expect = proplists:get_value(expect, Props, exists),
    KFun = proplists:get_value(kfun, Props, fun default_kfun/1),
    VFun = proplists:get_value(vfun, Props, fun default_vfun/1),

    {ok, C} = riak:client_connect(Node),
    F =
        fun(N, Acc) ->
            K = KFun(N),
            case Expect of
                exists ->
                    case C:get(Bucket, K, R) of
                        {ok, Obj} ->
                            Val = VFun(N),
                            case riak_object:get_value(Obj) of
                                Val ->
                                    Acc;
                                WrongVal ->
                                    [{N, {wrong_val, WrongVal, expected, Val}}
                                    | Acc]
                            end;
                        Other ->
                            [{N, Other} | Acc]
                    end;
                deleted ->
                    case C:get(Bucket, K, R) of
                        {error, notfound} ->
                            Acc;
                        Other ->
                            [{N, {not_deleted, Other}} | Acc]
                    end
            end
        end,
    lists:foldl(F, [], lists:seq(Start, End)).

% @todo Maybe replace systest_read
delete_some(Node, Props) ->
    Bucket = proplists:get_value(bucket, Props, <<"test_bucket">>),
    Start = proplists:get_value(first, Props, 0),
    End = proplists:get_value(last, Props, 1000),
    KFun = proplists:get_value(kfun, Props, fun default_kfun/1),

    {ok, C} = riak:client_connect(Node),
    F =
        fun(N, Acc) ->
            K = KFun(N),
            case C:delete(Bucket, K) of
                ok -> Acc;
                _ -> [{N, {could_not_delete}} | Acc]
            end
        end,
    lists:foldl(F, [], lists:seq(Start, End)).

verify_search_count(Pid, SearchQuery, Count) ->
    {ok, #search_results{num_found=NumFound}} = riakc_pb_socket:search(Pid, ?SEARCH_BUCKET, SearchQuery),
    lager:info("Found ~p search results for query ~p", [NumFound, SearchQuery]),
    ?assertEqual(Count, NumFound).
