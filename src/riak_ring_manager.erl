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

%% @doc this owns the local view of the cluster's ring configuration

-module(riak_ring_manager).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server2).
-export([start_link/0,start_link/1,stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([get_my_ring/0,set_my_ring/1,write_ringfile/0,prune_ringfiles/0,
        read_ringfile/1,find_latest_ringfile/0]).

start_link() -> gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(test) -> % when started this way, run a mock server (no disk, etc)
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [test], []).

%% @private
init([]) ->
    Ring = riak_ring:fresh(),
    ets:new(nodelocal_ring, [protected, named_table]),
    ets:insert(nodelocal_ring, {ring, Ring}),
    {ok, stateless_server};
init([test]) ->
    Ring = riak_ring:fresh(16,node()),
    ets:new(nodelocal_ring, [protected, named_table]),
    ets:insert(nodelocal_ring, {ring, Ring}),
    {ok, test}.

%% @spec get_my_ring() -> {ok, riak_ring:riak_ring()} | {error, Reason}
get_my_ring() ->
    case ets:match(nodelocal_ring, {ring, '$1'}) of
        [[Ring]] -> {ok, Ring};
        [] -> {error, no_ring}
    end.
%% @spec set_my_ring(riak_ring:riak_ring()) -> ok
set_my_ring(Ring) -> gen_server2:call(?MODULE, {set_my_ring, Ring}).
%% @spec write_ringfile() -> ok
write_ringfile() -> gen_server2:cast(?MODULE, write_ringfile).
%% @private (only used for test instances)
stop() -> gen_server2:cast(?MODULE, stop).

%% @private
handle_cast(stop, State) -> {stop,normal,State};

handle_cast(write_ringfile, test) -> {noreply,test};
handle_cast(write_ringfile, State) ->
    {ok, Ring} = get_my_ring(),
    spawn(fun() -> do_write_ringfile(Ring) end),
    {noreply,State}.

handle_info(_Info, State) -> {noreply, State}.

%% @private
handle_call({set_my_ring, Ring}, _From, State) -> 
    ets:insert(nodelocal_ring, {ring, Ring}),
    {reply,ok,State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

do_write_ringfile(Ring) ->
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:universal_time(),
    TS = io_lib:format(".~B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",
                       [Year, Month, Day, Hour, Minute, Second]),
    case riak:get_app_env(ring_state_dir) of
        "<nostore>" -> nop;
        Dir ->
            Cluster = riak:get_app_env(cluster_name),
            FN = Dir ++ "/riak_ring." ++ Cluster ++ TS,
            ok = filelib:ensure_dir(FN),
            riak_eventer:notify(riak_ring_manager, write_ringfile, iolist_to_binary(FN)),
            file:write_file(FN, term_to_binary(Ring))
    end.

%% @spec find_latest_ringfile() -> string()
find_latest_ringfile() ->
    Dir = riak:get_app_env(ring_state_dir),
    Cluster = riak:get_app_env(cluster_name),
    {ok, Filenames} = file:list_dir(Dir),
    Timestamps = [list_to_integer(TS) || {"riak_ring", C1, TS} <- 
                   [list_to_tuple(string:tokens(FN, ".")) || FN <- Filenames],
                                         C1 =:= Cluster],
    [Latest|_] = lists:reverse(lists:sort(Timestamps)),
    FN = Dir ++ "/riak_ring." ++ Cluster ++ "." ++ integer_to_list(Latest),
    FN.

%% @spec read_ringfile(string()) -> riak_ring:riak_ring()
read_ringfile(RingFile) ->
    {ok, Binary} = file:read_file(RingFile),
    riak_eventer:notify(riak_ring_manager, read_ringfile, RingFile),
    binary_to_term(Binary).

%% @spec prune_ringfiles() -> ok
prune_ringfiles() ->
    Dir = riak:get_app_env(ring_state_dir),
    Cluster = riak:get_app_env(cluster_name),
    {ok, Filenames} = file:list_dir(Dir),
    Timestamps = [TS || {"riak_ring", C1, TS} <- 
                   [list_to_tuple(string:tokens(FN, ".")) || FN <- Filenames],
                                         C1 =:= Cluster],
    TSPat = [io_lib:fread("~4d~2d~2d~2d~2d~2d",TS) || TS <- Timestamps],
    TSL = lists:reverse(lists:sort([TS || {ok,TS,[]} <- TSPat])),
    Keep = prune_list(TSL),
    KeepTSs = [lists:flatten(
                 io_lib:format("~B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",K))
                    || K <- Keep],
    DelFNs = [Dir ++ "/" ++ FN || FN <- Filenames, 
                 lists:all(fun(TS) -> string:str(FN,TS)=:=0 end, KeepTSs)],
    riak_eventer:notify(riak_ring_manager, prune_ringfiles,
                        {length(DelFNs),length(Timestamps)}),
    [file:delete(DelFN) || DelFN <- DelFNs],
    ok.

prune_list([X|Rest]) ->
    lists:usort(lists:append([[X],back(1,X,Rest),back(2,X,Rest),
                  back(3,X,Rest),back(4,X,Rest),back(5,X,Rest)])).
back(_N,_X,[]) -> [];
back(N,X,[H|T]) ->
    case lists:nth(N,X) =:= lists:nth(N,H) of
        true -> back(N,X,T);
        false -> [H]
    end.
