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

-module(riak_startup).
-export([initiate_cluster/0,join_cluster/1,rejoin/0,no_config/0]).

% functions called by the shell scripts used to start riak

join_cluster([IP, PortStr]) ->
    case check_deps() of
        ok ->
            Port = list_to_integer(PortStr),
            riak_doorbell:ring(IP, Port),
            timer:sleep(1000),
            riak_ring_gossiper:get_ring_from(hd(nodes()));
        X ->
            X
    end.

rejoin() ->
    case check_deps() of
        ok ->
            riak_ring_manager:prune_ringfiles(),
            riak_ring_manager:set_my_ring(
              riak_ring_manager:read_ringfile(
                riak_ring_manager:find_latest_ringfile()));
        X ->
            X
    end.

initiate_cluster() -> check_deps().

check_deps() ->
    % explicit list of external modules we should fail-fast on missing
    Deps = [vclock, chash, merkerl],
    Fails = [Fail || {Fail, {error,_}} <-
             [{Dep, code:ensure_loaded(Dep)} || Dep <- Deps]],
    case Fails of
        [] -> ok;
        _ ->
            application:stop(riak),
            {error, {failed, Fails}}
    end.
             
no_config() -> application:set_env(riak, no_config, true).

