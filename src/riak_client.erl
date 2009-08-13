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

%% @doc The client object used for all access into the riak system.
%% @type riak_client() = term()

-module(riak_client, [Node,ClientId]).
-author('Justin Sheehy <justin@basho.com>').

-export([mapred/2,mapred/3]).
-export([get/3,get/4]).
-export([put/2,put/3,put/4]).
-export([delete/3,delete/4]).
-export([list_keys/1]).
-export([set_bucket/2,get_bucket/1]).
-export([reload_all/1]).
-export([remove_from_cluster/1]).
-export([send_event/2]).
%% @type default_timeout() = 15000
-define(DEFAULT_TIMEOUT, 15000).

%% @spec mapred(Inputs :: list(),
%%              Query :: [riak_mapreduce_fsm:mapred_queryterm()]) ->
%%       {ok, riak_mapreduce_fsm:mapred_result()} |
%%       {error, {bad_qterm, riak_mapreduce_fsm:mapred_queryterm()}} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job across the cluster.
%%      See the map/reduce documentation for explanation of behavior.
%% @equiv mapred(Inputs, Query, default_timeout())
mapred(Inputs,Query) -> mapred(Inputs,Query,?DEFAULT_TIMEOUT).

%% @spec mapred(Inputs :: list(),
%%              Query :: [riak_mapreduce_fsm:mapred_queryterm()],
%%              TimeoutMillisecs :: integer()) ->
%%       {ok, riak_mapreduce_fsm:mapred_result()} |
%%       {error, {bad_qterm, riak_mapreduce_fsm:mapred_queryterm()}} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job across the cluster.
%%      See the map/reduce documentation for explanation of behavior.
mapred(Inputs,Query,Timeout)
  when is_list(Inputs), is_list(Query), is_integer(Timeout) ->
    gen_server:call({riak_api,Node}, {mapred,Inputs,Query,Timeout}, Timeout).


%% @spec get(riak_object:bucket(), riak_object:key(), R :: integer()) ->
%%       {ok, riak_object:riak_object()} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Fetch the object at Bucket/Key.  Return a value as soon as R
%%      nodes have responded with a value or error.
%% @equiv get(Bucket, Key, R, default_timeout())
get(Bucket, Key, R) -> get(Bucket, Key, R, ?DEFAULT_TIMEOUT).

%% @spec get(riak_object:bucket(), riak_object:key(), R :: integer(),
%%           TimeoutMillisecs :: integer()) ->
%%       {ok, riak_object:riak_object()} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Fetch the object at Bucket/Key.  Return a value as soon as R
%%      nodes have responded with a value or error, or TimeoutMillisecs passes.
get(Bucket, Key, R, Timeout) when is_atom(Bucket),
                                  (is_list(Key) orelse is_binary(Key)),
                                  is_integer(R), is_integer(Timeout) ->
    gen_server:call({riak_api,Node}, {get,Bucket,Key,R,Timeout}, Timeout).


%% @spec put(RObj :: riak_object:riak_object(), W :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, timeout}
%% @doc Store RObj in the cluster.
%%      Return as soon as at least W nodes have received the request.
%% @equiv put(RObj, W, W, default_timeout())
put(RObj, W) -> put(RObj, W, W, ?DEFAULT_TIMEOUT).

%% @spec put(RObj::riak_object:riak_object(),W :: integer(),RW :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, timeout}
%% @doc Store RObj in the cluster.
%%      Return as soon as at least W nodes have received the request, and
%%      at least DW nodes have stored it in their storage backend.
%% @equiv put(Robj, W, DW, default_timeout())
put(RObj, W, DW) -> put(RObj, W, DW, ?DEFAULT_TIMEOUT).

%% @spec put(RObj::riak_object:riak_object(), W :: integer(), RW :: integer(),
%%           TimeoutMillisecs :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, timeout}
%% @doc Store RObj in the cluster.
%%      Return as soon as at least W nodes have received the request, and
%%      at least DW nodes have stored it in their storage backend, or
%%      TimeoutMillisecs passes.
put(RObj, W, DW, Timeout) ->
    R0 = riak_object:increment_vclock(RObj, ClientId),
    gen_server:call({riak_api,Node}, {put,R0,W,DW,Timeout}, Timeout).


%% @spec delete(riak_object:bucket(), riak_object:key(), RW :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as RW
%%      nodes have responded with a value or error.
%% @equiv delete(Bucket, Key, RW, default_timeout())
delete(Bucket,Key,RW) -> delete(Bucket,Key,RW,?DEFAULT_TIMEOUT).

%% @spec delete(riak_object:bucket(), riak_object:key(), RW :: integer(),
%%           TimeoutMillisecs :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as RW
%%      nodes have responded with a value or error, or TimeoutMillisecs passes.
delete(Bucket,Key,RW,Timeout) ->
    gen_server:call({riak_api,Node}, {delete,Bucket,Key,RW,Timeout}, Timeout).


%% @spec list_keys(riak_object:bucket()) ->
%%       {ok, [Key :: riak_object:key()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List the keys known to be present in Bucket.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after a put or delete.
list_keys(Bucket) -> 
    gen_server:call({riak_api,Node}, {list_keys,Bucket}, ?DEFAULT_TIMEOUT*4).

%% @spec set_bucket(riak_object:bucket(), [BucketProp :: {atom(),term()}]) -> ok
%% @doc Set the given properties for Bucket.
%%      This is generally best if done at application start time,
%%      to ensure expected per-bucket behavior.
%% See riak_bucket for expected useful properties.
set_bucket(BucketName,BucketProps) ->
    gen_server:call({riak_api,Node}, {set_bucket,BucketName,BucketProps}).
%% @spec get_bucket(riak_object:bucket()) -> [BucketProp :: {atom(),term()}]
%% @doc Get all properties for Bucket.
%% See riak_bucket for expected useful properties.
get_bucket(BucketName) ->
    gen_server:call({riak_api,Node}, {get_bucket,BucketName}).

%% @spec reload_all(Module :: atom()) -> term()
%% @doc Force all Riak nodes to reload Module.
%%      This is used when loading new modules for map/reduce functionality.
reload_all(Module) -> gen_server:call({riak_api,Node}, {reload_all, Module}).

%% @spec remove_from_cluster(ExitingNode :: atom()) -> term()
%% @doc Cause all partitions owned by ExitingNode to be taken over
%%      by other nodes.
remove_from_cluster(ExitingNode) ->
    gen_server:call({riak_api,Node}, {remove_from_cluster,ExitingNode}).

%% @spec send_event(EventName::atom(), EventDetail::term()) -> ok
%% @doc  Send a client-generated event to the Riak eventer.
send_event(EventName, EventDetail) ->
    gen_server:cast({riak_api,Node},
                    {send_event,ClientId,EventName,EventDetail}).
