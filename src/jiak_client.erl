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

%% @doc The equivalent of {@link riak_client}, but for the "jiak"
%%      pattern of access.  Functions defined here are equivalent
%%      to the like-named functions in riak_client, but expect
%%      jiak-style paramenters (like binary object keys) and
%%      return {@link jiak_object}s.
%% @type jiak_client() = term()
-module(jiak_client, [RiakClient]).
-author('Bryan Fink <bryan@basho.com>').

-export([mapred/2,mapred/3]).
-export([get/3,get/4]).
-export([put/2,put/3,put/4]).
-export([delete/3,delete/4]).
-export([list_keys/1]).
-export([set_bucket/2,get_bucket/1]).
-export([reload_all/1]).
-export([riak_client/0]).

%% @type default_timeout() = 15000
-define(DEFAULT_TIMEOUT, 15000).

%% @see riak_object:mapred/2
%% @equiv mapred(Inputs, Query, default_timeout())
mapred(Inputs,Query) -> mapred(Inputs, Query, ?DEFAULT_TIMEOUT).

%% @see riak_object:mapred/3
%% @equiv riak_client:mapred(Inputs, Query, Timeout)
mapred(Inputs,Query,Timeout) ->
    RiakClient:mapred(Inputs,Query,Timeout).

%% @spec get(riak_object:bucket(), jiak_object:key(), R :: integer()) ->
%%       {ok, jiak_object()} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Fetch the object at Bucket/Key.  Return a value as soon as R
%%      nodes have responded with a value or error.
%% @equiv get(Bucket, Key, R, default_timeout())
get(Bucket, Key, R) -> get(Bucket, Key, R, ?DEFAULT_TIMEOUT).

%% @spec get(riak_object:riak_object(), jiak_object:key(), R :: integer(),
%%           TimeoutMillisecs :: integer()) ->
%%       {ok, jiak_object()} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Fetch the object at Bucket/Key.  Return a value as soon as R
%%      nodes have responded with a value or error, or TimeoutMillisecs passes.
get(Bucket, Key, R, Timeout) when is_binary(Bucket), is_binary(Key) ->
    case RiakClient:get(Bucket, Key, R, Timeout) of
        {ok, RiakObject} ->
            JiakObject = jiak_object:from_riak_object(RiakObject),
            {ok, JiakObject};
        Error ->
            Error
    end.

%% @spec put(JiakObject :: jiak_object(), W :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, timeout}
%% @doc Store RObj in the cluster.
%%      Return as soon as at least W nodes have received the request.
%% @equiv put(RObj, W, W, default_timeout())
put(JiakObject, W) -> put(JiakObject, W, W, ?DEFAULT_TIMEOUT).

%% @spec put(JiakObject :: jiak_object(), W :: integer(), RW :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, timeout}
%% @doc Store RObj in the cluster.
%%      Return as soon as at least W nodes have received the request, and
%%      at least DW nodes have stored it in their storage backend.
%% @equiv put(Robj, W, DW, default_timeout())
put(JiakObject, W, DW) -> put(JiakObject, W, DW, ?DEFAULT_TIMEOUT).

%% @spec put(JiakObject :: jiak_object(), W :: integer(), RW :: integer(),
%%           TimeoutMillisecs :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, timeout}
%% @doc Store RObj in the cluster.
%%      Return as soon as at least W nodes have received the request, and
%%      at least DW nodes have stored it in their storage backend, or
%%      TimeoutMillisecs passes.
put(JiakObject, W, DW, Timeout) ->    
    RiakObject = jiak_object:to_riak_object(JiakObject),
    RiakClient:put(RiakObject, W, DW, Timeout).

%% @spec delete(riak_object:bucket(), jiak_object:key(), RW :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as RW
%%      nodes have responded with a value or error.
%% @equiv delete(Bucket, Key, RW, default_timeout())
delete(Bucket,Key,RW) -> delete(Bucket,Key,RW,?DEFAULT_TIMEOUT).

%% @spec delete(niak_object:bucket(), jiak_object:key(), RW :: integer(),
%%           TimeoutMillisecs :: integer()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as RW
%%      nodes have responded with a value or error, or TimeoutMillisecs passes.
%% @equiv riak_client:delete(Bucket, Key, RW, Timeout)
delete(Bucket,Key,RW,Timeout) ->
    RiakClient:delete(Bucket, Key, RW, Timeout).

%% @spec list_keys(riak_object:bucket()) ->
%%       {ok, [Key :: riak_object:key()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List the keys known to be present in Bucket.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after a put or delete.
%% @equiv riak_client:list_keys(Bucket)
list_keys(Bucket) -> RiakClient:list_keys(Bucket).

%% @spec set_bucket(riak_object:bucket(), [BucketProp :: {atom(),term()}]) -> ok
%% @doc Set the given properties for Bucket.
%% @equiv riak_client:set_bucket(BucketName, BucketProps)
set_bucket(BucketName,BucketProps) ->
    RiakClient:set_bucket(BucketName, BucketProps).

%% @spec get_bucket(riak_object:bucket()) -> [BucketProp :: {atom(),term()}]
%% @doc Get all properties for Bucket.
%% @equiv riak_client:get_bucket(BucketName)
get_bucket(BucketName) -> RiakClient:get_bucket(BucketName).

%% @spec reload_all(Module :: atom()) -> term()
%% @doc Force all Riak nodes to reload Module.
%%      This is used when loading new modules for map/reduce functionality.
%% @equiv riak_client:reload_all(Module)
reload_all(Module) -> RiakClient:reload_all(Module).

%% @spec riak_client() -> riak_client()
%% @doc Get the riak_client that this jiak_client is using.
riak_client() -> RiakClient.
