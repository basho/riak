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
-export([list_keys/1,list_keys/2]).
-export([filter_keys/2,filter_keys/3]).
-export([list_buckets/0,list_buckets/1]).
-export([set_bucket/2,get_bucket/1]).
-export([reload_all/1]).
-export([remove_from_cluster/1]).
-export([send_event/2]).
-export ([add_event_handler/2, add_event_handler/3, add_event_handler/4]).
-export ([remove_event_handler/3]).
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
    Me = self(),
    ReqId = mk_reqid(),
    spawn(Node, riak_mapreduce_fsm, start, [ReqId,Inputs,Query,Timeout,Me]),
    wait_for_reqid(ReqId, Timeout).

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
get(Bucket, Key, R, Timeout) when is_binary(Bucket), is_binary(Key),
                                  is_integer(R), is_integer(Timeout) ->
    Me = self(),
    ReqId = mk_reqid(),
    spawn(Node, riak_get_fsm, start, [ReqId,Bucket,Key,R,Timeout,Me]),
    wait_for_reqid(ReqId, Timeout).

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
    Me = self(),
    ReqId = mk_reqid(),
    spawn(Node, riak_put_fsm, start, [ReqId,R0,W,DW,Timeout,Me]),
    wait_for_reqid(ReqId, Timeout).

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
    Me = self(),
    ReqId = mk_reqid(),
    spawn(Node, riak_delete, delete, [ReqId,Bucket,Key,RW,Timeout,Me]),
    wait_for_reqid(ReqId, Timeout).

%% @spec list_keys(riak_object:bucket()) ->
%%       {ok, [Key :: riak_object:key()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List the keys known to be present in Bucket.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after a put or delete.
%% @equiv list_keys(Bucket, default_timeout()*8)
list_keys(Bucket) -> 
    list_keys(Bucket, ?DEFAULT_TIMEOUT*8).

%% @spec list_keys(riak_object:bucket(), TimeoutMillisecs :: integer()) ->
%%       {ok, [Key :: riak_object:key()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List the keys known to be present in Bucket.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after a put or delete.
list_keys(Bucket, Timeout) -> 
    Me = self(),
    ReqId = mk_reqid(),
    spawn(Node, riak_keys_fsm, start, [ReqId,Bucket,Timeout,Me]),
    wait_for_reqid(ReqId, Timeout).

%% @spec filter_keys(riak_object:bucket(), Fun :: function()) ->
%%       {ok, [Key :: riak_object:key()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List the keys known to be present in Bucket, 
%%      filtered at the vnode according to Fun, via lists:filter.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after a put or delete.
%% @equiv filter_keys(Bucket, Fun, default_timeout()*8)
filter_keys(Bucket, Fun) -> 
    list_keys({filter, Bucket, Fun}, ?DEFAULT_TIMEOUT*8).

%% @spec filter_keys(riak_object:bucket(), Fun :: function(), TimeoutMillisecs :: integer()) ->
%%       {ok, [Key :: riak_object:key()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List the keys known to be present in Bucket, 
%%      filtered at the vnode according to Fun, via lists:filter.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after a put or delete.
filter_keys(Bucket, Fun, Timeout) -> 
    list_keys({filter, Bucket, Fun}, Timeout).

%% @spec list_buckets() ->
%%       {ok, [Bucket :: riak_object:bucket()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List buckets known to have keys.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after any operation that
%%      either adds the first key or removes the last remaining key from
%%      a bucket.
%% @equiv list_buckets(default_timeout()*8)
list_buckets() -> 
    list_buckets(?DEFAULT_TIMEOUT*8).

%% @spec list_buckets(TimeoutMillisecs :: integer()) ->
%%       {ok, [Bucket :: riak_object:bucket()]} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc List buckets known to have keys.
%%      Key lists are updated asynchronously, so this may be slightly
%%      out of date if called immediately after any operation that
%%      either adds the first key or removes the last remaining key from
%%      a bucket.
list_buckets(Timeout) -> 
    list_keys('_', Timeout).

%% @spec set_bucket(riak_object:bucket(), [BucketProp :: {atom(),term()}]) -> ok
%% @doc Set the given properties for Bucket.
%%      This is generally best if done at application start time,
%%      to ensure expected per-bucket behavior.
%% See riak_bucket for expected useful properties.
set_bucket(BucketName,BucketProps) ->
    rpc:call(Node,riak_bucket,set_bucket,[BucketName,BucketProps]).
%% @spec get_bucket(riak_object:bucket()) -> [BucketProp :: {atom(),term()}]
%% @doc Get all properties for Bucket.
%% See riak_bucket for expected useful properties.
get_bucket(BucketName) ->
    rpc:call(Node,riak_bucket,get_bucket,[BucketName]).
%% @spec reload_all(Module :: atom()) -> term()
%% @doc Force all Riak nodes to reload Module.
%%      This is used when loading new modules for map/reduce functionality.
reload_all(Module) -> rpc:call(Node,riak_util,reload_all,[Module]).

%% @spec remove_from_cluster(ExitingNode :: atom()) -> term()
%% @doc Cause all partitions owned by ExitingNode to be taken over
%%      by other nodes.
remove_from_cluster(ExitingNode) ->
    rpc:call(Node, riak_connect, remove_from_cluster,[ExitingNode]).

%% @spec send_event(EventName::atom(), EventDetail::term()) -> ok
%% @doc  Send a client-generated event to the Riak eventer.
send_event(EventName, EventDetail) ->
    rpc:call(Node,riak_eventer,notify,
             [client_event, EventName, {ClientId, EventDetail}]).

%% @equiv add_event_handler(Pid, Desc, {'_', '_', '_', '_'}, [])
add_event_handler(Pid, Desc) -> 
    add_event_handler(Pid, Desc, {'_', '_', '_', '_'}).

%% @equiv add_event_handler(Pid, Desc, MatchHead, [])
add_event_handler(Pid, Desc, MatchHead) -> 
    add_event_handler(Pid, Desc, MatchHead, []).
    
%% @doc
%% Register a process that will receive Riak events 
%% generated by the cluster in the form of Erlang messages.
%% See {@link riak_eventer:add_handler/4.} for more information.
add_event_handler(Pid, Desc, MatchHead, MatchGuard) ->
    rpc:call(Node, riak_eventer, add_handler, [Pid, Desc, MatchHead, MatchGuard]). 

%% @doc
%% Remove an event handler added by {@link add_event_handler/4}, if it exists.
%% See {@link riak_eventer:remove_handler/3.} for more information.
remove_event_handler(Pid, MatchHead, MatchGuard) ->
    rpc:call(Node, riak_eventer, remove_handler, [Pid, MatchHead, MatchGuard]). 

%% @private
mk_reqid() -> erlang:phash2(erlang:now()). % only has to be unique per-pid

%% @private
wait_for_reqid(ReqId, Timeout) ->
    receive
        {ReqId, {error, Err}} -> {error, Err};
        {ReqId, ok} -> ok;
        {ReqId, {ok, Res}} -> {ok, Res}
    after Timeout ->
            {error, timeout}
    end.
