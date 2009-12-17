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

%% @doc Functions for manipulating bucket properties.
%% @type riak_bucketprops() = [{Propkey :: atom(), Propval :: term()}]

-module(riak_bucket).
-include_lib("eunit/include/eunit.hrl").
-export([set_bucket/2, get_bucket/1, get_bucket/2]).
-export([defaults/0]).

%% @spec set_bucket(riak_object:bucket(), BucketProps::riak_bucketprops()) -> ok
%% @doc Set the given BucketProps in Bucket.
set_bucket(Name, BucketProps) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    OldBucket = get_bucket(Name),
    NewKeys = proplists:get_keys(BucketProps),
    PrunedOld = [{K,V} || {K,V} <- OldBucket, not lists:member(K,NewKeys)],
    R1 = riak_ring:update_meta({bucket,Name},
                               BucketProps ++ PrunedOld,
                               Ring),
    riak_ring_manager:set_my_ring(R1),
    riak_ring_manager:write_ringfile(),
    riak_eventer:notify(riak_bucket, set_bucket, {Name,BucketProps++PrunedOld}),
    RandomNode = riak_ring:random_node(R1),
    riak_connect:send_ring(RandomNode),
    ok.
    
%% @spec get_bucket(riak_object:bucket()) ->
%%         {ok, BucketProps :: riak_bucketprops()}
%% @doc Return the complete current list of properties for Bucket.
%% Properties include but are not limited to:
%% <pre>
%% n_val: how many replicas of objects in this bucket (default: 3)
%% allow_mult: can objects in this bucket have siblings? (default: false)
%% linkfun: a function returning a m/r FunTerm for link extraction
%% </pre>
%% 
get_bucket(Name) ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    get_bucket(Name, Ring).


%% @spec get_bucket(Name, Ring::riak_ring:riak_ring()) ->
%%          {ok, BucketProps :: riak_bucketprops()}
%% @private
get_bucket(Name, Ring) ->
    case riak_ring:get_meta({bucket, Name}, Ring) of
        undefined ->
            [{name, Name}
             |riak:get_app_env(default_bucket_props, defaults())];
        {ok, Bucket} -> Bucket
    end.

defaults() ->
    [{n_val,3},
     {allow_mult,false},
     {linkfun,{modfun, jiak_object, mapreduce_linkfun}},
     {chash_keyfun, {riak_util, chash_std_keyfun}},
     {old_vclock, 86400},
     {young_vclock, 20},
     {big_vclock, 50},
     {small_vclock, 10}].

simple_set_test() ->
    riak_ring_manager:start_link(test),
    riak_eventer:start_link(test),
    ok = set_bucket(a_bucket,[{key,value}]),
    Bucket = get_bucket(a_bucket),
    riak_ring_manager:stop(),
    riak_eventer:stop(),
    ?assertEqual(value, proplists:get_value(key, Bucket)).
