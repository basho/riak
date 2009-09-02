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

%% @doc Utilities for jiak_resource and jiak_object.
-module(jiak_util).
-export([jiak_required_props/0,
         jiak_module_for_bucket/1, 
         get_jiak_module/1, 
         default_jiak_module/1,
         bucket_from_uri/1]).

-include_lib("eunit/include/eunit.hrl").

%% @private
jiak_required_props() -> [allowed_fields,required_fields,read_mask,write_mask].

%% @private
default_jiak_module(BucketName) when is_atom(BucketName) ->
    BucketProps = riak_bucket:get_bucket(BucketName),
    case lists:filter(
           fun(I) -> 
                   proplists:get_value(I, BucketProps) =:= undefined
           end, 
           jiak_required_props()) of
        [] ->
            jiak_default:new(BucketProps);
        _ ->
            undefined
    end.

%% @private
get_jiak_module(ReqData) ->
    case bucket_from_uri(ReqData) of
        {ok, Bucket} when is_atom(Bucket) ->
            jiak_module_for_bucket(Bucket);
        {error, no_such_bucket} -> 
            undefined
    end.

%% @private
jiak_module_for_bucket(Bucket) when is_atom(Bucket) ->
    case code:which(Bucket) of
        non_existing ->
            case default_jiak_module(Bucket) of
                undefined -> undefined;
                Mod when is_tuple(Mod) -> Mod
            end;
        ModPath when is_list(ModPath) -> Bucket
    end.

%% @spec bucket_from_uri(webmachine:wrq()) ->
%%         {ok, atom()}|{error, no_such_bucket}
%% @doc Extract the bucket name, as an atom, from the request URI.
%%      The bucket name must be an existing atom, or this function
%%      will return {error, no_such_bucket}
bucket_from_uri(RD) ->
    try {ok, list_to_existing_atom(wrq:path_info(bucket, RD))}
    catch _:_ -> {error, no_such_bucket} end.

dynamic_bucket_test() ->
    riak_ring_manager:start_link(test),
    riak_eventer:start_link(test),    
    BucketProps = [{allowed_fields, [<<"test">>]},
                   {required_fields, []},
                   {read_mask, [<<"test">>]},
                   {write_mask, [<<"test">>]}],
    riak_bucket:set_bucket(dynamic_bucket_test, BucketProps),
    Mod = jiak_module_for_bucket(dynamic_bucket_test),
    ?assertEqual([<<"test">>], Mod:allowed_fields()),
    ?assertEqual([], Mod:required_fields()),
    ?assertEqual([<<"test">>], Mod:read_mask()),
    ?assertEqual([<<"test">>], Mod:write_mask()),
    riak_ring_manager:stop(),
    riak_eventer:stop().

existing_bucket_from_uri_test() ->
    foo, %% make sure the atom exists
    PI = dict:store(bucket, "foo", dict:new()),
    RD0 = wrq:create('PUT', "1.1", "/jiak/foo", mochiweb_headers:empty()),
    RD = wrq:load_dispatch_data(PI, none, none, none, none, RD0),
    ?assertEqual({ok, foo}, bucket_from_uri(RD)).

nonexisiting_bucket_from_uri_test() ->
    PI = dict:store(bucket, "thisatomshouldntexistever", dict:new()),
    RD0 = wrq:create('PUT', "1.1", "/jiak/foo", mochiweb_headers:empty()),
    RD = wrq:load_dispatch_data(PI, none, none, none, none, RD0),    
    ?assertEqual({error, no_such_bucket}, bucket_from_uri(RD)).
    
