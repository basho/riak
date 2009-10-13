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
         bucket_from_reqdata/1]).

-include_lib("eunit/include/eunit.hrl").

%% @private
jiak_required_props() -> [allowed_fields,required_fields,read_mask,write_mask].

%% @private
jiak_module_for_bucket(BucketName) when is_binary(BucketName) ->
    BucketProps = riak_bucket:get_bucket(BucketName),
    case proplists:lookup(bucket_mod, BucketProps) of
        {bucket_mod, Module} when Module /= undefined ->
            Module;
        _ ->
            case bucket_props_defined(BucketProps) of
                true ->
                    jiak_default:new(BucketProps);
                false ->
                    undefined
            end
    end.

bucket_props_defined(BucketProps) ->
    [] == lists:filter(
            fun(I) -> 
                    proplists:get_value(I, BucketProps) =:= undefined
            end, 
            jiak_required_props()).

%% @private
get_jiak_module(ReqData) ->
    jiak_module_for_bucket(bucket_from_reqdata(ReqData)).

%% @spec bucket_from_reqdata(webmachine:wrq()) -> binary()
%% @doc Extract the bucket name, as a binary, from the request URI.
bucket_from_reqdata(RD) ->
    list_to_binary(mochiweb_util:unquote(wrq:path_info(bucket, RD))).

dynamic_bucket_test() ->
    riak_ring_manager:start_link(test),
    riak_eventer:start_link(test),    
    BucketProps = [{allowed_fields, [<<"test">>]},
                   {required_fields, []},
                   {read_mask, [<<"test">>]},
                   {write_mask, [<<"test">>]}],
    riak_bucket:set_bucket(<<"dynamic_bucket_test">>, BucketProps),
    Mod = jiak_module_for_bucket(<<"dynamic_bucket_test">>),
    ?assertEqual([<<"test">>], Mod:allowed_fields()),
    ?assertEqual([], Mod:required_fields()),
    ?assertEqual([<<"test">>], Mod:read_mask()),
    ?assertEqual([<<"test">>], Mod:write_mask()),
    riak_ring_manager:stop(),
    riak_eventer:stop().

%% just like dynamic_bucket_test, but ensuring that
%% {bucket_mod, undefined} gets treated as if bucket_mod
%% were not set.
dynamic_bucket_undefined_test() ->
    riak_ring_manager:start_link(test),
    riak_eventer:start_link(test),    
    BucketProps = [{bucket_mod, undefined},
                   {allowed_fields, [<<"test">>]},
                   {required_fields, []},
                   {read_mask, [<<"test">>]},
                   {write_mask, [<<"test">>]}],
    riak_bucket:set_bucket(<<"dynamic_bucket_test">>, BucketProps),
    Mod = jiak_module_for_bucket(<<"dynamic_bucket_test">>),
    ?assertEqual([<<"test">>], Mod:allowed_fields()),
    ?assertEqual([], Mod:required_fields()),
    ?assertEqual([<<"test">>], Mod:read_mask()),
    ?assertEqual([<<"test">>], Mod:write_mask()),
    riak_ring_manager:stop(),
    riak_eventer:stop().

module_bucket_test() ->
    riak_ring_manager:start_link(test),
    riak_eventer:start_link(test),    
    BucketProps = [{bucket_mod, jiak_example}],
    riak_bucket:set_bucket(<<"module_bucket_test">>, BucketProps),
    Mod = jiak_module_for_bucket(<<"module_bucket_test">>),
    ?assertEqual([<<"foo">>,<<"bar">>,<<"baz">>,<<"quux">>],
                 Mod:allowed_fields()),
    ?assertEqual([<<"foo">>], Mod:required_fields()),
    ?assertEqual([<<"foo">>,<<"bar">>], Mod:read_mask()),
    ?assertEqual([<<"foo">>,<<"baz">>], Mod:write_mask()),
    riak_ring_manager:stop(),
    riak_eventer:stop().
    

bucket_from_uri_test() ->
    PI = dict:store(bucket, "foo", dict:new()),
    RD0 = wrq:create('PUT', "1.1", "/jiak/foo", mochiweb_headers:empty()),
    RD = wrq:load_dispatch_data(PI, none, none, none, none, RD0),
    ?assertEqual(<<"foo">>, bucket_from_reqdata(RD)).
