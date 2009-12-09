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

%% @doc Tests for jiak_default.  Parameterized modules must
%%      have their eunit tests in a separate file.
-module(jiak_default_tests).

-include_lib("eunit/include/eunit.hrl").

static_props_test_() ->
    M = jiak_default:new(
          [{allowed_fields,  [foo, bar, baz, quux]},
           {required_fields, [foo, bar]},
           {read_mask,       [foo, bar, baz]},
           {write_mask,      [foo]}]),
    {"Static Properties",
     [{"init", ?_assertEqual({ok, context_ignored},
                             M:init(key_ignored, context_ignored))},
      {"auth_ok", ?_assertEqual({true, reqdata_ignored, context_ignored},
                                M:auth_ok(key_ignored, reqdata_ignored,
                                          context_ignored))},
      {"bucket_listable", ?_assert(M:bucket_listable())},
      {"allowed_fields", ?_assertEqual([foo, bar, baz, quux],
                                       M:allowed_fields())},
      {"required_fields", ?_assertEqual([foo, bar],
                                        M:required_fields())},
      {"read_mask", ?_assertEqual([foo, bar, baz], M:read_mask())},
      {"write_mask", ?_assertEqual([foo], M:write_mask())}
     ]}.

expires_test_() ->
    {"Expires",
     [{"default", ?_assertEqual({600, reqdata_ignored, context_ignored},
                                (jiak_default:new([])):expires_in_seconds(
                                  key_ignored, reqdata_ignored,
                                  context_ignored))},
      {"explicit",
       ?_assertEqual(
          {10, reqdata_ignored, context_ignored},
          (jiak_default:new([{expiry_seconds, 10}])):expires_in_seconds(
           key_ignored, reqdata_ignored, context_ignored))}
     ]}.

write_test_() ->
    M = jiak_default:new([]),
    O = jiak_object:new(<<"bucket_ignored">>, <<"key_ignored">>),
    {"*_write functions",
     [{"check_write", 
       ?_assertEqual(
          {{ok, O}, reqdata_ignored, context_ignored},
          M:check_write({type_ignored, key_ignored}, O,
                        reqdata_ignored, context_ignored))},
      {"effect_write",
       ?_assertEqual(
          {{ok, O}, reqdata_ignored, context_ignored},
          M:effect_write({type_ignored, key_ignored}, O,
                         reqdata_ignored, context_ignored))},
      {"after_write",
       ?_assertEqual(
          {ok, reqdata_ignored, context_ignored},
          M:after_write(key_ignored, O,
                        reqdata_ignored, context_ignored))}
     ]}.

sibling_test() ->
    M = jiak_default:new([]),
    A = {dict:store(<<"X-Riak-Last-Modified">>,
                    httpd_util:rfc1123_date(),
                    dict:store(a, a, dict:new())),
         {{struct, [{a,a}]},
          [a]}},
    B = {dict:store(<<"X-Riak-Last-Modified">>,
                    httpd_util:rfc1123_date(),
                    dict:store(b, b, dict:new())),
         {{struct, [{b,b}]},
          [b]}},
    {CM, {{struct, CO}, CL}} = M:merge_siblings([A,B]),

    ?assertEqual(3, dict:size(CM)),
    ?assertEqual([a,b], [ dict:fetch(P, CM) || P <- [a,b] ]),
    
    ?assertEqual(2, length(CO)),
    ?assertEqual([a,b], [ proplists:get_value(P, CO) || P <- [a,b] ]),
    
    ?assertEqual(2, length(CL)),
    ?assert(lists:all(fun(L) -> lists:member(L, CL) end, [a,b])).
