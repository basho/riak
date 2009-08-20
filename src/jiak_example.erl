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

%% @doc jiak_example is a demonstration of the sort of module that
%%      jiak_resource needs for storing objects in a bucket.
%%
%%      jiak_example controls the jiak_example Riak bucket.  In
%%      general, jiak_resource expects the name of the Riak bucket
%%      and the name of the controlling module to match.
%%
%%      Objects in the jiak_example bucket must have at least a field
%%      named "foo", foo are allowed to also have fields named "bar",
%%      "baz", and "quux".  Clients can only read "foo" and "bar", and
%%      can only write "foo" and "baz".  Or, a little more graphically:
%%<pre>
%%             READ   WRITE
%%      "foo":  X       X
%%      "bar":  X
%%      "baz":          X
%%     "quux":
%%</pre>
-module(jiak_example).

-export([init/2,
         auth_ok/3,
         bucket_listable/0,
         allowed_fields/0,
         required_fields/0,
         read_mask/0,
         write_mask/0,
         expires_in_seconds/3,
         check_write/4,
         effect_write/4,
         after_write/4]).
-export([merge_siblings/1]).

%% @spec init(jiak_resource:key(), jiak_context()) -> {ok, jiak_context()}
%% @doc initialize Context for use in this module
init(_Key, Context) ->
    {ok, Context}.

%% @spec auth_ok(jiak_resource:key(), webmachine:wrq(), jiak_context()) ->
%%         {true|webmachine:auth_header(),
%%          webmachine:wrq(),
%%          jiak_context()}
%% @doc This function should behave exactly as the is_authorized/2
%%      function of any Webmachine resource would behave.
auth_ok(_Key, ReqData, Context) ->
    {true, ReqData, Context}.

%% @spec bucket_listable() -> boolean()
%% @doc Return 'true' if you want clients to be able to request the
%%      list of keys in this bucket through jiak_resource.  Return
%%      'false' if the keylist should not be client-visible.
bucket_listable() -> true.

%% @spec allowed_fields() -> [binary()]
%% @doc Return a list of field names that are allowed to exist
%%      in objects of this type.
allowed_fields() ->
    [<<"foo">>, <<"bar">>, <<"baz">>, <<"quux">>].

%% @spec required_fields() -> [binary()]
%% @doc Return a list of field names that must exist in a valid
%%      object of this type
required_fields() ->
    [<<"foo">>].

%% @spec read_mask() -> [binary()]
%% @doc Return a list of fields that a client using jiak_resource
%%      should see.  Fields not in this list will be removed from
%%      the object before sending it to the client.
read_mask() ->
    [<<"foo">>, <<"bar">>].

%% @spec write_mask() -> [binary()]
%% @doc Return a list of fields that a client may change through
%%      jiak_resource.  Edits made by a client to fields that are
%%      not in this list will generate an error.
write_mask() ->
    [<<"foo">>, <<"baz">>].

%% @spec expires_in_seconds(jiak_resource:key(),
%%                          webmachine:wrq(),
%%                          jiak_context()) ->
%%          {integer(), webmachine:wrq(), jiak_context()}
%% @doc Return the number of seconds a client should be allowed to
%%      cache an object of this type.  This is very similar to the
%%      expires/2 function of a Webmachine resource, except that it
%%      returns a number of seconds instead of a datetime.
expires_in_seconds(_Key, ReqData, Context) ->
    {600, ReqData, Context}.

%% @spec check_write({container|item, riak_object:key()},
%%                   jiak_object(), webmachine:wrq(), jiak_context()) ->
%%         {{ok, jiak_object()}|{error, term()},
%%          webmachine:wrq(), jiak_context()}
%% @doc Decide whether or not a write should be allowed.  This
%%      function should check the validity of JiakObject and/or the
%%      set of diffs in Context, then return a tuple including {ok, J}
%%      if the write should be allowed or {error, Reason} if it should
%%      not be.  The returned JiakObject may be the same JiakObject
%%      passed in, or a modified one.
check_write({_PutType, _Key}, JiakObject, ReqData, Context) ->
    {{ok, JiakObject}, ReqData, Context}.

%% @spec effect_write(jiak_resource:key(), jiak_object(),
%%                    webmachine:wrq(), jiak_context()) ->
%%         {{ok, jiak_object()}|{error, term()},
%%          webmachine:wrq(), jiak_context()}
%% @doc It has been determined that JiakObject is valid, and it will
%%      be written - this function is an opportunity to act on that
%%      information before the write happens.  The object returned
%%      from this function will be the one stored into Riak (plus
%%      the chosen key if this is a new object POSTed to the bucket).
effect_write(_Key, JiakObject, ReqData, Context) ->
    {{ok, JiakObject}, ReqData, Context}.

%% @spec after_write(jiak_resource:key(), jiak_object(),
%%                   webmachine:wrq(), jiak_context()) ->
%%         {ok, webmachine:wrq(), jiak_context()}
%% @doc JiakObject has been stored in riak - this function is an
%%      opportunity to act on that information.
after_write(_Key, _JiakObject, ReqData, Context) ->
    {ok, ReqData, Context}.

%% @spec merge_siblings([{Metadata::dict(),
%%                        {Object::mochijson2:struct(),
%%                         Links::[jiak_object:link()]}}]) ->
%%         {dict(),{mochijson2:struct(), [jiak_object:link()]}}
%% @doc Merge Riak-sibling jiak objects.  Siblings are passed in
%%      in the form {RiakObjectMetadata,{JiakObjectData,JiakLinks}},
%%      and this functino should return exactly one structure of
%%      the same shape.
merge_siblings(Siblings) ->
    jiak:standard_sibling_merge(Siblings).
