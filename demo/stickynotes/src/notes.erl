%% @doc notes is the handler for the "notes" Riak bucket, which
%%      conforms to the Jiak protocol
-module(notes).

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
    [<<"text">>, <<"x">>, <<"y">>, <<"z">>, <<"color">>].

%% @spec required_fields() -> [binary()]
%% @doc Return a list of field names that must exist in a valid
%%      object of this type
required_fields() -> allowed_fields().

%% @spec read_mask() -> [binary()]
%% @doc Return a list of fields that a client using jiak_resource
%%      should see.  Fields not in this list will be removed from
%%      the object before sending it to the client.
read_mask() -> allowed_fields().

%% @spec write_mask() -> [binary()]
%% @doc Return a list of fields that a client may change through
%%      jiak_resource.  Edits made by a client to fields that are
%%      not in this list will generate an error.
write_mask() -> allowed_fields().

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

%% @spec check_write({container|item, riak_object:binary_key()},
%%                   jiak_object(), webmachine:wrq(), jiak_context()) ->
%%         {{ok, jiak_object()}|{error, term()},
%%          webmachine:wrq(), jiak_context()}
%% @doc Decide whether or not a write should be allowed.  This
%%      function should check the validity of JiakObject and/or the
%%      set of diffs in Context, then return a tuple including {ok, J}
%%      if the write should be allowed or {error, Reason} if it should
%%      not be.  The returned JiakObject may be the same JiakObject
%%      passed in, or a modified one.
check_write({_PutType, Key}, JiakObject, ReqData, Context) ->
    {ObjDiffs,_} = Context:diff(),
    case lists:foldl(fun check_diff/2, [], ObjDiffs) of
        [] ->
            {{ok, JiakObject}, ReqData, Context:set_prop(key, Key)};
        Errors ->
            {{error, list_to_binary(string:join(Errors, ", "))},
             ReqData, Context}
    end.

-define(COLORS, [<<"yellow">>, <<"pink">>, <<"green">>, <<"blue">>]).

check_diff({<<"text">>, _, Value}, ErrorAcc) ->
    if is_binary(Value) -> ErrorAcc;
       true             -> ["text field must be a string"|ErrorAcc]
    end;
check_diff({Coord, _, Value}, ErrorAcc)
  when Coord==<<"x">>;Coord==<<"y">>;Coord==<<"z">> ->
    if is_integer(Value) -> ErrorAcc;
       true ->
            [io_lib:format("~s field must be an integer", [Coord])
             |ErrorAcc]
    end;
check_diff({<<"color">>, _, Value}, ErrorAcc) ->
    case lists:member(Value, ?COLORS) of
        true -> ErrorAcc;
        false ->
            [io_lib:format("color field must be one of (~s)",
                           [string:join([binary_to_list(C)||C<-?COLORS],
                                        ",")])
             |ErrorAcc]
    end.

%% @spec effect_write(jiak_resource:key(), jiak_object(),
%%                    webmachine:wrq(), jiak_context()) ->
%%         {{ok, jiak_object()}|{error, term()},
%%          webmachine:wrq(), jiak_context()}
%% @doc It has been determined that JiakObject is valid, and it will
%%      be written - this function is an opportunity to act on that
%%      information before the write happens.
effect_write(_Key, JiakObject, ReqData, Context) ->
    {{ok, JiakObject}, ReqData, Context}.

%% @spec after_write(jiak_resource:key(), jiak_object(),
%%                   webmachine:wrq(), jiak_context()) ->
%%         {ok, webmachine:wrq(), jiak_context()}
%% @doc JiakObject has been stored in riak - this function is an
%%      opportunity to act on that information.
after_write(_Key, JiakObject, ReqData, Context) ->
    spawn(fun() ->
                  [[_, GroupKey, _]] = jiak_object:links(JiakObject, groups),
                  {ok, C} = jiak:local_client(),
                  {ok, G} = C:get(groups, GroupKey, 2),
                  Key = Context:get_prop(key),
                  C:put(jiak_object:add_link(G, notes, Key, <<"note">>), 2)
          end),
    {ok, ReqData, Context}.

%% @spec merge_siblings([{Metadata::dict(),
%%                        {Object::mochijson2:struct(),
%%                         Links::[jiak_object:link()]}}]) ->
%%         {dict(),{mochijson2:struct(), [jiak_object:link()]}}
%% @doc Merge Riak-sibling jiak objects.  Siblings are passed in
%%      in the form {RiakObjectMetadata,{JiakObjectData,JiakLinks}},
%%      and this function should return exactly one structure of
%%      the same shape.
merge_siblings(Siblings) ->
    jiak:standard_sibling_merge(Siblings).
