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

%% @doc jiak_resource provides access to Jiak objects over HTTP.
%%      Resources are provided at URIs shaped like:
%%        ```http://host/JiakBase/Bucket/Key'''
%%      That is, an object stored in the Riak bucket "Bucket" at key
%%      "Key" would be available at the path Bucket/Key, relative to
%%      jiak_resource's base path.
%%
%%      jiak_resource should be added to a Webmachine dispatch with
%%      two lines, one for bucket-targetted requests, the other for
%%      item-targetted requests:
%%<pre>
%%      {[JiakBase,bucket], jiak_resource,
%%       [{key_type, container}|Options]}.
%%      {[JiakBase,bucket,key], jiak_resource,
%%       [{key_type, item}|Options]}.
%%</pre>
%%
%%      Dispatch Configuration Options:
%%<dl><dt>  {jiak_name, string()}: (Required)
%%</dt><dd>   base path for jiak_resource
%%</dd><dt> {key_type, item|container}: (Required)
%%</dt><dd>   set to 'item' when the request path targets a specific
%%            object, or to 'container' when it targets a whole bucket
%%</dd><dt> {riak_local, boolean()}: (Optional)
%%</dt><dd>   set to 'true' to use jiak:local_client/0, otherwise
%%            jiak:client_connect/3 will be used
%%</dd><dt> {riak_ip, string()}: (Required if riak_local = false)
%%</dt><dd>   IP of the riak cluster, passed to jiak:client_connect/3
%%</dd><dt> {riak_port, integer()}: (Required if riak_local = false)
%%</dt><dd>   Port of the riak cluster, passed to jiak:client_connect/3
%%</dd><dt> {riak_cookie, atom()}: (Required if riak_local = false)
%%</dt><dd>   Cookie of the riak cluster, passd to jiak:client_connect/3
%%</dd></dl>
%%
%%      HTTP Query Parameters:
%%<dl><dt>  schema
%%</dt><dd>   allowed values: true (default), false
%%            when GETting a bucket, set schema=false if you do not
%%            want the schema included in the response
%%</dd><dt> keys
%%</dt><dd>   allowed values: true (default), false
%%            when GETting a bucket, set keys=false if you do not want
%%            the keylist included in the response
%%</dd><dt> returnbody
%%</dt><dd>   allowed values: true, false (default)
%%            when PUTting or POSTing an object, set returnbody=true
%%            if you want the response to included the updated object
%%            (saves the roundtrip for a subsequent GET), the response
%%            will be 204 No Content, otherwise
%%</dd><dt> r
%%</dt><dd>   specify the Riak R value for get operations
%%</dd><dt> w
%%</dt><dd>   specify the Riak W value for put operations
%%</dd><dt> dw
%%</dt><dd>   specify the Riak DW value for put operations
%%</dd><dt> rw
%%</dt><dd>   specify the Riak RW value for delete operations
%%</dd></dl>
%%
%%      HTTP Usage:
%%<dl><dt> GET /JiakBase/Bucket
%%</dt><dd>  If the bucket is listable, returns a JSON object
%%           of the form:
%%           {
%%            "schema":{
%%                      "allowed_fields":["FieldName1","FieldName2",...],
%%                      "required_fields":["FieldName1",...],
%%                      "write_mask":["FieldName1",...],
%%                      "read_mask":["FieldName1",...]
%%                     },
%%            "keys":["Key1","Key2",...]
%%           }
%%           Each element of the "schema" lists some fo the field names
%%           defined for objects of the requested bucket.
%%<dl><dt>     allowed_fields
%%</dt><dd>      Objects may only include the fields listed here
%%</dd><dt>    required_fields
%%</dt><dd>      Objects must have fields listed here
%%</dd><dt>    write_mask
%%</dt><dd>      Clients may change only the fields listed here
%%</dd><dt>    read_mask
%%</dt><dd>      Clients will see only the contents of fields listed here
%%</dd></dl>
%%
%%</dd><dt> GET /JiakBase/Bucket/Key
%%</dt><dd>   If the object exists, and access is permitted, returns
%%            the object JSON-encoded
%%
%%</dd><dt> PUT /JiakBase/Bucket/Key
%%</dt><dd>   Store the object in the request body in the given Bucket at
%%            the given Key.  The "bucket" and "key" fields in the object
%%            must match the Bucket and Key components of the URI.
%%
%%</dd><dt> POST /JiakBase/Bucket
%%</dt><dd>   Store the object in the request body in the given Bucket at
%%            a new, server-generated key.  Response will be empty (unless
%%            returnbody=true is specified in the query parameters) with
%%            the Location header set to the new object's URI.
%%
%%</dd><dt> PUT /JiakBase/Bucket
%%</dt><dd>   Create or update the schema for a bucket.  The request body 
%%            must be a JSON object of the form:
%%            {"schema":{
%%                      "allowed_fields":["FieldName1","FieldName2",...],
%%                      "required_fields":["FieldName1",...],
%%                      "write_mask":["FieldName1",...],
%%                      "read_mask":["FieldName1",...]
%%                     }
%%            }
%%</dd></dl>
-module(jiak_resource).

-export([init/1,
         service_available/2,
         allowed_methods/2,
         resource_exists/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         encodings_provided/2,
         post_is_create/2,
         create_path/2,
         handle_incoming/2,
         produce_body/2,
         delete_resource/2,
         malformed_request/2,
         forbidden/2,
	 last_modified/2,
	 generate_etag/2,
	 expires/2,
         apply_read_mask/1,
         pretty_print/2]).

%% @type context() = term()
%% @type jiak_module() = atom()|{jiak_default, list()}
-record(ctx, {bucket,       %% binary() - Bucket name (from uri)
              key,          %% binary()|container|schema - Key (or sentinal
                            %%   meaning "no key provided")
              module,       %% atom()
              jiak_context, %% jiak_context() - context for the request
              jiak_name,    %% string() - prefix for jiak uris
              jiak_client,  %% jiak_client() - the store client
              etag,         %% string() - ETag header
              bucketkeys,   %% [binary()] - keys in the bucket
              diffs,        %% {[object_diff()],{AddedLinks::[jiak_link()],
                            %%                   RemovedLinks::[jiak_link()]}
              incoming,     %% jiak_object() - object the client is storing
              storedobj}).  %% jiak_object() - object stored in Riak

-include_lib("eunit/include/eunit.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% @type key() = container|schema|riak_object:key()

%% @spec init(proplist()) -> {ok, context()}
%% @doc Initialize this webmachine resource.  This function will
%%      attempt to open a client to Riak, and will fail if it is
%%      unable to do so.
init(Props) ->
    {ok, JiakClient} = 
        case proplists:get_value(riak_local, Props) of
            true ->
                jiak:local_client();
            _ ->
                Node = proplists:get_value(riak_node),
                Cookie = proplists:get_value(riak_cookie),
                erlang:set_cookie(node(), Cookie),
                jiak:client_connect(Node)
        end,
    {ok, #ctx{jiak_name=proplists:get_value(jiak_name, Props),
              key=proplists:get_value(key_type, Props),
              jiak_client=JiakClient}}.

%% @spec service_available(webmachine:wrq(), context()) -> 
%%           {boolean, webmachine:wrq(), context()}
%% @doc Ensure that a Jiak module for the requested bucket is available. 
%%      This function first checks for a compiled Erlang module with the 
%%      same name as the bucket.  If no module is found, the bucket 
%%      configuration metadata in the ring is used, and must contain
%%      a valid Jiak schema.  
service_available(ReqData, Context=#ctx{key=container}) ->
    {ServiceAvailable, NewCtx} = 
        case wrq:method(ReqData) of
            'PUT' -> 
                _ = list_to_binary(mochiweb_util:unquote(
                                     wrq:path_info(bucket, ReqData))),
                Mod = jiak_default:new([]),
                {true, Context#ctx{module=Mod, key=schema}};
            _ ->
                case jiak_util:get_jiak_module(ReqData) of
                    undefined -> {false, Context#ctx{module=no_module_found}};
                    Module -> {true, Context#ctx{module=Module}}
                end
        end,
    {ServiceAvailable, ReqData, NewCtx};
service_available(ReqData, Context) ->
    {ServiceAvailable, NewCtx} = 
        case jiak_util:get_jiak_module(ReqData) of
            undefined -> {false, Context#ctx{module=no_module_found}};
            Module -> {true, Context#ctx{module=Module}}
        end,
    {ServiceAvailable, ReqData, NewCtx}.

%% @spec allowed_methods(webmachine:wrq(), context()) ->
%%          {[http_method()], webmachine:wrq(), context()}
%% @type http_method() = 'HEAD'|'GET'|'POST'|'PUT'|'DELETE'
%% @doc Determine the list of HTTP methods that can be used on this
%%      resource.  Should be HEAD/GET/POST/PUT for buckets and
%%      HEAD/GET/POST/PUT/DELETE for objects.
%%      Exception: HEAD/GET is returned for an "unknown" bucket.
allowed_methods(RD, Ctx0=#ctx{module=Mod}) ->
    Key = case Ctx0#ctx.key of
              container -> container;
              schema -> schema;
              _         -> list_to_binary(mochiweb_util:unquote(
                                            wrq:path_info(key, RD)))
          end,
    Bucket = jiak_util:bucket_from_reqdata(RD),
    {ok, JC} = Mod:init(Key, jiak_context:new(not_diffed_yet, [])),
    Ctx = Ctx0#ctx{bucket=Bucket, key=Key, jiak_context=JC},
    case Key of
        container ->
            %% buckets have GET for list_keys, POST for create
            {['HEAD', 'GET', 'POST'], RD, Ctx};
        schema ->
            {['PUT'], RD, Ctx};
        _ ->
            %% keys have the "full" doc store set
            {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}
    end.

%% @spec malformed_request(webmachine:wrq(), context()) ->
%%          {boolean(), webmachine:wrq(), context()}
%% @doc Determine whether the request is propertly constructed.
%%      GET is always properly constructed
%%      PUT/POST is malformed if:
%%        - request body is not a valid JSON object
%%        - the object is in a bucket that is "undefined"
%%      PUT is malformed if:
%%        - the "bucket" field of the object does not match the
%%          bucket component of the URI
%%        - the "key" field of the object does not match the
%%          key component of the URI
%%        - when PUTing to a bucket schema, the schema is not of the
%%          form described above. 
malformed_request(ReqData, Context=#ctx{key=schema}) ->
    case decode_object(wrq:req_body(ReqData)) of
        {ok, _SchemaObj={struct, SchemaPL0}} ->
            ReqProps = [list_to_binary(atom_to_list(P)) || 
                           P <- jiak_util:jiak_required_props()],
            {struct, SchemaPL} = proplists:get_value(<<"schema">>,SchemaPL0),
            case lists:filter(
                   fun(I) -> 
                           proplists:get_value(I, SchemaPL) =:= undefined
                   end, 
                   ReqProps) of
                [] ->
                    {false, ReqData, Context#ctx{incoming=SchemaPL}};
                L ->
                    {true, 
                     wrq:append_to_response_body(
                     io_lib:format("missing required schema fields: ~p~n",[L]),
                       ReqData),
                     Context}
            end;
        Err ->
            {true,
             wrq:append_to_response_body(
               io_lib:format("bad JSON form: ~p~n",[Err]),
               ReqData),
             Context}
    end;
malformed_request(ReqData, Context=#ctx{bucket=Bucket,key=Key}) ->
    % just testing syntax and required fields on POST and PUT
    % also, bind the incoming body here
    case lists:member(wrq:method(ReqData), ['POST', 'PUT']) of
        false -> {false, ReqData, Context};
        true ->
            case decode_object(wrq:req_body(ReqData)) of
                {ok, JiakObject={struct,_}} ->
                    PT = wrq:method(ReqData) == 'PUT',
                    KM = jiak_object:key(JiakObject) == Key,
                    BM = jiak_object:bucket(JiakObject) == Bucket,
                    if (not PT); (PT andalso KM andalso BM) ->
                            {false, ReqData, Context#ctx{incoming=JiakObject}};
                       not KM ->
                            {true,
                             wrq:append_to_response_body("Object key does not match URI",
                                                         ReqData),
                             Context};
                       not BM ->
                            {true,
                             wrq:append_to_response_body("Object bucket does not match URI",
                                                         ReqData),
                             Context}
                    end;
                _ ->
                    {true,
                     wrq:append_to_response_body("Poorly formed JSON Body.",
                                                 ReqData),
                     Context}
            end
    end.

%% @spec decode_object(iolist()) -> {ok, mochijson2()}|{error, bad_json}
%% @doc Wrap up mochijson2:decode/1 so the process doesn't die if
%%      decode fails.
decode_object(Body) ->
    try {ok, mochijson2:decode(Body)}
    catch _:_ -> {error, bad_json} end.

%% @spec check_required(jiak_object(), [binary()]) -> boolean()
%% @doc Determine whether Obj contains all of the fields named in
%%      the Fields parameter.  Returns 'true' if all Fields are
%%      present in Obj, 'false' otherwise.
check_required(Obj, Fields) ->
    Required = sets:from_list(Fields),
    Has = sets:from_list(jiak_object:props(Obj)),
    sets:is_subset(Required, Has).

%% @spec check_allowed(jiak_object(), [binary()]) -> boolean()
%% @doc Determine whether Obj contains any fields not named in the
%%      Fields parameter.  Returns 'true' if Obj contains only
%%      fields named by Fields, 'false' if Obj contains any fields
%%      not named in Fields.
check_allowed(Obj, Fields) ->
    Allowed = sets:from_list(Fields),
    Has = sets:from_list(jiak_object:props(Obj)),
    sets:is_subset(Has, Allowed).

%% @spec check_write_mask(riak_object:bucket(), diff()) -> boolean()
%% @doc Determine whether any fields outside the write mask of the
%%      bucket have been modified.  Returns 'true' if only fields in
%%      the bucket's write mask were modified, 'false' otherwise.
check_write_mask(Mod, {PropDiffs,_}) ->
    WriteMask = Mod:write_mask(),
    %% XXX should probably use a special atom like 'JAPI_UNDEFINED' for
    %% non-existant keys produced by the diff.
    [{Key, OldVal} || {Key, OldVal, _NewVal} <- PropDiffs,
		      lists:member(Key, WriteMask) =:= false] =:= [].

%% @spec is_authorized(webmachine:wrq(), context()) ->
%%          {true|string(), webmachine:wrq(), context()}
%% @doc Determine whether the request is authorized.  This function
%%      calls through to the bucket's auth_ok/3 function.
is_authorized(ReqData, Context=#ctx{bucket={error, no_such_bucket}}) ->
    {{halt, 404},
     wrq:append_to_response_body("Unknown bucket.", ReqData),
     Context};
is_authorized(ReqData, Context=#ctx{key=Key,jiak_context=JC,module=Mod}) ->
    {Result, RD1, JC1} = Mod:auth_ok(Key, ReqData, JC),
    {Result, RD1, Context#ctx{jiak_context=JC1}}.

%% @spec forbidden(webmachine:wrq(), context()) ->
%%          {boolean(), webmachine:wrq(), context()}
%% @doc For an object GET/PUT/POST or a bucket POST, check to see
%%      whether the write request violates the write mask of the
%%      bucket.  For a bucket GET, check to see whether the keys of
%%      the bucket are listable.  PUT requests to bucket schemas are
%%      always accepted.
forbidden(ReqData, Context=#ctx{key=schema}) ->
    %% PUTs to container are for setting schemas and therefore always
    %% allowed
    {false, ReqData, Context};
forbidden(ReqData, Context=#ctx{key=container, module=Mod}) ->
    case wrq:method(ReqData) of
        'POST' -> object_forbidden(ReqData, Context);
        _      -> {not Mod:bucket_listable(), ReqData, Context}
    end;
forbidden(ReqData, Context) ->
    case lists:member(wrq:method(ReqData), ['POST', 'PUT']) of
	true  -> object_forbidden(ReqData, Context);
	false -> {false, ReqData, Context}
    end.

%% @spec object_forbidden(webmachine:wrq(), context()) ->
%%         {boolean(), webmachine:wrq(), context()}
%% @doc Determine whether an object write violates the write mask of
%%      the bucket.
object_forbidden(ReqData, Context=#ctx{jiak_context=JC,module=Mod}) ->
    {Diffs, NewContext0} = diff_objects(ReqData, Context),
    NewContext = NewContext0#ctx{jiak_context=JC:set_diff(Diffs)},
    Permitted = check_write_mask(Mod, Diffs),    
    case Permitted of
        false ->
            {true,
             wrq:append_to_response_body(
               io_lib:format(
                 "Write disallowed, some of ~p not writable.~n", 
                 [[K || {K,_,_} <- element(1, Diffs)]]),
               ReqData),
             NewContext};
        true ->
            {false, ReqData, NewContext}
    end.

%% @spec encodings_provided(webmachine:wrq(), context()) ->
%%         {[encoding()], webmachine:wrq(), context()}
%% @doc Get the list of encodings this resource provides.
%%      "identity" is provided for all methods, and "gzip" is
%%      provided for GET as well
encodings_provided(ReqData, Context) ->
    case wrq:method(ReqData) of
        'GET' ->
            {[{"identity", fun(X) -> X end},
              {"gzip", fun(X) -> zlib:gzip(X) end}], ReqData, Context};
        _ ->
            {[{"identity", fun(X) -> X end}], ReqData, Context}
    end.

%% @spec resource_exists(webmachine:wrq(), context()) ->
%%          {boolean, webmachine:wrq(), context()}
%% @doc Determine whether or not the resource exists.
%%      This resource exists if the bucket is known or the object
%%      was successfully fetched from Riak.
resource_exists(ReqData, Context=#ctx{key=schema}) ->
    %% schema-creation request, always exists.
    {true, ReqData, Context};
resource_exists(ReqData, Context=#ctx{key=container}) ->
    %% bucket existence was tested in is_authorized
    {true, ReqData, Context};
resource_exists(ReqData, Context) ->
    case retrieve_object(ReqData, Context) of
        {notfound, Context1} -> {false, ReqData, Context1};
        {error, {Err, Context1}} -> {{error, Err}, ReqData, Context1};
        {ok, {_Obj, Context1}} -> {true, ReqData, Context1}
    end.

%% @spec content_types_provided(webmachine:wrq(), context()) ->
%%          {[ctype()], webmachine:wrq(), context()}
%% @doc Get the list of content types this resource provides.
%%      "application/json" and "text/plain" are both provided
%%      for all requests.  "text/plain" is a "pretty-printed"
%%      version of the "application/json" content.
content_types_provided(ReqData, Context) ->
    {[{"application/json", produce_body},
      {"text/plain", pretty_print}],
     ReqData, Context}.

%% @spec content_types_accepted(webmachine:wrq(), context()) ->
%%          {[ctype()], webmachine:wrq(), context()}
%% @doc Get the list of content types accepted by this resource.
%%      Only "application/json" is accepted.
content_types_accepted(ReqData, Context) ->
    {[{"application/json", handle_incoming}], ReqData, Context}.

%% @spec produce_body(webmachine:wrq(), context()) ->
%%          {io_list(), webmachine:wrq(), context()}
%% @doc Get the representation of this resource that will be
%%      sent to the client.
produce_body(ReqData, Context=#ctx{key=container,module=Mod,bucket=Bucket}) ->
    Qopts = wrq:req_qs(ReqData),
    Schema = case proplists:lookup("schema", Qopts) of
                 {"schema", "false"} -> [];
                 _ -> [{schema, {struct, full_schema(Mod)}}]
             end,
    {Keys, Context1} = case proplists:lookup("keys", Qopts) of
                           {"keys", "false"} -> {[], Context};
                           _ -> 
                               {ok, {K, NewCtx}} = retrieve_keylist(Context),
                               {[{keys, K}], NewCtx}
                       end,
    KeyList = case Keys of
        [{keys,Ks}] -> Ks;
        _ -> []
    end,
    NewReqData = lists:foldl(fun(K,RD) ->
                                     add_link_head(Bucket,K,"contained",RD)
                             end,
                             ReqData, KeyList),
    JSONSpec = {struct, Schema ++ Keys},
    {mochijson2:encode(JSONSpec), NewReqData, Context1};
produce_body(ReqData, Context=#ctx{module=Module,bucket=Bucket}) ->
    {ok, {JiakObject0, Context1}} = retrieve_object(ReqData, Context),
    JiakObject = apply_read_mask(Module, JiakObject0),
    {struct,JOProps} = JiakObject,
    Links = proplists:get_value(<<"links">>, JOProps),
    NewReqData = add_container_link(Bucket,
                   lists:foldl(fun([B,K,T],RD) -> add_link_head(B,K,T,RD) end,
                               ReqData, Links)),
    {mochijson2:encode(JiakObject),
     wrq:set_resp_header("X-JIAK-VClock",
                         binary_to_list(jiak_object:vclock(JiakObject)),
                         NewReqData),
     Context1}.    

add_container_link(Bucket,ReqData) ->
    Val = io_lib:format("</~s/~s>; rel=\"up\"",
                    [riak:get_app_env(jiak_name, "jiak"),
                     mochiweb_util:quote_plus(Bucket)]),
    wrq:merge_resp_headers([{"Link",Val}],ReqData).

add_link_head(Bucket,Key,Tag,ReqData) ->
    Val = io_lib:format("</~s/~s/~s>; riaktag=\"~s\"",
                    [riak:get_app_env(jiak_name, "jiak")|
                     [mochiweb_util:quote_plus(E) ||
                         E <- [Bucket, Key, Tag] ]]),
    wrq:merge_resp_headers([{"Link",Val}],ReqData).

%% @spec full_schema(riak_object:bucket()) ->
%%          [{schema_type(), [binary()]}]
%% @type schema_type() = allowed_fields |
%%                       required_fields |
%%                       read_mask |
%%                       write_mask
%% @doc Get the schema for the bucket.
full_schema(Mod) ->
    [{allowed_fields, Mod:allowed_fields()},
     {required_fields, Mod:required_fields()},
     {read_mask, Mod:read_mask()},
     {write_mask, Mod:write_mask()}].

%% @spec make_uri(string(), riak_object:bucket(), string()) -> string()
%% @doc Get the string-path for the bucket and subpath under jiak.
make_uri(JiakName,Bucket,Path) ->
    "/" ++ JiakName ++ 
        "/" ++ mochiweb_util:quote_plus(Bucket) ++
        "/" ++ Path.

%% @spec handle_incoming(webmachine:wrq(), context()) ->
%%          {true, webmachine:wrq(), context()}
%% @doc Handle POST/PUT requests.  This is where the actual Riak-put
%%      happens, as well as where the bucket's check_write,
%%      effect_write, and after_write functions are called.
handle_incoming(ReqData, Context=#ctx{key=schema, 
                                      bucket=Bucket,
                                      incoming=SchemaPL}) ->
    SchemaProps = [{list_to_atom(binary_to_list(K)), V} || {K,V} <- SchemaPL],
    ok = riak_bucket:set_bucket(Bucket, SchemaProps),
    {<<>>, ReqData, Context};
handle_incoming(ReqData, Context=#ctx{bucket=Bucket,key=Key,
                                      jiak_context=JCTX,jiak_name=JiakName,
                                      jiak_client=JiakClient,
                                      incoming=JiakObject0,
                                      module=Mod})->
    {PutType, NewRD, ObjId} =
        case Key of
            container -> % POST to bucket has its fresh id in Path
                {container,
                 wrq:set_resp_header("Location",
                                     make_uri(JiakName,Bucket,
                                              wrq:disp_path(ReqData)),
                                     ReqData),
                 list_to_binary(mochiweb_util:unquote(wrq:disp_path(ReqData)))};
            _ ->
                {item, ReqData, Key}
        end,
    case Mod:check_write({PutType, ObjId},JiakObject0,NewRD,JCTX) of
        {{error, Reason}, RD1, JC1} ->
            {{halt,403},
             wrq:append_to_response_body(
               io_lib:format("Write disallowed, ~p.~n", [Reason]), RD1),
             Context#ctx{jiak_context=JC1}};
        {{ok, JiakObject1}, RD1, JC1} ->
	    Allowed = Mod:allowed_fields(),
	    case check_allowed(JiakObject1, Allowed) of
		true ->
		    Required = Mod:required_fields(),
		    case check_required(JiakObject1, Required) of
			true ->
			    case Mod:effect_write(Key,JiakObject1,RD1,JC1) of
				{{error, Reason},RD2,JC2} ->
                                    {{error, Reason}, RD2,
                                     Context#ctx{jiak_context=JC2}};
				{{ok, JiakObject2}, RD2, JC2} ->
                                    JiakObjectWrite = if Key == container ->
                                                              jiak_object:setf(JiakObject2, <<"key">>, ObjId);
                                                         true ->
                                                              JiakObject2
                                                      end,
                                    W = integer_query("w", 2, ReqData),
                                    DW = integer_query("dw", 2, ReqData),
				    ok = JiakClient:put(JiakObjectWrite, W, DW),
                                    {ok, RD3, JC3} = Mod:after_write(Key,JiakObject2,RD2,JC2),
                                    {RD4, Context1} =
                                        case proplists:lookup("returnbody", wrq:req_qs(RD1)) of
                                            {"returnbody", "true"} ->
                                                {Body, RD3a, Ctx1} =
                                                    produce_body(RD3,
                                                                 Context#ctx{
                                                                   storedobj=undefined,
                                                                   key=ObjId}),
                                                {wrq:append_to_response_body(Body, RD3a),
                                                 Ctx1#ctx{jiak_context=JC3}};
                                            _ -> {RD3, Context#ctx{jiak_context=JC3}}
                                        end,
				    {ok, RD4, Context1#ctx{incoming=JiakObject2}}
			    end;
			false ->
			    {{halt,403},
                             wrq:append_to_response_body(
                               "Missing Required Field.", RD1),
                             Context#ctx{jiak_context=JC1}}
		    end;
		false ->
		    {{halt, 403},
                     wrq:append_to_response_body(
                       "Invalid fields in request", RD1),
                     Context#ctx{jiak_context=JC1}}
            end
    end.

%% @spec post_is_create(webmachine:wrq(), context()) ->
%%          {true, webmachine:wrq(), context()}
%% @doc POST is always "create" here.  We'll make a path and
%%      handle it as a PUT to that path.
post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

%% @spec create_path(webmachine:wrq(), context()) ->
%%          {string(), webmachine:wrq(), context()}
%% @doc Create a path for converting a POST request to a PUT.  The
%%      returned path will be a fresh server-generated path in the
%%      case of a POST to a bucket, or the path for the given object
%%      in the case of a POST to a specific object.
create_path(ReqData, Context=#ctx{key=container}) ->
    %% riak_util:unique_id_62 produces url-safe strings
    {riak_util:unique_id_62(), ReqData, Context};
create_path(ReqData, Context=#ctx{key=Key}) ->
    {mochiweb_util:quote_plus(Key), ReqData, Context}.

%% @spec delete_resource(webmachine:wrq(), context()) ->
%%          {boolean(), webmachine:wrq(), context()}
%% @doc Delete the resource at the given Bucket and Key.
delete_resource(ReqData, Context=#ctx{bucket=Bucket,key=Key,
                                      jiak_client=JiakClient}) ->
    RW = integer_query("rw", 2, ReqData),
    {ok == JiakClient:delete(Bucket, Key, RW),
     ReqData, Context}.

%% @spec generate_etag(webmachine:wrq(), context()) ->
%%          {string(), webmachine:wrq(), context()}
%% @doc Generate an ETag for this resource.
generate_etag(ReqData, Context=#ctx{key=container,etag=undefined}) ->
    make_bucket_etag(ReqData, Context);
generate_etag(ReqData, Context=#ctx{etag=undefined}) ->
    make_object_etag(ReqData, Context);
generate_etag(_, #ctx{etag=ETag}) -> ETag.

%% @spec make_bucket_etag(webmachine:wrq(), context()) ->
%%          {string(), webmachine:wrq(), context()}
%% @doc Generate the ETag for a bucket.
make_bucket_etag(ReqData, Context) ->
    {ok, {Keys, Context1}} = retrieve_keylist(Context),
    ETag = mochihex:to_hex(crypto:sha(term_to_binary(Keys))),
    {ETag, ReqData, Context1#ctx{etag=ETag}}.

%% @spec retrieve_keylist(context()) -> {ok, {[binary()], context()}}
%% @doc Get the list of keys in this bucket.  This function
%%      memoizes the keylist in the context so it can be
%%      called multiple times without duplicating work.
retrieve_keylist(Context=#ctx{bucket=Bucket,jiak_client=JiakClient,
                              bucketkeys=undefined}) ->
    {ok, Keys} = JiakClient:list_keys(Bucket),
    {ok, {Keys, Context#ctx{bucketkeys=Keys}}};
retrieve_keylist(Context=#ctx{bucketkeys=Keys}) ->
    {ok, {Keys, Context}}.

%% @spec make_object_etag(webmachine:wrq(), context()) ->
%%          {string(), webmachine:wrq(), context()}
%% @doc Generate the ETag for an object.
make_object_etag(ReqData, Context=#ctx{}) ->
    {ok, {JiakObject, Context1}} = retrieve_object(ReqData, Context),
    ETag = binary_to_list(jiak_object:vtag(JiakObject)),
    {ETag, ReqData, Context1#ctx{etag=ETag}}.

%% @spec retrieve_object(webmachine:wrq(), context()) ->
%%          {ok, {jiak_object(), context()}}
%% @doc Fetch the requested object from Riak.  This function
%%      memoizes the object in the context so it can be
%%      called multiple times without duplicating work.
retrieve_object(ReqData, Context=#ctx{bucket=Bucket,key=Key,
                                      storedobj=undefined,
                                      jiak_client=JiakClient}) ->
    R = integer_query("r", 2, ReqData),
    case JiakClient:get(Bucket, Key, R) of
        {error, notfound} -> 
            {notfound, Context};
        {error, Err} ->
            {error, {Err, Context}};
        {ok, Obj} ->
            {ok, {Obj, Context#ctx{storedobj=Obj}}}
    end;
retrieve_object(_ReqData, Context=#ctx{storedobj=StoredObj}) ->
    {ok, {StoredObj, Context}}.

%% @spec last_modified(webmachine:wrq(), context()) ->
%%          {datetime(), webmachine:wrq(), context()}
%% @doc Get the last-modified time for this resource.  Bucket keylists
%%      are said to have been last-modified "now".
last_modified(ReqData, Context=#ctx{storedobj=JiakObject,
                                    key=Key}) when Key /= container ->
    {httpd_util:convert_request_date(
       binary_to_list(jiak_object:lastmod(JiakObject))), ReqData, Context};
last_modified(ReqData, Context) ->
    {erlang:universaltime(), ReqData, Context}.

%% @spec expires(webmachine:wrq(), context()) ->
%%          {datetime(), webmachine:wrq(), context()}
%% @doc Get the time at which a cache should expire its last fetch for
%%      this resource.  This function calls through to the bucket's
%%      expires_in_seconds/3 function.
expires(ReqData, Context=#ctx{key=Key, 
                              jiak_context=JC,
                              module=Mod}) ->
    {ExpiresInSecs, RD1, JC1} = Mod:expires_in_seconds(Key, ReqData, JC),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(Now+ExpiresInSecs),
     RD1, Context#ctx{jiak_context=JC1}}.

%% @spec diff_objects(webmachine:wrq(), context()) -> {diff(), context()}
%% @type diff() = {object_diff(), links_diff()}
%% @doc Compare the incoming object to the last-known value of this
%%      object (or an empty object if the incoming is new) to determine
%5      the list of changes made by the client.  This function memoizes
%%      its result in the context so it can be called multiple times
%%      without duplicating work.
diff_objects(_ReqData, Context=#ctx{incoming=NewObj, key=container}) ->
    %% same as notfound
    Diffs = jiak_object:diff(undefined, NewObj),
    {Diffs, Context#ctx{diffs=Diffs}};
diff_objects(ReqData, Context=#ctx{incoming=NewObj0, module=Mod}) ->
    case retrieve_object(ReqData, Context) of
	{notfound, NewContext} ->
	    Diffs = jiak_object:diff(undefined, NewObj0),
	    {Diffs, NewContext#ctx{diffs=Diffs}};
	{ok, {JiakObject, NewContext}} ->
	    NewObj = copy_unreadable_props(Mod,JiakObject, NewObj0),
	    Diffs = jiak_object:diff(JiakObject, NewObj),
	    {Diffs, NewContext#ctx{diffs=Diffs, storedobj=NewObj, 
				   incoming=NewObj}}
    end.

%% @spec apply_read_mask(jiak_object()) -> jiak_object()
%% @doc Remove fields from the jiak object that are not in the
%%      bucket's read mask.  Determines the module to use, then
%%      calls apply_read_mask/2.
apply_read_mask(JiakObject={struct,_}) ->
    Bucket = jiak_object:bucket(JiakObject),
    apply_read_mask(jiak_util:jiak_module_for_bucket(Bucket), JiakObject).

%% @spec apply_read_mask(jiak_module(), jiak_object()) -> jiak_object()
%% @doc Remove fields from the jiak object that are not in the
%%      bucket's read mask.
apply_read_mask(Module, JiakObject={struct,_}) ->
    {struct, OldData} = jiak_object:object(JiakObject),
    NewData = apply_read_mask1(OldData, Module:read_mask(), []),
    jiak_object:set_object(JiakObject, {struct, NewData}).

%% @private
apply_read_mask1([], _ReadMask, Acc) ->
    lists:reverse(Acc);
apply_read_mask1([{K,_V}=H|T], ReadMask, Acc) ->
    case lists:member(K, ReadMask) of
	true ->
	    apply_read_mask1(T, ReadMask, [H|Acc]);
	false ->
	    apply_read_mask1(T, ReadMask, Acc)
    end.

%% @spec copy_unreadable_props(riak_object:bucket(), jiak_object(),
%%                             jiak_object()) -> jiak_object()
%% @doc Copy fields that are not in the bucket's read mask from OldObj
%%      to NewObj.  This is necessary for computing client changes:
%%      since the client can't know the values of fields not in the
%%      read mask, it can't preserve their values, so we have to do it
%%      for them.
copy_unreadable_props(Mod, OldObj, NewObj) ->
    Allowed = Mod:allowed_fields(),
    ReadMask = Mod:read_mask(),
    Unreadable = sets:to_list(sets:subtract(
				sets:from_list(Allowed),
				sets:from_list(ReadMask))),
    {struct, OldData} = jiak_object:object(OldObj),
    {struct, NewData} = jiak_object:object(NewObj),
    UnreadableData = copy_unreadable1(Unreadable, OldData, NewData),
    jiak_object:set_object(NewObj, {struct, UnreadableData}).

%% @private    
copy_unreadable1([], _OldObj, NewObj) ->
    NewObj;
copy_unreadable1([H|T], OldObj, NewObj) ->
    copy_unreadable1(T, OldObj,
                     case proplists:lookup(H, OldObj) of
                         {H, Val} -> [{H, Val}|NewObj];
                         none     -> NewObj
                     end).

%% @spec pretty_print(webmachine:wrq(), context()) ->
%%          {string(), webmachine:wrq(), context()}
%% @doc Format the respons JSON object is a "pretty-printed" style.
pretty_print(RD1, C1=#ctx{}) ->
    {Json, RD2, C2} = produce_body(RD1, C1),
    {json_pp:print(binary_to_list(list_to_binary(Json))), RD2, C2}.

integer_query(ParamName, Default, ReqData) ->
    case wrq:get_qs_value(ParamName, ReqData) of
        undefined -> Default;
        String    -> list_to_integer(String)
    end.

%%
%% Tests
%%

mochijson_roundtrip_test() ->
    J0 = jiak_object:new(<<"fake_bucket">>, <<"fake_key">>,
                         {struct, [{<<"a">>, 1}]},
                         [[<<"other_bucket">>, <<"other_key">>, <<"fake_tag">>]]),
    R0 = jiak_object:to_riak_object(J0),
    [{M,V}] = riak_object:get_contents(R0),
    R1 = riak_object:set_vclock(
           riak_object:set_contents(
             R0,
             [{dict:store(<<"X-Riak-Last-Modified">>,
                          httpd_util:rfc1123_date(),
                          dict:store(<<"X-Riak-VTag">>, "hello", M)),
               V}]),
           vclock:increment(<<"foo">>, vclock:fresh())),
    J1 = jiak_object:from_riak_object(R1),
    J2 = mochijson2:decode(mochijson2:encode(J1)),
    ?assertEqual(jiak_object:bucket(J1), jiak_object:bucket(J2)),
    ?assertEqual(jiak_object:key(J1), jiak_object:key(J2)),
    ?assertEqual(jiak_object:vclock(J1), jiak_object:vclock(J2)),
    
    ?assertEqual(jiak_object:props(J1), jiak_object:props(J2)),
    ?assert(lists:all(fun(P) ->
                              jiak_object:getp(J1, P) ==
                                  jiak_object:getp(J2, P)
                      end,
                      jiak_object:props(J2))),

    ?assertEqual(jiak_object:links(J1), jiak_object:links(J2)).

copy_unreadable_test() ->
    Mod = jiak_default:new([{allowed_fields,
                             [<<"read0">>, <<"read1">>,
                              <<"unread0">>, <<"unread1">>]},
                            {read_mask,
                             [<<"read0">>, <<"read1">>]}]),
    Masked = jiak_object:new(
               <<"fake_bucket">>, <<"fake_key">>,
               {struct, [{<<"read0">>, <<"val0">>}]},
               []),
    UnMasked = jiak_object:new(
                 <<"fake_bucket">>, <<"fake_key">>,
                 {struct, [{<<"read0">>, <<"val1">>},
                           {<<"read1">>, <<"val2">>},
                           {<<"unread0">>, <<"val3">>}]},
                 []),
    Copied = copy_unreadable_props(Mod, UnMasked, Masked),
    
    %% should not have overwritten readable value
    ?assertEqual(jiak_object:getp(Masked, <<"read0">>),
                 jiak_object:getp(Copied, <<"read0">>)),
    
    %% should not have copied non-existent readable value
    ?assertEqual(undefined, jiak_object:getp(Copied, <<"read1">>)),
    
    %% should have copied unreadable value
    ?assertEqual(jiak_object:getp(UnMasked, <<"unread0">>),
                 jiak_object:getp(Copied, <<"unread0">>)),
    
    %% Should not have copied non-existent unreadable value
    ?assertEqual(undefined, jiak_object:getp(Copied, <<"unread1">>)).

apply_read_mask_test() ->
    Mod = jiak_default:new([{read_mask,
                             [<<"read0">>,<<"read1">>,<<"read2">>]}]),
    UnMasked = jiak_object:new(
                 <<"fake_bucket">>, <<"fake_key">>,
                 {struct, [{<<"read0">>, <<"val1">>},
                           {<<"read1">>, <<"val2">>},
                           {<<"unreadable0">>, <<"val1">>},
                           {<<"unreadable1">>, <<"val2">>}]}),
    Masked = apply_read_mask(Mod, UnMasked),

    %% unreadables removed
    ?assertEqual(2, length(jiak_object:props(Masked))),

    %% readables not removed
    ?assertEqual(jiak_object:getp(UnMasked, <<"read0">>),
                 jiak_object:getp(Masked, <<"read0">>)),
    ?assertEqual(jiak_object:getp(UnMasked, <<"read1">>),
                 jiak_object:getp(Masked, <<"read1">>)).
