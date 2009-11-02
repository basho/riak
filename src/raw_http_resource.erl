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

%% @doc Resource for serving Riak objects over HTTP in a more "raw"
%%      form (when compared to jiak_resource).
%%
%% Available operations:
%%
%% GET /Prefix/Bucket
%%   Get information about the named Bucket, in JSON form:
%%     {"props":{Prop1:Val1,Prop2:Val2,...},
%%      "keys":[Key1,Key2,...]}.
%%   Each bucket property will be included in the "props" object.
%%   "linkfun" and "chash_keyfun" properties will be encoded as
%%   JSON objects of the form:
%%     {"mod":ModuleName,
%%      "fun":FunctionName}
%%   Where ModuleName and FunctionName are each strings representing
%%   a module and function.
%%   Including the query param "props=false" will cause the "props"
%%   field to be omitted from the response.
%%   Including the query param "keys=false" will cause the "keys"
%%   field to be omitted from the response.
%%
%% PUT /Prefix/Bucket
%%   Modify bucket properties.
%%   Content-type must be application/json, and the body must have
%%   the form:
%%     {"props":{Prop:Val}}
%%   Where the "props" object takes the same form as returned from
%%   a GET of the same resource.
%%
%% POST /Prefix/Bucket
%%   Equivalent to "PUT /Prefix/Bucket/Key" where Key is chosen
%%   by the server.
%%
%% GET /Prefix/Bucket/Key
%%   Get the data stored in the named Bucket under the named Key.
%%   Content-type of the response will be whatever incoming 
%%   Content-type was used in the request that stored the data.
%%   Additional headers will include:
%%     X-Riak-Vclock: The vclock of the object.
%%     Link: The links the object has
%%     Etag: The Riak "vtag" metadata of the object
%%     Last-Modified: The last-modified time of the object
%%     Encoding: The value of the incoming Encoding header from
%%       the request that stored the data.
%%   Specifying the query param "r=R", where R is an integer will
%%   cause Riak to use R as the r-value for the read request. A
%%   default r-value of 2 will be used if none is specified.
%%   If the object is found to have siblings (only possible if the
%%   bucket property "allow_mult" has been set to true), then
%%   Content-type will be text/plain; Link, Etag, and Last-Modified
%%   headers will be omitted; and the body of the response will
%%   be a list of the vtags of each sibling.  To request a specific
%%   sibling, include the query param "vtag=V", where V is the vtag
%%   of the sibling you want.
%%
%% PUT /Prefix/Bucket/Key
%%   Store new data in the named Bucket under the named Key.
%%   A Content-type header *must* be included in the request.  The
%%   value of this header will be used in the response to subsequent
%%   GET requests.
%%   The body of the request will be stored literally as the value
%%   of the riak_object, and will be served literally as the body of
%%   the response to subsequent GET requests.
%%   Include an X-Riak-Vclock header to modify data without creating
%%   siblings.
%%   Include a Link header to set the links of the object.
%%   Include an Encoding header if you would like an Encoding header
%%   to be included in the response to subsequent GET requests.
%%   Specifying the query param "w=W", where W is an integer will
%%   cause Riak to use W as the w-value for the write request. A
%%   default w-value of 2 will be used if none is specified.
%%   Specifying the query param "dw=DW", where DW is an integer will
%%   cause Riak to use DW as the dw-value for the write request. A
%%   default dw-value of 0 will be used if none is specified.
%%   Specifying the query param "r=R", where R is an integer will
%%   cause Riak to use R as the r-value for the read request (used
%%   to determine whether or not the resource exists). A default
%%   r-value of 2 will be used if none is specified.
%%
%% POST /Prefix/Bucket/Key
%%   Equivalent to "PUT /Prefix/Bucket/Key" (useful for clients that
%%   do not support the PUT method).
%%
%% DELETE /Prefix/Bucket/Key
%%   Delete the data stored in the named Bucket under the named Key.
%%   Specifying the query param "rw=RW", where RW is an integer will
%%   cause Riak to use RW as the rw-value for the delete request. A
%%   default rw-value of 2 will be used if none is specified.
%%
%% Webmachine dispatch lines for this resource should look like:
%%
%%  {["raw", bucket],
%%   raw_http_resource,
%%   [{prefix, "raw"},
%%    {riak, local} %% or {riak, {'riak@127.0.0.1', riak_cookie}}
%%   ]}.
%%  {["raw", bucket, key],
%%   raw_http_resource,
%%   [{prefix, "raw"},
%%    {riak, local} %% or {riak, {'riak@127.0.0.1', riak_cookie}}
%%   ]}.
%%
%% These example dispatch lines will expose this resource at
%% /raw/Bucket and /raw/Bucket/Key.  The resource will attempt to
%% connect to Riak on the same Erlang node one which the resource
%% is executing.  Using the alternate {riak, {Node, Cookie}} form
%% will cause the resource to connect to riak on the specified
%% Node with the specified Cookie.
-module(raw_http_resource).
-author('Bryan Fink <bryan@basho.com>').

%% webmachine resource exports
-export([
         init/1,
         service_available/2,
         allowed_methods/2,
         malformed_request/2,
         resource_exists/2,
         last_modified/2,
         generate_etag/2,
         content_types_provided/2,
         charsets_provided/2,
         encodings_provided/2,
         content_types_accepted/2,
         produce_bucket_body/2,
         accept_bucket_body/2,
         post_is_create/2,
         create_path/2,
         process_post/2,
         produce_doc_body/2,
         accept_doc_body/2,
         produce_sibling_message_body/2,
         produce_multipart_body/2,
         multiple_choices/2,
         delete_resource/2
        ]).

%% utility exports (used in raw_http_link_walker_resource)
-export([
         vclock_header/1,
         format_link/2, format_link/4,
         multipart_encode_body/3
        ]).

%% @type context() = term()
-record(ctx, {bucket,       %% binary() - Bucket name (from uri)
              key,          %% binary() - Key (from uri)
              client,       %% riak_client() - the store client
              r,            %% integer() - r-value for reads
              w,            %% integer() - w-value for writes
              dw,           %% integer() - dw-value for writes
              rw,           %% integer() - rw-value for deletes
              prefix,       %% string() - prefix for resource uris
              riak,         %% local | {node(), atom()} - params for riak client
              doc,          %% {ok, riak_object()}|{error, term()} - the object found
              vtag,         %% string() - vtag the user asked for
              bucketprops,  %% proplist() - properties of the bucket
              links         %% [link()] - links of the object
             }).
%% @type link() = {{Bucket::binary(), Key::binary()}, Tag::binary()}

-include_lib("webmachine/include/webmachine.hrl").
-include("raw_http.hrl").

%% @spec init(proplist()) -> {ok, context()}
%% @doc Initialize this resource.  This function extracts the
%%      'prefix' and 'riak' properties from the dispatch args.
init(Props) ->
    {ok, #ctx{prefix=proplists:get_value(prefix, Props),
              riak=proplists:get_value(riak, Props)}}.

%% @spec service_available(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Determine whether or not a connection to Riak
%%      can be established.  This function also takes this
%%      opportunity to extract the 'bucket' and 'key' path
%%      bindings from the dispatch, as well as any vtag
%%      query parameter.
service_available(RD, Ctx=#ctx{riak=RiakProps}) ->
    case get_riak_client(RiakProps) of
        {ok, C} ->
            {true,
             RD,
             Ctx#ctx{
               client=C,
               bucket=list_to_binary(wrq:path_info(bucket, RD)),
               key=case wrq:path_info(key, RD) of
                       undefined -> undefined;
                       K -> list_to_binary(K)
                   end,
               vtag=wrq:get_qs_value(?Q_VTAG, RD)
              }};
        Error ->
            {false,
             wrq:set_resp_body(
               io_lib:format("Unable to connect to Riak: ~p~n", [Error]),
               wrq:set_resp_header(?HEAD_CTYPE, "text/plain", RD)),
             Ctx}
    end.

%% @spec get_riak_client(local|{node(),Cookie::atom()}) ->
%%          {ok, riak_client()} | error()
%% @doc Get a riak_client.
get_riak_client(local) ->
    riak:local_client();
get_riak_client({Node, Cookie}) ->
    erlang:set_cookie(node(), Cookie),
    riak:client_connect(Node).

%% @spec allowed_methods(reqdata(), context()) ->
%%          {[method()], reqdata(), context()}
%% @doc Get the list of methods this resource supports.
%%      HEAD, GET, POST, and PUT are supported at both
%%      the bucket and key levels.  DELETE is supported
%%      at the key level only.
allowed_methods(RD, Ctx=#ctx{key=undefined}) ->
    %% bucket-level: no delete
    {['HEAD', 'GET', 'POST', 'PUT'], RD, Ctx};
allowed_methods(RD, Ctx) ->
    %% key-level: just about anything
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], RD, Ctx}.

%% @spec is_bucket_put(reqdata(), context()) -> boolean()
%% @doc Determine whether this request is of the form
%%      PUT /Prefix/Bucket
%%      This method expects the 'key' path binding to have
%%      been set in the 'key' field of the context().
is_bucket_put(RD, Ctx) ->
    {undefined, 'PUT'} =:= {Ctx#ctx.key, wrq:method(RD)}.

%% @spec malformed_request(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Determine whether query parameters, request headers,
%%      and request body are badly-formed.
%%      Body format is checked to be valid JSON, including
%%      a "props" object for a bucket-PUT.  Body format
%%      is not tested for a key-level request (since the
%%      body may be any content the client desires).
%%      Query parameters r, w, dw, and rw are checked to
%%      be valid integers.  Their values are stored in
%%      the context() at this time.
%%      Link headers are checked for the form:
%%        &lt;/Prefix/Bucket/Key&gt;; riaktag="Tag",...
%%      The parsed links are stored in the context()
%%      at this time.
malformed_request(RD, Ctx) ->
    case is_bucket_put(RD, Ctx) of
        true ->
            malformed_bucket_put(RD, Ctx);
        false ->
            case malformed_rw_params(RD, Ctx) of
                Result={true, _, _} -> Result;
                {false, RWRD, RWCtx} ->
                    malformed_link_headers(RWRD, RWCtx)
            end
    end.

%% @spec malformed_bucket_put(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Check the JSON format of a bucket-level PUT.
%%      Must be a valid JSON object, containing a "props" object.
malformed_bucket_put(RD, Ctx) ->
    case catch mochijson2:decode(wrq:req_body(RD)) of
        {struct, Fields} ->
            case proplists:get_value(?JSON_PROPS, Fields) of
                {struct, Props} ->
                    {false, RD, Ctx#ctx{bucketprops=Props}};
                _ ->
                    {true, bucket_format_message(RD), Ctx}
            end;
        _ ->
            {true, bucket_format_message(RD), Ctx}
    end.

%% @spec bucket_format_message(reqdata()) -> reqdata()
%% @doc Put an error about the format of the bucket-PUT body
%%      in the response body of the reqdata().
bucket_format_message(RD) ->
    wrq:append_to_resp_body(
      ["bucket PUT must be a JSON object of the form:\n",
       "{\"",?JSON_PROPS,"\":{...bucket properties...}}"],
      wrq:set_resp_header(?HEAD_CTYPE, "text/plain", RD)).

%% @spec malformed_rw_params(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Check that r, w, dw, and rw query parameters are
%%      string-encoded integers.  Store the integer values
%%      in context() if so.
malformed_rw_params(RD, Ctx) ->
    lists:foldl(fun malformed_rw_param/2,
                {false, RD, Ctx},
                [{#ctx.r, "r", "2"},
                 {#ctx.w, "w", "2"},
                 {#ctx.dw, "dw", "0"},
                 {#ctx.rw, "rw", "2"}]).

%% @spec malformed_rw_param({Idx::integer(), Name::string(), Default::string()},
%%                          {boolean(), reqdata(), context()}) ->
%%          {boolean(), reqdata(), context()}
%% @doc Check that a specific r, w, dw, or rw query param is a
%%      string-encoded integer.  Store its result in context() if it
%%      is, or print an error message in reqdata() if it is not.
malformed_rw_param({Idx, Name, Default}, {Result, RD, Ctx}) ->
    case catch list_to_integer(wrq:get_qs_value(Name, Default, RD)) of
        N when is_integer(N) ->
            {Result, RD, setelement(Idx, Ctx, N)};
        _ ->
            {true,
             wrq:append_to_resp_body(
               io_lib:format("~s query parameter must be an integer~n",
                             [Name]),
               wrq:set_resp_header(?HEAD_CTYPE, "text/plain", RD)),
             Ctx}
    end.

%% @spec malformed_link_headers(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Check that the Link header in the request() is valid.
%%      Store the parsed links in context() if the header is valid,
%%      or print an error in reqdata() if it is not.
%%      A link header should be of the form:
%%        &lt;/Prefix/Bucket/Key&gt;; riaktag="Tag",...
malformed_link_headers(RD, Ctx) ->
    case catch get_link_heads(RD, Ctx) of
        Links when is_list(Links) ->
            {false, RD, Ctx#ctx{links=Links}};
        _Error ->
            {true,
             wrq:append_to_resp_body(
               io_lib:format("Invalid Link header. Links must be of the form~n"
                             "</~s/BUCKET/KEY>; riaktag=\"TAG\"~n",
                             [Ctx#ctx.prefix]),
               wrq:set_resp_header(?HEAD_CTYPE, "text/plain", RD)),
             Ctx}
    end.

%% @spec content_types_provided(reqdata(), context()) ->
%%          {[{ContentType::string(), Producer::atom()}], reqdata(), context()}
%% @doc List the content types available for representing this resource.
%%      "application/json" is the content-type for bucket-level GET requests
%%      The content-type for a key-level request is the content-type that
%%      was used in the PUT request that stored the document in Riak.
content_types_provided(RD, Ctx=#ctx{key=undefined}) ->
    %% bucket-level: JSON description only
    {[{"application/json", produce_bucket_body}], RD, Ctx};
content_types_provided(RD, Ctx0) ->
    DocCtx = ensure_doc(Ctx0),
    case DocCtx#ctx.doc of
        {ok, _} ->
            case select_doc(DocCtx) of
                {MD, _} ->
                    {[{dict:fetch(?MD_CTYPE, MD), produce_doc_body}], RD, DocCtx};
                multiple_choices ->
                    {[{"text/plain", produce_sibling_message_body},
                      {"multipart/mixed", produce_multipart_body}], RD, DocCtx}
            end;
        {error, notfound} ->
            {[{"text/plain", produce_error_message}], RD, DocCtx}
    end.

%% @spec charsets_provided(reqdata(), context()) ->
%%          {no_charset|[{Charset::string(), Producer::function()}],
%%           reqdata(), context()}
%% @doc List the charsets available for representing this resource.
%%      No charset will be specified for a bucket-level request.
%%      The charset for a key-level request is the charset that was used
%%      in the PUT request that stored the document in Riak (none if
%%      no charset was specified at PUT-time).
charsets_provided(RD, Ctx=#ctx{key=undefined}) ->
    %% default charset for bucket-level request
    {no_charset, RD, Ctx};
charsets_provided(RD, Ctx0) ->
    DocCtx = ensure_doc(Ctx0),
    case DocCtx#ctx.doc of
        {ok, _} ->
            case select_doc(DocCtx) of
                {MD, _} ->
                    case dict:find(?MD_CHARSET, MD) of
                        {ok, CS} ->
                            {[{CS, fun(X) -> X end}], RD, DocCtx};
                        error ->
                            {no_charset, RD, DocCtx}
                    end;
                multiple_choices ->
                    {no_charset, RD, DocCtx}
            end;
        {error, notfound} ->
            {no_charset, RD, DocCtx}
    end.

%% @spec encodings_provided(reqdata(), context()) ->
%%          {[{Encoding::string(), Producer::function()}], reqdata(), context()}
%% @doc List the encodings available for representing this resource.
%%      "identity" and "gzip" are available for bucket-level requests.
%%      The encoding for a key-level request is the encoding that was
%%      used in the PUT request that stored the document in Riak, or
%%      "identity" and "gzip" if no encoding was specified at PUT-time.
encodings_provided(RD, Ctx=#ctx{key=undefined}) ->
    %% identity and gzip for bucket-level request
    {default_encodings(), RD, Ctx};
encodings_provided(RD, Ctx0) ->
    DocCtx = ensure_doc(Ctx0),
    case DocCtx#ctx.doc of
        {ok, _} ->
            case select_doc(DocCtx) of
                {MD, _} ->
                    case dict:find(?MD_ENCODING, MD) of
                        {ok, Enc} ->
                            {[{Enc, fun(X) -> X end}], RD, DocCtx};
                        error ->
                            {default_encodings(), RD, DocCtx}
                    end;
                multiple_choices ->
                    {default_encodings(), RD, DocCtx}
            end;
        {error, notfound} ->
            {default_encodings(), RD, DocCtx}
    end.

%% @spec default_encodings() -> [{Encoding::string(), Producer::function()}]
%% @doc The default encodings available: identity and gzip.
default_encodings() ->
    [{"identity", fun(X) -> X end},
     {"gzip", fun(X) -> zlib:gzip(X) end}].

%% @spec content_types_accepted(reqdata(), context()) ->
%%          {[{ContentType::string(), Acceptor::atom()}],
%%           reqdata(), context()}
%% @doc Get the list of content types this resource will accept.
%%      "application/json" is the only type accepted for bucket-PUT.
%%      Whatever content type is specified by the Content-Type header
%%      of a key-level PUT request will be accepted by this resource.
%%      (A key-level put *must* include a Content-Type header.)
content_types_accepted(RD, Ctx) ->
    case is_bucket_put(RD, Ctx) of
        true ->
            %% bucket-PUT: JSON only
            {[{"application/json", accept_bucket_body}], RD, Ctx};
        false ->
            case wrq:get_req_header(?HEAD_CTYPE, RD) of
                undefined ->
                    %% user must specify content type of the data
                    {[], RD, Ctx};
                CType ->
                    %% accept whatever the user says
                    {[{hd(string:tokens(CType, ";")), accept_doc_body}],
                     RD, Ctx}
            end
    end.

%% @spec resource_exists(reqdata(), context()) -> {boolean(), reqdata(), context()}
%% @doc Determine whether or not the requested item exists.
%%      All buckets exists, whether they have data in them or not.
%%      Documents exists if a read request to Riak returns {ok, riak_object()},
%%      and either no vtag query parameter was specified, or the value of the
%%      vtag param matches the vtag of some value of the Riak object.
resource_exists(RD, Ctx=#ctx{key=undefined}) ->
    %% all buckets exist
    {true, RD, Ctx};
resource_exists(RD, Ctx0) ->
    DocCtx = ensure_doc(Ctx0),
    case DocCtx#ctx.doc of
        {ok, Doc} ->
            case DocCtx#ctx.vtag of
                undefined ->
                    {true, RD, DocCtx};
                Vtag ->
                    MDs = riak_object:get_metadatas(Doc),
                    {lists:any(fun(M) ->
                                       dict:fetch(?MD_VTAG, M) =:= Vtag
                               end,
                               MDs),
                     RD, DocCtx#ctx{vtag=Vtag}}
            end;
        {error, notfound} ->
            {false, RD, DocCtx}
    end.

%% @spec produce_bucket_body(reqdata(), context()) -> {binary(), reqdata(), context()}
%% @doc Produce the JSON response to a bucket-level GET.
%%      Includes the bucket props unless the "props=false" query param
%%      is specified.
%%      Includes the keys of the documents in the bucket unless the
%%      "keys=false" query param is specified.
%%      A Link header will also be added to the response by this function
%%      if the keys are included in the JSON object.  The Link header
%%      will include links to all keys in the bucket, with the property
%%      "rel=contained".
produce_bucket_body(RD, Ctx=#ctx{bucket=B, client=C}) ->
    SchemaPart = 
        case wrq:get_qs_value(?Q_PROPS, RD) of
            ?Q_FALSE -> [];
            _ ->
                Props = C:get_bucket(B),
                JsonProps = lists:map(fun jsonify_bucket_prop/1, Props),
                [{?JSON_PROPS, {struct, JsonProps}}]
        end,
    {KeyPart, KeyRD} =
        case wrq:get_qs_value(?Q_KEYS, RD) of
            ?Q_FALSE -> {[], RD};
            _ ->
                {ok, KeyList} = C:list_keys(B),
                {[{?Q_KEYS, KeyList}],
                 lists:foldl(
                   fun(K, Acc) ->
                           add_link_head(B, K, "contained", Acc, Ctx)
                   end,
                   RD, KeyList)}
        end,
    {mochijson2:encode({struct, SchemaPart++KeyPart}), KeyRD, Ctx}.

%% @spec accept_bucket_body(reqdata(), context()) -> {true, reqdata(), context()}
%% @doc Modify the bucket properties according to the body of the
%%      bucket-level PUT request.
accept_bucket_body(RD, Ctx=#ctx{bucket=B, client=C, bucketprops=Props}) ->
    ErlProps = lists:map(fun erlify_bucket_prop/1, Props),
    C:set_bucket(B, ErlProps),
    {true, RD, Ctx}.

%% @spec jsonify_bucket_prop({Property::atom(), erlpropvalue()}) ->
%%           {Property::binary(), jsonpropvalue()}
%% @type erlpropvalue() = integer()|string()|boolean()|
%%                        {modfun, atom(), atom()}|{atom(), atom()}
%% @type jsonpropvalue() = integer()|string()|boolean()|{struct,[jsonmodfun()]}
%% @type jsonmodfun() = {mod_binary(), binary()}|{fun_binary(), binary()}
%% @doc Convert erlang bucket properties to JSON bucket properties.
%%      Property names are converted from atoms to binaries.
%%      Integer, string, and boolean property values are left as integer,
%%      string, or boolean JSON values.
%%      {modfun, Module, Function} or {Module, Function} values of the
%%      linkfun and chash_keyfun properties are converted to JSON objects
%%      of the form:
%%        {"mod":ModuleNameAsString,
%%         "fun":FunctionNameAsString}
jsonify_bucket_prop({linkfun, {modfun, Module, Function}}) ->
    {?JSON_LINKFUN, {struct, [{?JSON_MOD,
                               list_to_binary(atom_to_list(Module))},
                              {?JSON_FUN,
                               list_to_binary(atom_to_list(Function))}]}};
jsonify_bucket_prop({linkfun, {qfun, _}}) ->
    {?JSON_LINKFUN, <<"qfun">>};
jsonify_bucket_prop({chash_keyfun, {Module, Function}}) ->
    {?JSON_CHASH, {struct, [{?JSON_MOD,
                             list_to_binary(atom_to_list(Module))},
                            {?JSON_FUN,
                             list_to_binary(atom_to_list(Function))}]}};
jsonify_bucket_prop({Prop, Value}) ->
    {list_to_binary(atom_to_list(Prop)), Value}.

%% @spec erlify_bucket_prop({Property::binary(), jsonpropvalue()}) ->
%%          {Property::atom(), erlpropvalue()}
%% @doc The reverse of jsonify_bucket_prop/1.  Converts JSON representation
%%      of bucket properties to their Erlang form.
erlify_bucket_prop({?JSON_LINKFUN, {struct, Props}}) ->
    {linkfun, {modfun,
               list_to_existing_atom(
                 binary_to_list(
                   proplists:get_value(?JSON_MOD, Props))),
               list_to_existing_atom(
                 binary_to_list(
                   proplists:get_value(?JSON_FUN, Props)))}};
erlify_bucket_prop({?JSON_CHASH, {struct, Props}}) ->
    {chash_keyfun, {list_to_existing_atom(
                      binary_to_list(
                        proplists:get_value(?JSON_MOD, Props))),
                    list_to_existing_atom(
                      binary_to_list(
                        proplists:get_value(?JSON_FUN, Props)))}};
erlify_bucket_prop({Prop, Value}) ->
    {list_to_existing_atom(binary_to_list(Prop)), Value}.

%% @spec post_is_create(reqdata(), context()) -> {boolean(), reqdata(), context()}
%% @doc POST is considered a document-creation operation for bucket-level
%%      requests (this makes webmachine call create_path/2, where the key
%%      for the created document will be chosen).
post_is_create(RD, Ctx=#ctx{key=undefined}) ->
    %% bucket-POST is create
    {true, RD, Ctx};
post_is_create(RD, Ctx) ->
    %% key-POST is not create
    {false, RD, Ctx}.

%% @spec create_path(reqdata(), context()) -> {string(), reqdata(), context()}
%% @doc Choose the Key for the document created during a bucket-level POST.
%%      This function also sets the Location header to generate a
%%      201 Created response.
create_path(RD, Ctx=#ctx{prefix=P, bucket=B}) ->
    K = riak_util:unique_id_62(),
    {K,
     wrq:set_resp_header("Location",
                         lists:append(["/",P,"/",binary_to_list(B),"/",K]),
                         RD),
     Ctx#ctx{key=list_to_binary(K)}}.

%% @spec process_post(reqdata(), context()) -> {true, reqdata(), context()}
%% @doc Pass-through for key-level requests to allow POST to function
%%      as PUT for clients that do not support PUT.
process_post(RD, Ctx) -> accept_doc_body(RD, Ctx).

%% @spec accept_doc_body(reqdata(), context()) -> {true, reqdat(), context()}
%% @doc Store the data the client is PUTing in the document.
%%      This function translates the headers and body of the HTTP request
%%      into their final riak_object() form, and executes the Riak put.
accept_doc_body(RD, Ctx=#ctx{bucket=B, key=K, client=C, links=L}) ->
    Doc0 = case Ctx#ctx.doc of
               {ok, D} -> D;
               _       -> riak_object:new(B, K, <<>>)
           end,
    VclockDoc = riak_object:set_vclock(Doc0, decode_vclock_header(RD)),
    {CType, Charset} = extract_content_type(RD),
    CTypeMD = dict:store(?MD_CTYPE, CType, dict:new()),
    CharsetMD = if Charset /= undefined ->
                        dict:store(?MD_CHARSET, Charset, CTypeMD);
                   true -> CTypeMD
                end,
    EncMD = case wrq:get_req_header(?HEAD_ENCODING, RD) of
                undefined -> CharsetMD;
                E -> dict:store(?MD_ENCODING, E, CharsetMD)
            end,
    LinkMD = dict:store(?MD_LINKS, L, EncMD),
    MDDoc = riak_object:update_metadata(VclockDoc, LinkMD),
    Doc = riak_object:update_value(MDDoc, wrq:req_body(RD)),
    ok = C:put(Doc, Ctx#ctx.w, Ctx#ctx.dw),
    {true, RD, Ctx#ctx{doc={ok, Doc}}}.

%% @spec extract_content_type(reqdata()) ->
%%          {ContentType::string(), Charset::string()|undefined}
%% @doc Interpret the Content-Type header in the client's PUT request.
%%      This function extracts the content type and charset for use
%%      in subsequent GET requests.
extract_content_type(RD) ->
    RawCType = wrq:get_req_header(?HEAD_CTYPE, RD),
    [CType|RawParams] = string:tokens(RawCType, "; "),
    Params = [ list_to_tuple(string:tokens(P, "=")) || P <- RawParams],
    {CType, proplists:get_value("charset", Params)}.

%% @spec multiple_choices(reqdata(), context()) ->
%%          {boolean(), reqdata(), context()}
%% @doc Determine whether a document has siblings.  If the user has
%%      specified a specific vtag, the document is considered not to
%%      have sibling versions.  This is a safe assumption, because
%%      resource_exists will have filtered out requests earlier for
%%      vtags that are invalid for this version of the document.
multiple_choices(RD, Ctx=#ctx{key=undefined}) ->
    %% bucket operations never have multiple choices
    {false, RD, Ctx};
multiple_choices(RD, Ctx=#ctx{vtag=undefined, doc={ok, Doc}}) ->
    %% user didn't specify a vtag, so there better not be siblings
    case riak_object:get_update_value(Doc) of
        undefined ->
            case riak_object:value_count(Doc) of
                1 -> {false, RD, Ctx};
                _ -> {true, RD, Ctx}
            end;
        _ ->
            %% just updated can't have multiple
            {false, RD, Ctx}
    end;
multiple_choices(RD, Ctx) ->
    %% specific vtag was specified
    {false, RD, Ctx}.

%% @spec produce_doc_body(reqdata(), context()) -> {binary(), reqdata(), context()}
%% @doc Extract the value of the document, and place it in the response
%%      body of the request.  This function also adds the Link header
%%      to the response.  One link will point to the bucket, with the
%%      property "rel=container".  The rest of the links will be constructed
%%      from the links of the document.
produce_doc_body(RD, Ctx) ->
    case select_doc(Ctx) of
        {MD, Doc} ->
            Links = case dict:find(?MD_LINKS, MD) of
                        {ok, L} -> L;
                        error -> []
                    end,
            LinkRD = add_container_link(
                       lists:foldl(fun({{B,K},T},Acc) ->
                                           add_link_head(B,K,T,Acc,Ctx)
                                   end,
                                   RD, Links),
                       Ctx),
            {Doc, encode_vclock_header(LinkRD, Ctx), Ctx};
        multiple_choices ->
            {<<"">>, RD, Ctx}
    end.

%% @spec produce_sibling_message_body(reqdata(), context()) ->
%%          {iolist(), reqdata(), context()}
%% @doc Produce the text message informing the user that there are multiple
%%      values for this document, and giving that user the vtags of those
%%      values so they can get to them with the vtag query param.
produce_sibling_message_body(RD, Ctx=#ctx{doc={ok, Doc}}) ->
    Vtags = [ dict:fetch(?MD_VTAG, M)
              || M <- riak_object:get_metadatas(Doc) ],
    {[<<"Siblings:\n">>, [ [V,<<"\n">>] || V <- Vtags]],
     wrq:set_resp_header(?HEAD_CTYPE, "text/plain",
                         encode_vclock_header(RD, Ctx)),
     Ctx}.

%% @spec produce_multipart_body(reqdata(), context()) ->
%%          {iolist(), reqdata(), context()}
%% @doc Produce a multipart body representation of an object with multiple
%%      values (siblings), each sibling being one part of the larger
%%      document.
produce_multipart_body(RD, Ctx=#ctx{doc={ok, Doc}, bucket=B, prefix=P}) ->
    Boundary = riak_util:unique_id_62(),
    {[[["\n--",Boundary,"\n",
        multipart_encode_body(P, B, Content)]
       || Content <- riak_object:get_contents(Doc)],
      "\n--",Boundary,"--\n"],
     wrq:set_resp_header(?HEAD_CTYPE,
                         "multipart/mixed; boundary="++Boundary,
                         encode_vclock_header(RD, Ctx)),
     Ctx}.

%% @spec multipart_encode_body(string(), binary(), {dict(), binary()}) -> iolist()
%% @doc Produce one part of a multipart body, representing one sibling
%%      of a multi-valued document.
multipart_encode_body(Prefix, Bucket, {MD, V}) ->
    [{LHead, Links}] =
        mochiweb_headers:to_list(
          mochiweb_headers:make(
            [{?HEAD_LINK, format_link(Prefix,Bucket)}|
             [{?HEAD_LINK, format_link(Prefix,B,K,T)}
              || {{B,K},T} <- case dict:find(?MD_LINKS, MD) of
                                  {ok, Ls} -> Ls;
                                  error -> []
                              end]])),
    [?HEAD_CTYPE, ": ",dict:fetch(?MD_CTYPE, MD),
     case dict:find(?MD_CHARSET, MD) of
         {ok, CS} -> ["; charset=",CS];
         error -> []
     end,
     "\n",
     case dict:find(?MD_ENCODING, MD) of
         {ok, Enc} -> [?HEAD_ENCODING,": ",Enc,"\n"];
         error -> []
     end,
     LHead,": ",Links,"\n",
     "Etag: ",dict:fetch(?MD_VTAG, MD),"\n",
     "Last-Modified: ",
     case dict:fetch(?MD_LASTMOD, MD) of
         Datetime={_,_} ->
             httpd_util:rfc1123_date(
               calendar:universal_time_to_local_time(Datetime));
         Rfc1123 when is_list(Rfc1123) ->
             Rfc1123
     end,
     "\n",
     "\n",V].
    

%% @spec select_doc(context()) -> {metadata(), value()}|multiple_choices
%% @doc Selects the "proper" document:
%%  - chooses update-value/metadata if update-value is set
%%  - chooses only val/md if only one exists
%%  - chooses val/md matching given Vtag if multiple contents exist
%%      (assumes a vtag has been specified)
select_doc(#ctx{doc={ok, Doc}, vtag=Vtag}) ->
    case riak_object:get_update_value(Doc) of
        undefined ->
            case riak_object:get_contents(Doc) of
                [Single] -> Single;
                Mult ->
                    case lists:dropwhile(
                           fun({M,_}) ->
                                   dict:fetch(?MD_VTAG, M) /= Vtag
                           end,
                           Mult) of
                        [Match|_] -> Match;
                        [] -> multiple_choices
                    end
            end;
        UpdateValue ->
            {riak_object:get_update_metadata(Doc), UpdateValue}
    end.

%% @spec encode_vclock_header(reqdata(), context()) -> reqdata()
%% @doc Add the X-Riak-Vclock header to the response.
encode_vclock_header(RD, #ctx{doc={ok, Doc}}) ->
    {Head, Val} = vclock_header(Doc),
    wrq:set_resp_header(Head, Val, RD).

%% @spec vclock_header(riak_object()) -> {Name::string(), Value::string()}
%% @doc Transform the Erlang representation of the document's vclock
%%      into something suitable for an HTTP header
vclock_header(Doc) ->
    {?HEAD_VCLOCK,
     binary_to_list(
       base64:encode(zlib:zip(term_to_binary(riak_object:vclock(Doc)))))}.

%% @spec decode_vclock_header(reqdata()) -> vclock()
%% @doc Translate the X-Riak-Vclock header value from the request into
%%      its Erlang representation.  If no vclock header exists, a fresh
%%      vclock is returned.
decode_vclock_header(RD) ->
    case wrq:get_req_header(?HEAD_VCLOCK, RD) of
        undefined -> vclock:fresh();
        Head      -> binary_to_term(zlib:unzip(base64:decode(Head)))
    end.

%% @spec ensure_doc(context()) -> context()
%% @doc Ensure that the 'doc' field of the context() has been filled
%%      with the result of a riak_client:get request.  This is a
%%      convenience for memoizing the result of a get so it can be
%%      used in multiple places in this resource, without having to
%%      worry about the order of executing of those places.
ensure_doc(Ctx=#ctx{doc=undefined, bucket=B, key=K, client=C, r=R}) ->
    Ctx#ctx{doc=C:get(B, K, R)};
ensure_doc(Ctx) -> Ctx.

%% @spec delete_resource(reqdata(), context()) -> {true, reqdata(), context()}
%% @doc Delete the document specified.
delete_resource(RD, Ctx=#ctx{bucket=B, key=K, client=C, rw=RW}) ->
    ok = C:delete(B, K, RW),
    {true, RD, Ctx}.

%% @spec generate_etag(reqdata(), context()) ->
%%          {undefined|string(), reqdata(), context()}
%% @doc Get the etag for this resource.
%%      Bucket requests will have no etag.
%%      Documents will have an etag equal to their vtag.  No etag will be
%%      given for documents with siblings, if no sibling was chosen with the
%%      vtag query param.
generate_etag(RD, Ctx=#ctx{key=undefined}) ->
    {undefined, RD, Ctx};
generate_etag(RD, Ctx) ->
    case select_doc(Ctx) of
        {MD, _} ->
            {dict:fetch(?MD_VTAG, MD), RD, Ctx};
        multiple_choices ->
            {undefined, RD, Ctx}
    end.

%% @spec last_modified(reqdata(), context()) -> 
%%          {undefined|datetime(), reqdata(), context()}
%% @doc Get the last-modified time for this resource.
%%      Bucket requests will have no last-modified time.
%%      Documents will have the last-modified time specified by the riak_object.
%%      No last-modified time will be given for documents with siblings, if no
%%      sibling was chosen with the vtag query param.
last_modified(RD, Ctx=#ctx{key=undefined}) ->
    {undefined, RD, Ctx};
last_modified(RD, Ctx) ->
    case select_doc(Ctx) of
        {MD, _} ->
            {case dict:fetch(?MD_LASTMOD, MD) of
                 Datetime={_,_} ->
                     Datetime;
                 Rfc1123 when is_list(Rfc1123) ->
                     httpd_util:convert_request_date(Rfc1123)
             end,
             RD, Ctx};
        multiple_choices ->
            {undefined, RD, Ctx}
    end.

%% @spec add_container_link(reqdata(), context()) -> reqdata()
%% @doc Add the Link header specifying the containing bucket of
%%      the document to the response.
add_container_link(RD, #ctx{prefix=Prefix, bucket=Bucket}) ->
    Val = format_link(Prefix, Bucket),
    wrq:merge_resp_headers([{?HEAD_LINK,Val}], RD).

%% @spec add_link_head(binary(), binary(), binary(), reqdata(), context()) ->
%%          reqdata()
%% @doc Add a Link header specifying the given Bucket and Key
%%      with the given Tag to the response.
add_link_head(Bucket, Key, Tag, RD, #ctx{prefix=Prefix}) ->
    Val = format_link(Prefix, Bucket, Key, Tag),
    wrq:merge_resp_headers([{?HEAD_LINK,Val}], RD).

%% @spec format_link(string(), binary()) -> string()
%% @doc Format a Link header to a bucket.
format_link(Prefix, Bucket) ->
    io_lib:format("</~s/~s>; rel=\"up\"",
                  [Prefix,
                   mochiweb_util:quote_plus(Bucket)]).

%% @spec format_link(string(), binary(), binary(), binary()) -> string()
%% @doc Format a Link header to another document.
format_link(Prefix, Bucket, Key, Tag) ->
    io_lib:format("</~s/~s/~s>; riaktag=\"~s\"",
                  [Prefix|
                   [mochiweb_util:quote_plus(E) ||
                       E <- [Bucket, Key, Tag] ]]).

%% @spec get_link_heads(reqdata(), context()) -> [link()]
%% @doc Extract the list of links from the Link request header.
%%      This function will die if an invalid link header format
%%      is found.
get_link_heads(RD, #ctx{prefix=Prefix, bucket=B}) ->
    case wrq:get_req_header(?HEAD_LINK, RD) of
        undefined -> [];
        Heads ->
            BucketLink = lists:flatten(format_link(Prefix, B)),
            {ok, Re} = re:compile("</([^/]+)/([^/]+)/([^/]+)>; riaktag=\"([^\"]+)\""),
            lists:map(
              fun(L) ->
                      {match,[InPrefix,Bucket,Key,Tag]} =
                          re:run(L, Re, [{capture,[1,2,3,4],binary}]),
                      Prefix = binary_to_list(InPrefix),
                      {{list_to_binary(mochiweb_util:unquote(Bucket)),
                        list_to_binary(mochiweb_util:unquote(Key))},
                       list_to_binary(mochiweb_util:unquote(Tag))}
              end,
              lists:delete(BucketLink, string:tokens(Heads, ",")))
    end.
