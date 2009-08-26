%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2007-2009 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Decision core for webmachine

-module(webmachine_decision_core).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Bryan Fink <bryan@basho.com>').
-export([handle_request/2]).
-export([do_log/1]).
-include("webmachine_logger.hrl").

handle_request(Req, Resource) ->
    put(req, Req),
    put(resource, Resource),
    try
        d(v3b13)
    catch
        error:_ ->            
            error_response(erlang:get_stacktrace())
    end.

wrcall(X) ->
    Req = get(req),
    Req:call(X).

get_header_val(H) -> wrcall({get_req_header, H}).

method() -> wrcall(method).

d(DecisionID) ->
    put(decision, DecisionID),
    log_decision(DecisionID),
    decision(DecisionID).
    
respond(Code) ->
    Resource = get(resource),
    EndTime = now(),
    case Code of
	404 ->
	    {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
	    Reason = {none, none, []},
	    ErrorHTML = ErrorHandler:render_error(Code, get(req), Reason),
            wrcall({set_resp_body, ErrorHTML});
        304 ->
            wrcall({remove_resp_header, "Content-Type"}),
            case resource_call(generate_etag) of
                undefined -> nop;
                ETag -> wrcall({set_resp_header, "ETag", ETag})
            end,
            case resource_call(expires) of
                undefined -> nop;
                Exp ->
                    wrcall({set_resp_header, "Expires",
                           httpd_util:rfc1123_date(
                              calendar:universal_time_to_local_time(Exp))})
            end;
	_ -> ignore
    end,
    put(code, Code),
    wrcall({set_response_code, Code}),
    resource_call(finish_request),
    wrcall({send_response, Code}),
    RMod = wrcall({get_metadata, 'resource_module'}),
    LogData0 = wrcall(log_data),
    LogData = LogData0#wm_log_data{resource_module=RMod,
				   end_time=EndTime},
    spawn(fun() -> do_log(LogData) end),
    Resource:stop(),
    Req = get(req),
    Req:stop().

respond(Code, Headers) ->
    wrcall({set_resp_headers, Headers}),
    respond(Code).

error_response(Code, Reason) ->
    {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
    ErrorHTML = ErrorHandler:render_error(Code, get(req), Reason),
    wrcall({set_resp_body, ErrorHTML}),
    respond(Code).
error_response(Reason) ->
    error_response(500, Reason).

decision_test(Test,TestVal,TrueFlow,FalseFlow) ->
    case Test of
	{error, Reason} -> error_response(Reason);
	{error, Reason0, Reason1} -> error_response({Reason0, Reason1});
	{halt, Code} -> respond(Code);
	TestVal -> decision_flow(TrueFlow, Test);
	_ -> decision_flow(FalseFlow, Test)
    end.
	    
decision_flow(X, TestResult) when is_integer(X) ->
    if X >= 500 -> error_response(X, TestResult);
       true -> respond(X)
    end;
decision_flow(X, _TestResult) when is_atom(X) -> d(X);
decision_flow({ErrCode, Reason}, _TestResult) when is_integer(ErrCode) ->
    error_response(ErrCode, Reason).

do_log(LogData) ->
    LoggerModule =
	case application:get_env(webmachine, webmachine_logger_module) of
	    {ok, Val} -> Val;
	    _ -> webmachine_logger
	end,
    LoggerModule:log_access(LogData),
    case application:get_env(webmachine, enable_perf_logger) of
	{ok, true} ->
	    webmachine_perf_logger:log(LogData);
	_ ->
	    ignore
    end.

resource_call(Fun) ->
    Resource = get(resource),
    {Reply, NewResource} = Resource:do(Fun,get()),
    put(resource, NewResource),
    Reply.

log_decision(DecisionID) -> 
    Resource = get(resource),
    Resource:log_d(DecisionID).

%% "Service Available"
decision(v3b13) ->	
    decision_test(resource_call(ping), pong, v3b13b, 503);
decision(v3b13b) ->	
    decision_test(resource_call(service_available), true, v3b12, 503);
%% "Known method?"
decision(v3b12) ->
    decision_test(lists:member(method(), resource_call(known_methods)),
		  true, v3b11, 501);
%% "URI too long?"
decision(v3b11) ->
    decision_test(resource_call(uri_too_long), true, 414, v3b10);
%% "Method allowed?"
decision(v3b10) ->
    Methods = resource_call(allowed_methods),
    case lists:member(method(), Methods) of
	true ->
	    d(v3b9);
	false ->
	    wrcall({set_resp_headers, [{"Allow",
		     string:join([atom_to_list(M) || M <- Methods], ", ")}]}),
	    respond(405)
    end;
%% "Malformed?"
decision(v3b9) ->
    decision_test(resource_call(malformed_request), true, 400, v3b8);
%% "Authorized?"
decision(v3b8) ->
    case resource_call(is_authorized) of
	true -> d(v3b7);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code}  ->
	    respond(Code);
        AuthHead ->
            wrcall({set_resp_header, "WWW-Authenticate", AuthHead}),
            respond(401)
    end;
%% "Forbidden?"
decision(v3b7) ->
    decision_test(resource_call(forbidden), true, 403, v3b6);
%% "Okay Content-* Headers?"
decision(v3b6) ->
    decision_test(resource_call(valid_content_headers), true, v3b5, 501);
%% "Known Content-Type?"
decision(v3b5) ->
    decision_test(resource_call(known_content_type), true, v3b4, 415);
%% "Req Entity Too Large?"
decision(v3b4) ->
    decision_test(resource_call(valid_entity_length), true, v3b3, 413);
%% "OPTIONS?"
decision(v3b3) ->
    case method() of 
	'OPTIONS' ->
	    Hdrs = resource_call(options),
	    respond(200, Hdrs);
	_ ->
	    d(v3c3)
    end;
%% Accept exists?
decision(v3c3) ->
    PTypes = [Type || {Type,_Fun} <- resource_call(content_types_provided)],
    case get_header_val("accept") of
	undefined ->
	    wrcall({set_metadata, 'content-type', hd(PTypes)}),
	    d(v3d4);
	_ ->
	    d(v3c4)
    end;
%% Acceptable media type available?
decision(v3c4) ->
    PTypes = [Type || {Type,_Fun} <- resource_call(content_types_provided)],
    AcceptHdr = get_header_val("accept"),
    case webmachine_util:choose_media_type(PTypes, AcceptHdr) of
	none ->
	    respond(406);
	MType ->
	    wrcall({set_metadata, 'content-type', MType}),
	    d(v3d4)
    end;
%% Accept-Language exists?
decision(v3d4) ->
    decision_test(get_header_val("accept-language"),
		  undefined, v3e5, v3d5);
%% Acceptable Language available? %% WMACH-46 (do this as proper conneg)
decision(v3d5) ->
    decision_test(resource_call(language_available), true, v3e5, 406);
%% Accept-Charset exists?
decision(v3e5) ->
    case get_header_val("accept-charset") of
        undefined -> decision_test(choose_charset("*"),
                                   none, 406, v3f6);
        _ -> d(v3e6)
    end;
%% Acceptable Charset available?
decision(v3e6) ->
    decision_test(choose_charset(get_header_val("accept-charset")),
                  none, 406, v3f6);
%% Accept-Encoding exists?
% (also, set content-type header here, now that charset is chosen)
decision(v3f6) ->
    CType = wrcall({get_metadata, 'content-type'}),
    CSet = case wrcall({get_metadata, 'chosen-charset'}) of
               undefined -> "";
               CS -> "; charset=" ++ CS
           end,
    wrcall({set_resp_header, "Content-Type", CType ++ CSet}),
    case get_header_val("accept-encoding") of
        undefined ->
            decision_test(choose_encoding("identity;q=1.0,*;q=0.5"),
                          none, 406, v3g7);
        _ -> d(v3f7)
    end;
%% Acceptable encoding available?
decision(v3f7) ->
    decision_test(choose_encoding(get_header_val("accept-encoding")),
                  none, 406, v3g7);
%% "Resource exists?"
decision(v3g7) ->
    % this is the first place after all conneg, so set Vary here
    case variances() of
        [] -> nop;
        Variances ->
            wrcall({set_resp_header, "Vary", string:join(Variances, ", ")})
    end,
    decision_test(resource_call(resource_exists), true, v3g8, v3h7);
%% "If-Match exists?"
decision(v3g8) ->
    decision_test(get_header_val("if-match"), undefined, v3h10, v3g9);
%% "If-Match: * exists"
decision(v3g9) ->
    decision_test(get_header_val("if-match"), "*", v3h10, v3g11);
%% "ETag in If-Match"
decision(v3g11) ->
    ReqETag = webmachine_util:unquote_header(get_header_val("if-match")),
    decision_test(resource_call(generate_etag), ReqETag, v3h10, 412);
%% "If-Match: * exists"
decision(v3h7) ->
    decision_test(get_header_val("if-match"), "*", 412, v3i7);
%% "If-unmodified-since exists?"
decision(v3h10) ->
    decision_test(get_header_val("if-unmodified-since"),undefined,v3i12,v3h11);
%% "I-UM-S is valid date?"
decision(v3h11) ->
    IUMSDate = get_header_val("if-unmodified-since"),
    decision_test(webmachine_util:convert_request_date(IUMSDate),
		  bad_date, v3i12, v3h12);
%% "Last-Modified > I-UM-S?"
decision(v3h12) ->
    ReqDate = get_header_val("if-unmodified-since"),
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    ResErlDate = resource_call(last_modified),
    decision_test(ResErlDate > ReqErlDate,
		  true, 412, v3i12);
%% "Moved permanently? (apply PUT to different URI)"
decision(v3i4) ->
    case resource_call(moved_permanently) of
	{true, MovedURI} ->
	    wrcall({set_resp_header, "Location", MovedURI}),
	    respond(301);
	false ->
	    d(v3p3);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code} ->
	    respond(Code)
    end;
%% PUT?
decision(v3i7) ->
    decision_test(method(), 'PUT', v3i4, v3k7);
%% "If-none-match exists?"
decision(v3i12) ->
    decision_test(get_header_val("if-none-match"), undefined, v3l13, v3i13);
%% "If-None-Match: * exists?"
decision(v3i13) ->
    decision_test(get_header_val("if-none-match"), "*", v3j18, v3k13);
%% GET or HEAD?
decision(v3j18) ->
    decision_test(lists:member(method(),['GET','HEAD']),
		  true, 304, 412);
%% "Moved permanently?"
decision(v3k5) ->
    case resource_call(moved_permanently) of
	{true, MovedURI} ->
	    wrcall({set_resp_header, "Location", MovedURI}),
	    respond(301);
	false ->
	    d(v3l5);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code} ->
	    respond(Code)
    end;
%% "Previously existed?"
decision(v3k7) ->
    decision_test(resource_call(previously_existed), true, v3k5, v3l7);
%% "Etag in if-none-match?"
decision(v3k13) ->
    ReqETag = webmachine_util:unquote_header(get_header_val("if-none-match")),
    decision_test(resource_call(generate_etag), ReqETag, v3j18, v3l13);
%% "Moved temporarily?"
decision(v3l5) ->
    case resource_call(moved_temporarily) of
	{true, MovedURI} ->
	    wrcall({set_resp_header, "Location", MovedURI}),
	    respond(307);
	false ->
	    d(v3m5);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code} ->
	    respond(Code)
    end;
%% "POST?"
decision(v3l7) ->
    decision_test(method(), 'POST', v3m7, 404);
%% "IMS exists?"
decision(v3l13) ->
    decision_test(get_header_val("if-modified-since"), undefined, v3m16, v3l14);
%% "IMS is valid date?"
decision(v3l14) -> 
    IMSDate = get_header_val("if-modified-since"),
    decision_test(webmachine_util:convert_request_date(IMSDate),
		  bad_date, v3m16, v3l15);
%% "IMS > Now?"
decision(v3l15) ->
    NowDateTime = calendar:universal_time(),
    ReqDate = get_header_val("if-modified-since"),
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    decision_test(ReqErlDate > NowDateTime,
		  true, v3m16, v3l17);
%% "Last-Modified > IMS?"
decision(v3l17) ->
    ReqDate = get_header_val("if-modified-since"),    
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    ResErlDate = resource_call(last_modified),
    decision_test(ResErlDate =:= undefined orelse ResErlDate > ReqErlDate,
                  true, v3m16, 304);
%% "POST?"
decision(v3m5) ->
    decision_test(method(), 'POST', v3n5, 410);
%% "Server allows POST to missing resource?"
decision(v3m7) ->
    decision_test(resource_call(allow_missing_post), true, v3n11, 404);
%% "DELETE?"
decision(v3m16) ->
    decision_test(method(), 'DELETE', v3m20, v3n16);
%% DELETE enacted immediately?
%% Also where DELETE is forced.
decision(v3m20) ->
    decision_test(resource_call(delete_resource), true, v3m20b, 500);
decision(v3m20b) ->
    decision_test(resource_call(delete_completed), true, v3o20, 202);
%% "Server allows POST to missing resource?"
decision(v3n5) ->
    decision_test(resource_call(allow_missing_post), true, v3n11, 410);
%% "Redirect?"
decision(v3n11) ->
    Stage1 = case resource_call(post_is_create) of
        true ->
            case resource_call(create_path) of
                undefined -> error_response("post_is_create w/o create_path");
                NewPath ->
                    case is_list(NewPath) of
                        false -> error_response("create_path not a string");
                        true ->
                            wrcall({set_disp_path, NewPath}),
                            Res = accept_helper(),
                            case Res of
                                {respond, Code} -> respond(Code);
                                {halt, Code} -> respond(Code);
                                {error, _,_} -> error_response(Res);
                                {error, _} -> error_response(Res);
                                _ -> stage1_ok
                            end
                    end
            end;
        _ ->
            case resource_call(process_post) of
                true -> stage1_ok;
                {halt, Code} -> respond(Code);
                Err -> error_response(Err)
            end
    end,
    case Stage1 of
        stage1_ok ->
            case wrcall(resp_redirect) of
                true ->
                    case wrcall({get_resp_header, "Location"}) of
                        undefined ->
                            respond(500,
                                    "Response had do_redirect but no Location");
                        _ ->
                            respond(303)
                    end;
                _ ->
                    d(v3p11)
            end;
        _ -> nop
    end;
%% "POST?"
decision(v3n16) ->
    decision_test(method(), 'POST', v3n11, v3o16);
%% Conflict?
decision(v3o14) ->
    case resource_call(is_conflict) of
        true -> respond(409);
        _ -> Res = accept_helper(),
             case Res of
                 {respond, Code} -> respond(Code);
                 {halt, Code} -> respond(Code);
                 {error, _,_} -> error_response(Res);
                 {error, _} -> error_response(Res);
                 _ -> d(v3p11)
             end
    end;
%% "PUT?"
decision(v3o16) ->
    decision_test(method(), 'PUT', v3o14, v3o18);
%% Multiple representations?
% (also where body generation for GET and HEAD is done)
decision(v3o18) ->    
    BuildBody = case method() of
        'GET' -> true;
        'HEAD' -> true;
        _ -> false
    end,
    FinalBody = case BuildBody of
        true ->
            case resource_call(generate_etag) of
                undefined -> nop;
                ETag -> wrcall({set_resp_header, "ETag", ETag})
            end,
            CT = wrcall({get_metadata, 'content-type'}),
            case resource_call(last_modified) of
                undefined -> nop;
                LM ->
                    wrcall({set_resp_header, "Last-Modified",
                           httpd_util:rfc1123_date(
                             calendar:universal_time_to_local_time(LM))})
            end,
            case resource_call(expires) of
                undefined -> nop;
                Exp ->
                    wrcall({set_resp_header, "Expires",
                           httpd_util:rfc1123_date(
                              calendar:universal_time_to_local_time(Exp))})
            end,
            F = hd([Fun || {Type,Fun} <- resource_call(content_types_provided),
                           CT =:= Type]),
            resource_call(F);
        false -> nop
    end,
    case FinalBody of
        {error, _} -> error_response(FinalBody);
        {error, _,_} -> error_response(FinalBody);
        {halt, Code} -> respond(Code);
        nop -> d(v3o18b);
        _ -> wrcall({set_resp_body,
                     encode_body(FinalBody)}),
             d(v3o18b)
    end;

decision(v3o18b) ->
    decision_test(resource_call(multiple_choices), true, 300, 200);
%% Response includes an entity?
decision(v3o20) ->
    decision_test(wrcall(has_resp_body), true, v3o18, 204);
%% Conflict?
decision(v3p3) ->
    case resource_call(is_conflict) of
        true -> respond(409);
        _ -> Res = accept_helper(),
             case Res of
                 {respond, Code} -> respond(Code);
                 {halt, Code} -> respond(Code);
                 {error, _,_} -> error_response(Res);
                 {error, _} -> error_response(Res);
                 _ -> d(v3p11)
             end
    end;

%% New resource?  (at this point boils down to "has location header")
decision(v3p11) ->
    case wrcall({get_resp_header, "Location"}) of
        undefined -> d(v3o20);
        _ -> respond(201)
    end.

accept_helper() ->
    CT = case get_header_val("Content-Type") of
             undefined -> "application/octet-stream";
             Other -> Other
         end,
    {MT, MParams} = webmachine_util:media_type_to_detail(CT),
    wrcall({set_metadata, 'mediaparams', MParams}),
    case [Fun || {Type,Fun} <-
                     resource_call(content_types_accepted), MT =:= Type] of
        [] -> {respond,415};
        AcceptedContentList ->
            F = hd(AcceptedContentList),
            resource_call(F)
    end.

encode_body(Body) ->
    ChosenCSet = wrcall({get_metadata, 'chosen-charset'}),
    Charsetter = 
    case resource_call(charsets_provided) of
        no_charset -> fun(X) -> X end;
        CP -> hd([Fun || {CSet,Fun} <- CP, ChosenCSet =:= CSet])
    end,
    ChosenEnc = wrcall({get_metadata, 'content-encoding'}),
    Encoder = hd([Fun || {Enc,Fun} <- resource_call(encodings_provided),
                         ChosenEnc =:= Enc]),
    case Body of
        {stream, StreamBody} ->
            {stream, make_encoder_stream(Encoder, Charsetter, StreamBody)};
        _ ->
            Encoder(Charsetter(iolist_to_binary(Body)))
    end.

make_encoder_stream(Encoder, Charsetter, {Body, done}) ->
    {Encoder(Charsetter(Body)), done};
make_encoder_stream(Encoder, Charsetter, {Body, Next}) ->
    {Encoder(Charsetter(Body)),
     fun() -> make_encoder_stream(Encoder, Charsetter, Next()) end}.
            
choose_encoding(AccEncHdr) ->
    Encs = [Enc || {Enc,_Fun} <- resource_call(encodings_provided)],
    case webmachine_util:choose_encoding(Encs, AccEncHdr) of
	none -> none;
	ChosenEnc ->
            case ChosenEnc of
                "identity" ->
                    nop;
                _ ->
                    wrcall({set_resp_header, "Content-Encoding",ChosenEnc})
            end,
            wrcall({set_metadata, 'content-encoding',ChosenEnc}),
	    ChosenEnc
    end.

choose_charset(AccCharHdr) ->
    case resource_call(charsets_provided) of
        no_charset ->
            no_charset;
        CL ->
            CSets = [CSet || {CSet,_Fun} <- CL],
            case webmachine_util:choose_charset(CSets, AccCharHdr) of
                none -> none;
                Charset ->
                    wrcall({set_metadata, 'chosen-charset',Charset}),
                    Charset
            end
    end.

variances() ->
    Accept = case length(resource_call(content_types_provided)) of
        1 -> [];
        0 -> [];
        _ -> ["Accept"]
    end,
    AcceptEncoding = case length(resource_call(encodings_provided)) of
	1 -> [];
	0 -> [];
	_ -> ["Accept-Encoding"]
    end,
    AcceptCharset = case resource_call(charsets_provided) of
        no_charset -> [];
        CP ->
            case length(CP) of
                1 -> [];
                0 -> [];
                _ -> ["Accept-Charset"]
            end
    end,
    Accept ++ AcceptEncoding ++ AcceptCharset ++ resource_call(variances).
    
