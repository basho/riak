%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2007-2009 Basho Technologies, Inc.  All Rights Reserved.
%% @doc Example webmachine_resource.

-module(webmachine_demo_resource).
-author('Justin Sheehy <justin@basho.com>').
-export([init/1, to_html/2, to_text/2, content_types_provided/2,
         is_authorized/2, generate_etag/2, expires/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
    
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html},{"text/plain",to_text}], ReqData, Context}.

to_text(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    Body = io_lib:format("Hello ~s from webmachine.~n", [Path]),
    {Body, ReqData, Context}.

to_html(ReqData, Context) ->
    {Body, _RD, Ctx2} = to_text(ReqData, Context),
    HBody = io_lib:format("<html><body>~s</body></html>~n",
                          [erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

is_authorized(ReqData, Context) ->
    case wrq:disp_path(ReqData) of
        "authdemo" -> 
            case wrq:get_req_header("authorization", ReqData) of
                "Basic "++Base64 ->
                    Str = base64:mime_decode_to_string(Base64),
                    case string:tokens(Str, ":") of
                        ["authdemo", "demo1"] ->
                            {true, ReqData, Context};
                        _ ->
                            {"Basic realm=webmachine", ReqData, Context}
                    end;
                _ ->
                    {"Basic realm=webmachine", ReqData, Context}
            end;
        _ -> {true, ReqData, Context}
    end.

expires(ReqData, Context) -> {{{2021,1,1},{0,0,0}}, ReqData, Context}.

generate_etag(ReqData, Context) -> {wrq:raw_path(ReqData), ReqData, Context}.

