%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
%% Based on mochiweb_request.erl, which is Copyright 2007 Mochi Media, Inc.
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

%% @doc Webmachine HTTP Request Abstraction.

-module(webmachine_request, [ReqState]).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([get_peer/0]). % used in initialization
-export([call/1]). % internal switching interface, used by wrcall

% actual interface for resource functions
-export([
         get_reqdata/0,
         set_reqdata/1,
	 socket/0,
	 method/0,
	 version/0,
         disp_path/0,
	 path/0,
	 raw_path/0,
         get_req_header/1,
	 req_headers/0,
	 req_body/1,
	 stream_req_body/1,
	 headers/0,
	 resp_headers/0,
	 out_headers/0,
	 get_out_header/1,
	 has_out_header/1,
	 peer/0,
	 get_header_value/1,
	 add_response_header/2,
	 add_response_headers/1,
	 remove_response_header/1,
	 merge_response_headers/1,
	 append_to_response_body/1,
	 send_response/1,
	 response_code/0,
	 set_response_code/1,
         set_resp_body/1,
	 response_body/0,
	 has_response_body/0,
         do_redirect/0,
         resp_redirect/0,
	 set_metadata/2,
	 get_metadata/1,
	 get_path_info/0,
	 get_path_info/1,
	 load_dispatch_data/6,
	 get_path_tokens/0,
	 get_app_root/0,
	 parse_cookie/0,
	 get_cookie_value/1,
	 parse_qs/0,
	 get_qs_value/1,
	 get_qs_value/2,
         range/0,
	 log_data/0
	 ]).

-include("webmachine_logger.hrl").
-include_lib("include/wm_reqstate.hrl").
-include_lib("include/wm_reqdata.hrl").

-define(WMVSN, "1.5.1").
-define(QUIP, "hack the charles gibson").
-define(IDLE_TIMEOUT, infinity).

get_peer() ->
    case ReqState#reqstate.peer of
	undefined ->
            Socket = ReqState#reqstate.socket,
	    Peer = case inet:peername(Socket) of 
		{ok, {Addr={10, _, _, _}, _Port}} ->
		    case get_header_value("x-forwarded-for") of
			{undefined, _} ->
			    inet_parse:ntoa(Addr);
			{Hosts, _} ->
			    string:strip(lists:last(string:tokens(Hosts, ",")))
		    end;
		{ok, {{127, 0, 0, 1}, _Port}} ->
		    case get_header_value("x-forwarded-for") of
			{undefined, _} ->
			    "127.0.0.1";
			{Hosts, _} ->
			    string:strip(lists:last(string:tokens(Hosts, ",")))
		    end;
		{ok, {Addr, _Port}} ->
		    inet_parse:ntoa(Addr)
            end,
            NewReqState = ReqState#reqstate{peer=Peer},
	    {Peer, NewReqState};
	_ ->
	    {ReqState#reqstate.peer, ReqState}
    end.

call(socket) -> {ReqState#reqstate.socket,ReqState};
call(get_reqdata) -> {ReqState#reqstate.reqdata, ReqState};
call({set_reqdata, RD}) -> {ok, ReqState#reqstate{reqdata=RD}};
call(method) -> {wrq:method(ReqState#reqstate.reqdata), ReqState};
call(version) -> {wrq:version(ReqState#reqstate.reqdata), ReqState};
call(raw_path) -> {wrq:raw_path(ReqState#reqstate.reqdata), ReqState};
call(req_headers) -> {wrq:req_headers(ReqState#reqstate.reqdata), ReqState};
call({req_body, MaxRecvBody}) ->
    case ReqState#reqstate.bodyfetch of
        stream ->
            {stream_conflict, ReqState};
        _ ->
            RD=(ReqState#reqstate.reqdata)#wm_reqdata{
                                      max_recv_body=MaxRecvBody},
            NewReqState=ReqState#reqstate{reqdata=RD},
            case RD#wm_reqdata.req_body of
                not_fetched_yet ->
                    NewBody = do_recv_body(NewReqState),
                    NewRD = RD#wm_reqdata{req_body=NewBody},
                    {NewBody, NewReqState#reqstate{
                                bodyfetch=standard,reqdata=NewRD}};
                X ->
                    {X, ReqState#reqstate{bodyfetch=standard}}
            end
    end;
call({stream_req_body, MaxHunk}) ->
    case ReqState#reqstate.bodyfetch of
        standard ->
            {stream_conflict, ReqState};
        _ ->
            {recv_stream_body(ReqState, MaxHunk),
             ReqState#reqstate{bodyfetch=stream}}
    end;
call(resp_headers) -> {wrq:resp_headers(ReqState#reqstate.reqdata), ReqState};
call(resp_redirect) -> {wrq:resp_redirect(ReqState#reqstate.reqdata), ReqState};
call({get_resp_header, HdrName}) ->
    Reply = mochiweb_headers:get_value(HdrName,
                wrq:resp_headers(ReqState#reqstate.reqdata)),
    {Reply, ReqState};
call(get_path_info) ->
    PropList = dict:to_list(wrq:path_info(ReqState#reqstate.reqdata)),
    {PropList, ReqState};
call({get_path_info, Key}) ->
    {wrq:path_info(Key, ReqState#reqstate.reqdata), ReqState};
call(peer) -> get_peer();
call(range) -> get_range();
call(response_code) -> {wrq:response_code(ReqState#reqstate.reqdata), ReqState};
call(app_root) -> {wrq:app_root(ReqState#reqstate.reqdata), ReqState};
call(disp_path) -> {wrq:disp_path(ReqState#reqstate.reqdata), ReqState};
call(path) -> {wrq:path(ReqState#reqstate.reqdata), ReqState};
call({get_req_header, K}) ->
    {wrq:get_req_header(K, ReqState#reqstate.reqdata), ReqState};
call({set_response_code, Code}) ->
    {ok, ReqState#reqstate{reqdata=wrq:set_response_code(
                                     Code, ReqState#reqstate.reqdata)}};
call({set_resp_header, K, V}) ->
    {ok, ReqState#reqstate{reqdata=wrq:set_resp_header(
                                     K, V, ReqState#reqstate.reqdata)}};
call({set_resp_headers, Hdrs}) ->
    {ok, ReqState#reqstate{reqdata=wrq:set_resp_headers(
                                     Hdrs, ReqState#reqstate.reqdata)}};
call({remove_resp_header, K}) ->
    {ok, ReqState#reqstate{reqdata=wrq:remove_resp_header(
                                     K, ReqState#reqstate.reqdata)}};
call({merge_resp_headers, Hdrs}) ->
    {ok, ReqState#reqstate{reqdata=wrq:merge_resp_headers(
                                     Hdrs, ReqState#reqstate.reqdata)}};
call({append_to_response_body, Data}) ->
    {ok, ReqState#reqstate{reqdata=wrq:append_to_response_body(
                                     Data, ReqState#reqstate.reqdata)}};
call({set_disp_path, P}) ->
    {ok, ReqState#reqstate{reqdata=wrq:set_disp_path(
                                     P, ReqState#reqstate.reqdata)}};
call(do_redirect) ->
    {ok, ReqState#reqstate{reqdata=wrq:do_redirect(true,
                                                   ReqState#reqstate.reqdata)}};
call({send_response, Code}) ->
    {Reply, NewState} = 
	case Code of
	    200 ->
		    send_ok_response();
	    _ ->
		    send_response(Code)
	end,
    LogData = NewState#reqstate.log_data,
    NewLogData = LogData#wm_log_data{finish_time=now()},
    {Reply, NewState#reqstate{log_data=NewLogData}};
call(resp_body) -> {wrq:resp_body(ReqState#reqstate.reqdata), ReqState};
call({set_resp_body, Body}) ->
    {ok, ReqState#reqstate{reqdata=wrq:set_resp_body(Body,
                                       ReqState#reqstate.reqdata)}};
call(has_resp_body) ->
    Reply = case wrq:resp_body(ReqState#reqstate.reqdata) of
                undefined -> false;
                <<>> -> false;
                [] -> false;
                _ -> true
            end,
    {Reply, ReqState};
call({get_metadata, Key}) ->
    Reply = case dict:find(Key, ReqState#reqstate.metadata) of
		{ok, Value} -> Value;
		error -> undefined
	    end,
    {Reply, ReqState};
call({set_metadata, Key, Value}) ->
    NewDict = dict:store(Key, Value, ReqState#reqstate.metadata),
    {ok, ReqState#reqstate{metadata=NewDict}};
call(path_tokens) -> {wrq:path_tokens(ReqState#reqstate.reqdata), ReqState};
call(req_cookie) -> {wrq:req_cookie(ReqState#reqstate.reqdata), ReqState};
call(req_qs) -> {wrq:req_qs(ReqState#reqstate.reqdata), ReqState};
call({load_dispatch_data, PathProps, HostTokens, Port,
      PathTokens, AppRoot, DispPath}) ->
    PathInfo = dict:from_list(PathProps),
    NewState = ReqState#reqstate{reqdata=wrq:load_dispatch_data(
                        PathInfo,HostTokens,Port,PathTokens,AppRoot,
                        DispPath,ReqState#reqstate.reqdata)},
    {ok, NewState};
call(log_data) -> {ReqState#reqstate.log_data, ReqState}.

get_header_value(K) ->
    {wrq:get_req_header(K, ReqState#reqstate.reqdata), ReqState}.

get_outheader_value(K) ->
    {mochiweb_headers:get_value(K,
      wrq:resp_headers(ReqState#reqstate.reqdata)), ReqState}.

send(Socket, Data) ->
    case gen_tcp:send(Socket, iolist_to_binary(Data)) of
	ok -> ok;
	{error,closed} -> ok;
	_ -> exit(normal)
    end.

send_stream_body(Socket, X) -> send_stream_body(Socket, X, 0).
send_stream_body(Socket, {<<>>, done}, SoFar) ->
    send_chunk(Socket, <<>>),
    SoFar;
send_stream_body(Socket, {Data, done}, SoFar) ->
    Size = send_chunk(Socket, Data),
    send_chunk(Socket, <<>>),
    Size + SoFar;
send_stream_body(Socket, {<<>>, Next}, SoFar) ->
    send_stream_body(Socket, Next(), SoFar);
send_stream_body(Socket, {[], Next}, SoFar) ->
    send_stream_body(Socket, Next(), SoFar);
send_stream_body(Socket, {Data, Next}, SoFar) ->
    Size = send_chunk(Socket, Data),
    send_stream_body(Socket, Next(), Size + SoFar).

send_chunk(Socket, Data) ->
    Size = iolist_size(Data),
    send(Socket, mochihex:to_hex(Size)),
    send(Socket, <<"\r\n">>),
    send(Socket, Data),
    send(Socket, <<"\r\n">>),
    Size.

send_ok_response() ->
    RD0 = ReqState#reqstate.reqdata,
    {Range, State} = get_range(),
    case Range of
	X when X =:= undefined; X =:= fail ->
	    send_response(200);
	Ranges ->
	    {PartList, Size} = range_parts(RD0, Ranges),
	    case PartList of
		[] -> %% no valid ranges
		    %% could be 416, for now we'll just return 200
		    send_response(200);
		PartList ->
		    {RangeHeaders, RangeBody} = parts_to_body(PartList, Size),
		    RespHdrsRD = wrq:set_resp_headers(
                        [{"Accept-Ranges", "bytes"} | RangeHeaders], RD0),
                    RespBodyRD = wrq:set_resp_body(
                                   RangeBody, RespHdrsRD),
		    NewState = State#reqstate{reqdata=RespBodyRD},
		    send_response(206, NewState)
	    end
    end.

send_response(Code) -> send_response(Code,ReqState).
send_response(Code, PassedState=#reqstate{reqdata=RD}) ->
    Body0 = wrq:resp_body(RD),
    {Body,Length} = case Body0 of
        {stream, StreamBody} -> {StreamBody, chunked};
        _ -> {Body0, iolist_size([Body0])}
    end,
    send(PassedState#reqstate.socket,
	 [make_version(wrq:version(RD)),
          make_code(Code), <<"\r\n">> | 
         make_headers(Code, Length, RD)]),
    FinalLength = case wrq:method(RD) of 
	'HEAD' -> Length;
	_ -> 
            case Length of
                chunked -> send_stream_body(PassedState#reqstate.socket, Body);
                _ -> send(PassedState#reqstate.socket, Body), Length
            end
    end,
    InitLogData = PassedState#reqstate.log_data,
    FinalLogData = InitLogData#wm_log_data{response_code=Code,
					   response_length=FinalLength},
    {ok, PassedState#reqstate{reqdata=wrq:set_response_code(Code, RD),
                     log_data=FinalLogData}}.

%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length() ->
    case get_header_value("transfer-encoding") of
        {undefined, _} ->
            case get_header_value("content-length") of
                {undefined, _} -> undefined;
                {Length, _} -> list_to_integer(Length)
            end;
        {"chunked", _} -> chunked;
        Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length
do_recv_body(PassedState=#reqstate{reqdata=RD}) ->
    MRB = RD#wm_reqdata.max_recv_body,
    read_whole_stream(recv_stream_body(PassedState, MRB), [], MRB, 0).

read_whole_stream({Hunk,_}, _, MaxRecvBody, SizeAcc)
  when SizeAcc + byte_size(Hunk) > MaxRecvBody -> 
    {error, req_body_too_large};
read_whole_stream({Hunk,Next}, Acc0, MaxRecvBody, SizeAcc) ->
    HunkSize = byte_size(Hunk),
    if SizeAcc + HunkSize > MaxRecvBody -> 
            {error, req_body_too_large};
       true ->
            Acc = [Hunk|Acc0],
            case Next of
                done -> iolist_to_binary(lists:reverse(Acc));
                _ -> read_whole_stream(Next(), Acc,
                                       MaxRecvBody, SizeAcc + HunkSize)
            end
    end.

recv_stream_body(PassedState=#reqstate{reqdata=RD}, MaxHunkSize) ->
    case get_header_value("expect") of
	{"100-continue", _} ->
	    send(PassedState#reqstate.socket, 
		 [make_version(wrq:version(RD)),
                  make_code(100), <<"\r\n">>]);
	_Else ->
	    ok
    end,
    case body_length() of
        {unknown_transfer_encoding, X} -> exit({unknown_transfer_encoding, X});
        undefined -> {<<>>, done};
        0 -> {<<>>, done};
        chunked -> recv_chunked_body(PassedState#reqstate.socket, MaxHunkSize);
        Length -> recv_unchunked_body(PassedState#reqstate.socket,
                                                       MaxHunkSize, Length)
    end.

recv_unchunked_body(Socket, MaxHunk, DataLeft) ->
    case MaxHunk >= DataLeft of
        true ->
            {ok,Data1} = gen_tcp:recv(Socket,DataLeft,?IDLE_TIMEOUT),
            {Data1, done};
        false ->
            {ok,Data2} = gen_tcp:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_unchunked_body(
                        Socket, MaxHunk, DataLeft-MaxHunk)
             end}
    end.
    
recv_chunked_body(Socket, MaxHunk) ->
    case read_chunk_length(Socket) of
        0 -> {<<>>, done};
        ChunkLength -> recv_chunked_body(Socket,MaxHunk,ChunkLength)
    end.
recv_chunked_body(Socket, MaxHunk, LeftInChunk) ->
    case MaxHunk >= LeftInChunk of
        true ->
            {ok,Data1} = gen_tcp:recv(Socket,LeftInChunk,?IDLE_TIMEOUT),
            {Data1,
             fun() -> recv_chunked_body(Socket, MaxHunk)
             end};
        false ->
            {ok,Data2} = gen_tcp:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_chunked_body(Socket, MaxHunk, LeftInChunk-MaxHunk)
             end}
    end.

read_chunk_length(Socket) ->
    inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            inet:setopts(Socket, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            case Hex of
                [] -> 0;
                _ -> erlang:list_to_integer(Hex, 16)
            end;
        _ ->
            exit(normal)
    end.

get_range() ->
    case get_header_value("range") of
	{undefined, _} ->
	    {undefined, ReqState#reqstate{range=undefined}};
	{RawRange, _} ->
	    Range = parse_range_request(RawRange),
	    {Range, ReqState#reqstate{range=Range}}
    end.

range_parts(_RD=#wm_reqdata{resp_body={file, IoDevice}}, Ranges) ->
    Size = iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    V ->
                        [V | Acc]
                end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};

range_parts(RD=#wm_reqdata{resp_body={stream, {Hunk,Next}}}, Ranges) ->
    % for now, streamed bodies are read in full for range requests
    MRB = RD#wm_reqdata.max_recv_body,
    range_parts(read_whole_stream({Hunk,Next}, [], MRB, 0), Ranges);

range_parts(_RD=#wm_reqdata{resp_body=Body0}, Ranges) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    {Skip, Length} ->
                        <<_:Skip/binary, PartialBody:Length/binary, _/binary>> = Body,
                        [{Skip, Skip + Length - 1, PartialBody} | Acc]
                end
        end,
    {lists:foldr(F, [], Ranges), Size}.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {_OutOfRange, _End} ->
            invalid_range
    end.

parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

parts_to_body([{Start, End, Body}], Size) ->
    %% return body for a range reponse with a single body
    ContentType = 
	case get_outheader_value("content-type") of
	    {undefined, _} ->
		"text/html";
	    {CT, _} ->
		CT
	end,
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    make_io(Start), "-", make_io(End),
                    "/", make_io(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, Size) when is_list(BodyList) ->
    %% return
    %% header Content-Type: multipart/byteranges; boundary=441934886133bdee4
    %% and multipart body
    ContentType = 
	case get_outheader_value("content-type") of
	    {undefined, _} ->
		"text/html";
	    {CT, _} ->
		CT
	end,
    Boundary = mochihex:to_hex(crypto:rand_bytes(8)),
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),
    {HeaderList, MultiPartBody}.

multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", make_io(Start), "-", make_io(End),
             "/", make_io(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].

iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

make_headers(Code, Length, RD) ->
    Hdrs0 = case Code of
        304 ->
            mochiweb_headers:make(wrq:resp_headers(RD));
        _ -> 
            case Length of
                chunked ->
                    mochiweb_headers:enter(
                      "Transfer-Encoding","chunked",
                      mochiweb_headers:make(wrq:resp_headers(RD)));
                _ ->
                    mochiweb_headers:enter(
                      "Content-Length",integer_to_list(Length),
                      mochiweb_headers:make(wrq:resp_headers(RD)))
            end
    end,
    ServerHeader = "MochiWeb/1.1 WebMachine/" ++ ?WMVSN ++ " (" ++ ?QUIP ++ ")",
    WithSrv = mochiweb_headers:enter("Server", ServerHeader, Hdrs0),
    Hdrs = case mochiweb_headers:get_value("date", WithSrv) of
	undefined ->
            mochiweb_headers:enter("Date", httpd_util:rfc1123_date(), WithSrv);
	_ ->
	    WithSrv
    end,
    F = fun({K, V}, Acc) ->
		[make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
	end,
    lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Hdrs)).

get_reqdata() -> call(get_reqdata).

set_reqdata(RD) -> call({set_reqdata, RD}).

socket() -> call(socket).

method() -> call(method).

version() -> call(version).

disp_path() -> call(disp_path).

path() -> call(path).

raw_path() -> call(raw_path).

req_headers() -> call(req_headers).
headers() -> req_headers().

req_body(MaxRevBody) -> call({req_body,MaxRevBody}).
stream_req_body(MaxHunk) -> call({stream_req_body, MaxHunk}).

resp_headers() -> call(resp_headers).
out_headers() -> resp_headers().

get_resp_header(HeaderName) -> call({get_resp_header, HeaderName}).
get_out_header(HeaderName) -> get_resp_header(HeaderName).

has_resp_header(HeaderName) ->
    case get_out_header(HeaderName) of
        undefined -> false;
        _ -> true
    end.
has_out_header(HeaderName) -> has_resp_header(HeaderName).

has_resp_body() -> call(has_resp_body).
has_response_body() -> has_resp_body().

response_code() -> call(response_code).
set_response_code(Code) -> call({set_response_code, Code}).

peer() -> call(peer).

range() -> call(range).

req_cookie() -> call(req_cookie).
parse_cookie() -> req_cookie().
get_cookie_value(Key) -> proplists:get_value(Key, req_cookie()).

req_qs() -> call(req_qs).
parse_qs() -> req_qs().
get_qs_value(Key) -> proplists:get_value(Key, req_qs()).
get_qs_value(Key, Default) -> proplists:get_value(Key, req_qs(), Default).

set_resp_body(Body) -> call({set_resp_body, Body}).
resp_body() -> call(resp_body).
response_body() -> resp_body().

get_req_header(K) -> call({get_req_header, K}).

set_resp_header(K, V) -> call({set_resp_header, K, V}).
add_response_header(K, V) -> set_resp_header(K, V).

set_resp_headers(Hdrs) -> call({set_resp_headers, Hdrs}).
add_response_headers(Hdrs) -> set_resp_headers(Hdrs).

remove_resp_header(K) -> call({remove_resp_header, K}).
remove_response_header(K) -> remove_resp_header(K).

merge_resp_headers(Hdrs) -> call({merge_resp_headers, Hdrs}).
merge_response_headers(Hdrs) -> merge_resp_headers(Hdrs).

append_to_response_body(Data) -> call({append_to_response_body, Data}).

do_redirect() -> call({do_redirect}).

resp_redirect() -> call({resp_redirect}).

get_metadata(Key) -> call({get_metadata, Key}).

set_metadata(Key, Value) -> call({set_metadata, Key, Value}).

get_path_info() -> call(get_path_info).

get_path_info(Key) -> call({get_path_info, Key}).

path_tokens() -> call(path_tokens).
get_path_tokens() -> path_tokens().

app_root() -> call(app_root).
get_app_root() -> app_root().

load_dispatch_data(Bindings, HostTokens, Port, PathTokens,
                   AppRoot, DispPath) ->
    call({load_dispatch_data, Bindings, HostTokens, Port,
          PathTokens, AppRoot, DispPath}).

log_data() -> call(log_data).
