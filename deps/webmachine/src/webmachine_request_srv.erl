%% @author Andy Gross <andy@basho.com> 
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2007-2009 Basho Technologies
%% Portions derived from code Copyright 2007-2008 Bob Ippolito, Mochi Media
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

-module(webmachine_request_srv).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_server).
-export([start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-include("webmachine_logger.hrl").
-include_lib("include/wm_reqdata.hrl").

-define(WMVSN, "1.4").
-define(QUIP, "our dis is finally out").

% 120 second default idle timeout
-define(IDLE_TIMEOUT, infinity).
-record(state, {socket=undefined,
		metadata=dict:new(),
		range=undefined,
		peer=undefined,
                reqdata=undefined,
                bodyfetch=undefined,
		log_data=#wm_log_data{}
	       }).

start_link(Socket, Method, RawPath, Version, Headers) ->
    gen_server:start_link(?MODULE,
                          [Socket, Method, RawPath, Version, Headers], []).

init([Socket, Method, RawPath, Version, Headers]) ->
    %%process_flag(trap_exit, true),
    %% Calling get_peer() here is a little bit of an ugly way to populate the
    %% client IP address but it will do for now.
    {Peer, State} = get_peer(#state{socket=Socket,
         reqdata=wrq:create(Method,Version,RawPath,Headers)}),
    PeerState = State#state{reqdata=wrq:set_peer(Peer,State#state.reqdata)},
    LogData = #wm_log_data{start_time=now(),
			   method=Method,
			   headers=Headers,
			   peer=State#state.peer,
			   path=RawPath,
			   version=Version,
			   response_code=404,
			   response_length=0},
    {ok, PeerState#state{log_data=LogData}}.

handle_call(socket, _From, State) ->
    Reply = State#state.socket,
    {reply, Reply, State};
handle_call(get_reqdata, _From, State) ->
    {reply, State#state.reqdata, State};
handle_call({set_reqdata, RD=#wm_reqdata{req_body=RBody}}, _From, State) ->
    TheRD = case RBody of
        not_fetched_yet ->
            OldRD = State#state.reqdata,
            OldBody = OldRD#wm_reqdata.req_body,
            RD#wm_reqdata{req_body=OldBody};
        _ ->
            RD
    end,
    {reply, ok, State#state{reqdata=TheRD}};
handle_call(method, _From, State) ->
    {reply, wrq:method(State#state.reqdata), State};
handle_call(version, _From, State) ->
    {reply, wrq:version(State#state.reqdata), State};
handle_call(raw_path, _From, State) ->
    {reply, wrq:raw_path(State#state.reqdata), State};
handle_call(req_headers, _From, State) ->
    {reply, wrq:req_headers(State#state.reqdata), State};
handle_call(req_body, _From, State=#state{bodyfetch=stream}) ->
    {reply, stream_conflict, State};
handle_call({req_body, MaxRecvBody}, _From, State0=#state{reqdata=RD0}) ->
    RD=RD0#wm_reqdata{max_recv_body=MaxRecvBody},
    State=State0#state{reqdata=RD},
    {Body, FinalState} = case RD#wm_reqdata.req_body of
        not_fetched_yet ->
            NewBody = do_recv_body(State),
            NewRD = RD#wm_reqdata{req_body=NewBody},
            {NewBody, State#state{bodyfetch=standard,reqdata=NewRD}};
        X ->
            {X, State#state{bodyfetch=standard}}
    end,
    {reply, Body, FinalState};
handle_call({stream_req_body,_}, _From, State=#state{bodyfetch=standard}) ->
    {reply, stream_conflict, State};
handle_call({stream_req_body, MaxHunk}, _From, State) ->
    {reply, recv_stream_body(State, MaxHunk), State#state{bodyfetch=stream}};
handle_call(resp_headers, _From, State) ->
    {reply, wrq:resp_headers(State#state.reqdata), State};
handle_call(resp_redirect, _From, State) ->
    {reply, wrq:resp_redirect(State#state.reqdata), State};
handle_call({get_resp_header, HdrName}, _From, State) ->
    Reply = mochiweb_headers:get_value(HdrName,
                wrq:resp_headers(State#state.reqdata)),
    {reply, Reply, State};
handle_call(get_path_info, _From, State) ->
    PropList = dict:to_list(wrq:path_info(State#state.reqdata)),
    {reply, PropList, State};
handle_call({get_path_info, Key}, _From, State) ->
    {reply, wrq:path_info(Key, State#state.reqdata), State};
handle_call(peer, _From, State) ->
    {Reply, NewState} = get_peer(State),
    {reply, Reply, NewState};
handle_call(range, _From, State) ->
    {Reply, NewState} = get_range(State),
    {reply, Reply, NewState};
handle_call(response_code, _From, State) ->
    {reply, wrq:response_code(State#state.reqdata), State};
handle_call(app_root, _From, State) ->
    {reply, wrq:app_root(State#state.reqdata), State};
handle_call(disp_path, _From, State) ->
    {reply, wrq:disp_path(State#state.reqdata), State};
handle_call(path, _From, State) ->
    {reply, wrq:path(State#state.reqdata), State};
handle_call({get_req_header, K}, _From, State) ->
    {reply, wrq:get_req_header(K, State#state.reqdata), State};
handle_call({set_response_code, Code}, _From, State) ->
    NewState = State#state{reqdata=wrq:set_response_code(
                                     Code, State#state.reqdata)},
    {reply, ok, NewState};
handle_call({set_resp_header, K, V}, _From, State) ->
    NewState = State#state{reqdata=wrq:set_resp_header(
                                     K, V, State#state.reqdata)},
    {reply, ok, NewState};
handle_call({set_resp_headers, Hdrs}, _From, State) ->
    NewState = State#state{reqdata=wrq:set_resp_headers(
                                     Hdrs, State#state.reqdata)},
    {reply, ok, NewState};
handle_call({remove_resp_header, K}, _From, State) ->
    NewState = State#state{reqdata=wrq:remove_resp_header(
                                     K, State#state.reqdata)},
    {reply, ok, NewState};
handle_call({merge_resp_headers, Hdrs}, _From, State) ->
    NewState = State#state{reqdata=wrq:merge_resp_headers(
                                     Hdrs, State#state.reqdata)},
    {reply, ok, NewState};
handle_call({append_to_response_body, Data}, _From, State) ->
    NewState = State#state{reqdata=wrq:append_to_response_body(
                                     Data, State#state.reqdata)},
    {reply, ok, NewState};
handle_call({set_disp_path, P}, _From, State) ->
    NewState = State#state{reqdata=wrq:set_disp_path(
                                     P, State#state.reqdata)},
    {reply, ok, NewState};
handle_call(do_redirect, _From, State) ->
    NewState = State#state{reqdata=wrq:do_redirect(true,
                                     State#state.reqdata)},
    {reply, ok, NewState};
handle_call({send_response, Code}, _From, State) ->
    {Reply, NewState} = 
	case Code of
	    200 ->
		    send_ok_response(Code, State);
	    _ ->
		    send_response(Code, State)
	end,
    LogData = NewState#state.log_data,
    NewLogData = LogData#wm_log_data{finish_time=now()},
    {reply, Reply, NewState#state{log_data=NewLogData}};
handle_call(resp_body, _From, State) ->
    {reply, wrq:resp_body(State#state.reqdata), State};
handle_call({set_resp_body, Body}, _From, State) ->
    NewState = State#state{reqdata=wrq:set_resp_body(Body,
                                     State#state.reqdata)},
    {reply, ok, NewState};
handle_call(has_resp_body, _From, State) ->
    Reply = case wrq:resp_body(State#state.reqdata) of
                undefined -> false;
                <<>> -> false;
                [] -> false;
                _ -> true
            end,
    {reply, Reply, State};
handle_call({get_metadata, Key}, _From, State) ->
    Reply = case dict:find(Key, State#state.metadata) of
		{ok, Value} -> Value;
		error -> undefined
	    end,
    {reply, Reply, State};
handle_call({set_metadata, Key, Value}, _From, State) ->
    NewDict = dict:store(Key, Value, State#state.metadata),
    {reply, ok, State#state{metadata=NewDict}};
handle_call(path_tokens, _From, State) ->
    {reply, wrq:path_tokens(State#state.reqdata), State};
handle_call(req_cookie, _From, State) ->
    {reply, wrq:req_cookie(State#state.reqdata), State};
handle_call(req_qs, _From, State) ->
    {reply, wrq:req_qs(State#state.reqdata), State};
handle_call({load_dispatch_data, PathProps,PathTokens,AppRoot,DispPath,WMReq},
            _From, State) ->
    PathInfo = dict:from_list(PathProps),
    NewState = State#state{reqdata=wrq:load_dispatch_data(
               PathInfo,PathTokens,AppRoot,DispPath,WMReq,State#state.reqdata)},
    {reply, ok, NewState};
handle_call(log_data, _From, State) -> {reply, State#state.log_data, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

get_peer(State) ->
    case State#state.peer of
	undefined ->
            Socket = State#state.socket,
	    Peer = case inet:peername(Socket) of 
		{ok, {Addr={10, _, _, _}, _Port}} ->
		    case get_header_value("x-forwarded-for", State) of
			{undefined, _} ->
			    inet_parse:ntoa(Addr);
			{Hosts, _} ->
			    string:strip(lists:last(string:tokens(Hosts, ",")))
		    end;
		{ok, {{127, 0, 0, 1}, _Port}} ->
		    case get_header_value("x-forwarded-for", State) of
			{undefined, _} ->
			    "127.0.0.1";
			{Hosts, _} ->
			    string:strip(lists:last(string:tokens(Hosts, ",")))
		    end;
		{ok, {Addr, _Port}} ->
		    inet_parse:ntoa(Addr)
            end,
            NewState = State#state{peer=Peer},
	    {Peer, NewState};
	_ ->
	    {State#state.peer, State}
    end.

get_header_value(K, State) ->
    {wrq:get_req_header(K, State#state.reqdata), State}.

get_outheader_value(K, State) ->
    {mochiweb_headers:get_value(K,
      wrq:resp_headers(State#state.reqdata)), State}.

send(Socket, Data) ->
    case gen_tcp:send(Socket, iolist_to_binary(Data)) of
	ok -> ok;
	{error,closed} -> ok;
	_ -> exit(normal)
    end.

send_stream_body(Socket, X) -> send_stream_body(Socket, X, 0).
send_stream_body(Socket, {Data, done}, SoFar) ->
    Size = send_chunk(Socket, Data),
    send_chunk(Socket, <<>>),
    Size + SoFar;
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

send_ok_response(200, InitState) ->
    RD0 = InitState#state.reqdata,
    {Range, State} = get_range(InitState),
    case Range of
	X when X =:= undefined; X =:= fail ->
	    send_response(200, State);
	Ranges ->
	    {PartList, Size} = range_parts(RD0, Ranges),
	    case PartList of
		[] -> %% no valid ranges
		    %% could be 416, for now we'll just return 200
		    send_response(200, State);
		PartList ->
		    {RangeHeaders, RangeBody} =
			parts_to_body(PartList, State, Size),
		    RespHdrsRD = wrq:set_resp_headers(
                        [{"Accept-Ranges", "bytes"} | RangeHeaders], RD0),
                    RespBodyRD = wrq:set_resp_body(
                                   RangeBody, RespHdrsRD),
		    NewState = State#state{reqdata=RespBodyRD},
		    send_response(206, NewState)
	    end
    end.

send_response(Code, State=#state{reqdata=RD}) ->
    Body0 = wrq:resp_body(RD),
    {Body,Length} = case Body0 of
        {stream, StreamBody} -> {StreamBody, chunked};
        _ -> {Body0, iolist_size([Body0])}
    end,
    send(State#state.socket,
	 [make_version(wrq:version(RD)),
          make_code(Code), <<"\r\n">> | 
         make_headers(Code, Length, RD)]),
    FinalLength = case wrq:method(RD) of 
	'HEAD' -> Length;
	_ -> 
            case Length of
                chunked -> send_stream_body(State#state.socket, Body);
                _ -> send(State#state.socket, Body), Length
            end
    end,
    InitLogData = State#state.log_data,
    FinalLogData = InitLogData#wm_log_data{response_code=Code,
					   response_length=FinalLength},
    {ok, State#state{reqdata=wrq:set_response_code(Code, RD),
                     log_data=FinalLogData}}.

%% @spec body_length(state()) -> undefined | chunked | unknown_transfer_encoding | integer()
%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length(State) ->
    case get_header_value("transfer-encoding", State) of
        {undefined, _} ->
            case get_header_value("content-length", State) of
                {undefined, _} -> undefined;
                {Length, _} -> list_to_integer(Length)
            end;
        {"chunked", _} -> chunked;
        Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%% @spec do_recv_body(state()) -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length
do_recv_body(State=#state{reqdata=RD}) ->
    MRB = RD#wm_reqdata.max_recv_body,
    read_whole_stream(recv_stream_body(State, MRB), [], MRB, 0).

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

recv_stream_body(State = #state{reqdata=RD}, MaxHunkSize) ->
    case get_header_value("expect", State) of
	{"100-continue", _} ->
	    send(State#state.socket, 
		 [make_version(wrq:version(RD)),
                  make_code(100), <<"\r\n">>]);
	_Else ->
	    ok
    end,
    case body_length(State) of
        {unknown_transfer_encoding, X} -> exit({unknown_transfer_encoding, X});
        undefined -> {<<>>, done};
        0 -> {<<>>, done};
        chunked -> recv_chunked_body(State#state.socket, MaxHunkSize);
        Length -> recv_unchunked_body(State#state.socket, MaxHunkSize, Length)
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

get_range(State) ->
    case get_header_value("range", State) of
	{undefined, _} ->
	    {undefined, State#state{range=undefined}};
	{RawRange, _} ->
	    Range = parse_range_request(RawRange),
	    {Range, State#state{range=Range}}
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

parts_to_body([{Start, End, Body}], State, Size) ->
    %% return body for a range reponse with a single body
    ContentType = 
	case get_outheader_value("content-type", State) of
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
parts_to_body(BodyList, State, Size) when is_list(BodyList) ->
    %% return
    %% header Content-Type: multipart/byteranges; boundary=441934886133bdee4
    %% and multipart body
    ContentType = 
	case get_outheader_value("content-type", State) of
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

