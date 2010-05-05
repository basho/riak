%% -------------------------------------------------------------------
%%
%% riak_kv_pb_socket: service protocol buffer clients
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc service protocol buffer clients

-module(riak_kv_pb_socket).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("riakc/include/riakclient_pb.hrl").
-include_lib("riakc/include/riakc_pb.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type msg() ::  atom() | tuple().

-record(state, {sock,      % protocol buffers socket
                client,    % local client
                req,       % current request (for multi-message requests like list keys)
                req_ctx}). % context to go along with request (partial results, request ids etc)


-define(PROTO_MAJOR, 1).
-define(PROTO_MINOR, 0).
-define(DEFAULT_TIMEOUT, 60000).

%% ===================================================================
%% Public API
%% ===================================================================

start_link(Socket) ->
    gen_server2:start_link(?MODULE, [Socket], []).

init([Socket]) -> 
    riak_kv_stat:update(pbc_connect),
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {ok, C} = riak:local_client(),
    {ok, #state{sock = Socket, client = C}}.

handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info({tcp_closed, Socket}, State=#state{sock=Socket}) ->
    {stop, normal, State};
handle_info({tcp, _Sock, Data}, State=#state{sock=Socket}) ->
    [MsgCode|MsgData] = Data,
    Msg = riakc_pb:decode(MsgCode, MsgData),
    case process_message(Msg, State) of
        {pause, NewState} ->
            ok;
        NewState ->
            inet:setopts(Socket, [{active, once}])
    end,
    {noreply, NewState};

%% Handle responses from stream_list_keys 
handle_info({ReqId, done},
            State=#state{sock = Socket, req=#rpblistkeysreq{}, req_ctx=ReqId}) ->
    NewState = send_msg(#rpblistkeysresp{done = 1}, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState#state{req = undefined, req_ctx = undefined}};
handle_info({ReqId, {keys, []}}, State=#state{req=#rpblistkeysreq{}, req_ctx=ReqId}) ->
    {noreply, State}; % No keys - no need to send a message, will send done soon.
handle_info({ReqId, {keys, Keys}}, State=#state{req=#rpblistkeysreq{}, req_ctx=ReqId}) ->
    {noreply, send_msg(#rpblistkeysresp{keys = Keys}, State)};

%% Handle response from mapred_stream/mapred_bucket_stream
handle_info({flow_results, ReqId, done},
            State=#state{sock = Socket, req=#rpbmapredreq{}, req_ctx=ReqId}) ->
    NewState = send_msg(#rpbmapredresp{done = 1}, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState#state{req = undefined, req_ctx = undefined}};

handle_info({flow_results, ReqId, {error, Reason}},
            State=#state{sock = Socket, req=#rpbmapredreq{}, req_ctx=ReqId}) ->
    NewState = send_error("~p", [Reason], State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState#state{req = undefined, req_ctx = undefined}};

handle_info({flow_results, PhaseId, ReqId, Res},
            State=#state{sock=Socket,
                         req=#rpbmapredreq{content_type = ContentType}, 
                         req_ctx=ReqId}) ->
    case encode_mapred_phase(Res, ContentType) of
        {error, Reason} ->
            NewState = send_error("~p", [Reason], State),
            inet:setopts(Socket, [{active, once}]),
            {noreply, NewState#state{req = undefined, req_ctx = undefined}};
        Response ->
            {noreply, send_msg(#rpbmapredresp{phase=PhaseId, 
                                              response=Response}, State)}
    end;

handle_info({flow_error, ReqId, Error},
            State=#state{sock = Socket, req=#rpbmapredreq{}, req_ctx=ReqId}) ->
    NewState = send_error("~p", [Error], State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState#state{req = undefined, req_ctx = undefined}};

handle_info(_, State) -> % Ignore any late replies from gen_servers/messages from fsms
    {noreply, State}.

terminate(_Reason, _State) -> 
    riak_kv_stat:update(pbc_disconnect),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Message Handling
%% ===================================================================

%% Process an incoming protocol buffers message.  Return either
%% a new #state{} if new incoming messages should be received
%% or {pause, #state{}} if the incoming TCP socket should not be
%% set active again.
%%
%% If 'pause' is returned, it needs to be re-enabled by whatever
%% callbacks are waiting for it.
%%
-spec process_message(msg(), #state{}) ->  #state{} | {pause, #state{}}.
process_message(rpbpingreq, State) ->
    send_msg(rpbpingresp, State);

process_message(rpbgetclientidreq, #state{client=C} = State) ->
    Resp = #rpbgetclientidresp{client_id = C:get_client_id()},
    send_msg(Resp, State);

process_message(#rpbsetclientidreq{client_id = ClientId}, State) ->
    {ok, C} = riak:local_client(ClientId),
    send_msg(rpbsetclientidresp, State#state{client = C});

process_message(rpbgetserverinforeq, State) ->
    Resp = #rpbgetserverinforesp{node = riakc_pb:to_binary(node()), 
                                 server_version = get_riak_version()},
    send_msg(Resp, State);

process_message(#rpbgetreq{bucket=B, key=K, r=R}, 
                #state{client=C} = State) ->
    case C:get(B, K, default_r(R)) of
        {ok, O} ->
            PbContent = riakc_pb:pbify_rpbcontents(riak_object:get_contents(O), []),
            GetResp = #rpbgetresp{content = PbContent,
                                  vclock = pbify_rpbvc(riak_object:vclock(O))},
            send_msg(GetResp, State);
        {error, notfound} ->
            send_msg(#rpbgetresp{}, State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

process_message(#rpbputreq{bucket=B, key=K, vclock=PbVC, content=RpbContent,
                           w=W, dw=DW, return_body=ReturnBody}, 
                #state{client=C} = State) ->

    O0 = riak_object:new(B, K, <<>>),  
    O1 = update_rpbcontent(O0, RpbContent),
    O  = update_pbvc(O1, PbVC),
    % erlang_protobuffs encodes as 1/0/undefined
    Options = case ReturnBody of 1 -> [returnbody]; _ -> [] end,
    case C:put(O, default_w(W), default_dw(DW), default_timeout(), Options) of
        ok ->
            send_msg(#rpbputresp{}, State);
        {ok, Obj} ->
            PbContents = riakc_pb:pbify_rpbcontents(riak_object:get_contents(Obj), []),
            PutResp = #rpbputresp{contents = PbContents,
                                  vclock = pbify_rpbvc(riak_object:vclock(Obj))},
            send_msg(PutResp, State);
        {error, notfound} ->
            send_msg(#rpbputresp{}, State);            
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

process_message(#rpbdelreq{bucket=B, key=K, rw=RW}, 
                #state{client=C} = State) ->
    case C:delete(B, K, default_rw(RW)) of
        ok ->
            send_msg(rpbdelresp, State);
        {error, notfound} ->  %% delete succeeds if already deleted
            send_msg(rpbdelresp, State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

process_message(rpblistbucketsreq, 
                #state{client=C} = State) ->
    case C:list_buckets() of
        {ok, Buckets} ->
            send_msg(#rpblistbucketsresp{buckets = Buckets}, State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

%% Start streaming in list keys 
process_message(#rpblistkeysreq{bucket=B}=Req, 
                #state{client=C} = State) ->
    %% Pause incoming packets - stream_list_keys results
    %% will be processed by handle_info, it will 
    %% set socket active again on completion of streaming.
    {ok, ReqId} = C:stream_list_keys(B),
    {pause, State#state{req = Req, req_ctx = ReqId}};

%% Get bucket properties
process_message(#rpbgetbucketreq{bucket=B}, 
                #state{client=C} = State) ->
    Props = C:get_bucket(B),
    PbProps = riakc_pb:pbify_rpbbucketprops(Props),
    send_msg(#rpbgetbucketresp{props = PbProps}, State);

%% Set bucket properties
process_message(#rpbsetbucketreq{bucket=B, props = PbProps}, 
                #state{client=C} = State) ->
    Props = riakc_pb:erlify_rpbbucketprops(PbProps),
    ok = C:set_bucket(B, Props),
    send_msg(rpbsetbucketresp, State);

%% Start map/reduce job - results will be processed in handle_info
process_message(#rpbmapredreq{request=MrReq, content_type=ContentType}=Req, 
                #state{client=C} = State) ->

    case decode_mapred_query(MrReq, ContentType) of
        {error, Reason} ->
            send_error("~p", [Reason], State);

        {ok, Inputs, Query, Timeout} ->
            if is_list(Inputs) ->
                    case C:mapred_stream(Query, self(), Timeout) of
                        {stop, Error} ->
                            send_error("~p", [Error], State);
                        
                        {ok, {ReqId, FSM}} ->
                            luke_flow:add_inputs(FSM, Inputs),
                            luke_flow:finish_inputs(FSM),
                            %% Pause incoming packets - map/reduce results
                            %% will be processed by handle_info, it will 
                            %% set socket active again on completion of streaming.
                            {pause, State#state{req = Req, req_ctx = ReqId}}
                    end;
               is_binary(Inputs) ->
                    case C:mapred_bucket_stream(Inputs, Query, 
                                                self(), Timeout) of
                        {stop, Error} ->
                            send_error("~p", [Error], State);

                        {ok, ReqId} ->
                            {pause, State#state{req = Req, req_ctx = ReqId}}
                    end
            end
    end.

%% Send a message to the client
-spec send_msg(msg(), #state{}) -> #state{}.
send_msg(Msg, State) ->
    Pkt = riakc_pb:encode(Msg),
    gen_tcp:send(State#state.sock, Pkt),
    State.
    
%% Send an error to the client
-spec send_error(string(), list(), #state{}) -> #state{}.
send_error(Msg, Fmt, State) ->
    send_error(Msg, Fmt, ?RIAKC_ERR_GENERAL, State).

-spec send_error(string(), list(), non_neg_integer(), #state{}) -> #state{}.
send_error(Msg, Fmt, ErrCode, State) ->
    %% protocol buffers accepts nested lists for binaries so no need to flatten the list
    ErrMsg = io_lib:format(Msg, Fmt),
    send_msg(#rpberrorresp{errmsg=ErrMsg, errcode=ErrCode}, State).

%% Update riak_object with the pbcontent provided
update_rpbcontent(O0, RpbContent) -> 
    {MetaData, Value} = riakc_pb:erlify_rpbcontent(RpbContent),
    O1 = riak_object:update_metadata(O0, MetaData),
    riak_object:update_value(O1, Value).

%% Update riak_object with vector clock 
update_pbvc(O0, PbVc) ->
    Vclock = erlify_rpbvc(PbVc),
    riak_object:set_vclock(O0, Vclock).

%% Set default values in the options record if none are provided.
%% Erlang protobuffs does not support default, so have to do it here.
default_r(undefined) ->
    2;
default_r(R) ->
    R.

default_w(undefined) ->
    2;
default_w(W) ->
    W.

default_dw(undefined) ->
    0;
default_dw(DW) ->
    DW.

default_rw(undefined) ->
    2;
default_rw(RW) ->
    RW.

default_timeout() ->
    60000.
        
%% Convert a vector clock to erlang
erlify_rpbvc(undefined) ->
    vclock:fresh();
erlify_rpbvc(PbVc) ->
    binary_to_term(zlib:unzip(PbVc)).

%% Convert a vector clock to protocol buffers
pbify_rpbvc(Vc) ->
    zlib:zip(term_to_binary(Vc)).

%% Return the current version of riak_kv
-spec get_riak_version() -> binary().
get_riak_version() ->
    {ok, Vsn} = application:get_key(riak_kv, vsn),
    riakc_pb:to_binary(Vsn).

%% Decode a mapred query
%% {ok, ParsedInputs, ParsedQuery, Timeout};
decode_mapred_query(Query, <<"application/json">>) ->
    riak_kv_mapred_json:parse_request(Query);
decode_mapred_query(Query, <<"application/x-erlang-binary">>) ->
    riak_kv_mapred_term:parse_request(Query);
decode_mapred_query(_Query, ContentType) ->
    {error, {unknown_content_type, ContentType}}.

%% Convert a map/reduce phase to the encoding requested
encode_mapred_phase(Res, <<"application/json">>) ->
    mochijson2:encode(Res);
encode_mapred_phase(Res, <<"application/x-erlang-binary">>) ->
    term_to_binary(Res);
encode_mapred_phase(_Res, ContentType) ->
    {error, {unknown_content_type, ContentType}}.

