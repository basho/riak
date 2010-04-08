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
-behaviour(gen_server2).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type msg() ::  atom() | tuple().

-record(state, {sock,      % protocol buffers socket
                hello,     % hello message from client
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
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {ok, #state{sock = Socket}}.

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
            State=#state{req=#rpbmapredreq{}, req_ctx=ReqId}) ->
    {noreply, send_msg(#rpbmapredresp{phase=PhaseId, data = riakc_pb:pbify_rpbterm(Res)}, State)};

handle_info({flow_error, ReqId, Error},
            State=#state{sock = Socket, req=#rpbmapredreq{}, req_ctx=ReqId}) ->
    NewState = send_error("~p", [Error], State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState#state{req = undefined, req_ctx = undefined}};

handle_info(_, State) -> % Ignore any late replies from gen_servers/messages from fsms
    {noreply, State}.

terminate(_Reason, _State) -> ok.

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
process_message(#rpbhelloreq{proto_major = 1, client_id = ClientId} = Hello, State) ->
    {ok, Client} = riak:local_client(ClientId), % optional, will be undefined if not given
    send_msg(#rpbhelloresp{proto_major = ?PROTO_MAJOR, 
                           proto_minor = ?PROTO_MINOR,
                           node = list_to_binary(atom_to_list(node())),
                           client_id = Client:get_client_id(),
                           server_version = get_riak_version()},
             State#state{hello = Hello, client = Client});

process_message(#rpbhelloreq{}, State) ->
    send_error("Only proto_major 1 currently supported", [], State);

process_message(_Req, #state{hello = undefined} = State) ->
    send_error("Please say Hello first", [], State);

process_message(rpbpingreq, State) ->
    send_msg(rpbpingresp, State);

process_message(#rpbgetreq{bucket=B, key=K, options=RpbOptions0}, 
                #state{client=C} = State) ->
    Opts = default_rpboptions(RpbOptions0),
    case C:get(B, K, Opts#rpboptions.r) of
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

process_message(#rpbputreq{bucket=B, key=K, vclock=PbVC, 
                           content = RpbContent, options=RpbOptions0}, 
                #state{client=C} = State) ->

    Opts = default_rpboptions(RpbOptions0),
    O0 = riak_object:new(B, K, <<>>),  
    O1 = update_rpbcontent(O0, RpbContent),
    O  = update_pbvc(O1, PbVC),

    case C:put(O, Opts#rpboptions.w, Opts#rpboptions.dw) of
        ok ->
            case Opts#rpboptions.return_body of % erlang_protobuffs encodes as 1/0/undefined
                1 ->
                    send_put_return_body(B, K, Opts, State);
                _ ->
                    send_msg(#rpbputresp{}, State)
            end;
        {error, precommit_fail} ->
            send_error("precommit fail", [], State);
        {error, {precommit_fail, Reason}} ->
            send_error("precommit fail - ~p", [Reason], State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

process_message(#rpbdelreq{bucket=B, key=K, options=RpbOptions0}, 
                #state{client=C} = State) ->
    Opts = default_rpboptions(RpbOptions0),
    case C:delete(B, K, Opts#rpboptions.rw) of
        ok ->
            send_msg(rpbdelresp, State);
        {error, notfound} ->  %% delete succeeds if already deleted
            send_msg(rpbdelresp, State);
        {error, precommit_fail} ->
            send_error("precommit fail", [], State);
        {error, {precommit_fail, Reason}} ->
            send_error("precommit fail - ~p", [Reason], State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

process_message(#rpbgetbucketpropsreq{bucket=B, names = Names}, 
                #state{client=C} = State) ->
    Props = C:get_bucket(B),
    PbProps = riakc_pb:pbify_bucket_props(filter_props(Names, Props)),
    Resp = #rpbgetbucketpropsresp{properties = PbProps},
    send_msg(Resp, State);

process_message(#rpbsetbucketpropsreq{bucket=B, properties = RpbTerm}, 
                #state{client=C} = State) ->
    ErlProps = riakc_pb:erlify_bucket_props(RpbTerm),
    C:set_bucket(B, ErlProps),
    send_msg(rpbsetbucketpropsresp, State);

process_message(rpblistbucketsreq, 
                #state{client=C} = State) ->
    case C:list_buckets() of
        {ok, Buckets} ->
            send_msg(#rpblistbucketsresp{buckets = Buckets, done = 1}, State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end;

%% Start streaming in list keys - results will be processed in handle_info
process_message(#rpblistkeysreq{bucket=B}=Req, 
                #state{client=C} = State) ->
    case C:stream_list_keys(B) of
        {ok, ReqId} ->
            {pause, State#state{req = Req, req_ctx = ReqId}}
    end;

%% Start map/reduce job - results will be processed in handle_info
process_message(#rpbmapredreq{input_bucket=B, input_keys=PbKeys, phases=PbQuery}=Req, 
                #state{client=C} = State) ->
    case riakc_pb:erlify_mapred_query(PbQuery) of
        {error, Reason} ->
            send_error("~p", [Reason], State);

        {ok, Query} ->
            if %% Check we have B or PbKeys
                (B =/= undefined andalso PbKeys =:= undefined) ->
                    {ok, ReqId} = C:mapred_bucket_stream(B, Query, self(), ?DEFAULT_TIMEOUT),
                    {pause, State#state{req = Req, req_ctx = ReqId}};
                (B =:= undefined andalso PbKeys =/= undefined) -> %
                    Inputs = [riakc_pb:erlify_mapred_input(PbKey) || PbKey <- PbKeys],
                    {ok, {ReqId, FSM}} = C:mapred_stream(Query, self(), ?DEFAULT_TIMEOUT),
                    luke_flow:add_inputs(FSM, Inputs),
                    luke_flow:finish_inputs(FSM),
                    {pause, State#state{req = Req, req_ctx = ReqId}};
                true ->
                    send_error("map/reduce takes either an input_bucket or input_keys, not both", 
                               [], State)
            end
    end.

%% @private
%% @doc if return_body was requested, call the client to get it and return
send_put_return_body(B, K, Opts, State=#state{client = C}) ->
    case C:get(B, K, Opts#rpboptions.r) of
        {ok, O} ->
            PbContents = riakc_pb:pbify_rpbcontents(riak_object:get_contents(O), []),
            PutResp = #rpbputresp{contents = PbContents,
                                  vclock = pbify_rpbvc(riak_object:vclock(O))},
            send_msg(PutResp, State);
        {error, notfound} ->
            %% User may have NRW set so this is possible - send the same as a get not found
            send_msg(#rpbputresp{}, State);
        {error, Reason} ->
            send_error("~p", [Reason], State)
    end.

%% Send a message to the client
-spec send_msg(msg(), #state{}) -> #state{}.
send_msg(Msg, State) ->
    Pkt = riakc_pb:encode(Msg),
    ok = gen_tcp:send(State#state.sock, Pkt),
    State.
    
%% Send an error to the client
-spec send_error(string(), list(), #state{}) -> #state{}.
send_error(Msg, Fmt, State) ->
    ErrMsg = lists:flatten(io_lib:format(Msg, Fmt)),
    send_msg(#rpberrorresp{errmsg = ErrMsg}, State).

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
default_rpboptions(undefined) ->
    #rpboptions{r = 2, w = 2, dw = 0, rw = 2};
default_rpboptions(RpbOptions) ->
    lists:foldl(fun default_rpboption/2, RpbOptions, 
                [{#rpboptions.r, 2},
                 {#rpboptions.w, 2},
                 {#rpboptions.dw, 0},
                 {#rpboptions.rw, 2}]).

default_rpboption({Idx, Default}, RpbOptions) ->
    case element(Idx, RpbOptions) of
        undefined ->
            setelement(Idx, RpbOptions, Default);
        _ ->
            RpbOptions
    end.
            
%% Filter out the requested properties
filter_props(undefined, Props) ->
    Props;
filter_props(Names, Props) ->
    Atoms = make_bucket_prop_names(Names, []),
    [Prop || {Name,_Value}=Prop <- Props, lists:member(Name, Atoms)].
        
%% Make bucket prop names - converting any binaries/lists to atoms   
%% Drop any unknown names
make_bucket_prop_names([], Acc) ->
    Acc;
make_bucket_prop_names([Name|Rest], Acc) when is_binary(Name) ->   
    make_bucket_prop_names([binary_to_list(Name) | Rest], Acc);
make_bucket_prop_names([Name|Rest], Acc) when is_list(Name) ->
    case catch list_to_existing_atom(Name) of
        {'EXIT', _} ->
            make_bucket_prop_names(Rest, Acc);
        Atom ->
            make_bucket_prop_names(Rest, [Atom | Acc])
    end;
make_bucket_prop_names([Name|Rest], Acc) when is_atom(Name) ->
    make_bucket_prop_names(Rest, [Name | Acc]).

%% Return the current version of riak_kv
-spec get_riak_version() -> binary().
get_riak_version() ->
    Apps = application:which_applications(),
    {value,{riak_kv,_,Vsn}} = lists:keysearch(riak_kv, 1, Apps),
    riakc_pb:to_binary(Vsn).

%% Convert a vector clock to erlang
erlify_rpbvc(undefined) ->
    vclock:fresh();
erlify_rpbvc(PbVc) ->
    binary_to_term(zlib:unzip(PbVc)).

%% Convert a vector clock to protocol buffers
pbify_rpbvc(Vc) ->
    zlib:zip(term_to_binary(Vc)).

