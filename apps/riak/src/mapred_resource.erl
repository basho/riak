-module(mapred_resource).

-export([init/1, service_available/2, allowed_methods/2]).
-export([malformed_request/2, process_post/2, content_types_provided/2]).
-export([nop/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(QUERY_TOKEN, <<"query">>).
-define(TARGETS_TOKEN, <<"targets">>).
-define(DEFAULT_TIMEOUT, 30000).

-record(state, {client, targets, mrquery}).

init(_) ->
    {ok, undefined}.

service_available(RD, State) ->
    case riak:local_client() of
        {ok, Client} ->
            {true, RD, #state{client=Client}};
        Error ->
            error_logger:error_report(Error),
            {false, RD, State}
    end.

allowed_methods(RD, State) ->
    {['POST'], RD, State}.

malformed_request(RD, State) ->
    {IsMalformed, NewState} = case wrq:req_body(RD) of
                                  undefined ->
                                      {true, State};
                                  Body ->
                                      {Verified, State1} = verify_body(Body, State),
                                      io:format("Verified: ~p~nState:~p~n", [Verified, State1]),
                                      {not(Verified), State1}
                              end,
    {IsMalformed, RD, NewState}.

content_types_provided(RD, State) ->
    {[{"application/json", nop}], RD, State}.

%% This should never get called
nop(_RD, _State) ->
    ok.

process_post(RD, #state{targets=Targets, mrquery=Query}=State) ->
    Me = self(),
    {ok, Client} = riak:local_client(),
    case wrq:get_qs_value("chunked", RD) of
        "true" ->
            {ok, {ReqId, FSM}} = Client:mapred_stream(Query, Me, ?DEFAULT_TIMEOUT),
            gen_fsm:send_event(FSM,{input, Targets }),
            gen_fsm:send_event(FSM,input_done),
            RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
            {true, wrq:set_resp_body({stream, stream_mapred_results(RD1, ReqId)}, RD1), State};
        Param when Param =:= "false";
                   Param =:= undefined ->
            case Client:mapred(Targets, Query) of
                {ok, Result} ->
                    RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
                    {true, wrq:set_resp_body(mochijson2:encode(Result), RD1), State};
                Error ->
                    error_logger:error_report(Error),
                    send_error(Error, RD),
                    {halt, 500}
            end
    end.

%% Internal functions
send_error({error, Message}=Error, RD) when is_atom(Message);
                                            is_binary(Message),
                                            is_list(Message) ->
    RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
    wrq:set_resp_body(mochijson2:encode({struct, [Error]}), RD1);
send_error(_Error, RD) ->
    RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
    wrq:set_resp_body(mochijson2:encode({struct, [{error, map_reduce_error}]}), RD1).

stream_mapred_results(RD, ReqId) ->
    receive
        {ReqId, done} -> {<<"">>, done};
        {ReqId, {mr_results, Res}} ->
            Body = mochijson2:encode(Res),
            {iolist_to_binary(Body), fun() -> stream_mapred_results(RD, ReqId) end};
        {ReqId, {error, _}} ->
            {<<"">>, done}
    after ?DEFAULT_TIMEOUT ->
            {<<"">>, done}
    end.

verify_body(Body, State) ->
    case mochijson2:decode(Body) of
        {struct, MapReduceDesc} ->
            Targets = proplists:get_value(?TARGETS_TOKEN, MapReduceDesc),
            Query = proplists:get_value(?QUERY_TOKEN, MapReduceDesc),
            case not(Targets =:= undefined) andalso not(Query =:= undefined) of
                true ->
                    case riak_mapred_json:parse_targets(Targets) of
                        {ok, ParsedTargets} ->
                            case riak_mapred_json:parse_query(Query) of
                                {ok, ParsedQuery} ->
                                    {true, State#state{targets=ParsedTargets, mrquery=ParsedQuery}};
                                error ->
                                    {false, State}
                            end;
                        error ->
                            {false, State}
                    end;
                false ->
                    {false, State}
            end
    end.
