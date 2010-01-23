-module(mapred_resource).

-export([init/1, service_available/2, allowed_methods/2]).
-export([malformed_request/2, process_post/2, content_types_provided/2]).
-export([nop/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(QUERY_TOKEN, <<"query">>).
-define(INPUTS_TOKEN, <<"inputs">>).
-define(DEFAULT_TIMEOUT, 30000).

-record(state, {client, inputs, mrquery}).

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
    {['GET','HEAD','POST'], RD, State}.

malformed_request(RD, State) ->
    {Verified, Message, NewState} =
        case {wrq:method(RD), wrq:req_body(RD)} of
            {'POST', Body} when Body /= undefined ->
                verify_body(Body, State);
            _ ->
                {false, usage(), State}
        end,
    {not Verified,
     if Verified -> RD;
        true ->
             wrq:set_resp_header(
               "Content-Type", "text/plain",
               wrq:set_resp_body(Message, RD))
     end,
     NewState}.

content_types_provided(RD, State) ->
    {[{"application/json", nop}], RD, State}.

nop(RD, State) ->
    {usage(), RD, State}.

process_post(RD, #state{inputs=Inputs, mrquery=Query}=State) ->
    Me = self(),
    {ok, Client} = riak:local_client(),
    case wrq:get_qs_value("chunked", RD) of
        "true" ->
            {ok, ReqId} =
                if is_list(Inputs) ->
                        {ok, {RId, FSM}} = Client:mapred_stream(Query, Me,
                                                                ?DEFAULT_TIMEOUT),
                        gen_fsm:send_event(FSM,{input, Inputs }),
                        gen_fsm:send_event(FSM,input_done),
                        {ok, RId};
                   is_binary(Inputs) ->
                        Client:mapred_bucket_stream(Inputs, Query, Me,
                                                    ?DEFAULT_TIMEOUT)
                end,
            RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
            {true, wrq:set_resp_body({stream, stream_mapred_results(RD1, ReqId)}, RD1), State};
        Param when Param =:= "false";
                   Param =:= undefined ->
            Results = if is_list(Inputs) ->
                              Client:mapred(Inputs, Query);
                         is_binary(Inputs) ->
                              Client:mapred_bucket(Inputs, Query)
                      end,
            case Results of
                {ok, Result} ->
                    RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
                    {true, wrq:set_resp_body(mochijson2:encode(Result), RD1), State};
                Error ->
                    error_logger:error_report(Error),
                    {{halt, 500}, send_error(Error, RD), State}
            end
    end.

%% Internal functions
send_error(Error, RD)  ->
    RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
    wrq:set_resp_body(format_error(Error), RD1).

format_error({error, Message}=Error) when is_atom(Message);
                                    is_binary(Message),
                                    is_list(Message) ->
    mochijson2:encode({struct, [Error]});
format_error(_Error) ->
    mochijson2:encode({struct, [{error, map_reduce_error}]}).

stream_mapred_results(RD, ReqId) ->
    receive
        {ReqId, done} -> {<<"">>, done};
        {ReqId, {mr_results, Res}} ->
            Body = mochijson2:encode(Res),
            {iolist_to_binary(Body), fun() -> stream_mapred_results(RD, ReqId) end};
        {ReqId, {error, _}=Error} ->
            {format_error(Error), done}
    after ?DEFAULT_TIMEOUT ->
            {format_error({error, timeout}), done}
    end.

verify_body(Body, State) ->
    case catch mochijson2:decode(Body) of
        {struct, MapReduceDesc} ->
            Inputs = proplists:get_value(?INPUTS_TOKEN, MapReduceDesc),
            Query = proplists:get_value(?QUERY_TOKEN, MapReduceDesc),
            case not(Inputs =:= undefined) andalso not(Query =:= undefined) of
                true ->
                    case riak_mapred_json:parse_inputs(Inputs) of
                        {ok, ParsedInputs} ->
                            case riak_mapred_json:parse_query(Query) of
                                {ok, ParsedQuery} ->
                                    {true, [], State#state{inputs=ParsedInputs,
                                                           mrquery=ParsedQuery}};
                                {error, Message} ->
                                    {false,
                                     ["An error occurred parsing "
                                      "the \"query\" field.\n",
                                      Message],
                                     State}
                            end;
                        {error, Message} ->
                            {false,
                             ["An error occurred parsing the \"inputs\" field.\n",
                              Message],
                             State}
                    end;
                false ->
                    {false,
                     "The post body was missing the "
                     "\"inputs\" or \"query\" field.\n",
                     State}
            end;
        {'EXIT', Message} ->
            {false,
             io_lib:format("The POST body was not valid JSON.~n"
                           "The error from the parser was:~n~p~n",
                           [Message]),
             State};
        _ ->
            {false, "The POST body was not a JSON object.\n", State}
    end.

usage() ->
    "This resource accepts POSTs with bodies containing JSON of the form:\n"
        "{\n"
        " \"inputs\":[...list of inputs...],\n"
        " \"query\":[...list of map/reduce phases...]\n"
        "}\n".
