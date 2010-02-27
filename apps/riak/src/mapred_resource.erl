%% -------------------------------------------------------------------
%%
%% mapred_resource: webmachine resource for mapreduce requests
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

%% @doc webmachine resource for mapreduce requests

-module(mapred_resource).

-export([init/1, service_available/2, allowed_methods/2]).
-export([malformed_request/2, process_post/2, content_types_provided/2]).
-export([nop/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(QUERY_TOKEN, <<"query">>).
-define(INPUTS_TOKEN, <<"inputs">>).
-define(TIMEOUT_TOKEN, <<"timeout">>).
-define(DEFAULT_TIMEOUT, 60000).

-record(state, {client, inputs, timeout, mrquery, boundary}).

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

process_post(RD, #state{inputs=Inputs, mrquery=Query, timeout=Timeout}=State) ->
    Me = self(),
    {ok, Client} = riak:local_client(),
    case wrq:get_qs_value("chunked", RD) of
        "true" ->
            {ok, ReqId} =
                if is_list(Inputs) ->
                        {ok, {RId, FSM}} = Client:mapred_stream(Query, Me,
                                                                Timeout),
                        luke_flow:add_inputs(FSM, Inputs),
                        luke_flow:finish_inputs(FSM),
                        {ok, RId};
                   is_binary(Inputs) ->
                        Client:mapred_bucket_stream(Inputs, Query, Me,
                                                    Timeout)
                end,
            Boundary = riak_util:unique_id_62(),
            RD1 = wrq:set_resp_header("Content-Type", "multipart/mixed;boundary=" ++ Boundary, RD),
            State1 = State#state{boundary=Boundary},
            {true, wrq:set_resp_body({stream, stream_mapred_results(RD1, ReqId, State1)}, RD1), State1};
        Param when Param =:= "false";
                   Param =:= undefined ->
            Results = if is_list(Inputs) ->
                              Client:mapred(Inputs, Query);
                         is_binary(Inputs) ->
                              Client:mapred_bucket(Inputs, Query)
                      end,
            RD1 = wrq:set_resp_header("Content-Type", "application/json", RD),
            case Results of
                {error, _} ->
                    {{halt, 500}, send_error(Results, RD1), State};
                {ok, Result} ->
                    {true, wrq:set_resp_body(mochijson2:encode(Result), RD1), State}
            end
    end.

%% Internal functions
send_error(Error, RD)  ->
    wrq:set_resp_body(format_error(Error), RD).

format_error({error, Message}=Error) when is_atom(Message);
                                          is_binary(Message) ->
    mochijson2:encode({struct, [Error]});
format_error({error, Error}) when is_list(Error) ->
    mochijson2:encode({struct, Error});
format_error(_Error) ->
    mochijson2:encode({struct, [{error, map_reduce_error}]}).

stream_mapred_results(RD, ReqId, #state{timeout=Timeout}=State) ->
    FinalTimeout = erlang:trunc(Timeout * 1.02),
    receive
        {flow_results, ReqId, done} -> {iolist_to_binary(["\n--", State#state.boundary, "--\n"]), done};
        {flow_results, ReqId, {error, Error}} ->
            {format_error(Error), done};
        {flow_error, ReqId, Error} ->
            {format_error({error, Error}), done};
        {flow_results, ReqId, Res} ->
            Data = mochijson2:encode(Res),
            Body = ["\n--", State#state.boundary, "\n",
                    "Content-Type: application/json\n\n",
                    Data],
            {iolist_to_binary(Body), fun() -> stream_mapred_results(RD, ReqId, State) end}
    after FinalTimeout ->
            {format_error({error, timeout}), done}
    end.

verify_body(Body, State) ->
    case catch mochijson2:decode(Body) of
        {struct, MapReduceDesc} ->
            Timeout = case proplists:get_value(?TIMEOUT_TOKEN, MapReduceDesc, ?DEFAULT_TIMEOUT) of
                          X when is_number(X) andalso X > 0 ->
                              X;
                          _ ->
                              ?DEFAULT_TIMEOUT
                      end,
            Inputs = proplists:get_value(?INPUTS_TOKEN, MapReduceDesc),
            Query = proplists:get_value(?QUERY_TOKEN, MapReduceDesc),
            case not(Inputs =:= undefined) andalso not(Query =:= undefined) of
                true ->
                    case riak_mapred_json:parse_inputs(Inputs) of
                        {ok, ParsedInputs} ->
                            case riak_mapred_json:parse_query(Query) of
                                {ok, ParsedQuery} ->
                                    {true, [], State#state{inputs=ParsedInputs,
                                                           mrquery=ParsedQuery,
                                                           timeout=Timeout}};
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
