%% -------------------------------------------------------------------
%%
%% riak_mapred_json: JSON parsing for mapreduce
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

%% @doc JSON parsing for mapreduce

-module(riak_mapred_json).

-export([parse_inputs/1, parse_query/1]).

parse_inputs(Bucket) when is_binary(Bucket) ->
    {ok, Bucket};
parse_inputs(Targets) when is_list(Targets) ->
    parse_inputs(Targets, []);
parse_inputs(Invalid) ->
    {error, ["Unrecognized format of \"inputs\" field:",
             "   ",mochijson2:encode(Invalid),
             "\n\nValid formats are:\n"
             "   - a bucket name, as a string\n"
             "   - a list of bucket/key pairs\n"]}.

parse_inputs([], Accum) ->
    if
        length(Accum) > 0 ->
            {ok, lists:reverse(Accum)};
        true ->
            {error, "No inputs were given.\n"}
    end;
parse_inputs([[Bucket, Key]|T], Accum) when is_binary(Bucket),
                                             is_binary(Key) ->
    parse_inputs(T, [{Bucket, Key}|Accum]);
parse_inputs([[Bucket, Key, KeyData]|T], Accum) when is_binary(Bucket),
                                                      is_binary(Key) ->
    parse_inputs(T, [{{Bucket, Key}, KeyData}|Accum]);
parse_inputs([Input|_], _Accum) ->
    {error, ["Unrecognized format of input element:\n"
             "   ",mochijson2:encode(Input),
             "\n\nValid formats are:\n"
             "   [Bucket, Key]\n"
             "   [Bucket, Key, KeyData]\n"
             "where Bucket and Key are strings\n"]}.

parse_query(Query) ->
    parse_query(Query, []).

parse_query([], Accum) ->
    if
        length(Accum) > 0 ->
            {ok, lists:reverse(Accum)};
        true ->
            {error, "No query phases were given\n"}
    end;
parse_query([{struct, [{Type, {struct, StepDef}}]}|T], Accum)
  when Type =:= <<"map">>; Type =:= <<"reduce">>; Type =:= <<"link">> ->
    StepType = case Type of
                   <<"map">> -> map;
                   <<"reduce">> -> reduce;
                   <<"link">> -> link
               end,
    Keep = proplists:get_value(<<"keep">>, StepDef, T==[]),
    Step = case not(Keep =:= true orelse Keep =:= false) of
               true ->
                   {error, ["The \"keep\" field was not a boolean value in:\n"
                            "   ",mochijson2:encode(
                                    {struct,[{Type,{struct,StepDef}}]}),
                            "\n"]};
               false ->
                   if StepType == link ->
                          case parse_link_step(StepDef) of
                              {ok, {Bucket, Tag}} ->
                                  {ok, {link, Bucket, Tag, Keep}};
                              LError ->
                                  LError
                          end;
                      true -> % map or reduce
                           Lang = proplists:get_value(<<"language">>, StepDef),
                           case parse_step(Lang, StepDef) of
                               {ok, ParsedStep} ->
                                   Arg = proplists:get_value(<<"arg">>, StepDef, none),
                                   {ok, {StepType, ParsedStep, Arg, Keep}};
                               QError ->
                                   QError
                           end
                   end
           end,
    case Step of
        {ok, S} -> parse_query(T, [S|Accum]);
        SError  -> SError
    end;
parse_query([Phase|_], _Accum) ->
    {error, ["Unrecognized format of query phase:\n"
             "   ",mochijson2:encode(Phase),
             "\n\nValid formats are:\n"
             "   {\"map\":{...spec...}}\n"
             "   {\"reduce\":{...spec...}}\n"
             "   {\"link\:{...spec}}\n"]};
parse_query(Invalid, _Accum) ->
    {error, ["The value of the \"query\" field was not a list:\n"
             "   ",mochijson2:encode(Invalid),"\n"]}.

parse_link_step(StepDef) ->
    Bucket = proplists:get_value(<<"bucket">>, StepDef, <<"_">>),
    Tag = proplists:get_value(<<"tag">>, StepDef, <<"_">>),
    case not(is_binary(Bucket) andalso is_binary(Tag)) of
        true ->
            {error, ["Invalid link step specification:\n"
                     "   ",mochijson2:encode({struct,StepDef}),
                     "\n\n \"bucket\" and \"tag\" fields"
                     " must have string values.\n"]};
        false ->
            {ok, {if Bucket == <<"_">> -> '_';
                     true              -> Bucket
                  end,
                  if Tag == <<"_">> -> '_';
                     true           -> Tag
                  end}}
    end.

parse_step(<<"javascript">>, StepDef) ->
    Source = proplists:get_value(<<"source">>, StepDef),
    Name = proplists:get_value(<<"name">>, StepDef),
    Bucket = proplists:get_value(<<"bucket">>, StepDef),
    Key = proplists:get_value(<<"key">>, StepDef),
    case Source of
        undefined ->
            case Name of
                undefined ->
                    case Bucket of
                        undefined ->
                            {error, ["No function specified in Javascript phase:\n"
                                     "   ",mochijson2:encode({struct,StepDef}),
                                     "\n\nFunctions may be specified by:\n"
                                     "   - a \"source\" field, with source for"
                                     " a Javascript function\n"
                                     "   - a \"name\" field, naming a predefined"
                                     " Javascript function\n"
                                     "   - \"bucket\" and \"key\" fields,"
                                     " specifying a Riak object containing"
                                     " Javascript function source\n"]};
                        _ ->
                            case Key of
                                undefined ->
                                    {error, ["Javascript phase was missing a"
                                             " \"key\" field to match the \"bucket\""
                                             " field, pointing to the function"
                                             " to evaluate in:"
                                             "   ",mochijson2:encode(
                                                    {struct,StepDef}),
                                             "\n"]};
                                _ ->
                                    {ok, {jsanon, {Bucket, Key}}}
                            end
                    end;
                _ ->
                    {ok, {jsfun, Name}}
            end;
        _ ->
            {ok, {jsanon, Source}}
    end;
parse_step(<<"erlang">>, StepDef) ->
    case bin_to_atom(proplists:get_value(<<"module">>, StepDef)) of
        {ok, Module} ->
            case bin_to_atom(proplists:get_value(<<"function">>, StepDef)) of
                {ok, Function} ->
                    {ok, {modfun, Module, Function}};
                error ->
                    {error, ["Could not convert \"function\" field value"
                             " to an atom in:"
                             "   ",mochijson2:encode({struct, StepDef}),
                             "\n"]}
            end;
        error ->
            {error, ["Could not convert \"module\" field value"
                     " to an atom in:"
                     "   ",mochijson2:encode({struct, StepDef}),"\n"]}
    end;
parse_step(undefined, StepDef) ->
    {error, ["No \"language\" was specified for the phase:\n",
             "   ",mochijson2:encode({struct,StepDef}),"\n"]};
parse_step(Language,StepDef) ->
    {error, ["Unknown language ",mochijson2:encode(Language)," in phase:\n",
             "   ",mochijson2:encode({struct,StepDef}),"\n"]}.

bin_to_atom(Binary) ->
    L = binary_to_list(Binary),
    try
        {ok, list_to_atom(L)}
    catch
        error:badarg -> error
    end.
