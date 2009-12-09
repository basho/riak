%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2009 Basho Technologies

%% @doc Utility for parsing multipart form bodies.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%% http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(webmachine_multipart).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([get_all_parts/2,stream_parts/2, find_boundary/1]).
-export([test_body/0,test_body2/0]).

% @type incoming_req_body() = binary().
% The request body, in "multipart/form-data" (rfc2388) form,

% @type boundary() = string().
% The multipart boundary, as taken from the containing message's content-type.

% @type fpart() = {fpartname(), {[fparam()],[fheader()]}, fcontent()}.
% A single part of a multipart form.

% @type fpartname() = string().
% The name from the form field of a form part.

% @type fparam() = {binary(), binary()}.
% A key-value parameter from the content-disposition header in a form part.

% @type fheader() = {binary(), binary()}.
% A header name and value supplied within a form part.

% @type fcontent() = binary().
% The body content within a form part.

% @doc Find the multipart boundary for a request.
% @spec find_boundary(wrq:wm_reqdata()) -> boundary()
find_boundary(ReqData) ->
    ContentType = wrq:get_req_header("content-type", ReqData),
    string:substr(ContentType, string:str(ContentType, "boundary=") 
                  + length("boundary=")).

% @doc Turn a multipart form into component parts.
% @spec get_all_parts(incoming_req_body(), boundary()) -> [fpart()]
get_all_parts(Body, Boundary) when is_binary(Body), is_list(Boundary) ->
    StreamStruct = send_streamed_body(Body,1024),
    getparts1(stream_parts(StreamStruct, "--" ++ Boundary), []).

% @doc Similar to get_all_parts/2, but for streamed/chunked bodies.
%   Takes as input the result of wrq:stream_req_body/2, and provides
%   either the atom 'done_parts' when no more parts are available, or
%   a tuple with the next part and a function.  That function will
%   have 0-arity and the same return type as stream_parts/2 itself.
% @spec stream_parts(wm_stream(), boundary()) ->
%                                    'done_parts' | {fpart(), function()}
stream_parts(StreamStruct, Boundary) ->
    stream_form(StreamStruct, "--" ++ Boundary, []).

stream_form(_, _, [<<"----\n">>|_]) -> done_parts;
stream_form(_, _, [<<"--\n">>|_]) -> done_parts;
stream_form({Hunk, Next}, Boundary, []) ->
    stream_form(get_more_data(Next), Boundary, re:split(Hunk, Boundary,[]));
stream_form({Hunk, Next}, Boundary, [<<>>|DQ]) ->
    stream_form({Hunk, Next}, Boundary, DQ);
stream_form({Hunk, Next}, Boundary, [H|[T1|T2]]) ->
    {make_part(H), fun() ->
                    stream_form({Hunk, Next}, Boundary, [T1|T2]) end};
stream_form({Hunk, really_done}, Boundary, DQ) ->
    DQBin = iolist_to_binary(DQ),
    FullHunk = <<DQBin/binary, Hunk/binary>>,
    stream_parts(re:split(FullHunk, Boundary,[]));
stream_form({Hunk, Next}, Boundary, [Single]) ->
    FullHunk = <<Single/binary, Hunk/binary>>,
    stream_form(get_more_data(Next), Boundary, re:split(FullHunk, Boundary,[])).

stream_parts([]) -> done_parts;
% browsers are fun, and terminate posts slightly differently from each other:
stream_parts([<<"----\n">>]) -> done_parts;
stream_parts([<<"--\n">>]) -> done_parts;
stream_parts([<<"----\r\n">>]) -> done_parts;
stream_parts([<<"--\r\n">>]) -> done_parts;
stream_parts([<<"--\r\n--\n">>]) -> done_parts;
stream_parts([<<"--\r\n--\r\n">>]) -> done_parts;
stream_parts([H|T]) -> {make_part(H), fun() -> stream_parts(T) end}.

get_more_data(done) -> {<<"--\n">>, really_done};
get_more_data(Fun) -> Fun().
   
make_part(PartData) ->
    [HeadData, Body] = re:split(PartData, "\\r\\n\\r\\n", [{parts,2}]),
    HeadList = [list_to_binary(X) ||
                   X <- string:tokens(binary_to_list(HeadData), "\r\n")],
    {Name, Params, Headers} = make_headers(HeadList),
    {Name, {Params,Headers}, Body}.

make_headers(X) -> 
    make_headers(X, name_undefined, params_undefined, []).
make_headers([], Name, Params, Headers) -> {Name, Params, Headers};
make_headers([<<>>|HL], Name, Params, Headers) ->
    make_headers(HL, Name, Params, Headers);
make_headers(
  [<<"Content-Disposition: form-data; ", Names/binary>>|HL],
  _, _, Headers) ->
    {Name, Params} = extract_names(Names),
    make_headers(HL, Name, Params, Headers);
make_headers([H|HL], Name, Params, Headers) ->
    make_headers(HL, Name, Params, [cheap_parse_header(H)|Headers]).

extract_names(NamesString) ->
    Params = [{K, V} ||
              {K, [<<>>, V, <<>>]} <- [{K0, re:split(V0,"\"",[])} ||
                          [K0, V0] <- [re:split(N, "=", [{parts, 2}]) ||
                                 N <- re:split(NamesString, "; ", [])]]],
    Name = hd([binary_to_list(V) || {<<"name">>,V} <- Params]),
    {Name, Params}.

cheap_parse_header(HeadBin) ->
    [K,V] = re:split(HeadBin, ": ", [{parts,2}]),
    {K,V}.

getparts1(done_parts, Acc) ->
    lists:reverse(Acc);
getparts1({Part, Streamer}, Acc) ->
    getparts1(Streamer(), [Part|Acc]).

send_streamed_body(Body, Max) ->
    HunkLen=8*Max,
    case Body of        
        <<A:HunkLen,Rest/binary>> ->
            {<<A:HunkLen>>, fun() -> send_streamed_body(Rest,Max) end};
        _ ->
            {Body, done}
    end.

test_body() ->
    Body = <<"------------ae0gL6gL6Ij5KM7Ef1KM7ei4ae0cH2\r\nContent-Disposition: form-data; name=\"Filename\"\r\n\r\ntestfile.txt\r\n------------ae0gL6gL6Ij5KM7Ef1KM7ei4ae0cH2\r\nContent-Disposition: form-data; name=\"Filedata\"; filename=\"testfile.txt\"\r\nContent-Type: application/octet-stream\r\n\r\n%%% The contents of this file are a test,\n%%% do not be alarmed.\n\r\n------------ae0gL6gL6Ij5KM7Ef1KM7ei4ae0cH2\r\nContent-Disposition: form-data; name=\"Upload\"\r\n\r\nSubmit Query\r\n------------ae0gL6gL6Ij5KM7Ef1KM7ei4ae0cH2--">>,
    Boundary = "----------ae0gL6gL6Ij5KM7Ef1KM7ei4ae0cH2",
    [{"Filename",
      {[{<<"name">>,<<"Filename">>}],[]},
      <<"testfile.txt\r\n">>},
     {"Filedata",
      {[{<<"name">>,<<"Filedata">>},
        {<<"filename">>,<<"testfile.txt">>}],
       [{<<"Content-Type">>,<<"application/octet-stream">>}]},
      <<"%%% The contents of this file are a test,\n%%% do not be alarmed.\n\r\n">>},
     {"Upload",
      {[{<<"name">>,<<"Upload">>}],[]},
      <<"Submit Query\r\n">>}]
    = get_all_parts(Body, Boundary),
    ok.
    
test_body2() ->
    Body = <<"-----------------------------89205314411538515011004844897\r\nContent-Disposition: form-data; name=\"Filedata\"; filename=\"akamai.txt\"\r\nContent-Type: text/plain\r\n\r\nCAMBRIDGE, MA - February 18, 2009 - Akamai Technologies, Inc. (NASDAQ: AKAM), the leader in powering rich media, dynamic transactions and enterprise applications online, today announced that its Service & Support organization was awarded top honors for Innovation in Customer Service at the 3rd Annual Stevie Awards for Sales & Customer Service, an international competition recognizing excellence in disciplines that are crucial to business success.\n\n\"We have always set incredibly high standards with respect to the service and support we provide our customers,\" said Sanjay Singh, vice president of Global Service & Support at Akamai. \"Our support team provides highly responsive service around the clock to our global customer base and, as a result, has become an extension of our customers' online businesses. This prestigious award is validation of Akamai's commitment to customer service and technical support.\"\n\nAkamai Service & Support professionals are dedicated to working with customers on a daily basis to fine tune, optimize, and support their Internet initiatives. Akamai's winning submission highlighted the key pillars of its service and support offering, as well as the initiatives established to meet customer requirements for proactive communication, simplification, and faster response times.\n\n\"This year's honorees demonstrate that even in challenging economic times, it's possible for organizations to continue to shine in sales and customer service, the two most important functions in business: acquiring and keeping customers,\" said Michael Gallagher, president of the Stevie Awards.\n\nThe awards are presented by the Stevie Awards, which organizes several of the world's leading business awards shows, including the prestigious American Business Awards. Nicknamed the Stevies for the Greek word \"crowned,\" winners were announced during a gala banquet on Monday, February 9 at Caesars Palace in Las Vegas. Nominated customer service and sales executives from the U.S.A. and several other countries attended. More than 500 entries from companies of all sizes and in virtually every industry were submitted to this year's competition. There are 27 categories for customer service professionals, as well as 41 categories for sales professionals.\n\nDetails about the Stevie Awards for Sales & Customer Service and the list of honorees in all categories are available at www.stevieawards.com/sales. \n\r\n-----------------------------89205314411538515011004844897--\r\n">>,
    Boundary = "---------------------------89205314411538515011004844897",
    get_all_parts(Body,Boundary).
