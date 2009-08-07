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

-module(webmachine_request, [Pid]).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([
         get_reqdata/0,
         set_reqdata/1,
	 socket/0,
	 method/0,
	 version/0,
         disp_path/0,
	 path/0,
	 raw_path/0,
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
	 stop/0,
         do_redirect/0,
         resp_redirect/0,
	 set_metadata/2,
	 get_metadata/1,
	 get_path_info/0,
	 get_path_info/1,
	 load_dispatch_data/5,
	 get_path_tokens/0,
	 get_app_root/0,
	 parse_cookie/0,
	 get_cookie_value/1,
	 parse_qs/0,
	 get_qs_value/1,
	 get_qs_value/2,
         range/0,
	 log_data/0,
         call/1
	 ]).

-define(TIMEOUT, 150000).

call(Message) -> gen_server:call(Pid, Message, ?TIMEOUT).

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

stop() -> gen_server:cast(Pid, stop).

set_resp_body(Body) -> call({set_resp_body, Body}).
resp_body() -> call(resp_body).
response_body() -> resp_body().

get_req_header(K) -> call({get_req_header, K}).
get_header_value(K) -> get_req_header(K).

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

send_response(Code) -> call({send_response, Code}).

get_metadata(Key) -> call({get_metadata, Key}).

set_metadata(Key, Value) -> call({set_metadata, Key, Value}).

get_path_info() -> call(get_path_info).

get_path_info(Key) -> call({get_path_info, Key}).

path_tokens() -> call(path_tokens).
get_path_tokens() -> path_tokens().

app_root() -> call(app_root).
get_app_root() -> app_root().

load_dispatch_data(Bindings, PathTokens, AppRoot, DispPath, Req) ->
    call({load_dispatch_data, Bindings, PathTokens, AppRoot, DispPath, Req}).

log_data() -> call(log_data).
