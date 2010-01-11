%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
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

-module(webmachine).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([start/0, stop/0]).
-export([new_request/2]).

-include("webmachine_logger.hrl").
-include_lib("include/wm_reqstate.hrl").
-include_lib("include/wm_reqdata.hrl").

%% @spec start() -> ok
%% @doc Start the webmachine server.
start() ->
    webmachine_deps:ensure(),
    application:start(crypto),
    application:start(webmachine).

%% @spec stop() -> ok
%% @doc Stop the webmachine server.
stop() ->
    application:stop(webmachine).

new_request(mochiweb, Request) ->
    Socket = Request:get(socket),
    Method = Request:get(method),
    RawPath = Request:get(raw_path), 
    Version = Request:get(version),
    Headers = Request:get(headers),
    InitState = #wm_reqstate{socket=Socket,
                          reqdata=wrq:create(Method,Version,RawPath,Headers)},
    
    InitReq = {webmachine_request,InitState},
    {Peer, ReqState} = InitReq:get_peer(),
    PeerState = ReqState#wm_reqstate{reqdata=wrq:set_peer(Peer,
                                              ReqState#wm_reqstate.reqdata)},
    LogData = #wm_log_data{start_time=now(),
			   method=Method,
			   headers=Headers,
			   peer=PeerState#wm_reqstate.peer,
			   path=RawPath,
			   version=Version,
			   response_code=404,
			   response_length=0},
    webmachine_request:new(PeerState#wm_reqstate{log_data=LogData}).




  
