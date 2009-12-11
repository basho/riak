%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.    

%% @doc Riak: A lightweight, decentralized key-value store.
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2007-2009 Basho Technologies, Inc.  All Rights Reserved.

-module(riak).
-author('Andy Gross <andy@basho.com>').
-author('Justin Sheehy <justin@basho.com>').
-author('Bryan Fink <bryan@basho.com>').
-export([stop/0, stop/1]).
-export([get_app_env/1,get_app_env/2]).
-export([client_connect/1,client_connect/2,
         local_client/0,local_client/1]).
-export([code_hash/0]).

-include_lib("eunit/include/eunit.hrl").

%% @spec stop() -> ok
%% @doc Stop the riak application and the calling process.
stop() -> stop("riak stop requested").
stop(Reason) ->
    % we never do an application:stop because that makes it very hard
    %  to really halt the runtime, which is what we need here.
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    init:stop().    

%% @spec get_app_env(Opt :: atom()) -> term()
%% @doc The official way to get the values set in riak's configuration file.
%%      Will return the undefined atom if that option is unset.
get_app_env(Opt) -> get_app_env(Opt, undefined).

%% @spec get_app_env(Opt :: atom(), Default :: term()) -> term()
%% @doc The official way to get the values set in riak's configuration file.
%%      Will return Default if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(riak, Opt) of
	{ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
	    {ok, [[Val | _]]} -> Val;
	    error       -> Default
        end
    end.

%% @spec local_client() -> {ok, Client :: riak_client()}
%% @equiv local_client(undefined)
local_client() ->
    local_client(undefined).

%% @spec local_client(binary()|undefined) -> {ok, Client :: riak_client()}
%% @doc When you want a client for use on a running Riak node.
%%      ClientId should be a 32-bit binary.  If it is not, a
%%      32-bit binary will be created from ClientId by phash2/1.
%%      If ClientId is the atom 'undefined', a random ClientId will
%%      be chosen.
local_client(ClientId) ->
    client_connect(node(), ClientId).

%% @spec client_connect(Node :: node())
%%        -> {ok, Client :: riak_client()} | {error, timeout}
%% @equiv client_connect(Node, undefined)
client_connect(Node) -> 
    client_connect(Node, undefined).

%% @spec client_connect(node(), binary()|undefined)
%%         -> {ok, Client :: riak_client} | {error, timeout}
%% @doc The usual way to get a client.  Timeout often means either a bad
%%      cookie or a poorly-connected distributed erlang network.
%%      ClientId should be a 32-bit binary.  If it is not, a
%%      32-bit binary will be created from ClientId by phash2/1.
%%      If ClientId is the atom 'undefined', a random ClientId will
%%      be chosen.
client_connect(Node, ClientId= <<_:32>>) ->
    % Make sure we can reach this node...
    case net_adm:ping(Node) of
        pang -> {error, {could_not_reach_node, Node}};
        pong -> {ok, riak_client:new(Node, ClientId)}
    end;
client_connect(Node, undefined) ->
    client_connect(Node, riak_util:mkclientid(Node));
client_connect(Node, Other) ->
    client_connect(Node, <<(erlang:phash2(Other)):32>>).


%% 719528 days from Jan 1, 0 to Jan 1, 1970
%%  *86400 seconds/day
-define(SEC_TO_EPOCH, 62167219200).

%% @spec confirm_epoch() -> ok
%% @doc 
confirm_epoch() ->
    %% doc for erlang:now/0 says return value is platform-dependent
    %% -> let's emit an error if this platform doesn't think the epoch
    %%    is Jan 1, 1970
    {MSec, Sec, _} = erlang:now(),
    GSec = calendar:datetime_to_gregorian_seconds(
             calendar:universal_time()),
    case GSec - ((MSec*1000000)+Sec) of
        N when (N < ?SEC_TO_EPOCH+5 andalso N > ?SEC_TO_EPOCH-5);
        (N < -?SEC_TO_EPOCH+5 andalso N > -?SEC_TO_EPOCH-5) ->
            %% if epoch is within 10 sec of expected, accept it
            ok;
        N ->
            io:format("Epoch fail:~n"),
            Epoch = calendar:gregorian_seconds_to_datetime(N),
            io:format("Riak expects your system's epoch to be Jan 1, 1970,~n"
                      "but your system says the epoch is ~p~n", [Epoch]),
            throw(epoch_fail)
    end.

code_hash() ->
    {ok, AllMods0} = application:get_key(riak, modules),
    AllMods = lists:sort(AllMods0),
    <<MD5Sum:128>> = erlang:md5_final(
                       lists:foldl(
                         fun(D, C) -> erlang:md5_update(C, D) end, 
                         erlang:md5_init(),
                         [C || {_, C, _} <- [code:get_object_code(M) || M <- AllMods]]
                        )),
    riak_util:integer_to_list(MD5Sum, 62).
