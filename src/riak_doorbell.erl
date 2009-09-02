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

%% @doc The doorbell (and door knocker) is a UDP socket server that provides a discovery mechanism for other nodes to connect into the Riak cluster.

-module(riak_doorbell).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([ring/2,knock/3, stop/0]).

-include_lib("eunit/include/eunit.hrl").
	 
-record(state, {port, sock}).

%% @spec start_link() -> {ok, pid()}
start_link() ->
    Port = riak:get_app_env(doorbell_port),
    pong = net_adm:ping(node()), % fail if not distributed
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% @spec knock(IP :: list(), Port :: integer(), RiakCookie :: atom()) ->
%%       ok | {error, Reason}
%% @doc This is used by a node not seeking to be a member of the cluster,
%%      to establish a distributed-erlang connection used by a riak_client.
knock(IP, Port, RiakCookie) ->
    % for non-riak nodes seeking a client proc
    Nonce = random:uniform(),
    {ok, SendSock} = gen_udp:open(0),
    gen_udp:send(SendSock, IP, Port,term_to_binary({
              {knock,Nonce,self(),erlang:get_cookie(),RiakCookie},node()})),
    gen_udp:close(SendSock),
    Nonce.

%% @spec ring(IP :: list(), Port :: integer()) ->
%%       ok | {error, Reason}
%% @doc This is used by a node joining the riak cluster.
ring(IP, Port) ->
    % for riak nodes joining
    {ok, SendSock} = gen_udp:open(0),
    Res = gen_udp:send(SendSock, IP, Port, term_to_binary({ring,node()})),
    gen_udp:close(SendSock),
    Res.

stop() -> gen_server:cast(?MODULE, stop).
    
% @private
init([Port]) ->
    Opts = [{active, true},
            list,
            {reuseaddr, true}],
    {ok, Sock} = gen_udp:open(Port, Opts),
    {ok, #state{port=Port,sock=Sock}}.

% @private
handle_info({udp, _Socket, IP, _InPortNo, Packet0},State) ->
    {RingType, Node} = binary_to_term(list_to_binary(Packet0)),
    case RingType of
        ring ->
            case net_adm:ping(Node) of
                pong -> riak_eventer:notify(riak_doorbell,connected,{IP,Node});
                pang -> riak_eventer:notify(riak_doorbell,connectfail,{IP,Node})
            end;
        {knock,Nonce,Pid,Cookie,RiakCookie} ->
            case riak:get_app_env(riak_cookie) of
                RiakCookie ->
                    riak_eventer:notify(riak_doorbell, client_connected,
                                        {IP, Pid, Node}),
                    erlang:set_cookie(Node,Cookie),
                    Pid ! {riak_connect, Nonce, node()};
                _ ->
                    riak_eventer:notify(riak_doorbell, client_connectfail,
                                        {IP, Pid, Node})
            end
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%% @private
handle_cast(stop, State) -> {stop, normal,State}.

%% @private
handle_call(_Request, _From, State) -> {reply, no_call, State}.

%% @private
terminate(_Reason, _State=#state{sock=Sock}) ->
    gen_udp:close(Sock),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

knock_test() ->
    application:set_env(riak, doorbell_port, 9001),
    application:set_env(riak, riak_cookie, default_riak_cookie),
    {ok, _Pid} = riak_doorbell:start_link(),
    Nonce = riak_doorbell:knock("127.0.0.1", 9001, default_riak_cookie),
    receive
        {riak_connect, Nonce, _} ->
            ok
    after 1000 ->
            throw(knock_test_timeout)
    end,
    riak_doorbell:stop().
       
    
            
    

    

