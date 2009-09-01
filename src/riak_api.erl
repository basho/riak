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

%% @doc The gen_server entry point for riak client objects.
%%      See riak_client for usage.

-module(riak_api).

-behaviour(gen_server2).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {ring}).

%% @private
start_link() -> gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
handle_call(_,_From,State) -> {noreply,State}.

%% @private
init([]) -> {ok, #state{ring=undefined}}.

%% @private
handle_cast(_Msg, State) -> {noreply,State}.

%% @private
handle_info({set_ring, Ring}, State) ->
    {noreply, State#state{ring=Ring}};

handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

