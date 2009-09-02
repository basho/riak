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

%% @doc The default functions used for claiming partition ownership.
%%      Generally, a wants_claim function should return either
%%      {yes, Integer} or 'no' where Integer is the number of
%%       additional partitions wanted by this node.
%%      A choose_claim function should return a riak_ring with
%%      one more partition claimed by this node than in the input ring.

-module(riak_claim).
-export([default_wants_claim/1, default_choose_claim/1,
         never_wants_claim/1]).

-include_lib("eunit/include/eunit.hrl").

%% @spec default_wants_claim(riak_ring()) -> {yes, integer()} | no
%% @doc Want a partition if we currently have less than floor(ringsize/nodes).
default_wants_claim(Ring) ->
    NumPart = riak_ring:num_partitions(Ring),
    NumOwners = length(lists:usort([node() | riak_ring:all_members(Ring)])),
    Mine = length(riak_ring:my_indices(Ring)),
    Want = trunc(NumPart / NumOwners) - Mine,
    case Want > 0 of 
        true -> {yes, Want};
        false -> no
    end.

%% @spec default_choose_claim(riak_ring()) -> riak_ring()
%% @doc Choose a partition at random.
default_choose_claim(Ring) ->
    riak_ring:transfer_node(riak_ring:random_other_index(Ring),
                              node(), Ring).

%% @spec never_wants_claim(riak_ring()) -> no
%% @doc For use by nodes that should not claim any partitions.
never_wants_claim(_) -> no.

wants_claim_test() ->
    {ok, Ring} = riak_ring_manager:get_my_ring(),
    {yes, _}  = default_wants_claim(Ring).
