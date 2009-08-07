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

-module(riak_perftest).

-compile([export_all]).

client_setup(ClientSettings) ->
    Defaults = [{fresh_clients, false},
                {netlocs, [{"127.0.0.1",9000}]},
                {cookie, default_riak_cookie},
                {bucket, riak_perftest_bucket},
                {object_size, 1000},
                {num_unique_objects, 5000},
                {num_test_attempts, 1000},
                {read_write_ratio, {10,1}}],
    NewKeys = proplists:get_keys(ClientSettings),
    PrunedDefaults = [{K,V} || {K,V} <- Defaults, not lists:member(K,NewKeys)],
    Settings = ClientSettings ++ PrunedDefaults,
    % fresh_clients is bool, if true make new client connect each time
    % netlocs is list of {IP,Port} to connect with
    % cookie is cluster cookie
    % object_size is the number of bytes in each object's value
    % num_unique_objects is the integer number of independent objects to use
    % read_write_ratio is a two-tuple expressing the average # of reads/writes
    CGen = client_gen(proplists:get_value(fresh_clients, Settings),
                      proplists:get_value(netlocs, Settings),
                      proplists:get_value(cookie, Settings)),
    ObjBitSize = 8 * proplists:get_value(object_size, Settings),
    ObjValue = <<1:ObjBitSize>>,
    Bucket = proplists:get_value(bucket, Settings),
    KGen = populate(CGen, Bucket, ObjValue,
                    proplists:get_value(num_unique_objects, Settings)),

    TestAttempts = proplists:get_value(num_test_attempts, Settings),
    {R,W} = proplists:get_value(read_write_ratio, Settings),
    RWSelector = [read || _ <- lists:seq(1,R)] ++ 
                [write || _ <- lists:seq(1,W)],
    {CGen,KGen,ObjValue,RWSelector,TestAttempts}.

perftest({CGen,KGen,ObjValue,RWSelector,TestAttempts}) ->
    perftest(CGen,KGen,ObjValue,RWSelector,[],TestAttempts).
perftest(_,_,_,_,Acc,0) -> Acc;
perftest(CGen,KGen,ObjValue,RWSelector,Acc,TestAttempts) ->
    {Client,NextCGen} = CGen(),
    {{Bucket,Key},NextKGen} = KGen(),
    R = 3,
    W = 3, % SET THESE ABOVE!
    {ReadRes, ReadObj} = case perf_read(Client,Bucket,Key,R) of
        {Time, {ok, Obj}} -> {{Time, ok}, Obj};
        X -> {X, fail}
    end,
    Result = case lists:nth(random:uniform(length(RWSelector)),RWSelector) of
        read ->
            {read, ReadRes};
        write -> 
            case ReadObj of
                fail ->
                    {read, ReadRes};
                _ -> 
                    NewObj = riak_object:update_value(ReadObj,ObjValue),
                    {write, perf_write(Client,NewObj,W)}
            end
    end,
    perftest(NextCGen,NextKGen,ObjValue,RWSelector,
             [Result|Acc],TestAttempts-1).

perf_read(Client,Bucket,Key,R) ->
    timer:tc(?MODULE, perf_read1, [Client,Bucket,Key,R]).
perf_read1(Client,Bucket,Key,R) -> Client:get(Bucket,Key,R).

%%% NEED TO READ OBJECT FIRST!
perf_write(Client,Obj,W) ->
    timer:tc(?MODULE, perf_write1, [Client,Obj,W]).
perf_write1(Client,Obj,W) -> Client:put(Obj,W).

% create an endless thunk producing {Client, ClientGen}
client_gen(FreshClients, NetLocs, Cookie) ->
    case FreshClients of
        true -> % make a new client every time, can be slower
            {IP, Port} = hd(NetLocs),
            NextLocs = tl(NetLocs) ++ [hd(NetLocs)],
            fun() ->
                    {ok,C} = riak:client_connect(IP, Port, Cookie),
                    {C, client_gen(true, NextLocs, Cookie)}
            end;
        false -> % make the clients up front and re-use
            C0 = [riak:client_connect(IP, Port, Cookie) ||
                     {IP, Port} <- NetLocs],
            Clients = [C || {ok,C} <- C0],
            client_gen(Clients)
    end.
client_gen(Clients) ->
    H = hd(Clients),
    fun() -> {H, client_gen(tl(Clients) ++ [H])} end.

% put a lot of objects into the perftest_bucket, then return
% an endless thunk of {{Bucket,Key}, KeyGen}
populate(CGen, Bucket, ObjValue, NumUnique) ->
    populate(CGen, Bucket, ObjValue, NumUnique, NumUnique).
populate(_,Bucket,_,NumUnique,0) ->
    fun() ->
            {{Bucket,integer_to_list(NumUnique)},
             make_next_keygen(Bucket,NumUnique,NumUnique)}
    end;
populate(CGen, Bucket, ObjValue, NumUnique, NumLeft) ->
    {Client, NextCGen} = CGen(),
    Key = integer_to_list(NumLeft),
    Obj = riak_object:new(Bucket,Key,ObjValue),
    Client:put(Obj,3),
    populate(NextCGen, Bucket, ObjValue, NumUnique, NumLeft-1).
    
make_next_keygen(Bucket,NumUnique,Prev) ->
    Next = case Prev of
        1 -> NumUnique;
        _ -> Prev - 1
    end,
    fun() -> {{Bucket,integer_to_list(Next)},
              make_next_keygen(Bucket,NumUnique,Next)}
    end.

analyze_times(Times) ->
    T = lists:sort(Times),
    N = length(T),
    [Fifty,Ninety,NinetyFive,NinetyNine,NineNineNine] =
        [lists:nth(trunc(N * X),T) || X <- [0.5, 0.9, 0.95, 0.99, 0.999]],
    {lists:nth(1,T), 
     trunc(lists:sum(T) / N), Fifty,Ninety,NinetyFive,NinetyNine,NineNineNine,
    lists:nth(N,T)}.


% below are various notes, mainly snippets from other storage systems
%  people on their benchmarking.  obviously to be removed before public
%  release.

%% important performance note: goal isn't "simple fast"
%%  but goal does include not reducing performance when large in # objects/nodes

%% on testing, vary across:
%%  - client against 1 node or spread across all
%%  - all-write/all-read/various mixes
%%  - object size
%%  - number of nodes
%%  - number of partitions
%%  - storage backend (include nop backend, always 'ok')
%%  - R, W, RW settings (rw=0!)
%% measure
%%  - req latency (avg, min, max, 50/90/95/99/99.9 percentiles)
%%  - requests/sec (just derive from above)

%% http://cliffmoon.tumblr.com/post/128847520/performance-followup-from-nosql

%% I know that if I configure Dynomite to not hit disk and use the native
%% binary term protocol then I can get the throughput up to 20k reqs/s
%% with random reads and writes.

%% http://jan.prima.de/~jan/plok/archives/175-Benchmarks-You-are-Doing-it-Wrong.html

%% Hypertable has failure inducers throughout the code that can be triggered for testing. 
%% also hypertable:Doug: Perf on single node: 1 dual core Opteron, 4GB RAM. 31000 inserts/sec in batched mode(?), 500 inserts/sec, 800 random reads/sec #nosql

%% june 11 2009:
%% Cliff: Latency: 10ms avg, 5ms median, 1s at 99.9% #nosql
%% Cliff: Throughput is influenced by Erlang runtime: R12B 2000 req/s; R13B 6500 req/s. No code changes. #nosql
%% Cliff: Used for image serving at Powerset. 12 machines; 6M images + metadata; 2TB of data; 139KB avg size #nosql

%% some couch perf notes:
%% Just passed 4,000,000 documents in CouchDB. I'm excited. It's still fast.
%% Approaching 4 million documents in CouchDB. Handling ~400 writes/s 
%% the most I've pulled off on my MacBook was 6k docs/sec but that's
%% direct in Erlang skipping the HTTP and JSON conversion.

%% and dynomite sometime in jan:
%% dynomite avg latency for read/write is under 20ms, median is under
%% 10ms. 99.9 is still high, but that's due to running in virt hosts.

%% dynomite feb 22:
%% buffered write stats: 4ms avg, 2ms median, 98 ms 99.9%
%% JS NOTE: 99.9 is the one that matters!

%% feb 23 on twit:
%% Am seeing Couch's puts taking up to 1.3s (99% level) under load,
%% Dynomite's 99% only hits 200ms under load. 

%% feb 24:
%% ~~ while performing 3000 writes [a min] ~ CouchDB's beam process
%% used a total of 7 MB memory~ 
%% http://till.klampaeckel.de/blog/archives/16-Measuring-CouchDB-performance.html

%% cliff moon:
%% at_kevsmith ops/sec is a silly metric for benchmarks imo. it's easier to
%% manipulate and depends heavily on concurrent load and value size.
%% at_kevsmith latency percentiles are a much better proxy for the relative
%% overhead of your particular system.

%% benblack, twit mar 8
%% nice work! RT at_antirez New Redis benchmark: 100000 writes/sec, 54000
%% reads/sec: http://tinyurl.com/am9nzw
%% at_antirez what happens if you run the tests 100x longer? similar
%% numbers?

%% http://code.google.com/p/redis/wiki/Benchmarks is good!


%% http://wiki.github.com/jpellerin/dynomite/ec2-performance-tests

%% gets: 23502 puts: 23502 collisions: 0
%% get avg: 14.1821250.3ms median: 7.9438690.3ms 99.9: 228.2829280.3ms
%% put avg: 19.9633930.3ms median: 11.6100310.3ms 99.9: 191.6468140.3ms
%% gets:
%%   10% <   1.790ms
%%   20% <   2.918ms
%%   30% <   4.905ms
%%   40% <   6.386ms
%%   50% <   7.941ms
%%   60% <  10.555ms
%%   70% <  13.275ms
%%   80% <  18.824ms
%%   90% <  29.427ms
%%  100% < 3685.608ms
%% puts:
%%   10% <   3.440ms
%%   20% <   4.979ms
%%   30% <   6.438ms
%%   40% <   8.214ms
%%   50% <  11.607ms
%%   60% <  15.936ms
%%   70% <  22.062ms
%%   80% <  31.926ms
%%   90% <  47.970ms
%%  100% < 3778.776ms

%% VPORK

%% Mar 31, 2009 6:21:17 PM - Writes:
%% Mar 31, 2009 6:21:17 PM -   Num Writes:           159855
%% Mar 31, 2009 6:21:17 PM -   Write Failures:       0
%% Mar 31, 2009 6:21:17 PM -   Write Latency:        85.94 ms
%% Mar 31, 2009 6:21:19 PM -   Write Latency (%99):  319.00 ms
%% Mar 31, 2009 6:21:19 PM -   Bytes Written:        3658.79 MB
%% Mar 31, 2009 6:21:19 PM -   Thread w/Throughput:  0.27 KB / ms
%% Mar 31, 2009 6:21:19 PM -   Total w/Throughput:   24.29 KB / ms
%% Mar 31, 2009 6:21:19 PM - 
%% Mar 31, 2009 6:21:19 PM - Reads:
%% Mar 31, 2009 6:21:19 PM -   Num Read:             19872
%% Mar 31, 2009 6:21:19 PM -   Read Failures:        0
%% Mar 31, 2009 6:21:19 PM -   Read Latency:         70.36 ms
%% Mar 31, 2009 6:21:19 PM -   Read Latency (%99):   298.00 ms
%% Mar 31, 2009 6:21:19 PM -   Read Not Found:       12 (%0.06)
%% Mar 31, 2009 6:21:19 PM -   Bytes Read:           453.94 MB
%% Mar 31, 2009 6:21:19 PM -   Thread r/Throughput:  0.33 KB / ms
%% Mar 31, 2009 6:21:19 PM -   Total r/Throughput:   3.01 KB / ms
