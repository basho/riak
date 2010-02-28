%% -------------------------------------------------------------------
%%
%% slide: collected aggregation in sliding time windows
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

%%      @doc Keep track of thing in a sliding time window.  The idea here
%%      is that you have some reading to take several times.
%%      Occasionally, you want to compute some aggregation of those
%%      readings for the last N seconds.
%%
%%      For example, you might read the weight of cars passing a point
%%      in the road.  You want to compute some statistics every hour.
%%      You could:
%%
%%      %% create a new slide, with an hour window
%%      T0 = slide:fresh(60*60)
%%
%%      %% update it every time a car passes
%%      T1 = slide:update(T0, Weight, slide:moment())
%%
%%      %% eventually ask for stats
%%      {NumberOfCars, TotalWeight} = slide:sum(TN, slide:moment())
%%      {NumberOfCars, AverageWeight} = slide:mean(TN, slide:moment())
%%      {NumberOfCars, {MedianWeight,
%%                      NinetyFivePercentWeight,
%%                      NinetyNinePercentWeight,
%%                      HeaviestWeight} = slide:nines(TN, slide:moment())
%%
%%      The slide module attempts to be tunably efficient by exposing
%%      the ability to determine how often to "prune" readings.  By default,
%%      readings are pruned whenever a reading is made with a timestamp
%%      that is two window-lengths newer than the oldest timestamp in the
%%      slide.  At that point, all readings older than one window-length away
%%      from the latest reading will be removed.  Use a different value
%%      for the Trigger parameter of fresh/2 to change when pruning is
%%      triggered.
-module(slide).

-export([fresh/0, fresh/1, fresh/2]).
-export([update/2, update/3, moment/0]).
-export([sum/1, sum/2, sum/3]).
-export([mean/1, mean/2, mean/3]).
-export([nines/1, nines/2, nines/3]).
-include_lib("eunit/include/eunit.hrl").

-record(slide, {
          oldest,   %% oldest timestamp here
          window,   %% window to which to trim
          trigger,  %% age at which to trigger pruning
          readings  %% {timestamp, reading}
         }).

%% @spec fresh() -> slide()
%% @equiv fresh(60)
fresh() -> fresh(60).

%% @spec fresh(integer()) -> slide()
%% @equiv fresh(Window, Window*2)
fresh(Window) -> fresh(Window, Window*2).

%% @spec fresh(integer(), integer()) -> slide()
%% @doc Create an empty slide for tracking Window-seconds worth of
%%      readings, and pruning those readings after Trigger seconds.
fresh(Window, Trigger) when Trigger >= Window ->
    #slide{window=Window, trigger=Trigger, readings=[]}.

%% @spec moment() -> integer()
%% @doc Get the current time in seconds.
moment() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% @spec update(slide(), term()) -> slide()
%% @equiv update(S, Reading, moment())
update(S, Reading) -> update(S, Reading, moment()).

%% @spec update(slide(), term(), integer()) -> slide()
%% @doc Store a new reading.  The current list of readings will be
%%      pruned if Moment is as new as or newer than the most recent
%%      reading stored, and more than Trigger seconds newer than the
%%      oldest reading stored.
update(S=#slide{oldest=O, trigger=P, readings=R=[{Y,_}|_]},
       Reading, Moment) ->
    if Moment >= Y ->
            %% Reading is newest
            Pruned = maybe_prune(S, Moment),
            Pruned#slide{oldest=case Pruned#slide.oldest of
                                     undefined -> Moment;
                                     PrunedOldest -> PrunedOldest
                                 end,
                          readings=[{Moment, Reading}|Pruned#slide.readings]};
       Moment > Y-P ->
            %% Reading is after our trigger time
            %% assume normal use case adds a 'newest' regularly,
            %% and don't bother pruning here
            {Younger, Older} =
                lists:splitwith(fun({T,_}) -> T > Moment end, R),
            S#slide{
              oldest=if Moment < O -> Moment;
                        true -> O
                     end,
              readings=Younger++[{Moment, Reading}]++Older};
       true ->
            %% Reading is before the trigger time
            S
    end;
update(S=#slide{readings=[]}, Reading, Moment) ->
    S#slide{oldest=Moment, readings=[{Moment, Reading}]}.

%% @spec maybe_prune(slide(), moment()) -> slide()
%% @doc Prune if the trigger has been ... er, triggered.
maybe_prune(S=#slide{oldest=O, trigger=P, window=W}, Moment) ->
    if Moment-P > O -> prune(S, Moment-W);
       true -> S
    end.

%% @spec prune(slide(), integer()) -> slide()
%% @doc Remove all readings taken before MaxAge.
prune(S=#slide{readings=R}, MaxAge) ->
    Prune = fun(Reading={T, _}, Acc) ->
                    if T > MaxAge -> [Reading|Acc];
                       true       -> Acc
                    end
            end,
    case lists:foldl(Prune, [], R) of
        [] ->
            S#slide{oldest=undefined, readings=[]};
        RevFiltered=[{NewOldest,_}|_] ->
            S#slide{oldest=NewOldest,
                     readings=lists:reverse(RevFiltered)}
    end.

%% @spec sum(slide()) -> {Count::integer(), Sum::integer()}
%% @doc Sum of readings from now through Window seconds ago.  Return is
%%      number of readings in the range and the sum of those readings.
sum(Slide) -> sum(Slide, moment()).

%% @spec sum(slide(), integer()) -> {Count::integer(), Sum::integer()}
%% @doc Sum of readings from Moment through Window seconds before Moment.
%%      Return is number of readings in the range and the sum of those
%%      readings.
sum(Slide, Moment) -> sum(Slide, Moment, Slide#slide.window).

%% @spec sum(slide(), integer(), integer()) ->
%%          {Count::integer(), Sum::integer()}
%% @doc Sum of readings from Moment through Seconds seconds before
%%      Moment.  Return is number of readings in the range and the sum
%%      of those readings.
sum(#slide{readings=R}, Moment, Seconds) ->
    Cutoff = Moment-Seconds,
    Sum = fun({T, Reading}, {Count, Sum}) when T =< Moment, T > Cutoff ->
                  {Count+1, Sum+Reading};
             (_, Acc) -> Acc
          end,
    lists:foldl(Sum, {0, 0}, R).

%% @spec mean(slide()) -> {Count::integer(), Mean::number()}
%% @doc Mean of readings from now through Window seconds ago.  Return is
%%      number of readings in the range and the mean of those readings.
mean(Slide) -> mean(Slide, moment()).

%% @spec mean(slide(), integer()) -> {Count::integer(), Mean::number()}
%% @doc Mean of readings from Moment through Window seconds before Moment.
%%      Return is number of readings in the range and the mean of those
%%      readings.
mean(Slide, Moment) -> mean(Slide, Moment, Slide#slide.window).

%% @spec mean(slide(), integer(), integer()) ->
%%          {Count::integer(), Mean::number()}
%% @doc Mean of readings from Moment through Seconds seconds before
%%      Moment.  Return is number of readings in the range and the mean
%%      of those readings.
mean(S, Moment, Seconds) ->
    case sum(S, Moment, Seconds) of
        {0, _}       -> {0, undefined};
        {Count, Sum} -> {Count, Sum/Count}
    end.

%% @spec nines(slide()) ->
%%         {Count::integer(), {Median::number(), NinetyFive::number(),
%%                             NinetyNine::number(), Hundred::number()}}
%% @doc Median, 95%, 99%, and 100% readings from now through Window
%%  seconds ago.  Return is number of readings in the range and the
%%  nines of those readings.
nines(Slide) -> nines(Slide, moment()).

%% @spec nines(slide(), integer()) ->
%%         {Count::integer(), {Median::number(), NinetyFive::number(),
%%                             NinetyNine::number(), Hundred::number()}}
%% @doc Median, 95%, 99%, and 100% readings from Moment through Window
%%      seconds before Moment.  Return is number of readings in the
%%      range and the nines of those readings.
nines(Slide, Moment) -> nines(Slide, Moment, Slide#slide.window).

%% @spec nines(slide(), integer(), integer()) ->
%%         {Count::integer(), {Median::number(), NinetyFive::number(),
%%                             NinetyNine::number(), Hundred::number()}}
%% @doc Median, 95%, 99%, and 100% readings from Moment through
%%      Seconds seconds before Moment.  Return is number of readings
%%      in the range and the nines of those readings.
nines(#slide{readings=R}, Moment, Seconds) ->
    Cutoff = Moment-Seconds,
    case lists:sort([ Reading || {T, Reading} <- R,
                                 T < Moment, T > Cutoff]) of
        [] -> {0, {undefined, undefined, undefined, undefined}};
        Window ->
            Count = length(Window),
            {Count,
             {lists:nth(mochinum:int_ceil(Count*0.5), Window),
              lists:nth(mochinum:int_ceil(Count*0.95), Window),
              lists:nth(mochinum:int_ceil(Count*0.99), Window),
              lists:last(Window)}}
    end.

%%
%% Test
%%

direct_prune_test() ->
    S0 = slide:fresh(10),
    S1 = slide:update(S0, 5, 3),
    ?assertEqual(S1, prune(S1, 2)),
    ?assertEqual(S0, prune(S1, 4)).

maybe_prune_test() ->
    S0 = slide:fresh(10, 20),
    S1 = slide:update(S0, 3, 1),
    S2 = slide:update(S1, 5, 15),
    ?assertEqual(S2, maybe_prune(S2, 12)),
    ?assertEqual({1,5}, slide:sum(maybe_prune(S2, 22), 22)).

auto_prune_test() ->
    S0 = slide:fresh(10),
    S1 = slide:update(S0, 5, 3),
    S2 = slide:update(S1, 6, 14),
    ?assertEqual({1, 5}, slide:sum(S1, 4, 10)),
    ?assertEqual({1, 6}, slide:sum(S2, 15, 10)).

sum_test() ->
    S0 = slide:fresh(10),
    ?assertEqual({0, 0}, % no points, sum = 0
                 slide:sum(S0, 9, 10)),
    S1 = slide:update(S0, 3, 1),
    ?assertEqual({1, 3}, % one point, sum = 3
                 slide:sum(S1, 9, 10)),
    S2 = slide:update(S1, 5, 5),
    ?assertEqual({2, 8}, % two points, sum = 8
                 slide:sum(S2, 9, 10)),
    S3 = slide:update(S2, 7, 5),
    ?assertEqual({3, 15}, % three points (two concurrent), sum = 15
                 slide:sum(S3, 9, 10)),
    S4 = slide:update(S3, 11, 14),
    ?assertEqual({3, 23}, % ignoring first reading, sum = 23
                 slide:sum(S4, 14, 10)),
    ?assertEqual({1, 11}, % shifted window
                 slide:sum(S4, 18, 10)),
    S5 = slide:update(S4, 13, 22),
    ?assertEqual({1, 11}, % pruned early readings
                 slide:sum(S5, 14, 10)),
    ?assertEqual({2, 24}, % shifted window
                 slide:sum(S5, 22, 10)).

mean_test() ->
    S0 = slide:fresh(10),
    ?assertEqual({0, undefined}, % no points, no average
                 slide:mean(S0)),
    S1 = slide:update(S0, 3, 1),
    ?assertEqual({1, 3.0}, % one point, avg = 3
                 slide:mean(S1, 9, 10)),
    S2 = slide:update(S1, 5, 5),
    ?assertEqual({2, 4.0}, % two points, avg = 4
                  slide:mean(S2, 9, 10)),
    S3 = slide:update(S2, 7, 5),
    ?assertEqual({3, 5.0}, % three points (two concurrent), avg = 5
                  slide:mean(S3, 9, 10)),
    S4 = slide:update(S3, 11, 14),
    ?assertEqual({3, 23/3}, % ignoring first reading, avg = 
                 slide:mean(S4, 14, 10)),
    ?assertEqual({1, 11.0}, % shifted window
                 slide:mean(S4, 18, 10)),
    S5 = slide:update(S4, 13, 22),
    ?assertEqual({1, 11.0}, % pruned early readings
                 slide:mean(S5, 14, 10)),
    ?assertEqual({2, 12.0}, % shifted window
                 slide:mean(S5, 22, 10)).
    
nines_test() ->
    PushReadings = fun(S, Readings) ->
                           lists:foldl(
                             fun({R,T}, A) ->
                                     slide:update(A, R, T)
                             end,
                             S, Readings)
                   end,
    S0 = slide:fresh(10),
    ?assertEqual({0, {undefined, undefined, undefined, undefined}},
                 slide:nines(S0)),
    S1 = PushReadings(S0, [ {R, 1} || R <- lists:seq(1, 10) ]),
    ?assertEqual({10, {5, 10, 10, 10}}, slide:nines(S1, 9, 10)),
    S2 = PushReadings(S1, [ {R, 2} || R <- lists:seq(11, 20) ]),
    ?assertEqual({20, {10, 19, 20, 20}}, slide:nines(S2, 9, 10)),
    S3 = PushReadings(S2, [ {R, 3} || R <- lists:seq(21, 100) ]),
    ?assertEqual({100, {50, 95, 99, 100}}, slide:nines(S3, 9, 10)),
    ?assertEqual({90, {55, 96, 100, 100}}, slide:nines(S3, 11, 10)).

not_most_recent_test() ->
    S0 = slide:fresh(10, 20),
    S1 = slide:update(S0, 3, 13),
    S2 = slide:update(S1, 5, 1),
    S3 = slide:update(S2, 7, 22),
    ?assertEqual({2, 8}, slide:sum(S2, 13, 13)),
    ?assertEqual({1, 3}, slide:sum(S3, 13, 13)),
    ?assertEqual({2, 10}, slide:sum(S3, 22, 22)).

already_pruned_test() ->
    S0 = slide:fresh(10, 20),
    S1 = slide:update(S0, 3, 30),
    S2 = slide:update(S1, 5, 1),
    ?assertEqual(slide:sum(S1, 30, 30),
                 slide:sum(S2, 30, 30)).
