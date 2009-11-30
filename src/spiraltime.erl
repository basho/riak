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

%% @author Justin Sheehy <justin@basho.com>

%% @doc A set of sliding windows for recording N-per-second running stats.
%%
%% This keeps stats per second for the last minute, per minute for an hour,
%% per hour for a day, and per day for a week.
%%
%% The goal is not to have "perfect" stats; post-fact log analysis is
%% better for that.  The goal here is to have approximate running
%% data useful for quick understanding of performance trends.

-module(spiraltime).
-author('Justin Sheehy <justin@basho.com>').
-export([fresh/0,fresh/1,n/0,incr/2,incr/3,
         rep_second/1,rep_minute/1,rep_hour/1,rep_day/1,rep_week/1,
         test_spiraltime/0]).

%% @type moment() = integer().
%% This is a number of seconds, as produced by
%% calendar:datetime_to_gregorian_seconds(calendar:universal_time())

%% @type count() = integer().
%% The number of entries recorded in some time period.

-record(spiral, {moment :: integer(),
                 seconds :: [integer()],
                 minutes :: [integer()],
                 hours :: [integer()],
                 days :: [integer()]}).

n() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% @doc Create an empty spiral with which to begin recording entries.
%% @spec fresh() -> spiral()
fresh() ->
    fresh(n()).

%% @doc Create an empty spiral with which to begin recording entries.
%% @spec fresh(moment()) -> spiral()
fresh(Moment) ->
    #spiral{moment=Moment,
            seconds=[0 || _ <- lists:seq(1,60)],
            minutes=[0 || _ <- lists:seq(1,60)],
            hours=[0 || _ <- lists:seq(1,24)],
            days=[0 || _ <- lists:seq(1,7)]}.

fieldlen(#spiral.seconds) -> 60;
fieldlen(#spiral.minutes) -> 60;
fieldlen(#spiral.hours)   -> 24;
fieldlen(#spiral.days)    -> 7.

nextfield(#spiral.seconds) -> #spiral.minutes;
nextfield(#spiral.minutes) -> #spiral.hours;
nextfield(#spiral.hours)   -> #spiral.days;
nextfield(#spiral.days)    -> done.

%% @doc Produce the number of entries recorded in the last second.
%% @spec rep_second(spiral()) -> {moment(), count()}
rep_second(Spiral) ->
    {Spiral#spiral.moment, hd(Spiral#spiral.seconds)}.

%% @doc Produce the number of entries recorded in the last minute.
%% @spec rep_minute(spiral()) -> {moment(), count()}
rep_minute(Spiral) ->
    {Minute,_} = lists:split(60,Spiral#spiral.seconds),
    {Spiral#spiral.moment, lists:sum(Minute)}.

%% @doc Produce the approximate number of entries recorded in the last hour.
%% @spec rep_hour(spiral()) -> {moment(), count()}
rep_hour(Spiral) ->
    {Hour,_} = lists:split(60,Spiral#spiral.minutes),
    {Spiral#spiral.moment, lists:sum(Hour)}.

%% @doc Produce the approximate number of entries recorded in the last day.
%% @spec rep_day(spiral()) -> {moment(), count()}
rep_day(Spiral) ->
    {Day,_} = lists:split(24,Spiral#spiral.hours),
    {Spiral#spiral.moment, lists:sum(Day)}.

%% @doc Produce the approximate number of entries recorded in the last week.
%% @spec rep_week(spiral()) -> {moment(), count()}
rep_week(Spiral) ->
    {Week,_} = lists:split(7,Spiral#spiral.days),
    {Spiral#spiral.moment, lists:sum(Week)}.

%% @doc Add N to the counter of events, as recently as possible.
%% @spec incr(count(), spiral()) -> spiral()
incr(N, Spiral) -> incr(N,n(),Spiral).

%% @doc Add N to the counter of events occurring at Moment.
%% @spec incr(count(), moment(), spiral()) -> spiral()
incr(N, Moment, Spiral) when Spiral#spiral.moment =:= Moment ->
    % common case -- updates for "now"
    Spiral#spiral{seconds=[hd(Spiral#spiral.seconds)+N|
                           tl(Spiral#spiral.seconds)]};
incr(_N, Moment, Spiral) when Spiral#spiral.moment - Moment > 60 ->
    Spiral; % updates more than a minute old are dropped! whee!
incr(N, Moment, Spiral) ->
    S1 = update_moment(Moment, Spiral),
    {Front,Back} = lists:split(S1#spiral.moment - Moment,
                               S1#spiral.seconds),
    S1#spiral{seconds=Front ++ [hd(Back)+N|tl(Back)]}.

update_moment(Moment, Spiral) when Moment =< Spiral#spiral.moment ->
    Spiral;
update_moment(Moment, Spiral) when Moment - Spiral#spiral.moment > 36288000 ->
    fresh(Moment);
update_moment(Moment, Spiral) ->
    update_moment(Moment, push(0, Spiral#spiral{
                                    moment=Spiral#spiral.moment+1},
                               #spiral.seconds)).

getfield(Spiral,Field)   -> element(Field, Spiral).
setfield(Spiral,X,Field) -> setelement(Field, Spiral, X).

push(_N, Spiral, done) ->
    Spiral;
push(N, Spiral, Field) ->
    Full = [N|getfield(Spiral,Field)],
    Double = 2 * fieldlen(Field),
    case length(Full) of
        Double ->
            {Keep, _Past} = lists:split(fieldlen(Field), Full),
            push(lists:sum(Keep),setfield(Spiral,Keep,Field),nextfield(Field));
        _ ->
            setfield(Spiral,Full,Field)
    end.

test_spiraltime() ->
    Start = n(),
    S0 = fresh(Start),
    S1 = incr(17, Start, S0),
    PlusOne = Start+1,
    S2 = incr(3, PlusOne, S1),
    {PlusOne, 3} = rep_second(S2),
    {PlusOne, 20} = rep_minute(S2),
    true.
