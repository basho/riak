%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(etop_txt).
-author('siri@erix.ericsson.se').
-author('scott@basho.com').

%%-compile(export_all).
-export([init/1,stop/1]).
-export([do_update/3]).

%%-include("etop.hrl").
-record(etop_proc_info,
        {pid,
         mem=0,
         reds=0,
         name,
         runtime=0,
         cf,
         mq=0}).
-record(etop_info, 
        {now = {0, 0, 0},
         n_procs = 0,
         wall_clock = {0, 0},
         runtime = {0, 0},
         run_queue = 0,
         alloc_areas = [],
         memi = [{total, 0},
                 {processes, 0}, 
                 {ets, 0},
                 {atom, 0},
                 {code, 0},
                 {binary, 0}],
         procinfo = []
        }).
%%-include("etop_defs.hrl").
-define(SYSFORM,
        " ~-72w~10s~n"
        " Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w~n"
        "        procs~8w                        processes~8w    code     ~8w~n"
        "        runq ~8w                        atom     ~8w    ets      ~8w~n").
-record(opts, {node=node(), port = 8415, accum = false, intv = 5000, lines = 10, 
               width = 700, height = 340, sort = runtime, tracing = on,
               %% Other state information
               out_mod=etop_gui, out_proc, server, host, tracer, store, 
               accum_tab, remote}).

-import(etop,[loadinfo/1,meminfo/2]).
-import(etop_gui,[formatmfa/1,to_list/1]).

-define(PROCFORM,"~-20w~-25s~8w~11w~11w~11w ~-40s~n").

stop(Pid) -> Pid ! stop.

init(Config) ->
    loop(Config).

loop(Config) ->
    Info = do_update(Config),
    receive 
	stop -> stopped;
	{dump,Fd} -> do_update(Fd,Info,Config), loop(Config); 
	{config,_,Config1} -> loop(Config1)
    after Config#opts.intv-500 -> loop(Config)
    end.

do_update(Config) ->
    Info = etop:update(Config),
    do_update(standard_io,Info,Config).

do_update(Fd,Info,Config) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info),
    io:nl(Fd),
    writedoubleline(Fd),
    case Info#etop_info.memi of
	undefined ->
	    io:fwrite(Fd, " ~-72w~10s~n"
		      " Load:  cpu  ~8w~n"
		      "        procs~8w~n"
		      "        runq ~8w~n",
		      [Config#opts.node,Clock,
		       Cpu,NProcs,RQ]);
	Memi ->
	    [Tot,Procs,Atom,Bin,Code,Ets] = 
		meminfo(Memi, [total,processes,atom,binary,code,ets]),
	    io:fwrite(Fd, ?SYSFORM,
		      [Config#opts.node,Clock,
		       Cpu,Tot,Bin,
		       NProcs,Procs,Code,
		       RQ,Atom,Ets])
    end,
    io:nl(Fd),
    writepinfo_header(Fd),
    writesingleline(Fd),
    writepinfo(Fd,Info#etop_info.procinfo),
    %%writedoubleline(Fd),
    %%io:nl(Fd),
    Info.

writepinfo_header(Fd) ->
    io:fwrite(Fd,"Pid                 Name or Initial Func         Time       Reds     Memory       MsgQ Current Function~n",[]).

writesingleline(Fd) ->
    io:fwrite(Fd,"-------------------------------------------------------------------------------------------------------------------------------~n",[]).
writedoubleline(Fd) ->
    io:fwrite(Fd,"===============================================================================================================================~n",[]).
 
writepinfo(Fd,[#etop_proc_info{pid=Pid,
			       mem=Mem,
			       reds=Reds,
			       name=Name,
			       runtime=Time,
			       cf=MFA,
			       mq=MQ}
	       |T]) ->
    io:fwrite(Fd,?PROCFORM,[Pid,to_list(Name),Time,Reds,Mem,MQ,formatmfa(MFA)]), 
    writepinfo(Fd,T);
writepinfo(_Fd,[]) ->
    ok.

