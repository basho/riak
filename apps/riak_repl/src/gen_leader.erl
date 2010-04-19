%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% 
%%     $Id: gen_leader.erl,v 1.2 2005/07/04 06:55:56 hanssv Exp $
%%
%% @author Hans Svensson <hanssv@cs.chalmers.se>
%% @author Thomas Arts <thomas.arts@ituniv.se>
%% @author Ulf Wiger <ulf.wiger@ericsson.com>
%% @author Dave Fayram <dfayram@gmail.com> 
%%
%% @doc Leader election behavior.
%% <p>This application implements a leader election behavior modeled after
%% gen_server. This behavior intends to make it reasonably
%% straightforward to implement a fully distributed server with
%% master-slave semantics.</p>
%% <p>The gen_leader behavior supports nearly everything that gen_server
%% does (some functions, such as multicall() and the internal timeout,
%% have been removed), and adds a few callbacks and API functions to 
%% support leader election etc.</p>
%% <p>Also included is an example program, a global dictionary, based
%% on the modules gen_leader and dict. The callback implementing the
%% global dictionary is called 'test_cb', for no particularly logical
%% reason.</p>
%% <p><b>New version:</b> The internal leader election algorithm was faulty
%% and has been replaced with a new version based on a different leader
%% election algorithm. As a consequence of this the query functions
%% <tt>alive</tt> and <tt>down</tt> can no longer be provided.
%% The new algorithm also make use of an incarnation parameter, by 
%% default written to disk in the function <tt>incarnation</tt>. This
%% implies that only one <tt>gen_leader</tt> per node is permitted, if 
%% used in a diskless environment, <tt>incarnation</tt> must be adapted. 
%% </p>
%% @end
%%
%% @type election() = tuple(). Opaque state of the gen_leader behaviour.
%% @type node() = atom(). A node name.
%% @type name() = atom(). A locally registered name.
%% @type serverRef() = Name | {name(),node()} | {global,Name} | pid(). 
%%   See gen_server.
%% @type callerRef() = {pid(), reference()}. See gen_server.
%%

%% This version has been modified and debugged by Dave Fayram for
%%    Powerset/Microsoft.
-module(gen_leader).

% Time between rounds of query from the leader
-define(TAU,250).

% Exports for quickcheck
%-export([safe_loop/4,loop/4]).

-export([start/6,
	 start_link/6,
	 leader_call/2, leader_call/3, leader_cast/2, worker_announce/2,
	 call/2, call/3, cast/2,
	 reply/2]).

%% Query functions
-export([%% alive/1,
	 %% down/1,
	 candidates/1,
	 workers/1]).

-export([
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 format_status/2
	]).

-export([behaviour_info/1]).

%% Internal exports
-export([init_it/6, print_event/3
	 %%, safe_send/2
	]).

-import(error_logger , [format/2]).
-import(lists, [foldl/3,
		foreach/2,
		member/2,
		keydelete/3,
		keysearch/3]).

% Include for QuickCheck
% -include("eqc.hrl").

-record(election,{leader = none,
		  name,
		  leadernode = none,
		  candidate_nodes = [],	
		  worker_nodes = [],
		  alive = [],
		  down = [],
		  monitored = [],
		  buffered = [],
		  status,
		  elid,
		  acks = [],
		  work_down = [],
		  pendack,
		  incarn,
		  nextel
		 }).

-record(server, {parent,
		 mod,
		 state,
		 debug}).


%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------

%% @hidden 
behaviour_info(callbacks) ->
    [{init,1},
     {elected,2},
     {surrendered,3},
     {handle_leader_call,4},
     {handle_leader_cast,3},
     {from_leader,3},
     {handle_call,3},
     {handle_cast,2},
     {handle_DOWN,3},
     {handle_info,2},
     {terminate,2},
     {code_change,4}];
behaviour_info(_Other) ->
    undefined.

%% @spec start(Name::node(), CandidateNodes::[node()],
%%             Workers::[node()], Mod::atom(), Arg, Options::list()) ->
%%    {ok,pid()}
%%
%% @doc Starts a gen_leader process without linking to the parent.
%%
start(Name, CandidateNodes, Workers, Mod, Arg, Options) when is_atom(Name) ->
    gen:start(?MODULE, nolink, {local,Name},
	      Mod, {CandidateNodes, Workers, Arg}, Options).

%% @spec start_link(Name::atom(), CandidateNodes::[atom()],
%%             Workers::[atom()], Mod::atom(), Arg, Options::list()) ->
%%  {ok, pid()}
%%
%% @doc Starts a gen_leader process.
%% <table>
%%  <tr><td>Name</td><td>The locally registered name of the process</td></tr>
%%  <tr><td>CandidateNodes</td><td>The names of nodes capable of assuming
%%     a leadership role</td></tr>
%%  <tr><td>Workers</td>
%%     <td>The names of nodes that will be part of the "cluster",
%%         but cannot ever assume a leadership role.</td></tr>
%%  <tr><td>Mod</td><td>The name of the callback module</td></tr>
%%  <tr><td>Arg</td><td>Argument passed on to <code>Mod:init/1</code></td></tr>
%%  <tr><td>Options</td><td>Same as gen_server's Options</td></tr>
%% </table>
%%
%% <p>The list of candidates needs to be known from the start. Workers 
%% can be added at runtime.</p>
%% @end
start_link(Name, CandidateNodes, Workers, 
	   Mod, Arg, Options) when is_atom(Name) ->
    % Random delay for QuickCheck
    % timer:sleep(random:uniform(400)),
    gen:start(?MODULE, link, {local,Name}, Mod,
	      {CandidateNodes, Workers, Arg}, Options).


%% Query functions to be used from the callback module

%% alive(#election{alive = Alive}) ->
%%    Alive.

%% down(#election{down = Down}) ->
%%    Down.

%% @spec candidates(E::election()) -> [node()]
%%
%% @doc Returns a list of known candidates.
%%
candidates(#election{candidate_nodes = Cands}) ->
    Cands.

%% @spec workers(E::election()) -> [node()]
%%
%% @doc Returns a list of known workers.
%%
workers(#election{worker_nodes = Workers}) ->
    Workers.

%% Used by dynamically added workers.
%% @hidden
worker_announce(Name, Pid) ->
  Name ! {add_worker, Pid}.

%
% Make a call to a generic server.
% If the server is located at another node, that node will
% be monitored.
% If the client is trapping exits and is linked server termination
% is handled here (? Shall we do that here (or rely on timeouts) ?).
%
%% @spec call(Name::serverRef(), Request) -> term()
%%
%% @doc Equivalent to <code>gen_server:call/2</code>, but with a slightly
%% different exit reason if something goes wrong. This function calls 
%% the <code>gen_leader</code> process exactly as if it were a gen_server
%% (which, for practical purposes, it is.)
%% @end
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, local_call, [Name, Request]}})
    end.

%% @spec call(Name::serverRef(), Request, Timeout::integer()) ->
%%     Reply
%%
%%     Reply = term()
%%
%% @doc Equivalent to <code>gen_server:call/3</code>, but with a slightly
%% different exit reason if something goes wrong. This function calls 
%% the <code>gen_leader</code> process exactly as if it were a gen_server
%% (which, for practical purposes, it is.)
%% @end
call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, local_call, [Name, Request, Timeout]}})
    end.

%% @spec leader_call(Name::name(), Request::term())
%%    -> Reply
%%
%%    Reply = term()
%%
%% @doc Makes a call (similar to <code>gen_server:call/2</code>) to the 
%% leader. The call is forwarded via the local gen_leader instance, if 
%% that one isn't actually the leader. The client will exit if the 
%% leader dies while the request is outstanding.
%% <p>This function uses <code>gen:call/3</code>, and is subject to the
%% same default timeout as e.g. <code>gen_server:call/2</code>.</p>
%% @end
%%
leader_call(Name, Request) ->
    case catch gen:call(Name, '$leader_call', Request) of
	{ok,{leader,reply,Res}} ->
	    Res;
	{ok,{error, leader_died}} ->
	    exit({leader_died, {?MODULE, leader_call, [Name, Request]}});
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, leader_call, [Name, Request]}})
    end.

%% @spec leader_call(Name::name(), Request::term(), Timeout::integer())
%%    -> Reply
%%
%%    Reply = term()
%%
%% @doc Makes a call (similar to <code>gen_server:call/3</code>) to the 
%% leader. The call is forwarded via the local gen_leader instance, if 
%% that one isn't actually the leader. The client will exit if the 
%% leader dies while the request is outstanding.
%% @end
%%
leader_call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$leader_call', Request, Timeout) of
	{ok,{leader,reply,Res}} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, leader_call, [Name, Request, Timeout]}})
    end.


%% @equiv gen_server:cast/2
cast(Name, Request) ->
    catch do_cast('$gen_cast', Name, Request),
    ok.

%% @spec leader_cast(Name::name(), Msg::term()) -> ok
%% @doc Similar to <code>gen_server:cast/2</code> but will be forwarded to
%% the leader via the local gen_leader instance.
leader_cast(Name, Request) ->
    catch do_cast('$leader_cast', Name, Request),
    ok.


do_cast(Tag, Name, Request) when is_atom(Name) ->
    Name ! {Tag, Request};
do_cast(Tag, Pid, Request) when is_pid(Pid) ->
    Pid ! {Tag, Request}.


%% @spec reply(From::callerRef(), Reply::term()) -> Void
%% @equiv gen_server:reply/2
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.


%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
%%% @hidden 
init_it(Starter, self, Name, Mod, {CandidateNodes, Workers, Arg}, Options) ->
    init_it(Starter, self(), Name, Mod, 
	    {CandidateNodes, Workers, Arg}, Options);
init_it(Starter,Parent,Name,Mod,{CandidateNodes,Workers,Arg},Options) ->
    %% The following row is needed in case of trace analysis,
    %% starting tracing is too slow otherwise!
    receive after 100 -> ok end,
    
    Debug = debug_options(Name, Options),

    AmCandidate = member(node(), CandidateNodes),

    Election =  #election{candidate_nodes = CandidateNodes,
			  worker_nodes = Workers,
			  name = Name,
			  nextel = 0},

    case {catch Mod:init(Arg), AmCandidate} of
      {{stop, Reason},_} ->
        proc_lib:init_ack(Starter, {error, Reason}),
        exit(Reason);
      {ignore,_} ->
        proc_lib:init_ack(Starter, ignore),
        exit(normal);
      {{'EXIT', Reason},_} ->
        proc_lib:init_ack(Starter, {error, Reason}),
        exit(Reason);
      {{ok, State}, true} ->
        NewE = startStage1(Election#election{incarn = incarnation(node())}),
        
        proc_lib:init_ack(Starter, {ok, self()}),
        safe_loop(#server{parent = Parent,mod = Mod,state = State,debug = Debug}, 
                  candidate, NewE,{init});
      {{ok, State}, false} ->
        proc_lib:init_ack(Starter, {ok, self()}),
        case lists:member(self(), Workers) of 
          false ->
            rpc:multicall(CandidateNodes, gen_leader, worker_announce, [Name, node(self())]);
          _ -> nop
        end,
        safe_loop(#server{parent = Parent,mod = Mod,state = State,debug = Debug}, 
                  waiting_worker, Election,{init});
      Else ->
        Error = {bad_return_value, Else},
        proc_lib:init_ack(Starter, {error, Error}),
        exit(Error)
    end.




%%% ---------------------------------------------------
%%% The MAIN loops.
%%% ---------------------------------------------------


safe_loop(#server{mod = Mod, state = State} = Server, Role,
	  #election{name = Name} = E, _PrevMsg) ->
    % Event for QuickCheck
    % ?EVENT({Role,E}),
    receive
	{system, From, Req} ->
	    #server{parent = Parent, debug = Debug} = Server,
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [safe, Server, Role, E]);
	{'EXIT', _, Reason} = Msg ->
	    terminate(Reason, Msg, Server, Role, E);
  {report_leader, From} = Msg ->
        From ! {election_leader, self(), E#election.leader},
        safe_loop(Server, Role, E, Msg);
	{halt,T,From} = Msg ->
	    NewE = halting(E,T,From),
	    From ! {ackLeader,T,self()},
	    safe_loop(Server,Role,NewE,Msg);
	{hasLeader,Ldr,T,_} = Msg ->
	    NewE1 = mon_node(E,Ldr),
	    case ( (E#election.status == elec2) and (E#election.acks /= []) ) of
		true ->
		    lists:foreach(
		      fun(Node) ->
			       {Name,Node} ! {hasLeader,Ldr,T,self()}
		      end,E#election.acks);
		false ->
		    ok
	    end,
	    NewE = NewE1#election{elid = T,
				  status = wait,
				  leadernode = node(Ldr),
				  down = E#election.down -- [node(Ldr)],
				  acks = []},
	    Ldr ! {isLeader,T,self()},
	    safe_loop(Server,Role,NewE,Msg);
	{isLeader,T,From} = Msg ->
	    From ! {notLeader,T,self()},
	    safe_loop(Server,Role,E,Msg);
	{notLeader,T,_} = Msg ->
	    case ( (E#election.status == wait) and (E#election.elid == T) ) of
		true ->
		    NewE = startStage1(E);
		false ->
		    NewE = E
	    end,
	    safe_loop(Server,Role,NewE,Msg);
	{ackLeader,T,From} = Msg ->
	    case ( (E#election.status == elec2) and (E#election.elid == T) and 
		   (E#election.pendack == node(From)) ) of
		true ->
		    NewE = continStage2(E#election{acks = [node(From)|E#election.acks]});
		false ->
		    NewE = E
	    end,
	    hasBecomeLeader(NewE,Server,Msg);
	{ldr,Synch,T,From, LeaderE} = Msg ->
	    case ( (E#election.status == wait) and (E#election.elid == T) ) of
		true ->
		    NewE1 = mon_node(E,From),
		    NewE = NewE1#election{leader = From,
					  leadernode = node(From),
					  status = norm,
            worker_nodes = LeaderE#election.worker_nodes},
		    {ok,NewState} = Mod:surrendered(State,Synch,NewE),
		    loop(Server#server{state = NewState},surrendered,NewE,Msg);
		false ->
		    safe_loop(Server,Role,E,Msg)
	    end;
	{normQ,T,From} = Msg ->
	    case ( (E#election.status == elec1) or 
		   ( (E#election.status == wait) and (E#election.elid == T))) of
		true ->
		    NewE = halting(E,T,From),
		    From ! {notNorm,T,self()};
		false ->
		    NewE = E
	    end,
	    safe_loop(Server,Role,NewE,Msg);

	{notNorm,_,_} = Msg ->
	    safe_loop(Server,Role,E,Msg);
	{workerAlive,T,From} = Msg ->
	    case E#election.leadernode == none of
		true -> % We should initiate activation, monitor the possible leader!
		    NewE = mon_node(E#election{leadernode = node(From),
						    elid = T},
				    From),
		    From ! {workerIsAlive,T,self()};
		false -> 
		    % We should acutally ignore this, the present activation
		    % will complete or abort first...
		    NewE = E
	    end,
	    safe_loop(Server,Role,NewE,Msg);
	{workerIsAlive,_,_} = Msg ->
	    % If this happens, the activation process should abort
	    % This process is no longer the leader!
	    % The sender will notice this via a DOWN message
	    safe_loop(Server,Role,E,Msg);
	{activateWorker,T,Synch,From,OldE} = Msg ->
	    case ( (T == E#election.elid) and (node(From) == E#election.leadernode)) of
		true ->
		    NewE = E#election{ leader = From,
				       status = worker, worker_nodes = OldE#election.worker_nodes },
		    {ok,NewState} = Mod:surrendered(State,Synch,NewE),
		    loop(Server#server{state = NewState},worker,NewE,Msg);
		false ->
		    % This should be a VERY special case...
		    % But doing nothing is the right thing!
		    % A DOWN message should arrive to solve this situation
		    safe_loop(Server,Role,E,Msg)
	    end;
	
	{tau_timeout} = Msg ->
	    safe_loop(Server,Role,E,Msg);
	{'DOWN',_Ref,process,From,_Reason} = Msg when Role == waiting_worker ->
	    % We are only monitoring one proc, the leader!
	    Node = case From of
		       {Name,_Node} -> _Node;
		       _ when is_pid(From) -> node(From)
		   end,
	    case Node == E#election.leadernode of
		true ->
		    NewE = E#election{ leader = none, leadernode = none,
				       status = waiting_worker,
				       monitored = []};
		false ->
		    NewE = E
	    end,  
	    safe_loop(Server, Role, NewE,Msg);
	{'DOWN',Ref,process,From,_Reason} = Msg ->
	    Node = case From of
		       {Name,_Node} -> _Node;
		       _ when is_pid(From) -> node(From)
		   end,
	    NewMon = E#election.monitored -- [{Ref,Node}],
	    case lists:member(Node,E#election.candidate_nodes) of
		true ->
		    NewDown = [Node | E#election.down],
		    E1 = E#election{down = NewDown, monitored = NewMon},
		    case ( pos(Node,E#election.candidate_nodes) < 
			     pos(node(),E#election.candidate_nodes) ) of
			true ->
			    Lesser = lesser(node(),E#election.candidate_nodes),
			    LesserIsSubset = (Lesser -- NewDown) == [],
			    case ((E#election.status == wait) and (Node == E#election.leadernode)) of
				true ->
				    NewE = startStage1(E1);
				false ->
				    case ((E#election.status == elec1) and LesserIsSubset) of
					true ->
					    NewE = startStage2(E1#election{down = Lesser});
					false ->
					    NewE = E1
				    end
			    end;
			false ->
			    case ( (E#election.status == elec2) and (Node == E#election.pendack) ) of
				true ->
				    NewE = continStage2(E1);
				false ->
				    case ( (E#election.status == wait) and 
					   (Node == E#election.leadernode)) of
					true -> 
					    NewE = startStage1(E1);
					false ->
					    NewE = E1
				    end
			    end
		    end 
	    end,
      hasBecomeLeader(NewE,Server,Msg)
    end.


loop(#server{parent = Parent,
             mod = Mod,
             state = State,
             debug = Debug} = Server, Role, #election{name = Name} = E, _PrevMsg) ->
                                                % Event for QuickCheck
                                                % ?EVENT({Role,E}),
  receive
    Msg ->

	    case Msg of
        {system, From, Req} ->
          sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                                [normal, Server, Role, E]);
        {'EXIT', Parent, Reason} ->
          terminate(Reason, Msg, Server, Role, E);

        {halt,_,From} ->
          From ! {hasLeader,E#election.leader,E#election.elid,self()},
          loop(Server,Role,E,Msg);
        {hasLeader,_,_,_} ->
          loop(Server,Role,E,Msg);
        {isLeader,T,From} ->
          case (self() == E#election.leader) of
            true -> 
              NewE = mon_node(E#election{down = E#election.down -- [node(From)]},
                              From),
              {ok,Synch,NewState} = Mod:elected(State,NewE),
              From ! {ldr,Synch,E#election.elid,self(), NewE}, 
              loop(Server#server{state = NewState},Role,NewE,Msg);	
            false ->
              From ! {notLeader,T,self()},
              loop(Server,Role,E,Msg)
          end;
        {ackLeader,_,_} -> 
          loop(Server,Role,E,Msg);
        {notLeader,_,_} ->
          loop(Server,Role,E,Msg);
        {ack,_,_} ->
          loop(Server,Role,E,Msg);
        {ldr,_,_,_,_} ->
          loop(Server,Role,E,Msg);
        {normQ,_,_} ->
          loop(Server,Role,E,Msg);
        {notNorm,T,From} ->
          case ( (E#election.leader == self()) and (E#election.elid == T) ) of 
            true ->
              NewE = mon_node(E#election{down = E#election.down -- [node(From)]},
                              From),
              {ok,Synch,NewState} = Mod:elected(State,NewE),
              From ! {ldr,Synch,E#election.elid,self(), NewE},
              loop(Server#server{state = NewState},Role,NewE,Msg);	
            false ->
              loop(Server,Role,E,Msg)
          end;
        {workerAlive,_,_} ->
                                                % Do nothing if we get this from a new leader
                                                % We will soon notice that the prev leader has died, and
                                                % get the same message again when we are back in safe_loop!
          loop(Server,Role,E,Msg);
        {activateWorker,_,_,_} ->
                                                % We ignore this, we are already active... 
                                                % It must be an old message!
          loop(Server,Role,E,Msg);
        {workerIsAlive,T,From} ->
          case ((T == E#election.elid) and (self() == E#election.leader) 
                                                %            and iselem(node(From),E#election.monitored)
               ) of
            true ->
              NewE = mon_node(
                       E#election{work_down = E#election.work_down -- [node(From)]},
                       From),
                                                %			    NewE = E#election{work_down = E#election.work_down -- [node(From)]},
              {ok,Synch,NewState} = Mod:elected(State,NewE),
              From ! {activateWorker,T,Synch,self(), NewE},
              loop(Server#server{state = NewState},Role,NewE,Msg);		
            false ->
              loop(Server,Role,E,Msg)
          end;
        {tau_timeout} ->
          case (E#election.leader == self()) of
            true ->
              lists:foreach(
                fun(Node) ->
                    {Name,Node} ! {normQ,E#election.elid,self()}
                end,E#election.down -- [lists:nth(1,E#election.candidate_nodes)]),

              lists:foreach(
                fun(Node) ->
                    {Name,Node} ! {workerAlive,E#election.elid,self()}
                end,E#election.work_down),

              timer:send_after(?TAU,{tau_timeout});
            false ->
              ok
          end,
          loop(Server,Role,E,Msg);
        {'DOWN',_Ref,process,From,_Reason} when Role == worker ->
                                                % We are only monitoring one proc, the leader!
          Node = case From of
                   {Name,_Node} -> _Node;
                   _ when is_pid(From) -> node(From)
                 end,
          case Node == E#election.leadernode of
            true ->
              NewE = E#election{ leader = none, leadernode = none,
                                 status = waiting_worker,
                                 monitored = []},
              safe_loop(Server, waiting_worker, NewE,Msg);
            false ->
              loop(Server, Role, E,Msg)
          end;		    
        {'DOWN',Ref,process,From,_Reason} ->
          Node = case From of
                   {Name,_Node} -> _Node;
                   _ when is_pid(From) -> node(From)
                 end,
          NewMon = E#election.monitored -- [{Ref,Node}],
          case lists:member(Node,E#election.candidate_nodes) of
            true ->
              NewDown = [Node | E#election.down],
              E1 = E#election{down = NewDown, monitored = NewMon},
              case (Node == E#election.leadernode) of
                true -> 
                  NewE = startStage1(E1),
                  safe_loop(Server, candidate, NewE,Msg);

                false ->
                  loop(Server, Role, E1,Msg)
              end;
            false ->
                                                % I am the leader, make sure the dead worker is in work_down.
              E1 = E#election{monitored = NewMon,
                              work_down = [Node | (E#election.work_down -- [Node])]
                             },
              loop(Server, Role, E1,Msg)
          end;
        {add_worker, WorkerNode} ->
          case {node(self()) =:= node(self()), 
                lists:member(WorkerNode, E#election.worker_nodes)} of
            {false, _} ->
              loop(Server, Role, E, Msg);
            {true, false} ->
              {WNodes, DNodes} = {E#election.worker_nodes, E#election.work_down},
              loop(Server, Role, E#election{worker_nodes=[WorkerNode|WNodes],
                                            work_down=[WorkerNode|DNodes]},
                   Msg);
            {true, true} -> % Redundancy, meet the mirror
              loop(Server, Role, E, Msg)
          end;
        _Msg when Debug == [] ->
          handle_msg(Msg, Server, Role, E);
        _Msg ->
          Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
                                    E#election.name, {in, Msg}),
          handle_msg(Msg, Server#server{debug = Debug1}, Role, E)
	    end
  end.

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
%% @hidden 
system_continue(_Parent, _Debug, [safe, Server, Role, E]) ->
    safe_loop(Server, Role, E,{});
system_continue(_Parent, _Debug, [normal, Server, Role, E]) ->
    loop(Server, Role, E,{}).

%% @hidden 
system_terminate(Reason, _Parent, _Debug, [_Mode, Server, Role, E]) ->
    terminate(Reason, [], Server, Role, E).

%% @hidden 
system_code_change([Mode, Server, Role, E], _Module, OldVsn, Extra) ->
    #server{mod = Mod, state = State} = Server,
    case catch Mod:code_change(OldVsn, State, E, Extra) of
	{ok, NewState} ->
	    NewServer = Server#server{state = NewState},
	    {ok, [Mode, NewServer, Role, E]};
	{ok, NewState, NewE} ->
	    NewServer = Server#server{state = NewState},
	    {ok, [Mode, NewServer, Role, NewE]};
	Else -> Else
    end.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
%% @hidden 
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got local call ~p from ~w~n",
		      [Name, Call, From]);
	{'$leader_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got global call ~p from ~w~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got local cast ~p~n",
		      [Name, Cast]);
	{'$leader_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got global cast ~p~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n", 
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).

handle_msg({'$leader_call', From, Request} = Msg, 
	   #server{mod = Mod, state = State} = Server, elected = Role, E) ->
  case catch Mod:handle_leader_call(Request, From, State, E) of
	{reply, Reply, NState} ->
	    NewServer = reply(From, {leader,reply,Reply},
			      Server#server{state = NState}, Role, E),
	    loop(NewServer, Role, E,Msg);
	{reply, Reply, Broadcast, NState} ->
	    NewE = broadcast({from_leader,Broadcast}, E),
	    NewServer = reply(From, {leader,reply,Reply},
			      Server#server{state = NState}, Role,
			      NewE),
	    loop(NewServer, Role, NewE,Msg);
	{noreply, NState} = Reply ->
	    NewServer = handle_debug(Server#server{state = NState},
				     Role, E, Reply),
	    loop(NewServer, Role, E,Msg);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Msg, 
				 Server#server{state = NState},
				 Role, E)),
	    reply(From, Reply),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Msg, Server, Role, E)
    end;
handle_msg({'$leader_call', From, Request} = Msg, 
           Server, Role,
	   #election{buffered = Buffered, leader = Leader} = E) ->
    Ref = make_ref(),
    Leader ! {'$leader_call', {self(),Ref}, Request},
    NewBuffered = [{Ref,From}|Buffered],
    loop(Server, Role, E#election{buffered = NewBuffered},Msg);
handle_msg({'$leader_cast', Request} = Msg, 
           #server{mod = Mod, state = State} = Server, 
           elected = Role, 
           E) ->
  case catch Mod:handle_leader_cast(Request, State, E) of
    {noreply, NState} ->
      loop(Server#server{state = NState}, Role, E, Msg);
    {noreply, Broadcast, NState} ->
      NewE = broadcast({from_leader, Broadcast}, E),
      loop(Server#server{state = NState}, Role, NewE, Msg);
    {stop, Reason, NState} ->
      {'EXIT', R} = 
        (catch terminate(Reason, Msg, Server#server{state = NState}, Role, E)),
      exit(R);
    Other ->
      handle_common_reply(Other, Msg, Server, Role, E)
  end;
handle_msg({'$leader_cast', _} = Msg, 
           Server, 
           Role, 
           #election{buffered=_Buffered, leader = Leader} = E) ->
  Leader ! Msg,
  loop(Server, Role, E, Msg);
handle_msg({from_leader, Cmd} = Msg, 
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    handle_common_reply(catch Mod:from_leader(Cmd, State, E), 
			Msg, Server, Role, E);
handle_msg({Ref, {leader,reply,Reply}} = Msg, Server, Role,
	   #election{buffered = Buffered} = E) ->
    {value, {_,From}} = keysearch(Ref,1,Buffered),
    NewServer = reply(From, {leader,reply,Reply}, Server, Role,
		      E#election{buffered = keydelete(Ref,1,Buffered)}),
    loop(NewServer, Role, E, Msg);
handle_msg({'$gen_call', From, Request} = Msg, 
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    case catch Mod:handle_call(Request, From, State) of
	{reply, Reply, NState} ->
	    NewServer = reply(From, Reply, 
			      Server#server{state = NState}, Role, E),
	    loop(NewServer, Role, E, Msg);
	{noreply, NState} = Reply ->
	    NewServer = handle_debug(Server#server{state = NState},
				     Role, E, Reply),
	    loop(NewServer, Role, E, Msg);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Msg, Server#server{state = NState},
				 Role, E)),
	    reply(From, Reply),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Msg, Server, Role, E)
    end;
handle_msg({'$gen_cast',Msg} = Cast,
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    handle_common_reply(catch Mod:handle_cast(Msg, State), 
			Cast, Server, Role, E);
handle_msg(Msg,
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    handle_common_reply(catch Mod:handle_info(Msg, State),
			Msg, Server, Role, E).


handle_common_reply(Reply, Msg, Server, Role, E) ->
  case Reply of
    {noreply, NState} ->
      NewServer = handle_debug(Server#server{state = NState},
                               Role, E, Reply),
      loop(NewServer, Role, E, Msg);
    {ok, NState} ->
      NewServer = handle_debug(Server#server{state = NState},
                               Role, E, Reply),
      loop(NewServer, Role, E, Msg);
    {stop, Reason, NState} ->
	    terminate(Reason, Msg, Server#server{state = NState}, Role, E);
    {'EXIT', Reason} ->
	    terminate(Reason, Msg, Server, Role, E);
    _ ->
	    terminate({bad2_return_value, Reply}, Msg, Server, Role, E)
  end.


reply({To, Tag}, Reply, #server{state = State} = Server, Role, E) ->
    reply({To, Tag}, Reply),
    handle_debug(Server, Role, E, {out, Reply, To, State}).


handle_debug(#server{debug = []} = Server, _Role, _E, _Event) ->
    Server;
handle_debug(#server{debug = Debug} = Server, _Role, E, Event) ->
    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
			      E#election.name, Event),
    Server#server{debug = Debug1}.

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

terminate(Reason, Msg, #server{mod = Mod, 
			       state = State,
			       debug = Debug} = _Server, _Role,
	  #election{name = Name} = _E) ->
    case catch Mod:terminate(Reason, State) of
	{'EXIT', R} ->
	    error_info(R, Name, Msg, State, Debug),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		_ ->
		    error_info(Reason, Name, Msg, State, Debug),
		    exit(Reason)
	    end
    end.

%% Maybe we shouldn't do this?  We have the crash report...
error_info(Reason, Name, Msg, State, Debug) ->
    format("** Generic leader ~p terminating \n"
           "** Last message in was ~p~n"
           "** When Server state == ~p~n"
           "** Reason for termination == ~n** ~p~n",
	   [Name, Msg, State, Reason]),
    sys:print_log(Debug),
    ok.

%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Name, Opts) ->
    case opt(debug, Opts) of
	{ok, Options} -> dbg_options(Name, Options);
	_ -> dbg_options(Name, [])
    end.

dbg_options(Name, []) ->
    Opts = 
	case init:get_argument(generic_debug) of
	    error ->
		[];
	    _ ->
		[log, statistics]
	end,
    dbg_opts(Name, Opts);
dbg_options(Name, Opts) ->
    dbg_opts(Name, Opts).

dbg_opts(Name, Opts) ->
    case catch sys:debug_options(Opts) of
	{'EXIT',_} ->
	    format("~p: ignoring erroneous debug options - ~p~n",
		   [Name, Opts]),
	    [];
	Dbg ->
	    Dbg
    end.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
%% @hidden 
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [_Mode, Server, _Role, E]] = StatusData,
    Header = lists:concat(["Status for generic server ", E#election.name]),
    Log = sys:get_debug(log, Debug, []),
    #server{mod = Mod, state = State} = Server,
    Specific = 
	case erlang:function_exported(Mod, format_status, 2) of
	    true ->
		case catch apply(Mod, format_status, [Opt, [PDict, State]]) of
		    {'EXIT', _} -> [{data, [{"State", State}]}];
		    Else -> Else
		end;
	    _ ->
		[{data, [{"State", State}]}]
	end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log}]} |
     Specific].


%%-----------------------------------------------------------------
%% Leader-election functions
%%-----------------------------------------------------------------

%% Corresponds to startStage1 in Figure 1 in the Stoller-article
startStage1(E) -> 
    Elid = {pos(node(),E#election.candidate_nodes),E#election.incarn,E#election.nextel},
    NewE = E#election{
		 elid = Elid,
		 nextel = E#election.nextel + 1,
		 down = [],
		 status = elec1},    
    case ( pos(node(),E#election.candidate_nodes) == 1) of
	true ->
	    startStage2(NewE);
	false ->
	    mon_nodes(NewE,lesser(node(),E#election.candidate_nodes))
    end.

%% Corresponds to startStage2
startStage2(E) ->
    continStage2(E#election{		       
		   status = elec2,
		   pendack = node(),
		   acks = []}).

continStage2(E) ->
    case pos(E#election.pendack,E#election.candidate_nodes) < length(E#election.candidate_nodes) of
	true ->	    
	    Pendack = next(E#election.pendack,E#election.candidate_nodes),
	    NewE = mon_nodes(E,[Pendack]),
	    {E#election.name,Pendack} ! {halt,E#election.elid,self()},
	    NewE#election{pendack = Pendack}; 
       false ->
	    % I am the leader
	    % io:format("I am the leader (Node ~w) ~n", [node()]),
	    E#election{leader = self(), 
		       leadernode = node(),
		       status = norm}
    end.

%% corresponds to Halting
halting(E,T,From) ->
    NewE = mon_node(E,From),
    NewE#election{elid = T,
		  status = wait,
		  leadernode = node(From),
		  down = E#election.down -- [node(From)]
		 }.

%% Start monitor a bunch of nodes
mon_nodes(E,Nodes) ->
    foldl(
      fun(Node,_E) ->
	      mon_node(_E,{_E#election.name,Node})
      end,E,Nodes).

%% Star monitoring one Process
mon_node(E,Proc) ->
    Node = case Proc of
	       {_Name,Node_} -> 
		   Node_;
	       Pid when is_pid(Pid) -> 
		   node(Pid)
	   end,
    case iselem(Node,E#election.monitored) of
	true ->
	    E;
	false ->
	    Ref = erlang:monitor(process,Proc),
	    E#election{monitored = [{Ref,Node} | E#election.monitored]}
    end.
		       

%% Stop monitoring of a bunch of nodes
%demon_nodes(E) ->
%    foreach(fun({R,_}) ->
%                    erlang:demonitor(R)
%            end,E#election.monitored),
%    E#election{monitored = []}.

%% checks if the proc has become the leader, if so switch to loop
hasBecomeLeader(E,Server,Msg) ->
    case ((E#election.status == norm) and (E#election.leader == self())) of
	true ->
	    {ok,Synch,NewState} = (Server#server.mod):elected(Server#server.state,E),
	    lists:foreach(
	      fun(Node) ->
		      {E#election.name,Node} ! 
			  {ldr, Synch, E#election.elid, self(), E}
	      end,E#election.acks),

	    % Make sure we will try to contact all workers!
	    NewE = E#election{work_down = E#election.worker_nodes},

	    % Set the internal timeout (corresponds to Periodically)
	    timer:send_after(?TAU,{tau_timeout}), %It's meaningful only when I am the leader!
	    loop(Server#server{state = NewState},elected,NewE,Msg);
	false ->
	    safe_loop(Server,candidate,E,Msg)
    end.




%%%
%
% incarnation should return an integer value for the next
% incarnation of this node. We create a file for each node,
% this file contains a counter. When starting the system for the
% first time, the files should be intialized with 0 incarnation
% counter for all nodes orelse be removed, since we create 
% files if not present with counter 1.
%
% Atomicity: This approach is safe as long as there is only 
% one gen_leader running per node.
%
incarnation(Node) ->
    case file:read_file_info(Node) of
	{error,_Reason} ->
	    ok = file:write_file(Node,term_to_binary(1)),
	    0;
	{ok,_} ->
	    {ok,Bin} = file:read_file(Node),
	    Incarn = binary_to_term(Bin),
	    ok = file:write_file(Node,term_to_binary(Incarn+1)),
	    Incarn
    end.


broadcast(Msg, #election{monitored = Monitored} = E) ->
    %% This function is used for broadcasts,
    %% and we make sure only to broadcast to already known nodes.
    ToNodes = [N || {_,N} <- Monitored],
    broadcast(Msg, ToNodes, E).

broadcast({from_leader, Msg}, ToNodes, E) ->
    foreach(
      fun(Node) ->
	      {E#election.name,Node} ! {from_leader, Msg}
      end,ToNodes),
    E.

iselem(_,[]) ->
    false;
iselem(P,[{_,P}|_]) ->
    true;
iselem(P,[_ | Ns]) ->
    iselem(P,Ns).

lesser(_,[]) ->
    [];
lesser(N,[N|_]) ->
    [];
lesser(N,[M|Ms]) ->
    [M|lesser(N,Ms)].

next(_,[]) ->
    no_val;
next(N,[N|Ms]) ->
    lists:nth(1,Ms);
next(N,[_|Ms]) ->
    next(N,Ms).

pos(N1,[N1|_]) ->
    1;
pos(N1,[_|Ns]) ->
    1+pos(N1,Ns).
