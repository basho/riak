-module(riak_repl_eventer).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([subscribe/1, unsubscribe/1]).
-record(state, {subs, monrefs, client}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Self = self(),
    spawn(fun() -> riak_repl_util:wait_for_riak(Self) end),    
    {ok, #state{subs=sets:new(), monrefs=[]}}.

subscribe(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}).

handle_call({subscribe, Pid}, _From, State=#state{subs=Subs, monrefs=MRs}) ->
    case sets:is_element(Pid, Subs) of
        true -> 
            {reply, ok, State};
        false ->
            NewSubs = sets:add_element(Pid, Subs),
            MonRef = erlang:monitor(process, Pid),
            NewMRs = [{Pid, MonRef}|MRs],
            {reply, ok, State#state{subs=NewSubs, monrefs=NewMRs}}
    end;
handle_call({unsubscribe, Pid}, _From, State=#state{subs=Subs, monrefs=MRs})->
    NewMRs = case lists:keyfind(Pid, 1, MRs) of
                 {Pid, MonRef} ->
                     erlang:demonitor(MonRef),
                     lists:keydelete(Pid, 1,MRs);
                 false ->
                     MRs
             end,
    {reply, ok, State#state{subs=sets:del_element(Pid, Subs), monrefs=NewMRs}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(riak_up, State) ->
    Client = start_eventer(),
    {noreply, State#state{client=Client}};
handle_info({'DOWN', MonRef, process, Pid, _Reason}, State=#state{subs=Subs, monrefs=MRs}) ->
    {noreply, State#state{subs=sets:del_element(Pid, Subs), 
                          monrefs=lists:delete({Pid, MonRef}, MRs)}};
handle_info({event, {riak_put_fsm, put_fsm_reply_ok, _, 
                     {_,_,{Bucket,Key}}}}, State=#state{client=Client, subs=Subs}) ->
    case Client:get(Bucket, Key, 1) of
        {ok, O} -> [P ! {local_update, O} || P <- sets:to_list(Subs)];
        _Other -> nop
    end,
    {noreply, State};
handle_info({event, {riak_delete, finalize_reap, _,
                     {_, _Bucket, _Key, _}}}, State=#state{client=_Client}) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_eventer() ->
    {ok, Client} = riak:local_client(),
    EventerName = "Replication Eventer",
    MatchHead = {'$1', '$2', '_', '_'}, 
    MatchGuard = [{'orelse',
                   {'andalso', 
                    {'==', '$1', riak_put_fsm},
                    {'==', '$2', put_fsm_reply_ok}},
                   {'andalso',
                    {'==', '$1', riak_delete},
                    {'==', '$2', finalize_reap}}}],
    Client:add_event_handler(self(), EventerName, MatchHead, MatchGuard),
    Client.
