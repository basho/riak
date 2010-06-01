-module(riak_jmx_monitor).
-behaviour(gen_server).
%% API
-export([start_link/0, stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {port,
                sock,
                portnum}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop_jmx).

init([]) ->
    {ok, WebIp} = application:get_env(riak_core, web_ip),
    {ok, WebPort} = application:get_env(riak_core, web_port),
    case application:get_env(riak_jmx, enabled) of
        {ok, true} ->
            error_logger:info_msg("JMX server monitor starting (~p)~n", [self()]),
            CmdDir = priv_dir(),
            Cmd = filename:join([CmdDir, "riak_jmx.sh"]),
            case catch erlang:open_port({spawn_executable, Cmd}, 
                                        [stderr_to_stdout,
                                         {args, [WebIp, 
                                                 integer_to_list(WebPort)]},
                                         {cd, CmdDir}]) of
                {'EXIT', Error} ->
                    {stop, Error};
                Port when is_port(Port) ->
                    erlang:link(Port),
                    {ok, #state{port=Port}}
            end;
        _ ->
            ignore
    end.

handle_call(_Request, _From, State) -> {reply, ignore, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

priv_dir() ->
    case code:priv_dir(riak_jmx) of
        {error, bad_name} ->
            Path0 = filename:dirname(code:which(?MODULE)),
            Path1 = filename:absname_join(Path0, ".."),
            filename:join([Path1, "priv"]);
        Path ->
            filename:absname(Path)
    end.
