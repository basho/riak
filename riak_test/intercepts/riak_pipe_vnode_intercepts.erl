-module(riak_pipe_vnode_intercepts).
-compile(export_all).
-include("intercept.hrl").

-define(M, riak_pipe_vnode_orig).

%%% Intercepts

%% @doc Log every command received by the vnode during handoff, so a
%% test can analyze them later. Sends the first element of the `Cmd'
%% tuple (the command type) to the process registered as
%% `riak_test_collector'.
%%
%% Tests using this intercept are expected to start the collector
%% process themselves.
log_handoff_command(Cmd, Sender, State) ->
    try
        riak_test_collector ! element(1, Cmd)
    catch error:badarg when not is_tuple(Cmd) ->
            ?I_INFO("Cmd was not a tuple");
          error:badarg ->
            ?I_INFO("Collector process not registered")
    end,
    ?M:handle_handoff_command_orig(Cmd, Sender, State).

