-module(hashtree_intercepts).
-compile(export_all).
-include("intercept.hrl").

sleep_update_perform(State) ->
    ?I_INFO("sleeping update_perform 60s\n"),
    timer:sleep(60000),
    hashtree_orig:update_perform_orig(State).
