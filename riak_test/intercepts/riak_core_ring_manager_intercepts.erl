-module(riak_core_ring_manager_intercepts).
-compile(export_all).
-include("intercept.hrl").

noop_ring_trans(_Fun, _Args) ->
    ?I_INFO("overriding ring_trans as a noop"),
    not_changed.
