-module(riak_core_vnode_proxy_sup_intercepts).
-compile(export_all).
-include("intercept.hrl").
-define(M, riak_core_vnode_proxy_sup_orig).

sleep_start_proxies(Mod=riak_kv_vnode) ->
    ?I_INFO("Delaying start of riak_kv_vnode proxies for 3s\n"),
    timer:sleep(3000),
    ?M:start_proxies_orig(Mod);
sleep_start_proxies(Mod) ->
    ?M:start_proxies_orig(Mod).

