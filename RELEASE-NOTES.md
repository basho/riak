# Riak 1.2 Release Notes

## Features and Improvements for Riak

* Aggregation of non-streamed MapReduce results was
  improved. Previous versions used an O(n^2) process, where n is
  the number of outputs for a phase. The aggregation in Riak 1.2 is
  O(n). (riak_kv#331,333, riak-erlang-client#58,59).

* Timeouts of MapReduce jobs now produce less error log spam. The
  safe-to-ignore-but-confusing `{sink_died, normal}` messages have
  been removed. (riak_pipe#45)

* The `riak-admin transfers` command now reports the
  [status of active transfers] [xfer_status].  This gives more insight
  into what transfers are occurring, their type, their running time,
  and the rate at which data is being transferred.  Calling this
  command will no longer stall handoff.

* The memory storage backend for Riak KV now supports secondary indexes and has a "test" mode that lets developers quickly clear all local storage (useful in the context of an external test suite).

### Protocol Buffers Enhancements

* The design of the Protocol Buffers on the server-side has been significantly refactored, allowing sub-applications other than Riak KV to supply services to clients.
* Secondary indexes can be natively queried from Protocol Buffers clients, They no longer need to emulate them with MapReduce.
* Riak Search indexes can be natively queried from Protocol Buffers clients. They no longer need to emulate them with MapReduce.

### Stats improvements

* Riak now uses the open source [Folsom](https://github.com/boundary/folsom) library for stats
* Riak_search now has stats
* Getting stats from riak_kv should no longer timeout under very heavy load as there is no longer a gen_server
process for stats.
* Stats can still be retrieved as before, with the addition that one can now attach to a node and
query stats directly through folsom. Use `folsom_metrics:get_metrics()` to see a list of available stats.
* Configurable sample types for histogram metrics in riak_kv and riak_search. Defaults to a one minute sliding window, with random uniform reservoir size of 1028 readings per second. This means that the following statistics *may* show slightly different results from pre1.2 nodes as there may be fewer readings than the total number or events.
    * riak_kv_node_get_fsm_siblings
    * riak_kv_node_get_fsm_time,
    * riak_kv_node_put_fsm_time
    * riak_kv_node_get_fsm_objsize
* You can configure the sample type by adding
    `{stat_sample_type, {slide, Window::int()}}` or `{stat_sample_type, {slide_uniform, {Window::int(), Size::int()}}}` to your `app.config` under the section for riak_kv and/or riak_search. Further you may change the sample type for a named stat only, like this `{{riak_kv, node_get_fsm_time}, {slide_uniform, {60, 10000}}}`

### Packaging Improvements

* A binary package for FreeBSD 9 is now provided
* A binary package for SmartOS is now provided
* Ubuntu packages for 10.04 (Lucid), 11.04 (Natty), and 12.04 (Precise) are now provided as separate packages
* See "Bugs Fixed" for packaging related bug fixes

### Leveldb tuning
* Bloom filter code from google added.  This greatly reduces the search time for keys that do not exist.
* File sizes increased 10x or more.  This reduces the amount of disk activity, increasing performance.

### Capability Negotiation

* Riak nodes now negotiate with each other to determine supported operating modes,
  allowing clusters containing mixed-versions of Riak to work properly without special
  configuration.

* This simplifies rolling upgrades. In the past, users needed to disable new features
  during the rolling upgrade, and then enable them after all nodes were upgraded. This
  is now handled automatically by Riak.

* This change replaces several existing configuration parameters, with the old settings
  being ignored entirely in Riak 1.2. The following values are the ones that are no longer
  used in Riak 1.2, along with the new behavior:
  * `riak_core/legacy_vnode_routing`: Uses the newer vnode routing layer (introduced in Riak 1.1) when supported; otherwise, defaults to the legacy routing protocol.
  * `riak_kv/legacy_keylisting`: Uses coverage based keylisting (introduced in Riak 1.0) when supported; otherwise,
    defaults to the legacy keylisting behavior.
  * `riak_kv/listkeys_backpressure`: Enables listkeys backpressure (introduced in Riak 1.1) when supported.
  * `riak_kv/mapred_2i_pipe`: Use parallel secondary-index input to map/reduce jobs (introduced in Riak 1.1) when supported.
  * `riak_kv/mapred_system`: Use `riak_pipe` for map/reduce (introduced in Riak 1.0) when supported; otherwise, default to legacy `luke` system.

* To override capability negotiation (which is discouraged), there is now a per-component override setting
  that can be set in `app.config`. For example, the following could be added to the `riak_kv` section of
  `app.config` to alter negotiation of the `listkeys_backpressure` and `mapred_system` settings:

```erlang
%% Override listkeys_backpressure setting to always be set to 'false'.
%%
%% Override mapred_system setting to use 'legacy' if all nodes in the cluster
%% support 'legacy', otherwise use the built-in default setting.
[{override_capability,
  [{listkeys_backpressure, [{use, false}]},
   {mapred_system,         [{prefer, legacy}]}]
}]
```

### Overhauled Cluster Adminstration

* Riak now provides a multi-phase approach to cluster administration
  that allows changes to be staged and reviewed before being committed.

* This change allows multiple changes to be grouped together, such as
  adding multiple nodes at once, or adding some nodes while removing
  others.

* This new approach also provides details about how a set of staged
  changes will impact the cluster, listing the future ring ownership
  as well as the number of transfers necessary to implement the planned
  changes.

* This new approach is currently implemented only by `riak-admin`, and
  is not yet part of Riak Control. The older `riak-admin` commands such
  as `join, leave, force-remove` have been deprecated, although they can
  still be used by appending `-f`, eg. `riak-admin join -f`.

* The new cluster admin interface is accessed through `riak-admin cluster`:

```text
Usage: riak-admin cluster <command>

The following commands stage changes to cluster membership. These commands
do not take effect immediately. After staging a set of changes, the staged
plan must be committed to take effect:

   join <node>                    Join node to the cluster containing <node>
   leave                          Have this node leave the cluster and shutdown
   leave <node>                   Have <node> leave the cluster and shutdown

   force-remove <node>            Remove <node> from the cluster without
                                  first handing off data. Designed for
                                  crashed, unrecoverable nodes

   replace <node1> <node2>        Have <node1> transfer all data to <node2>,
                                  and then leave the cluster and shutdown

   force-replace <node1> <node2>  Reassign all partitions owned by <node1> to
                                  <node2> without first handing off data, and
                                  remove <node1> from the cluster.

Staging commands:
   plan                           Display the staged changes to the cluster
   commit                         Commit the staged changes
   clear                          Clear the staged changes
```

## Enhancements

* [Search - micro-optimization](https://github.com/basho/riak_search/pull/101)
* [Search - add repair cmd](https://github.com/basho/riak_search/pull/107)

## Known Issues

* The Protocol Buffers interface when returning `RpbErrorResp` responses to the client will set the `errcode` field to `0`, whereas before it was `1` or unset. Only client libraries that previously attempted to apply meaning to the `errcode` field will be affected. Improvement of the error responses from Protocol Buffers is planned for the next major release.
* Some spurious messages may be sent to the log after a Pipe-based MapReduce job sent via PBC has been shutdown. This does not affect normal operations. [basho/riak_kv#366](https://github.com/basho/riak_kv/issues/366)
* The SmartOS packages were tested against 1.5.x and 1.6.x datasets from Joyent.  The newest datasets of SmartOS 1.7.x have not been tested and are not supported currently.
* Secondary index queries against a heavily loaded cluster may hit an improperly-handled internal timeout and result in error responses. This affects both HTTP and Protocol Buffers interfaces and has existed since Riak 1.0. [basho/riak_kv#379](https://github.com/basho/riak_kv/pull/379)
* MapReduce queries may print messages in the log of the form, `[error] Module <module name> must be purged before loading`, due to a race in the code that ensures a module is loaded before it is used. This message may be safely ignored. It can be silenced by attaching to the Riak console and evaluating `code:purge(<module name>).`.
* Some users may experience a performance regression in 2I compared to 1.0 and 1.1. The problem manifests as higher latencies for range and equality queries. A preliminary investigation suggests the change of the Erlang VM from R14B04 to R15B01 is partially responsible, but there may be other factors.

## Bugs Fixed

* [Add CSRF protection to Riak Control resources](https://github.com/basho/riak_control/pull/28).
* Riak Control now returns the proper [content-types](https://github.com/basho/riak_control/pull/18) and [doctype](https://github.com/basho/riak_control/pull/16).
* [Updates to Riak Control to resolve issues when viewing
  Control on iOS devices and over connections with high latency.](https://github.com/basho/riak_control/pull/21)
* [Riak Control now reports pre-1.1 nodes in a mixed version cluster as incompatible instead of unreachable.] (https://github.com/basho/riak_control/pull/24)
* [Allow commented out -name lines in `vm.args`](https://github.com/basho/riak/issues/175)
* [Riak RPM package needs dep on `sudo`](https://github.com/basho/riak/issues/163)
* [Change ownership of `/etc/riak` for RPM packages](https://github.com/basho/riak/issues/165)
* [`/var/run/riak` is not recreated on demand at startup](https://github.com/basho/riak/issues/130)
* [Ensure `ring_creation_size` setting is in all packaged app.config files](https://github.com/basho/riak/issues/139)
* [Move the `pipe_dir` on fedora/centos to the `/tmp/riak` directory](https://github.com/basho/riak/issues/150)
* [Remove bashisms from shell scripts which use /bin/sh](https://github.com/basho/riak/issues/141)
* Stalls:  2i and leveldb each had scenarios where data operations would stall 7 to 120 seconds.
* "block_size" parameter within app.config for leveldb was ignored.  This parameter is now properly passed to leveldb.
* [Search - clear schema cache on update](https://github.com/basho/riak_search/pull/110)
* [Search - remove solr supervisor](https://github.com/basho/riak_search/pull/113)
* [Search - honor the '?' wildcard](https://github.com/basho/riak_search/pull/114)
* [Search - flatten 'MaxScore'](https://github.com/basho/riak_search/pull/119)
* [riak_api - Add riak_core as application dep to riak_api.app](https://github.com/basho/riak_api/issues/6)
* [riak_api - Register riak_api_stat mod with riak_core at start up](https://github.com/basho/riak_api/issues/7)
* [Add eleveldb:close - Fixes MANIFEST file missing bug](https://github.com/basho/eleveldb/pull/33)
* [riak_kv - Call eleveldb:close before destroy](https://github.com/basho/riak_kv/pull/368)
* [riak_control - Resolve base64 cookie truncation race condition.
](https://github.com/basho/riak_control/pull/30)
* [Fix FreeBSD package permissions on sbin](https://github.com/basho/riak/pull/183)
* [Create SmartOS SMF service for epmd](https://github.com/basho/riak/pull/187)
* [riak_core - Restructure supervision tree so that folsom is an included app](https://github.com/basho/riak_core/pull/217)
* [riak_core - Ring mgr crash creates confused cluster](https://github.com/basho/riak_core/issues/166)
* [riak_core - Make the ring manager responsible for loading the ring](https://github.com/basho/riak_core/pull/214)
* [riak_core - Fix capability system race condition](https://github.com/basho/riak_core/pull/216)
* [riak_kv - Changed semantics of backend:drop - backend must close all handles](https://github.com/basho/riak_kv/pull/373)
* [riak_kv - Call eleveldb:close on vnode stop for eleveldb backend](https://github.com/basho/riak_kv/pull/372)
* [leveldb - Make ref count increase atomic operation under read lock](https://github.com/basho/leveldb/pull/36)
* [leveldb - Change LRUCache destructor so it does NOT look like a bad reference](https://github.com/basho/leveldb/pull/38)
* [riak_control - Patch handoff status to work with status_v2](https://github.com/basho/riak_control/pull/34)
* [riak_core - Ensure legacy nodes are probed when new capabilities registered](https://github.com/basho/riak_core/pull/219)
* [riak - `riak attach` fails on some versions of SmartOS](https://github.com/basho/riak/issues/198)




## Notes

* The Luke application, and with it the "legacy" MapReduce system should be considered deprecated.  All systems should be configured to use Riak Pipe as their MapReduce system (the default since 1.0).  The Luke application may be removed as soon as the next release.
* The Innostore storage backend is deprecated and will not be supported in the 1.2 release.

[xfer_status]: http://basho.com/blog/technical/2012/06/25/Riak-Admin-Transfers-in-1-2-Release/
