__Work in progress until final release__

## Riak 1.3.0 Release Notes

### New Features or Major Improvements for Riak

#### Active Anti-Entropy
New in Riak 1.3. Riak now includes an active anti-entropy (AAE) subsystem that works to verify and repair data cross an entire Riak cluster. The AAE system periodically exchanges information between data replicas in order to determine missing or divergent data.  When bad replicas are detected, AAE triggers read repair to correct the situation. AAE is entirely automatic, and provides an additional layer of protection against various data loss scenarios (eg. disk failure, restoring from an outdated backup, bit rot, etc).

AAE is implemented using hash tree exchange, which ensures that the information exchanged between data replicas is proportional to the amount of divergent data rather than the total amount of data stored in Riak. When all data is in sync (the common case), exchanges are fast and have extremely low overhead. For this reason, AAE is able to perform multiple exchanges a minute with negligible impact on a cluster.

AAE hash trees are persistent entities stored in LevelDB instances separate from normal Riak K/V data. When first starting a fresh Riak 1.3 cluster (or upgrading from an older release), Riak will generate the hash tree information by traversing over each partition's data. By default, Riak will build one hash tree per hour per node. If the traversal over a partition's data takes more than an hour, then Riak may trigger a second tree build. However, by default at most two tree builds can occur at once.

Once a hash tree is built, it is kept up-to-date in real-time as writes are sent to Riak. However, trees are periodically expired and rebuilt to protect against potential divergence between the K/V data and its corresponding hash tree. Rebuilding trees also protects against silent data corruption (eg. bit rot). By default, trees are expired and rebuilt once a week.

All of the above settings (and more) can be configured in `app.config`. The AAE settings are in the `riak_kv` section, and have comments documenting the different options.

To provide insight into AAE, Riak provides the `riak-admin aae-status` command. The AAE status output is broken into three parts: Exchanges, Entropy Trees, and Keys Repaired.

```
================================== Exchanges ==================================
Index                                              Last (ago)    All (ago)
-------------------------------------------------------------------------------
0                                                  3.8 min       4.1 min
91343852333181432387730302044767688728495783936    3.3 min       7.8 min
182687704666362864775460604089535377456991567872   2.8 min       8.3 min
274031556999544297163190906134303066185487351808   2.3 min       6.3 min
365375409332725729550921208179070754913983135744   1.8 min       5.5 min
<snip>
```

The Exchanges section shows information about AAE exchanges for each K/V partition.  The `Last` column lists when the most recent exchange between a partition and one of its sibling replicas was performed. The `All` column shows how long it has been since a partition exchanged with all of its sibling replicas. In essence, the `All` column sets the upperbound on how out-of-date an individual partition can be. Specifically, a partition can not have any missing or divergent data older that the value shown in `All`, unless all replicas for that data are invalid.

```
================================ Entropy Trees ================================
Index                                              Built (ago)
-------------------------------------------------------------------------------
0                                                  22.1 min
91343852333181432387730302044767688728495783936    22.6 min
182687704666362864775460604089535377456991567872   22.3 min
274031556999544297163190906134303066185487351808   22.9 min
365375409332725729550921208179070754913983135744   22.3 min
<snip>
```

The Entropy Trees section shows when the hash trees for a given partition were created. A hash tree must be built before a partition can participate in an exchange. As mentioned above, trees are built once and expired (by default) once a week.

```
================================ Keys Repaired ================================
Index                                                Last      Mean      Max
-------------------------------------------------------------------------------
0                                                     0         0         0
91343852333181432387730302044767688728495783936       87        21        87
182687704666362864775460604089535377456991567872      0         0         0
274031556999544297163190906134303066185487351808      0         0         0
365375409332725729550921208179070754913983135744      0         0         0
<snip>
```

The Keys Repaired section presents information about repairs triggered by AAE, including keys repaired in the most recent exchange, and the mean and max across all exchanges.

Note: All AAE status information is in-memory and is reset across a node restart. Only tree build times are persistent (since trees themselves are persistent).

Final notes about AAE:

1. Trees must be built before exchange can occur. Since trees are built once an hour by default, it will take up to `ring_size / number_of_nodes` hours before all trees are built after first starting or upgrading to 1.3, and therefore that amount of time until AAE is fully protecting all data.

2. Tree building typically uses 100% of a CPU when possible but should have minimal impact on Riak performance. When using Bitcask for K/V data, tree building may increase the latency for `list_keys`, `list_buckets`, and Riak EE's fullsync replication strategy. Once trees are built, these issues go away (until trees are expired/rebuilt a week later).

3. AAE may occasionally repair a small number of keys (typically 1 or 2) even in a healthy cluster without divergent or missing data. This occurs when AAE is performing an exchange at the same time incoming writes are occurring to the same nodes. For example, a write may reach node A while being concurrently in-flight to node B, yet AAE happens to run at just the right moment to see the write on A but not B, and force a repair. Since AAE just triggers reads (to trigger read repair) this behavior is entirely safe.

4. AAE is a feature of Riak K/V and does not protect Riak Search data.

#### MapReduce Sink Backpressure

* [RiakKV - MapReduce Sink Backpressure](https://github.com/basho/riak_kv/pull/429), supported by [RiakPipe - Sink Type FSM](https://github.com/basho/riak_pipe/pull/59)

Riak Pipe brought inter-stage backpressure to Riak KV's MapReduce system. However, prior to Riak 1.3, that backpressure did not extend to the sink. It was assumed that the Protocol Buffers or HTTP endpoint could handle the full output rate of the pipe. With Riak 1.3, backpressure has been extended to the sink so that those endpoint processes no longer become overwhelmed. This backpressure is tunable via a soft cap on the size of the sink's buffer, and a period at which a worker should check that cap. These can be configured at the Riak console by setting application environment variables, or in the riak_kv section of app.config (defaults shown):

    {riak_kv,
     ...
     %% Soft cap on the MapReduce sink's buffer,
     %% expressed as a positive integer number of messages
     %% (one message is used per MapReduce result)
     {mrc_sink_buffer, 1000},

     %% Period at which a MapReduce worker must check
     %% the sink's buffer cap, expressed as an integer
     %% number of messages to send before waiting on
     %% an clear-to-send acknowledgement
     %%   0 = wait for acknowledgement of each message
     %%   1 = wait every other message
     %%   'infinity' = never wait for acknowledgements
     {mrc_sink_sync_period, 10}
    }.

#### Additional IPv6 support

* [Allow gen_nb_server to support IPv6 addresses - riak_core #249](https://github.com/basho/riak_core/pull/249)

Riak Handoff and Protocol Buffers interfaces can now listen on IPv6 addresses (HTTP has always supported IPv6). You may specify the address using the short-hand string form, e.g. `"::1"` (for localhost), or as the 8-byte address in a tuple, e.g. `{0,0,0,0,0,0,0,1}` (for localhost). IPv4 addresses may also be specified in either form (except the latter will be 4 bytes). *Note: This does not affect Riak node names. Refer to the `inet_dist_*` settings in the [Erlang documentation](http://www.erlang.org/documentation/doc-5.9.1/lib/kernel-2.15.1/doc/html/kernel_app.html) to enable IPv6 support for cluster membership.*

#### Luke Removal

* [Remove Luke Usage](https://github.com/basho/riak_kv/pull/433)

The luke application was deprecated in the release of Riak 1.2. This release removes it, and all code using it.

#### `riak getpid` Added

A bug existed in how we used `riak stop` (listed below in Bugs Fixed) that justified a refactoring of how we got our own PID of Riak.  While fixing the bug, it was thought getpid might be useful to system admins out there who don't want to rely on outside scripts to find the PID of Riak.  `riak getpid` does what you expect, returns the PID of a running Riak or exits with 1 on failure.  It is a small feature, but might save some time with `ps`, `grep`, and `awk`.

#### Riaknostic Included by Default

To encourage its use, we have now included Riaknostic in the Riak packages.  Prior to 1.3, the user needed to download riaknostic separately, but now `riak-admin diag` will work out of the box.

#### Support added for SmartOS 1.8

Packages are now available for SmartOS machines based on 1.8 datasets as well as 1.6.

#### Health Check 

New in Riak 1.3. Riak Core now includes a health check subsystem that actively monitors each node for specific conditions and disables/enables services based on those conditions.

To enable/disable all health checks a new setting has been added to the `riak_core` section of `app.config`:

    %% Health Checks
    %% If disabled, health checks registered by an application will
    %% be ignored. NOTE: this option cannot be changed at runtime.
    %% To re-enable, the setting must be changed and the node restarted.
    {enable_health_checks, true},

Riak registers a health check with Riak Core to monitor the message queue lengths of KV vnodes. To configure the kv health check a new setting has been added to the `riak_kv` section of `app.config`:

    %% This option configures the riak_kv health check that monitors
    %% message queue lengths of riak_kv vnodes. The value is a 2-tuple,
    %% {EnableThreshold, DisableThreshold}. If a riak_kv_vnode's message
    %% queue length reaches DisableThreshold the riak_kv service is disabled
    %% on this node. The service will not be re-enabled until the message queue
    %% length drops below EnableThreshold.
    {vnode_mailbox_limit, {1, 5000}}

Note: the kv health check does not apply to Riak Search or Riak Pipe vnodes.

#### Reset Bucket Properties

The HTTP interface now supports resetting bucket properties to their default values. Bucket properties are stored in Riak's ring structure that is gossiped around the cluster. Resetting bucket properties for buckets that are no longer used or that are using the default properties can reduce the amount of gossiped data.

### Installation Notes

For RHEL/Centos/Fedora users, the RPM tools have added a dependency on `expect`, so if you see a message like this:

```
$ sudo rpm -i riak-1.3.0rc1-1.el5.x86_64.rpm
error: Failed dependencies:
    /usr/bin/expect is needed by riak-1.3.0rc1-1.x86_64
```

You can fix this issue by installing the Riak RPM with `yum` which will resolve any dependencies automatically:

```
$ sudo yum -y install riak-1.3.0rc1-1.el5.x86_64.rpm
Preparing...                ########################################### [100%]
   1:expect                 ########################################### [100%]
   2:riak                   ########################################### [100%]
```

### Issues / PR's Resolved

* riak: `ulimit -n` warning message bumped from 1024 to 4096
* riak/266: [Add libstdc++ library to LD_PRELOAD path to find proper symbols in SmartOS](https://github.com/basho/riak/issues/266)
* riak/261: [Add 'riak-admin aae_status', plus add AAE options to app.config](https://github.com/basho/riak/issues/261)
* riak/259: [remove legacy-mapred-only configs](https://github.com/basho/riak/issues/259)
* riak/253: [Properly exit on `/etc/init.d/riak status` command](https://github.com/basho/riak/issues/253)
* riak/251: [`riak stop` does not behave properly on BSD systems](https://github.com/basho/riak/issues/251)
* riak: [Set riak_sysmon's gc_ms_limit default value to zero](https://github.com/basho/riak/commit/065a2abf2ee3bbcd8da0fcf885f1fc8cd8f6327d)
* basho_stats/2: [update rebar to 2.0.0](https://github.com/basho/basho_stats/issues/2)
* bitcask/42: [Disable merges on startup to prevent high disk io with heavy requests](https://github.com/basho/bitcask/issues/42)
* bitcask/45: [remove arbitrary 120-char limit on log_needs_merge messages](https://github.com/basho/bitcask/issues/45)
* bitcask/46: [Support rebar binary in system](https://github.com/basho/bitcask/issues/46)
* bitcask/49: [update rebar to 2.0.0](https://github.com/basho/bitcask/issues/49)
* bitcask/54: [Adds "grace period" to stop just-written files from expiring.](https://github.com/basho/bitcask/issues/54)
* bitcask/55: [Change erlang:now() -> os:timestamp() when it is safe](https://github.com/basho/bitcask/issues/55)
* bitcask/56: [remove -author attributes from source](https://github.com/basho/bitcask/issues/56)
* bitcask/58: [merge process should write CRC to hintfiles](https://github.com/basho/bitcask/issues/58)
* bitcask/59: [Dss timeshift crc](https://github.com/basho/bitcask/issues/59)
* bitcask/65: [Iterator API](https://github.com/basho/bitcask/issues/65)
* bitcask/66: [Fix log spam introduced by branch 'gh62-badrecord-mstate'](https://github.com/basho/bitcask/issues/66)
* bitcask/67: [Add bitcask:is_empty_estimate](https://github.com/basho/bitcask/issues/67)
* bitcask/70: [Clear all Dialyzer warnings](https://github.com/basho/bitcask/issues/70)
* cluster_info/11: [Remove Luke usage](https://github.com/basho/cluster_info/issues/11)
* cluster_info/8: [update rebar to 2.0.0](https://github.com/basho/cluster_info/issues/8)
* ebloom/8: [update rebar to 2.0.0](https://github.com/basho/ebloom/issues/8)
* eleveldb/37: [raise bits per key from 10 to 16.  reduces false positive rate.](https://github.com/basho/eleveldb/issues/37)
* eleveldb/38: [updated to rebar 2.0.0](https://github.com/basho/eleveldb/issues/38)
* eleveldb/39: [Export iterator/3 so users can iterate over just keys](https://github.com/basho/eleveldb/issues/39)
* eleveldb/40: [make bloom2 the default bloom filter](https://github.com/basho/eleveldb/issues/40)
* eleveldb/42: [Add Erlang VM reduction count 'bumps' to all NIF calls](https://github.com/basho/eleveldb/issues/42)
* eleveldb/44: [Mv thread direct](https://github.com/basho/eleveldb/issues/44)
* eleveldb/45: [Jfw return value rodeo](https://github.com/basho/eleveldb/issues/45)
* eleveldb/48: [Jdb mv iterate5](https://github.com/basho/eleveldb/issues/48)
* eleveldb/49: [Mv unordered close](https://github.com/basho/eleveldb/issues/49)
* eleveldb/50: [Address race condition between queue asking for help and a single worker...](https://github.com/basho/eleveldb/issues/50)
* erlang_js/29: [update rebar to 2.0](https://github.com/basho/erlang_js/issues/29)
* erlang_js/30: [Dss fix ejslog](https://github.com/basho/erlang_js/issues/30)
* lager/53: [adding css for edocs; also adding edoc pointer to README](https://github.com/basho/lager/issues/53)
* lager/56: [Add support for a custom log truncation size compile time flag](https://github.com/basho/lager/issues/56)
* lager/67: [Added CRs to the LFs in lager_console_backend](https://github.com/basho/lager/issues/67)
* lager/68: [Direct the console logger output to user](https://github.com/basho/lager/issues/68)
* lager/69: [update rebar to 2.0.0](https://github.com/basho/lager/issues/69)
* lager/70: [Added lager:start() to the README](https://github.com/basho/lager/issues/70)
* lager/76: [Add informtion about loggly backend](https://github.com/basho/lager/issues/76)
* lager/77: [Use quickcheck to test for formatting equivalenve with io_lib](https://github.com/basho/lager/issues/77)
* lager_syslog/6: [Lager 2.0 support](https://github.com/basho/lager_syslog/issues/6)
* lager_syslog/7: [Use git:// rather than https:// so hosts don't need curl](https://github.com/basho/lager_syslog/issues/7)
* lager_syslog/8: [convert_level(?EMERGENCY) -> emergency. ](https://github.com/basho/lager_syslog/issues/8)
* merge_index/22: [update rebar to 2.0.0](https://github.com/basho/merge_index/issues/22)
* merge_index/25: [Change erlang:now() -> os:timestamp() when it is safe](https://github.com/basho/merge_index/issues/25)
* mochiweb/4: [erlang:now() -> os:timestamp](https://github.com/basho/mochiweb/issues/4)
* erlang_protobuffs/26: [Issue 25](https://github.com/basho/erlang_protobuffs/issues/26)
* erlang_protobuffs/34: [Fixed defaults for decodes to handle camelCase feilds.](https://github.com/basho/erlang_protobuffs/issues/34)
* erlang_protobuffs/36: [Fixed warnings about unused functions and variables](https://github.com/basho/erlang_protobuffs/issues/36)
* erlang_protobuffs/38: [Performance improvements](https://github.com/basho/erlang_protobuffs/issues/38)
* riak_api/10: [Add deregistration of services](https://github.com/basho/riak_api/issues/10)
* riak_api/12: [Restore stat mod registration](https://github.com/basho/riak_api/issues/12)
* riak_api/14: [Allow multiple replies to be sent in the middle of a streaming operation.](https://github.com/basho/riak_api/issues/14)
* riak_api/17: [Use riak_api_pb_sup active children count for pb active stat](https://github.com/basho/riak_api/issues/17)
* riak_api/18: [Use folsom's `gauge` type to store the function needed to pbcconnects ](https://github.com/basho/riak_api/issues/18)
* riak_api/19: [Performance improvements](https://github.com/basho/riak_api/issues/19)
* riak_control/26: [Typo fix (no such function - gen_server:cast/3)](https://github.com/basho/riak_control/issues/26)
* riak_control/27: [Re-export admin_ring:node_ring_details/2 back](https://github.com/basho/riak_control/issues/27)
* riak_control/37: [Convert to Ember.js.](https://github.com/basho/riak_control/issues/37)
* riak_control/38: [update rebar to 2.0.0](https://github.com/basho/riak_control/issues/38)
* riak_control/40: [Deprecate unused resources.](https://github.com/basho/riak_control/issues/40)
* riak_control/42: [Add new Riak Control theme.](https://github.com/basho/riak_control/issues/42)
* riak_control/44: [Do not require secure only cookie.](https://github.com/basho/riak_control/issues/44)
* riak_control/48: [Move formatting functions to riak_control_formatting.](https://github.com/basho/riak_control/issues/48)
* riak_core/137: [Change node to use claim_v1 when in legacy mode](https://github.com/basho/riak_core/issues/137)
* riak_core/188: [Eunit cleanups](https://github.com/basho/riak_core/issues/188)
* riak_core/195: [Change write_ringfile to create a temporary ring file, check and rename.](https://github.com/basho/riak_core/issues/195)
* riak_core/220: [update rebar to 2.0.0](https://github.com/basho/riak_core/issues/220)
* riak_core/223: [Change ticks from timer to more efficient erlang:send_after](https://github.com/basho/riak_core/issues/223)
* riak_core/224: [erlang:now() -> os:timestamp() in all the places it is safe](https://github.com/basho/riak_core/issues/224)
* riak_core/225: [remove -author attributes from source](https://github.com/basho/riak_core/issues/225)
* riak_core/230: [Remove publish_capabilities race](https://github.com/basho/riak_core/issues/230)
* riak_core/232: [Address high memory use by riak_core_sysmon_handler](https://github.com/basho/riak_core/issues/232)
* riak_core/235: [No open source license specified](https://github.com/basho/riak_core/issues/235)
* riak_core/236: [adding license file, closes #235](https://github.com/basho/riak_core/issues/236)
* riak_core/240: [health check system](https://github.com/basho/riak_core/issues/240)
* riak_core/246: [Vnode shutdown message severity should be info](https://github.com/basho/riak_core/issues/246)
* riak_core/249: [Allow gen_nb_server to use IPv6 addresses.](https://github.com/basho/riak_core/issues/249)
* riak_core/250: [Make vnode check for existing handoff before starting another](https://github.com/basho/riak_core/issues/250)
* riak_core/251: [rewriting revised readme in .md and removing .org version.](https://github.com/basho/riak_core/issues/251)
* riak_core/254: [Export path and stat_name types from riak_core_stat_q](https://github.com/basho/riak_core/issues/254)
* riak_core/255: [upgrade legacy ring only if needed](https://github.com/basho/riak_core/issues/255)
* riak_core/257: [Enable riak_core apps to provide a health_check callback](https://github.com/basho/riak_core/issues/257)
* riak_core/259: [Adjust riak_core_sup child order for cleaner shutdown](https://github.com/basho/riak_core/issues/259)
* riak_core/261: [Fix bug in riak_core_format:human_time + add test](https://github.com/basho/riak_core/issues/261)
* riak_core/262: [Add ability to selectively disable incoming/outgoing handoff](https://github.com/basho/riak_core/issues/262)
* riak_core/264: [Make vnode terminate backend for any exit reason](https://github.com/basho/riak_core/issues/264)
* riak_core/265: [Fix bug in riak_core_util:rpc_every_member_ann](https://github.com/basho/riak_core/issues/265)
* riak_core/268: [Fix riak_core_wm_urlmap](https://github.com/basho/riak_core/issues/268)
* riak_kv/290: [timeout/forward_preflist MapReduce error.](https://github.com/basho/riak_kv/issues/290)
* riak_kv/408: ["fitting was gone before startup" errors.](https://github.com/basho/riak_kv/pull/408)
* riak_kv/354: [Add PB service deregistration on stop.](https://github.com/basho/riak_kv/issues/354)
* riak_kv/360: [Fix eunit failures](https://github.com/basho/riak_kv/issues/360)
* riak_kv/366: [Spurious #pipe_log messages in logs as "Unrecognized message"](https://github.com/basho/riak_kv/issues/366)
* riak_kv/367: [riak 1.2rc1 - memory backend issue with 2i & $key/$bucket](https://github.com/basho/riak_kv/issues/367)
* riak_kv/379: [Resolve 2I timeout error from case clause](https://github.com/basho/riak_kv/issues/379)
* riak_kv/380: [Improper match on w_val_unsatisfied error in HTTP](https://github.com/basho/riak_kv/issues/380)
* riak_kv/382: [Use regular logging on Travis for now so we can actually read the output](https://github.com/basho/riak_kv/issues/382)
* riak_kv/390: [erlang:now() -> os:timestamp() in all the places it is safe](https://github.com/basho/riak_kv/issues/390)
* riak_kv/395: [Add retry on eleveldb lock errors during open for up to 1 minute.](https://github.com/basho/riak_kv/issues/395)
* riak_kv/399: [2I backpressure](https://github.com/basho/riak_kv/issues/399)
* riak_kv/401: [Quick fix to prevent mapred_test from hanging](https://github.com/basho/riak_kv/issues/401)
* riak_kv/404: [Spawn remote vnodes using start_link rather than start.](https://github.com/basho/riak_kv/issues/404)
* riak_kv/405: [Randoming kv_put forwardee node](https://github.com/basho/riak_kv/issues/405)
* riak_kv/406: [Make riak_client work on non-riak nodes after get/put FSM startup change](https://github.com/basho/riak_kv/issues/406)
* riak_kv/415: [Fix stats for r15b02](https://github.com/basho/riak_kv/issues/415)
* riak_kv/419: [add knob to disable referer check](https://github.com/basho/riak_kv/issues/419)
* riak_kv/423: [Riak KV vnodes can block in certain scenarios when using Bitcask](https://github.com/basho/riak_kv/issues/423)
* riak_kv/424: [Change riak_kv_bitcask_backend to use bitcask:is_empty_estimate](https://github.com/basho/riak_kv/issues/424)
* riak_kv/426: [Since "Try again" is not relevant to a self-join, make that error explicit](https://github.com/basho/riak_kv/issues/426)
* riak_kv/429: [Apply backpressure from the MR sink](https://github.com/basho/riak_kv/issues/429)
* riak_kv/433: [Remove Luke usage](https://github.com/basho/riak_kv/issues/433)
* riak_kv/435: [remove the unused 'mget' command from riak_kv_vnode](https://github.com/basho/riak_kv/issues/435)
* riak_kv/438: [get put stats -> 1.2-perf](https://github.com/basho/riak_kv/issues/438)
* riak_kv/439: [Fixing Dialyzer Complaints Near MapReduce Code](https://github.com/basho/riak_kv/issues/439)
* riak_kv/440: [Re-instate code from #415 that was lost in later changes](https://github.com/basho/riak_kv/issues/440)
* riak_kv/442: [Merge 1.2-perf into master](https://github.com/basho/riak_kv/issues/442)
* riak_kv/447: [Add basic health check to Riak KV](https://github.com/basho/riak_kv/issues/447)
* riak_kv/449: [Update stats in process](https://github.com/basho/riak_kv/issues/449)
* riak_kv/451: [Change AAE hashtree to buffer and batch write to LevelDB](https://github.com/basho/riak_kv/issues/451)
* riak_kv/453: [Avoid a badarg in crypto:rand_uniform when N=1](https://github.com/basho/riak_kv/issues/453)
* riak_kv/454: [Try to make KV shutdown cleaner](https://github.com/basho/riak_kv/issues/454)
* riak_kv/456: [Add AAE status subsystem + finalize AAE for Riak 1.3 release](https://github.com/basho/riak_kv/issues/456)
* riak_kv/457: [Correct usage of capabilities API in riak_kv_pb_object.](https://github.com/basho/riak_kv/issues/457)
* riak_kv/458: [Make index_hashtree process exit when related vnode exits](https://github.com/basho/riak_kv/issues/458)
* riak_kv/459: [Fix health check code to handle dead pids](https://github.com/basho/riak_kv/issues/459)
* riak_kv/460: [Fix AAE exchange bug for the N=1 case](https://github.com/basho/riak_kv/issues/460)
* riak_pb/15: [Maven build](https://github.com/basho/riak_pb/issues/15)
* riak_pb/18: [Add proto_cmd to MANIFEST.in](https://github.com/basho/riak_pb/issues/18)
* riak_pb/19: [Add OSGi Manifest headers to riak-pb jar file](https://github.com/basho/riak_pb/issues/19)
* riak_pb/25: [Java POM changes for OSGI](https://github.com/basho/riak_pb/issues/25)
* riak_pb/26: [Fix bug with protobuffs encoding tests](https://github.com/basho/riak_pb/issues/26)
* riak_pb/27: [Add protobuf to install_requires.](https://github.com/basho/riak_pb/issues/27)
* riak_pb/29: [Pin python package version and bump it.](https://github.com/basho/riak_pb/issues/29)
* riak_pipe/52: [update to rebar 2.0.0](https://github.com/basho/riak_pipe/issues/52)
* riak_pipe/53: [erlang:now() -> os:timestamp()](https://github.com/basho/riak_pipe/issues/53)
* riak_pipe/59: ["Sink type" that can provide backpressure](https://github.com/basho/riak_pipe/issues/59)
* riak_pipe/61: [Clean up some dialyzer warnings](https://github.com/basho/riak_pipe/issues/61)
* riak_pipe/65: [Quickchecking riak_pipe_fitting](https://github.com/basho/riak_pipe/issues/65)
* riak_search/116: [Add PB service deregistration on stop.](https://github.com/basho/riak_search/issues/116)
* riak_search/127: [update rebar to 2.0.0](https://github.com/basho/riak_search/issues/127)
* riak_search/128: [Fix mis-reporting of fl=ID + sort field on PBC.](https://github.com/basho/riak_search/issues/128)
* riak_search/129: [erlang:now() -> os:timestamp() when it is safe](https://github.com/basho/riak_search/issues/129)
* riak_search/133: [Remove Luke usage](https://github.com/basho/riak_search/issues/133)
* riak_sysmon/7: [update rebar to 2.0.0](https://github.com/basho/riak_sysmon/issues/7)
* riaknostic/39: [Added some reassuring output.](https://github.com/basho/riaknostic/issues/39)
* riaknostic/41: [added a first pass at machine-readable output](https://github.com/basho/riaknostic/issues/41)
* riaknostic/50: [Export command](https://github.com/basho/riaknostic/issues/50)
* riaknostic/51: [Pevm sysctl checks](https://github.com/basho/riaknostic/issues/51)
* riaknostic/52: [Update README.md](https://github.com/basho/riaknostic/issues/52)
* webmachine/101: [pass method,scheme,http vsn into rewrite](https://github.com/basho/webmachine/issues/101)
* webmachine/102: [Update demo app: rebar deps, ensure inets is running, improve README](https://github.com/basho/webmachine/issues/102)
* webmachine/106: [Store the dispatch_list in ETS, not application:set_env](https://github.com/basho/webmachine/issues/106)
* webmachine/113: [Fix setup/teardown for etag EQC test](https://github.com/basho/webmachine/issues/113)
* webmachine/56: [Strip whitespace from content-type (and others) parsed by webmachine_uti...](https://github.com/basho/webmachine/issues/56)
* webmachine/65: [swap - to _ for app name.](https://github.com/basho/webmachine/issues/65)
* webmachine/73: [Fix for trailing CRLF ](https://github.com/basho/webmachine/issues/73)
* webmachine/75: [Exposed get_routes() to the public.](https://github.com/basho/webmachine/issues/75)
* webmachine/77: [Incorrect accept header in some blackberry devices.](https://github.com/basho/webmachine/issues/77)
* webmachine/81: [remove code:clash call](https://github.com/basho/webmachine/issues/81)
* webmachine/83: [update rebar to 2.0.0](https://github.com/basho/webmachine/issues/83)
* webmachine/86: [Bump WMVSN to 1.9.2.](https://github.com/basho/webmachine/issues/86)
* webmachine/93: [change parameterized modules to regular modules](https://github.com/basho/webmachine/issues/93)
* webmachine/97: [Header Rewriting](https://github.com/basho/webmachine/issues/97)
