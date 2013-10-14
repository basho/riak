# Riak 1.4.2 Release Notes

This is a bugfix release on the Riak 1.4.x series.

* Fixed various problems related to crashing stats
* Fixed extra noisy logs introduced in 1.4.1 (Not found errors and others)
* Fixed issues related to 2i queries timing out
* Added more protection against corrupt data in backends
* Fixed incorrect capability negotiation causing nodes to appear incompatible in Riak Control

## Issues / PR's Resolved

* leveldb/89: [Minor adjustments to throttle](https://github.com/basho/leveldb/wiki/mv-level-work4)
* node_package/77: [FreeBSD needs to scan lib and etc directories for extra files](https://github.com/basho/node_package/issues/77)
* node_package/82: [Deb: Deb postinst script attempts to chmod's /etc directory](https://github.com/basho/node_package/issues/82)
* node_package/83: [FreeBSD package incorrectly packaged as .tgz rather than .tbz](https://github.com/basho/node_package/issues/83)
* riak_control/132: [Resolve incorrect capability negotation order in 1.4](https://github.com/basho/riak_control/pull/132)
* riak_control/133: [Use expanded record macro.](https://github.com/basho/riak_control/pull/133)
* riak_control/135: [Incompatible is less serious](https://github.com/basho/riak_control/pull/135)
* riak_kv/644: [Fix webmachine 2i timeout](https://github.com/basho/riak_kv/pull/644)
* riak_kv/639: [Fix HTTP MR error reporting](https://github.com/basho/riak_kv/commit/01190f23099bf7febbb69d74f6b4922a91d045e3)
* riak_kv/636: [Fix riak_kv_stat crash leaking processes](https://github.com/basho/riak_kv/pull/636)
* riak_kv/638: [Bad deserialization and CRC errors to not_found](https://github.com/basho/riak_kv/pull/638)
* riak_kv/641: [LevelDB fold hardening for 1.4](https://github.com/basho/riak_kv/pull/641)
* riak_kv/635: [Fix 2i timeout responses](https://github.com/basho/riak_kv/pull/635)
* riak_kv/632: [Fix riak_kv_stat timeout](https://github.com/basho/riak_kv/pull/632)
* riak_core/356: [Add protection against folsom stat errors](https://github.com/basho/riak_core/pull/356)
* riak_core/359: [Support for corruption detecting during handoff](https://github.com/basho/riak_core/pull/359)
* webmachine/137: [Increase line coverage of Webmachine unit and integration tests](https://github.com/basho/webmachine/pull/137)
* webmachine/164: [Rework Webmachine error logging to use the built-in log handler](https://github.com/basho/webmachine/pull/164)
* webmachine/161: [Expose local socket via the Webmachine API](https://github.com/basho/webmachine/pull/161)
* webmachine/160: [Re-add compatibility for Erlang versions newer than R15B01](https://github.com/basho/webmachine/pull/160)
* webmachine/158: [Avoid localtime to universaltime](https://github.com/basho/webmachine/pull/158)
* webmachine/156: [Fix Erlang R15 compatibility](https://github.com/basho/webmachine/pull/156)


# Riak 1.4.1 Release Notes

This is a bugfix release.  The major fixes are to the Secondary Index,
Riak Control, and LevelDB subsystems.

* Pagination for equality queries is fixed by [riak_kv/615](https://github.com/basho/riak_kv/pull/615).

* The ability to set a timeout on a 2i query has been added by [riak_kv/616](https://github.com/basho/riak_kv/pull/616).

* Using 2i as input for a map-reduce job has been fixed by [riak_kv/618](https://github.com/basho/riak_kv/pull/618).

* Riak Control can crash its host node when in a mixed-cluster
  environment containing a 1.4.0 node.  This has been addressed by [riak_control/120](https://github.com/basho/riak_control/pull/120).

* Basho's leveldb fork has added better fadvise support and fixed some
  race conditions in the write path.  See [leveldb/88](https://github.com/basho/leveldb/pull/88).

## Issues / PR's Resolved

* riak_core/351: [Fix catch pattern to match all errors](https://github.com/basho/riak_core/pull/351)
* riak_core/352: [Fix TCP mon to correctly spot nodes coming up](https://github.com/basho/riak_core/pull/352)
* riak_kv/615: [Do not set the start_term to the last seen key for eq (2i)](https://github.com/basho/riak_kv/pull/615)
* riak_kv/616: [Add millisecond timeout parameter to API 2i endpoints](https://github.com/basho/riak_kv/pull/616)
* riak_kv/618: [Strip index term from result before passing to MR (2i)](https://github.com/basho/riak_kv/pull/618)
* riak-erlang-client/108: [Add timeouts to 2i queries](https://github.com/basho/riak-erlang-client/pull/108)
* riak_pb/50: [Add timeout field to 2i messages](https://github.com/basho/riak_pb/pull/50)
* riak_control/120: [Handle incompatible records between the 1.3 and 1.4 release](https://github.com/basho/riak_control/pull/120)
* leveldb/88: [More effective fadvise calls + fix write race conditions](https://github.com/basho/leveldb/pull/88)
* node_package/75: [In RPMs: Do not error on post install script if usermod fails](https://github.com/basho/node_package/pull/75)
* node_package/76: [Fix `riak version` for RPM packages](https://github.com/basho/node_package/pull/76)

# Riak 1.4.0 Release Notes

## Major Features / Improvements

### Improved Binary Format

Data stored in Riak can now be represented in a more compact
format. The new format reduces storage overhead, especially in the
case of small objects or those with large bucket names, keys, or
metadata.

By default new Riak clusters, starting with Riak 1.4, will have
the new format enabled by default. Users upgrading to Riak 1.4 should
first perform the upgrade and once happy with the operation enable the
new format. Riak supports both the old and new representation
simultaneously, so no additional upgrade process is necessary.

Which representation is used can be configured by setting
`object_format` to either `v0` or `v1` in the `riak_kv` section of
`app.config`. `v1` is the new format.

The new format is also used during handoff if the cluster supports it.

For users who upgrade to Riak 1.4 and enable the new format,
downgrading to a previous version requires reformatting any data
written in the new representation (previous version of Riak won't
understand it). A utility is provided via `riak-admin` to perform this
operation:

```
riak-admin downgrade-objects <kill-handoffs> [<concurrency>]
```

The utility should be run per-node immediately prior to downgrading
it. `<kill-handoffs>` must be either `true` or `false`. If `false` any
ongoing handoff will be waited on before performing the
reformat. Otherwise, all in-flight handoff, inbound to the node or
outbound from it, will be killed. During and after the reformat the
`transfer-limit` will be set to 0. The optional `<concurrency>`
argument must be an integer greater than zero. It determines how many
partitions are reformatted on the node concurrently. By default the
concurrency is two. Additionally, in anticipation that the entire
cluster will be downgraded `downgrade-objects` sets the preferred
format to `v0`. `downgrade-objects` can be run multiple times in the
case of error or if the node crashes.


### Changed behavior of `riak attach`

If you are a frequent user of `riak attach` it is worth noting that the behavior has changed in 1.4.  `riak attach` used to attach to a named pipe that erlang provides to talk to running erlang nodes. This is great except that an accidental Ctrl-C would not only kill your session, but also kill the running node.  The behavior has now changed to use `-remsh` (remote shell) to connect to the node.  This method is safer because a Ctrl-C will not kill a running node.  In cases where distributed erlang having problems for some reason and a -remsh is not wanted, `riak attach-direct` is a new command which uses the old pipe behavior of `riak attach`.

### `riak-admin transfers` Improvements

The output of `riak-admin transfers` now includes per-transfer
progress reporting and improved display of long node names.

Whether or not progress is reported and how the progress is calculated is
dependent on the cluster's backend. Progress reporting is enabled for
`riak_kv_bitcask_backend`, `riak_kv_eleveldb_backend` and
`riak_kv_memory_backend`. Clusters using `riak_kv_multi_backend` will
not have progress reporting enabled. When using `riak_kv_bitcask_backend` or
`riak_kv_memory_backend` progress is determined by the number of keys
already transferred out of the total number stored. Large variance in
value sizes can skew reporting. For `riak_kv_eleveldb_backend`
progress is measured in stored bytes. The total number of bytes used
may be an overestimate -- meaning progress will always be what is
reported or further along than reported in the worst case.

### Lager Upgrade 1.2.2 to 2.0.0

Lager has been updated in Riak from Lager 1.2.2 that was in the Riak 1.3.x series to Lager 2.0.0.  Please see the lager documentation at https://github.com/basho/lager for the new capabilities in Lager 2.0.


### Querying

#### Pagination Support in 2i

We've extended Riak's Secondary Indexing (2i) interface to allow for paginated results. This is done via the `max_results` option in both the Protocol Buffers and HTTP 2i end points. Full details can be found [here](https://github.com/basho/riak_kv/pull/540).


### Client APIs

#### Client-specified timeouts

Clients can now specify a timeout value, in milliseconds, that will
override the default internal timeout on requests that manipulate
objects (fetch, store, delete), list buckets, or list keys.

#### Protocol Buffers bucket properties

The Protocol Buffers interface now supports all known bucket
properties, and the ability to "reset" bucket properties to their
defaults.

#### List-buckets streaming

Similar to listing keys, listing buckets can be streamed to clients.
This means that Riak will send bucket names to the client as they are
received, rather than waiting for the request to complete on all
nodes.

#### Protocol buffers binds to multiple interfaces

Similar to HTTP, Protocol Buffers will now bind to multiple interfaces
and ports. Existing configurations will change the previous `pb_port`
and `pb_ip` settings to the singular `pb` setting, which is a list of
IP/port pairs.


### Data Types

#### PN-Counters

1.4 sees the addition of Riak's first data type: PN-Counters. A PN-Counter is capable of being both incremented (P) and decremented (N). The full details are [here](https://github.com/basho/riak_kv/pull/536). We're also fast at work on a [CRDT Cookbook](https://github.com/lenary/riak_crdt_cookbook) that will demonstrate this and future data types in Riak.


### Riak Control

Riak Control now has an improved cluster management interface, and standalone node management interface, for staging and committing changes to the cluster, which mimics the CLI API.


### New command: riak-debug

The command `riak-debug` is a shell script provided to aid in the automation of gathering information from Riak nodes for troubleshooting. Information gathered includes operating system command output, Riak command output, Riak configuration files, and Riak logs. See `riak-debug -h` and `man riak-debug` for more information on using the script and for tips on integrating its usage into your workflow.


### Packaging / Runtime changes

Riak 1.4 took a major step forward in how it is packaged by changing
over to using [node_package](http://github.com/basho/node_package) for
its packaging.  This is the same tool used for RiakCS since its first
release.  This commonality will improve overall feature parity and
stability of the packages themselves by cutting down on the number of
places packaging bug fixes need to happen.  See the 'node_package'
section in the Issues section for all the bug fixes to packaging in
this release.

##### Platforms Added / Removed

Support for Debian Wheezy and SmartOS 13.1 have been added to 1.4.  As
planned, support for 32bit packages has been dropped.

##### Major changes in packages and runtime

   * init.d scripts for Deb and RPM systems have been rewritten to
     comply with the standards of those distributions.  In particular
     the init scripts now actually return nonzero exit codes on
     failure.  This was a major issue we had that prevented tools from
     working seamlessly.
   * All start, stop, and status commands use return codes rather than
     reading stdout.  This has been a major 'technical debt' we've had
     for a long time and it is about time the rest of it is finally
     fixed.
   * `riak.pid` files are now created and removed on `riak
     start/stop`. This allows other tools to take advantage of .pid
     files without them needing knowledge about the riak script or
     nodetool.
   * Warnings added to `riak attach` and `riak attach-direct` to let
     users know about implications of q() and CTRL-C
   * The `riak` script now makes it more obvious which commands need
     to be run as the Riak user (or root user).  Status commands like
     `getpid` or `ping` can be run by any user while daemon commands
     like `start` and `stop` will error in a more graceful if not run
     by the Riak user.



## Issues / PR's Resolved

* bear/1: [Remove native flag and add kernel,stdlib to app deps](https://github.com/basho/bear/pull/1)
* bitcask/89: [add dialyzer targets](https://github.com/basho/bitcask/issues/89)
* bitcask/92: [Fix merge logging bug introduced by bs-merge-expiration-change branch](https://github.com/basho/bitcask/issues/92)
* folsom/2: [Improve performance of slide histogram](https://github.com/basho/folsom/issues/2)
* leveldb/73: [level work1](https://github.com/basho/leveldb/issues/73)
* leveldb/74: [Add status query for total bytes used by a LevelDB instance](https://github.com/basho/leveldb/issues/74)
* leveldb/75: [Merge of Google 1.6, 1.7, 1.8, and 1.9 releases](https://github.com/basho/leveldb/issues/75)
* leveldb/78: [Repair updated for edge case created with new directory structure.](https://github.com/basho/leveldb/issues/78)
* leveldb/79: [filecache tuning2](https://github.com/basho/leveldb/issues/79)
* leveldb/81: [bloom size limit](https://github.com/basho/leveldb/issues/81)
* leveldb/84: [level work3, change from 3 overlapped levels to 2](https://github.com/basho/leveldb/issues/84)
* merge_index/30: [Remove delayed_write option](https://github.com/basho/merge_index/pull/30)
* mochiweb/7: [Range header fix](https://github.com/basho/mochiweb/issues/7)
* mochiweb/8: [Remove parameterized modules.](https://github.com/basho/mochiweb/issues/8)
* node_package/40: [init script returns success even if riak does not start](https://github.com/basho/node_package/issues/40)
* node_package/43: [Add SRPMS and make RPM version field fully compatible](https://github.com/basho/node_package/issues/43)
* node_package/44: [Convert RPM init script to fall in line with Redhat style](https://github.com/basho/node_package/issues/44)
* node_package/47: [Add app_epath.sh, a POSIX app.config parsing utility.](https://github.com/basho/node_package/issues/47)
* node_package/49: [Create .pid files for package builds](https://github.com/basho/node_package/issues/49)
* node_package/50: [RPM %files changes behavior on Fedora 18](https://github.com/basho/node_package/issues/50)
* node_package/51: [Return nonzero exit codes on init function failure](https://github.com/basho/node_package/issues/51)
* node_package/54: [Fix %files section to not claim ownership of bindir and mandir](https://github.com/basho/node_package/issues/54)
* node_package/55: [Investigate shipping configuration to increase open files ulimit](https://github.com/basho/node_package/issues/55)
* node_package/56: [Name SunOS packages based on erlang architecture rather than uname](https://github.com/basho/node_package/issues/56)
* node_package/57: [Base architecture naming on erlc arch](https://github.com/basho/node_package/issues/57)
* node_package/60: [Add support for SmartOS 13.1](https://github.com/basho/node_package/issues/60)
* node_package/61: [add simple warnings on attach/attach-direct](https://github.com/basho/node_package/issues/61)
* node_package/63: [remove contract specification from SMF manifests (solaris)](https://github.com/basho/node_package/issues/63)
* node_package/65: [Create patches for SmartOS packages to handle differing behavior](https://github.com/basho/node_package/issues/65)
* erlang_protobuffs/41: [Fix some README example problems, callout deep lists change.](https://github.com/basho/erlang_protobuffs/issues/41)
* erlang_protobuffs/42: [Cleanup warnings](https://github.com/basho/erlang_protobuffs/issues/42)
* erlang_protobuffs/45: [Be more firewall-friendly :-)](https://github.com/basho/erlang_protobuffs/issues/45)
* erlang_protobuffs/46: [Fix parsing hex values](https://github.com/basho/erlang_protobuffs/issues/46)
* erlang_protobuffs/47: [fix compiler warnings about shadowed variables](https://github.com/basho/erlang_protobuffs/issues/47)
* erlang_protobuffs/49: [Fix enums when using packages](https://github.com/basho/erlang_protobuffs/issues/49)
* erlang_protobuffs/51: [Remove O(N^2) algorithm from repeated field extraction.](https://github.com/basho/erlang_protobuffs/issues/51)
* riak/254: [Changed `riak attach` to use a remsh](https://github.com/basho/riak/issues/254)
* riak/268: [Switch riak to use node_package for packaging](https://github.com/basho/riak/issues/268)
* riak/272: [Add new rebar binary and erlydtl opts info toplevel rebar.config for solving dialyzer glitches](https://github.com/basho/riak/issues/272)
* riak/283: [We insist on a minimum of 5 nodes in a cluster, adjust devrel](https://github.com/basho/riak/issues/283)
* riak/286: [move and clarify ulimit check](https://github.com/basho/riak/issues/286)
* riak/288: [remove embedded option ](https://github.com/basho/riak/issues/288)
* riak/290: [Support multiple PB listeners](https://github.com/basho/riak/issues/290)
* riak/294: [Add missing rm -rf dev/$@/lib/riaknostic on dev target](https://github.com/basho/riak/issues/294)
* riak/303: [update riak-admin transfers](https://github.com/basho/riak/issues/303)
* riak/310: [Remove incorrect `-embedded` flag from riak startup command](https://github.com/basho/riak/issues/310)
* riak/311: [add "cluster resize-ring <new-size>" to riak-admin](https://github.com/basho/riak/issues/311)
* riak/322: [Add riak-debug, a command for automating the collection of information for diagnosing problems.](https://github.com/basho/riak/issues/322)
* riak/329: [Riaknostic no longer escript. Alter build process accordingly.](https://github.com/basho/riak/issues/329)
* riak/331: [Lower net_ticktime to check for aliveness more often](https://github.com/basho/riak/issues/331)
* riak/339: [update vm.args for moving to OTP team scheduler patch](https://github.com/basho/riak/issues/339)
* riak/341: [update app.config to activate v1 object format on new installs](https://github.com/basho/riak/issues/341)
* riak/345: [Unable to build Riak from source tarball while offline](https://github.com/basho/riak/issues/345)
* riak_api/21: [Move setting/fetching bucket properties out of riak_kv](https://github.com/basho/riak_api/issues/21)
* riak_api/22: [Use init:script_id() for the server version.](https://github.com/basho/riak_api/issues/22)
* riak_api/23: [Enable multiple PB listeners.](https://github.com/basho/riak_api/issues/23)
* riak_api/24: [Add support for resetting bucket properties. Requires basho/riak_pb#35.](https://github.com/basho/riak_api/issues/24)
* riak_api/25: [Remove lager dependency because it is specified by riak_core.](https://github.com/basho/riak_api/issues/25)
* riak_api/28: [No PB listeners leads to repeated log messages concerning a failed stat calculation](https://github.com/basho/riak_api/issues/28)
* riak_control/54: [Fixes to get dialyzer working.](https://github.com/basho/riak_control/issues/54)
* riak_control/59: [Series of dialyzer and formatting changes.](https://github.com/basho/riak_control/issues/59)
* riak_control/71: [Add cluster management.](https://github.com/basho/riak_control/issues/71)
* riak_control/80: [Add ability to stop and down nodes.](https://github.com/basho/riak_control/issues/80)
* riak_control/81: [Normalize resource names.](https://github.com/basho/riak_control/issues/81)
* riak_control/83: [Make join node more explicit.](https://github.com/basho/riak_control/issues/83)
* riak_control/88: [Provide a default selection.](https://github.com/basho/riak_control/issues/88)
* riak_control/111: [Prevent badarith when memory is unavailable.](https://github.com/basho/riak_control/pull/111)
* riak_core/185: [inbound handoffs never cleanup](https://github.com/basho/riak_core/issues/185)
* riak_core/241: [potential fix for #185](https://github.com/basho/riak_core/issues/241)
* riak_core/270: [Dialyzer Fixes](https://github.com/basho/riak_core/issues/270)
* riak_core/274: [Allow parallel vnode initialization](https://github.com/basho/riak_core/issues/274)
* riak_core/282: [Extract out and export pending claim function.](https://github.com/basho/riak_core/issues/282)
* riak_core/284: [initial add of the Riak Core Connection Manager](https://github.com/basho/riak_core/issues/284)
* riak_core/290: [Add support for tracking progress of individual handoffs](https://github.com/basho/riak_core/issues/290)
* riak_core/291: [SSL support](https://github.com/basho/riak_core/issues/291)
* riak_core/297: [don't use hardcoded app names in SSL utils](https://github.com/basho/riak_core/issues/297)
* riak_core/298: [Race in vnode worker pool](https://github.com/basho/riak_core/issues/298)
* riak_core/299: [Vnode nonblocking reply, First draft (3rd edition), ready for some review](https://github.com/basho/riak_core/issues/299)
* riak_core/300: [Fix worker pool races](https://github.com/basho/riak_core/issues/300)
* riak_core/301: [Ring Resizing](https://github.com/basho/riak_core/issues/301)
* riak_core/302: [rework coverage fsm timeouts.](https://github.com/basho/riak_core/issues/302)
* riak_core/305: [Support for `plan/2` and `process_results/3` funs for coverage fsm](https://github.com/basho/riak_core/issues/305)
* riak_core/312: [Enhance transfer display + wrapping nodenames.](https://github.com/basho/riak_core/issues/312)
* riak_core/313: [format _stat_ts in connection manager](https://github.com/basho/riak_core/issues/313)
* riak_core/316: [handoff batching](https://github.com/basho/riak_core/issues/316)
* riak_core/319: [Optimize to better handle large rings/nodes](https://github.com/basho/riak_core/issues/319)
* riak_core/321: [proper return value for riak_core_console:transfers/1](https://github.com/basho/riak_core/issues/321)
* riak_core/322: [Handle node up/down in tcp_mon](https://github.com/basho/riak_core/issues/322)
* riak_core/323: [Permanently disable legacy gossip](https://github.com/basho/riak_core/issues/323)
* riak_core/325: [Fix a typo in tcp_mon init](https://github.com/basho/riak_core/issues/325)
* riak_core/328: [Fix overload test time outs](https://github.com/basho/riak_core/issues/328)
* riak_core/330: [dont start coverage timeout timer if timeout is infinite](https://github.com/basho/riak_core/issues/330)
* riak_core/331: [fix forced_ownership_handoff during resize](https://github.com/basho/riak_core/issues/331)
* riak_core/332: [update bad value protection for timer value](https://github.com/basho/riak_core/issues/332)
* riak_core/334: [Reporting 'normal' events is spammy, don't do it](https://github.com/basho/riak_core/issues/334)
* riak_core/336: [Fix crashing stat mod never getting rescheduled](https://github.com/basho/riak_core/issues/336)
* riak_core/339: [Fix repair handoff crash, missing not sent fun](https://github.com/basho/riak_core/issues/339)
* riak_core/340: [only silently drop DOWN-normal messages in deleted modstate](https://github.com/basho/riak_core/issues/340)
* riak_kv/30: [Bz982 - js_reload not working](https://github.com/basho/riak_kv/issues/30)
* riak_kv/31: [Key count reduce function](https://github.com/basho/riak_kv/issues/31)
* riak_kv/334: [Every read triggers a read-repair when Last-write-wins=true](https://github.com/basho/riak_kv/issues/334)
* riak_kv/385: [Objects cannot be updated if a bad CRC is encountered by Bitcask](https://github.com/basho/riak_kv/issues/385)
* riak_kv/462: [Expose FSM timeouts via the HTTP API](https://github.com/basho/riak_kv/issues/462)
* riak_kv/467: [add stats for coverage query starts](https://github.com/basho/riak_kv/issues/467)
* riak_kv/479: [More Compact Riak Object Binary Format](https://github.com/basho/riak_kv/issues/479)
* riak_kv/487: [provide a Location header for the same api version on POST](https://github.com/basho/riak_kv/issues/487)
* riak_kv/488: [Move setting/fetching bucket properties to riak_api](https://github.com/basho/riak_kv/issues/488)
* riak_kv/489: [Migrate mapred_test to riak_test](https://github.com/basho/riak_kv/issues/489)
* riak_kv/491: [Remove Link headers from bucket and key lists](https://github.com/basho/riak_kv/issues/491)
* riak_kv/492: [Make hashtree_eqc close trees before destroy.](https://github.com/basho/riak_kv/issues/492)
* riak_kv/495: [Add encoding capability for handoff.](https://github.com/basho/riak_kv/issues/495)
* riak_kv/496: [Standardize KV backend responses and handling](https://github.com/basho/riak_kv/issues/496)
* riak_kv/498: [Document the environment in the logs; advise on bad settings.](https://github.com/basho/riak_kv/issues/498)
* riak_kv/500: [vclock capability](https://github.com/basho/riak_kv/issues/500)
* riak_kv/510: [Remove merge_index dependency and unused erl_first_file.](https://github.com/basho/riak_kv/issues/510)
* riak_kv/512: [Protocol Buffers interface allows the creation of records with an empty key](https://github.com/basho/riak_kv/issues/512)
* riak_kv/520: [Adds X-Riak-Deleted where missing](https://github.com/basho/riak_kv/issues/520)
* riak_kv/521: [Changes needed to expose FSM timeouts to clients](https://github.com/basho/riak_kv/issues/521)
* riak_kv/526: [Expose Backend Size to Handoff for Progress Tracking](https://github.com/basho/riak_kv/issues/526)
* riak_kv/527: [List buckets timeout & streaming](https://github.com/basho/riak_kv/issues/527)
* riak_kv/529: [Count async MR results against the sink buffer size cap](https://github.com/basho/riak_kv/issues/529)
* riak_kv/530: [Ring Resizing Support](https://github.com/basho/riak_kv/issues/530)
* riak_kv/532: [kv_wm_utils expects ?MD_DELETED to be "true" not 'true'](https://github.com/basho/riak_kv/issues/532)
* riak_kv/536: [A simple way to store a PN-Counter in a riak_object](https://github.com/basho/riak_kv/issues/536)
* riak_kv/542: [add license header to riak_kv reformat](https://github.com/basho/riak_kv/issues/542)
* riak_kv/546: [Expose the put_fsm 'asis' option to clients](https://github.com/basho/riak_kv/issues/546)
* riak_kv/552: [Add new backend capability for Riak r_object use](https://github.com/basho/riak_kv/issues/552)
* riak_kv/554: [Optimize to better handle large rings/nodes](https://github.com/basho/riak_kv/issues/554)
* riak_kv/555: [Add init/final to AAE remote interface](https://github.com/basho/riak_kv/issues/555)
* riak_kv/559: [Alter env recommendations for 1.4](https://github.com/basho/riak_kv/issues/559)
* riak_kv/560: [Fix regression in 2i reformat status flag & add extra status function](https://github.com/basho/riak_kv/issues/560)
* riak_kv/562: [use old object format by default on upgrade](https://github.com/basho/riak_kv/issues/562)
* riak_kv/563: [Add binary format for counters](https://github.com/basho/riak_kv/issues/563)
* riak_kv/569: [Make sure client supplied N <= bucket N](https://github.com/basho/riak_kv/issues/569)
* riak_kv/576: [Add a capability for counters](https://github.com/basho/riak_kv/issues/576)
* riak_kv/579: [Skip start {val, key} pair if start_inclusive is false](https://github.com/basho/riak_kv/issues/579)
* riak_kv/581: [Wire up sidejob stats to /stats endpoint](https://github.com/basho/riak_kv/issues/581)
* riak_kv/585: [minor improvements to riak_object downgrade support](https://github.com/basho/riak_kv/issues/585)
* riak_kv/586: [Stop fold when a vnode reaches page size](https://github.com/basho/riak_kv/issues/586)
* riak_kv/587: [Fix incorrect arg in call to get_primary_apl/3 by put FSM](https://github.com/basho/riak_kv/issues/587)
* riak_kv/588: [Multi backend was missing data_size function](https://github.com/basho/riak_kv/issues/588)
* riak_pb/30: [Add remaining bucket properties to PBC](https://github.com/basho/riak_pb/issues/30)
* riak_pb/31: [Fix errors with repl bucket property.](https://github.com/basho/riak_pb/issues/31)
* riak_pb/32: [precommit/postcommit empty does not clear](https://github.com/basho/riak_pb/issues/32)
* riak_pb/33: [Fix commit hooks and symbolic properties as binaries](https://github.com/basho/riak_pb/issues/33)
* riak_pb/35: [Support reset bucket properties feature.](https://github.com/basho/riak_pb/issues/35)
* riak_pb/36: [Protoc dependency free version for Python Package Index](https://github.com/basho/riak_pb/issues/36)
* riak_pb/38: [Add timeouts to get, put, and delete](https://github.com/basho/riak_pb/issues/38)
* riak_pb/41: [Add messages for exporter tool & list timeouts](https://github.com/basho/riak_pb/issues/41)
* riak_pb/42: [Remove need to have protoc available in Python source package. Closes #36](https://github.com/basho/riak_pb/issues/42)
* riak_pb/43: [2i pagination support](https://github.com/basho/riak_pb/issues/43)
* riak_pb/44: [Add asis flag for RpbPutReq.](https://github.com/basho/riak_pb/issues/44)
* riak_pipe/50: [fitting was done before startup](https://github.com/basho/riak_pipe/issues/50)
* riak_pipe/62: [Move eunit system tests to riak_test](https://github.com/basho/riak_pipe/issues/62)
* riak_pipe/68: [assume handoff if vnode exits 'normal' during queue requeuest](https://github.com/basho/riak_pipe/issues/68)
* riak_pipe/71: [Fix opaque type warnings on R16B.](https://github.com/basho/riak_pipe/issues/71)
* riak_pipe/73: [PULSE test & fix riak_pipe_fitting](https://github.com/basho/riak_pipe/issues/73)
* riak_pipe/75: [lower "fitting was gone" log to debug level](https://github.com/basho/riak_pipe/issues/75)
* riak_pipe/76: [limited support for ring resizing](https://github.com/basho/riak_pipe/issues/76)
* riak_search/140: [remove guard on riak_search_vnode:start_vnode/1](https://github.com/basho/riak_search/issues/140)
* riaknostic/55: [Remove misplaced parathesis (sysctl check)](https://github.com/basho/riaknostic/issues/55)
* riaknostic/56: [Add OpenBSD bits](https://github.com/basho/riaknostic/issues/56)
* riaknostic/66: [Un-escriptize riaknostic and modify for lager 2.0 compatability](https://github.com/basho/riaknostic/issues/66)
* riaknostic/67: [Add an extra log line for clarity when running non-existent checks](https://github.com/basho/riaknostic/issues/67)
* webmachine/76: [Add logging for when webmachine crashes and body exists.](https://github.com/basho/webmachine/issues/76)
* webmachine/115: [Fix arguments to call to webmachine_request:recv_stream_body/2](https://github.com/basho/webmachine/issues/115)
* webmachine/117: [Decode Content-MD5 with base64, not hex](https://github.com/basho/webmachine/issues/117)
* webmachine/124: [Refine range header treatment](https://github.com/basho/webmachine/issues/124)
* webmachine/125: [Guess text/css MIME type for .less files](https://github.com/basho/webmachine/issues/125)
* webmachine/128: [Bugfix for multiple routers under Riak](https://github.com/basho/webmachine/issues/128)
* webmachine/134: [collapse 4 separate send calls into 1 in send_chunk](https://github.com/basho/webmachine/issues/134)
* webmachine/141: [Custom reason phrase](https://github.com/basho/webmachine/issues/141)
* webmachine/142: [Read body when DELETE to keep alive connection](https://github.com/basho/webmachine/issues/142)
* webmachine/143: [webmachine_dispatcher crashes on malformed Host header](https://github.com/basho/webmachine/issues/143)
* webmachine/144: [Allow responses for all HTTP errors to be customized](https://github.com/basho/webmachine/issues/144)
* webmachine/151: [Removing io:format/2 calls from log file processing](https://github.com/basho/webmachine/issues/151)


## Known Issues

### leveldb 1.3 to 1.4 conversion

The first execution of 1.4.0 leveldb using a 1.3.x or 1.2.x dataset will initiate an automatic conversion that could pause the startup of each node by 3 to 7 minutes.  The leveldb data in "level #1" is being adjusted such that "level #1" can operate as an overlapped data level instead of as a sorted data level.  The conversion is simply the reduction of the number of files in "level #1" to being less than eight via normal compaction of data from "level #1" into "level #2".  This is a one time conversion.

## Deprecation Warnings

### Ubuntu 11.04 (Natty) EOL

Ubuntu 11.04 Natty Narwhal reached its end-of-life October 2012 and
recently the public apt updates and security repos were removed.  Due
to this, Riak will no longer be built against 11.04 going forward.  We
will consider supporting the latest non-LTS release depending on the
timing of the next major Riak release.

Ubuntu LTS releases still supported (10.04 and 12.04) are unaffected.
