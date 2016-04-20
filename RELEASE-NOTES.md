#Riak KV 2.1.4 Release Notes

Released April 6, 2016.

Riak KV 2.1.4 is a bugfix release to address two large bugs.


## Bugs Fixed

- eleveldb has been upgraded to version 2.0.17, which contains fixes for these issues:
    - Riak KV restarts and active anti-entropy (AAE) tree rebuilds could cause a segfault, terminating Riak KV's process. ([More info](http://docs.basho.com/riak/latest/community/product-advisories/leveldbsegfault/))
    - LevelDB's tiered storage is susceptible to data loss if Riak KV is stopped and started with less than 60 Mb stored per vnode. ([More info](https://github.com/basho/leveldb/wiki/mv-tiered-recovery-log))
    - LevelDB's recovery option does not work with tier-storage due to failed parameter passing with Riak KV. ([More info](https://github.com/basho/leveldb/wiki/mv-tiered-recovery-log))
    - Riak KV's active anti-entropy (AAE) process has potential to induce a segfault, though this issue has only been seen in new development code and not in existing releases. ([More info](https://github.com/basho/leveldb/wiki/mv-aae-segfault))
- File permissions/ownership in packaged releases generated
by the `node_package` library has been tightened. ([PR #196](https://github.com/basho/node_package/pull/196))
- The wording of a failed communication attempt when using command-line tools
to interact with Riak KV has been changed, as the failure of attempt is not always due to a down node. ([PR #199](https://github.com/basho/node_package/pull/199))
    - **PLEASE NOTE:** If you use automated systems or scripts to deploy/manage Riak KV and those tools depend on the textual responses rather than system-level exit codes, you may need to update your scripts. Specifically, commands that used to fail with the message "Node is not running!" will now return "Node did not respond to ping!"


## Upgrading

### Changes to File Ownership/Permissions
When upgrading from an older version of a  Riak KV, you may need to verify the following file ownership/permissions changes. 

>**Note:** The examples will be for a Centos 7-based Linux installation of Riak, but should illustrate the required checks for most operating systems and similar packages. For more details, please see the [node_package 3.0 release notes](https://github.com/basho/node_package/blob/develop/RELEASE-NOTES.md))

- Validate permissions on existing directories and make them owned by root:root
  (or the appropriate user/group for your operating system) and not writable by
  the package_install_user/group. For this example, we will list the specific
  directories for the Centos 7 install and then their `node_package` template
  names in parenthesis afterward. Directories and files include:
	- /usr/lib64/riak (`platform_lib_dir`)
	- /etc/riak (`platform_etc_dir`)
	- /usr/bin (`platform_bin_dir`), specifically
		- riak
		- riak-admin
		- riak-debug
		- riak-repl
		- search-cmd
	- /etc/init.d/riak (`platform_etc_dir`/init.d/`package_install_name`)
- Validate the home directory of the `platform_install_user` user is set to the
  `platform_data_dir`, in the case of Riak on Centos 7 this should be the `riak`
  user and the `/var/lib/riak` directory, and not `/usr/lib64/riak`. If
  necessary, change the home directory of the `riak` (`package_install_user`)
  user to point to `/var/lib/riak` (`platform_data_dir`).
- Validate any automated scripts using Riak-provided command line tools
  do not depend upon the textual "Node is not running!" message returned by those
  tools by either using OS exit codes or by changing scripts to match the text
  "Node did not respond to ping!"


#Riak KV 2.1.2/2.1.3 Release Notes

**2.1.3 - Released December 9, 2015**

Riak KV 2.1.3 is a repackaging of KV 2.1.2 with the 2.1.10 version of eLevelDB library, the 2.0.10 version of LevelDB library, and 2.1.5 version of riak_core. All notes from 2.1.2 apply to 2.1.3, including the required configuration changes in the Upgrading section.

**2.1.2 - Released December 1, 2015.**

This is a bugfix release, incorporating bugfixes from the Riak KV 2.0 series into our 2.1 series. It also includes general updates and additions to improve performance and incorporate patches developed since the release of 2.1.1.

##Upgrading
If you are upgrading from previous 2.1 releases or from Riak KV 2.0.6, you will need to double check your default bucket properties. KV 2.1.2 has a small, subtle change in the way that default bucket properties work. If your `advanced.config` file contains a section for `default_bucket_props` under the `riak_core` section, you will need to ensure you specify the settings for ALL default bucket properties that you expect to differ from the hard-coded defaults. 

For example, if your advanced.config file has a section similar to:

```
[{riak_core,[{default_bucket_props,[{n_val,1}]}]
```

In order to maintain the current behavior of your 2.1.1 node, you will need to make sure to include, at minimum, `allow_mult` and `dvv_enabled` settings, as the treatment of these has changed in order to fix a bug that caused `allow_mult` to be `true` when it should have been `false`.

In the above example, the corrected `default_bucket_props` section would look like:

```
[{riak_core,[{default_bucket_props,[
               {n_val,1},
               {allow_mult, true},
               {dvv_enabled, true}
            ]}]
        }]
```

If you have been depending on this behavior unintentionally, learn more about what  `allow_mult` and `dvv_enabled` imply in our [Conflict Resolution](http://docs.basho.com/riak/latest/dev/using/conflict-resolution/) documentation.

For more information about this change, please see [issue #727](https://github.com/basho/riak/issues/727).


##Changes

* Search now works with write-once buckets. A modification has been made to the `yz_kv:index` function to support passing both the bucket and key. Passing a bucket is needed for the write-once PUT path, where there is no (decoded) Riak object. [[riak_kv PR #1159](https://github.com/basho/riak_kv/pull/1159)]
* A Bitcask downgrade script has been added to downgrade Bitcask data files to the format used before version 1.7.0. [[Bitcask PR #215](https://github.com/basho/bitcask/pull/215)]
* Cuttlefish now compiles in windows. [[Cuttlefish PR #187](https://github.com/basho/cuttlefish/pull/187)]
* `non-smp` and `no async thread` have been added as BEAM options. The `no async thread` flag and the `non-smp` flag both reduce execution time considerably.
* Administrative operations: `stop`, `restart`, and `reboot` are now logged. This change assists in debugging situations, such as when nodes are exiting gracefully (going through the application:stop sequence) but there are no log messages to explain why `init:stop()` was called (like toplevel app supervisors dying).  [[node_package PR #184](https://github.com/basho/node_package/pull/184)]
* Changed compile on Release 17 to use a list of integers. [[Lager PR #209](https://github.com/basho/lager/pull/209)]
* `msg_q` length of `rtsink_helper` has been added to status. [[riak_repl PR #675](https://github.com/basho/riak_repl/pull/675)]
* Under a heavy IO load on the sink side, transferred objects can get stuck in the sink queue and consume infinite memory. A metric has been added so you can monitor memory performance before things collapse. [[riak_repl PR #674](https://github.com/basho/riak_repl/pull/674)]


##Bugs Fixed

* [[riak_core PR #764](https://github.com/basho/riak_core/pull/764)] A node can no longer join the cluster during staged joins if the node is still launching. A race condition was identified where, when nodes are being joined quickly during automated testing, nodes which are still busy starting applications can lead to divergent rings; and a node can transition from being a member of a ring back to joining status with resulting deadlock. The race condition only manifested during staged joins, so this change checks the joining node's `init` status and blocks the join if the node is starting (or stopping).
* [[Issue #663](https://github.com/basho/riak_repl/issues/663 )/[riak_repl PR #707](https://github.com/basho/riak_repl/pull/707) & [riak_repl PR #665](https://github.com/basho/riak_repl/pull/665)] AAE fullsync now works with bucket types. `encode_obj_msg` has been updated to support the w2wire protocol. The AAE sink has also been changed to use an updated `decode_obj_msg` function which can now properly handle bucket type property hashes.
* [[Issue #727](https://github.com/basho/riak/issues/727)/[riak_core PR #765](https://github.com/basho/riak_core/pull/765)] In the event a legacy `app.config` is being used, the default bucket type will remain as it is. Previously, if `app.config` was still in use after an upgrade, the default bucket type would change from `allow_mult=false` to `allow_mult=true`. Now, custom bucket type defaults are split out so that the default bucket type, in the presence of a legacy `app.config`, does not pick up `dvv` or `allow_mult=true`.
* [[Issue #1069](https://github.com/basho/riak_kv/issues/1069)/[riak_kv PR #1174](https://github.com/basho/riak_kv/pull/1174) & [riak_kv PR #1162](https://github.com/basho/riak_kv/pull/1162)] A bucket property validation has been added to ensure that if `last_write_wins` is true then `dvv_enabled` is false. The bucket property validation applies to `create` and `update` on any buckets or bucket types. Since this validation applies to a combination of two separate properties in both the create and update scenarios, a new `validate_post_merge` function was added, which allows for validation on properties after they have been fully resolved and merged.
* [[bitcask PR #216](https://github.com/basho/bitcask/pull/216)] Workers will now complete all ready deletes. Before this change, if the worker came across a frozen delete, it would stop entirely.
* [[Issue #212](https://github.com/basho/bitcask/issues/212)/[bitcask PR #214](https://github.com/basho/bitcask/pull/214)] In the event a user-defined key transformation function fails on a key read from disk, the process will now continue and log an error rather than crashing.
* [[Issue #212](https://github.com/basho/bitcask/issues/212)/[bitcask PR #213](https://github.com/basho/bitcask/pull/213)] Keys will no longer be loaded from corrupted entries. Before this fix, keys being loaded from a data file due to a bad/missing hintfile were loaded even if the record did not pass the CRC test.
* [[Issue #88](https://github.com/basho/riak_api/issues/88)/[riak_api PR #89](https://github.com/basho/riak_api/pull/89)] Adds a TCP keepalive feature to `pb_listener`. Without this feature, `pb_listener` hangs on to established connections in cases of a network partition, which can lead to available sockets being exhausted on servers with a high number of concurrent connections.
* [[Issue #760](https://github.com/basho/riak_core/issues/760)/[riak_core PR #767](https://github.com/basho/riak_core/pull/767)] Several processes around `check_threshold` messages on vnodes have been fixed. Before the fix, if a heavily loaded vnode received more than `check_threshold` messages after it hit the direct mailbox check, the ping/pong would leave it in overload state. In cases where the load was removed (e.g. primary recovered), the vnode proxy would stay in overload state until enough messages were received to trigger pinging the vnode or rechecking the mailbox. Now, skipped message counting is stopped; the ping/pong mechanism uses a reference to safely resubmit checks; a ping/pong is immediately triggered after a direct check so the proxy is notified as soon as the mailbox is cleared; and an `overloaded/1` functions has been added to make testing easier.
* [[riak_core PR #732](https://github.com/basho/riak_core/pull/732)] The sysmon handler has been updated to treat long schedules on ports gracefully. See the PR for before/after examples.
* [[riak_core PR #713](https://github.com/basho/riak_core/pull/713)] The common name is now only refused if CommonName isn’t a wildcard certificate name.
* [[Issue #70](https://github.com/basho/riak_ensemble/issues/70)/[riak_ensemble PR #75](https://github.com/basho/riak_ensemble/pull/75)] The LevelDB synctree lock is now limited to the local node to help reduce the ensemble startup time.
* [[riak_kv PR #1228](https://github.com/basho/riak_kv/pull/1228)] In the event of a downgrade from a version of Riak KV that uses new binary types rather than term-to-binary or custom binaries, KV won't crash when trying to read those keys back. Also, in the event of a provided "X-Riak-Val-Encoding" metadata tag, the binary is stored with that value as its magic byte to provide round-trip support.
* [[Issue #1086](https://github.com/basho/riak_kv/issues/1086)/[riak_kv PR #1167](https://github.com/basho/riak_kv/pull/1167)] To avoid a stats crash, `riak_kv_stat` can now handle undefined object size during consistent PUT/GET.
* [[riak_kv PR #1164](https://github.com/basho/riak_kv/pull/1164)] Compile-time warnings identified by dialyzer have been fixed.
* [[riak_kv PR #1155](https://github.com/basho/riak_kv/pull/1155)] The occasional problem where paginated 2i queries would be missing values has been fixed. See the PR for the full (long) explanation.
* [[Issue #1138](https://github.com/basho/riak_kv/issues/1138)/[riak_kv PR #1153](https://github.com/basho/riak_kv/pull/1153)] The `bad_match` during handoff with write-once buckets has been fixed. Previously, if data was coming into a write-once bucket while handoff was taking place, `riak_core_vnode` would crash with a `bad_match`.
* [[riak_repl PR #671](https://github.com/basho/riak_repl/pull/671)] No more deadlocks when handoff occurs during fullsync, and `wait_keylist` has been recoded as an async. Before the deadlock fix, neither source would handle a 'DOWN' message with a normal reason code, which would cause them to deadlock waiting for the vnode to respond if it was handed off to another node. The `wait_keylist` change fixed a crash in the `riak_repl_keylist_server` because of missing handling of `wait_keylist({skip_partition, Partition}, ...)`.
* [[Issue #1142](https://github.com/basho/riak_kv/issues/1142)/[riak_kv PR #1144](https://github.com/basho/riak_kv/pull/1144)] When handling mixed `vnode_status` files KV will no longer crash.
* [[riak_kv PR #1143](https://github.com/basho/riak_kv/pull/1143) & [riak_kv PR #1115](https://github.com/basho/riak_kv/pull/1115)] Your Riak node will no longer crash if the vnode status file is unreadable. Before this fix, if the vnode status file was unreadable with `file:consult/1` for any reason, the vnode would start, fail to consult, crash until the restart limit was hit, and then the node would fail to start/crash.
* [[Issue #706](https://github.com/basho/riak_core/issues/706)/[riak_kv PR #1116](https://github.com/basho/riak_kv/pull/1116)] Fixed permissions issues with `riak_kv.get_preflist`. Prior to this fix, you would get a message similar to "could not grant this permission as it was not part of the `permissions` list"; when permission failed, the error message would not get printed due to `badarg`; and PBC `decode` function did not return the correct value.
* [[Issue #711](https://github.com/basho/riak_repl/issues/711)/[riak_repl PR #717](https://github.com/basho/riak_repl/pull/717)] Only the primary atom of the error reason is now saved in the statistic, since including the entire error reason tuple in the connection error can cause each connection error to create a new folsom statistic.
* [[Issue #654](https://github.com/basho/riak_repl/issues/654)/[riak_repl PR #709](https://github.com/basho/riak_repl/pull/709)] The configs and defaults between fullsync and realtime for Riak CS block tombstone have been separated. The config keys have been separated into realtime and fullsync for block tombstone filter, and different default values have been set for them: `propagate` in realtime and `filter away` in fullsync.
* [[Issue #679](https://github.com/basho/riak_repl/issues/679)/[riak_repl PR #700](https://github.com/basho/riak_repl/pull/700), [riak_repl PR #701](https://github.com/basho/riak_repl/pull/701), & [riak_repl PR #683](https://github.com/basho/riak_repl/pull/683)] These PRs contain three fixes: First, the `send_after`call has been fixed and unneeded calls removed. Second, the `keepalive_timer` has been removed from state. Third, resolves an issue where the provider process could become blocked or stuck for an extended period only to receive many pointless messages from the timer system. To do this, the `keepalive` was upped from 1 second to 60, and the `erlang:send_after/3 reset` pattern is used rather than `timer:send_interval/3`. By using the `erlang:send_after`, messages will not be sent if the process becomes stuck, as no code capable of resetting the timer will run.
* [[Issue #637 ](https://github.com/basho/riak_repl/issues/637)/[riak_repl PR #697](https://github.com/basho/riak_repl/pull/697)] A bug with heartbeat has been fixed by adding a new pattern match and debug line. In the case where the `hb_interval` is already set, the pattern match to get into the first case will not succeed and thus an error message will be thrown. However, the default error message is incorrect in the case that the heartbeat interval is already set. Now, there is just a debug statement (not an error printed to the logs) in the situation where the heartbeat is already set and should be left alone.
* [[Issue #27](https://github.com/basho/riak_snmp/issues/27)/[riak_snmp PR #28](https://github.com/basho/riak_snmp/pull/28)] A function head has been added to `NameSortFun` that correctly handles the `realtime_enabled` property having multiple values. This situation can happen when a cluster is configured for realtime replication with two or more other clusters.
* [[Issue #324](https://github.com/basho/yokozuna/issues/324)/[yokozuna PR #572](https://github.com/basho/yokozuna/pull/572)] A couple fixes to handle Solr request timeout configs have been made. First, ibrowse `timedout` has been increased for `entropy_data` Solr request, which prevents `timedout` from crashing during iteration or from allowing the build to pass if an error occurs because Solr was not pingable. This increase allows for a general configurable timeout within adv.config for all Solr requests. Second, `solr_request_timeout` is now configurable. `solr_request_timeout` is used by all Solr localhost HTTP requests. Additionally, macro timeout has been added to the `entropy_data` call. Third, `yz_solr:ping` now uses a HEAD request. 
* [[yokozuna PR #569](https://github.com/basho/yokozuna/pull/569)] Base64 decoding has been updated in the Java EntropyData handler to fix a bug. This change means you now have the ability to adjust the max number of documents returned from an EntropyData query.
* [[Issue #305](https://github.com/basho/yokozuna/issues/305#issue-27558978)/[yokozuna PR #478](https://github.com/basho/yokozuna/pull/478)] Query plans are now cached by `n_val` rather than index name. Which leads to a noticeable reduction in %CPU usage in htop while running.
* [[Issue #498](https://github.com/basho/yokozuna/issues/498)/[yokozuna PR #532](https://github.com/basho/yokozuna/pull/532)] More informative error handling for HTTP requests has been added to make certain errors more informative, as errors were getting caught by the scrub_headers.
* [[Issue #503](https://github.com/basho/yokozuna/issues/503)/[yokozuna PR #528](https://github.com/basho/yokozuna/pull/528)] The ability to handle POSTs for search queries when given the content-type `application/x-www-form-urlencoded` or return a 415 if given another CT on POST has been added.
* [[Issue #525](https://github.com/basho/yokozuna/issues/525)/[yokozuna PR #527](https://github.com/basho/yokozuna/pull/527)] Yokozuna now re-indexes the remainder of a partition after a non-indexable document is encountered.
* [[Issue #481](https://github.com/basho/yokozuna/issues/481)/[yokozuna PR #486](https://github.com/basho/yokozuna/pull/486)] The issue in building-from/iterating-through entropy data to match Riak KV `actual_put` data with default bucket-types has been fixed, and log messaging in the `yz_index_hashtree` has been cleaned up.
* [[Issue #401](https://github.com/basho/yokozuna/issues/401)/[yokozuna PR #413](https://github.com/basho/yokozuna/pull/413)] You can now set the Jetty temporary directory using the `search.temp_dir` configuration variable.
* [[Issue #382](https://github.com/basho/yokozuna/issues/382)/ [yokozuna PR #483](https://github.com/basho/yokozuna/pull/483)] mochiglobal is now used to store the extractor map. If the extractor map is not in mochiglobal, CMD is checked, and then the ring. Additionally, the capability to move the extractor map into CMD has been added.
* [[Issue #480](https://github.com/basho/yokozuna/issues/480)/[yokozuna PR #482](https://github.com/basho/yokozuna/pull/482)] `default_schema` has been updated to return sets in query responses by storing them.
* [[yokozuna PR #487](https://github.com/basho/yokozuna/pull/487)] The index creation loop on bad data has been stopped.


##Upgraded Components

* syslog has been upgraded to version 1.0.3
* goldrush has been upgraded to version 0.1.7
* lager has been upgraded to version 2.2.0
* merge_index has been upgraded to version 2.0.1
* lager_syslog has been upgraded to version 2.1.3
* neotoma has been upgraded to version 1.7.3
* getopt has been upgraded to version v0.8.2
* cuttlefish has been upgraded to version 2.0.5
* riak_sysmon has been upgraded to version 2.1.1
* eleveldb has been upgraded to version 2.1.10
* riak_ensemble has been upgraded to version 2.1.2
* clique has been upgraded to version 0.3.2
* riaknostic has been upgraded to version 2.1.3
* riak_snmp has been upgraded to version 2.1.2
* riak_core has been upgraded to version 2.1.4
* riak_pipe has been upgraded to version 2.1.1
* riak_api has been upgraded to version 2.1.2
* bitcask has been upgraded to version 1.7.3
* riak_kv has been upgraded to version 2.1.2
* yokozuna has been upgraded to version 2.1.2
* riak_search has been upgraded to version 2.1.1
* riak_repl has been upgraded to version 2.1.2
* riak_jmx has been upgraded to version 2.1.1
* riak_control has been upgraded to version 2.1.2
* node_package has been upgraded to version 2.0.3
* cluster_info has been upgraded to version 2.0.


# Riak 2.1.1 Release Notes
*NOTE: Riak 2.1.1 has replaced Riak 2.1.0*

## Fixes
Riak 2.1.0 introduced a bug that has been fixed in Riak 2.1.1. The default configuration for handoff.ip caused vnodes marked for transfer during handoff to be removed without transferring data to their new destination nodes. A mandatory change to configuration (riak.conf) mitigates this issue for 2.1.0 users. While not all users were impacted by this issue, we recommend that all 2.1.0 users upgrade to 2.1.1.

Detailed information on the issue is available in the Basho Documentation [Product Advisories](http://docs.basho.com/riak/latest/community/product-advisories/210-dataloss/).

* Make default `handoff_ip` value 0.0.0.0 in vars.config.
  * [riak/pull/734](https://github.com/basho/riak/pull/734)


# Riak 2.1.0 Release Notes

## New Features

### Performance Improvements

The introduction of write-once buckets allows users to experience up to a 2x performance improvement for write-heavy, write-once workloads.  More details on implementing write-once buckets are available below.

### Write-Once Bucket Type

Riak 2.1.0 introduces the concept of write-once buckets, buckets whose entries are intended to be written exactly once and never updated or overwritten. Since objects are intended to only be written once, Riak does not perform the usual “get, merge, update” cycle which reduces IOPs and improves throughput and latency.

It is still possible for multiple entries to be written to a single key -- through mis-use of the API, network partitions, simultaneous writes, etc.. In these cases, Riak will always resolve siblings using an algorithm based on the SHA-1 hashes of the conflicting objects. While this algorithm is repeatable and deterministic at the database level, it will have the appearance to the user of “random write wins”.

The `write_once` property is a boolean value applied to a bucket type and may only be set at bucket creation time. Once a bucket type has been set and activated with the `write_once` property, the property may not be modified

Limitations:

 * Pre/Post-commit hooks are not supported.
 * Large object warnings and limits are not enforced.
 * All nodes must be upgraded before enabling `write_once` - usage in mixed clusters will crash vnodes.
 * Riak Enterprise: There is no support for real-time replication with write-once buckets.  Full Synchronization is supported with `write_once` bucket types.

## Changes

* [Issue kv679](https://github.com/basho/riak_kv/issues/679) - Applies fix to most causes of data loss from repeated causal history. A monotonic counter is fsynced to disk when the vnode starts. Thus starting/re-starting a vnode has a cost associated that was not there before. 
  * [riak_kv/pull/1070](https://github.com/basho/riak_kv/pull/1070)


* Update API to retrieve active preflist based on particular bucket/key.
Updates the Riak API with an operation to GET, e.g. `/types/Type/buckets/Bucket/keys/Key/preflist`, an annotated (nodes being primary or fallback) preflist of which nodes host a particular bucket/key combination is returned.
  * [riak-erlang-http-client/pull/50](https://github.com/basho/riak-erlang-http-client/pull/50)
  * [riak_core/pull/705](https://github.com/basho/riak_core/pull/705)
  * [riak_pb/pull/105](https://github.com/basho/riak_pb/pull/105)
  * [riak-erlang-client/pull/204](https://github.com/basho/riak-erlang-client/pull/204)
  * [riak_kv/pull/1083](https://github.com/basho/riak_kv/pull/1083)
  * [riak_api/pull/81](https://github.com/basho/riak_api/pull/81)
  * [riak_api/pull/75](https://github.com/basho/riak_api/pull/75)

* Add a JSON writer for several riak-admin commands, including cluster status, cluster partition[s], cluster partition-count, any of the riak-admin handoff commands, and riak-admin set/show/describe.
  * [clique/pull/47](https://github.com/basho/clique/pull/47)

* Integrate performance patches into Erlang R16 VM used by Riak.
  * [opt/pull/10](https://github.com/basho/otp/pull/10)

* Add riak_core_ring_manager:is_stable_ring/0 to public API. Allows clients to ensure that the ring has not changed in X seconds (currently 90).
  * [riak_core/pull/716](https://github.com/basho/riak_core/pull/716)

* [yokozuna/issues/468](https://github.com/basho/yokozuna/issues/468) - Add `search_query_latency_mean` and `search_index_latency_mean` metrics.
  * [yokozuna/pull/473](https://github.com/basho/yokozuna/pull/473)

## Fixes

* [yokozuna/issues/437](https://github.com/basho/yokozuna/issues/437): Timeouts during bulk data load, possible tie in to yz_events crash and a bad state on core create.
  * [yokozuna/pull/458](https://github.com/basho/yokozuna/pull/458)

* [yokozuna/issues/452](https://github.com/basho/yokozuna/issues/452):  Don't delete siblings in YZ when using SC or CRDTs.
  * [yokozuna/pull/457](https://github.com/basho/yokozuna/pull/457)

* [yokozuna/issues/402](https://github.com/basho/yokozuna/issues/402): Creating a new search index via HTTP responds before the index is available.
  * [riak_test/pull/745](https://github.com/basho/riak_test/pull/745)
  * [riak_pb/pull/112](https://github.com/basho/riak_pb/pull/112)
  * [riak_pb/pull/111](https://github.com/basho/riak_pb/pull/111)
  * [riak-erlang-client/pull/207](https://github.com/basho/riak-erlang-client/pull/207)
  * [yokozuna/pull/463](https://github.com/basho/yokozuna/pull/463)

* [yokozuna/issues/450](https://github.com/basho/yokozuna/issues/450): AAE Failing hourly, with exit value {badarg,46}, [{base64,decode_binary[{base64.erl…
  * [yokozuna/pull/459](https://github.com/basho/yokozuna/pull/459)

* [yokozuna/issues/437](https://github.com/basho/yokozuna/issues/437): yz_events:handle_info called with bad arguments.
  * [yokozuna/pull/458](https://github.com/basho/yokozuna/pull/458)
  * [yokozuna/pull/42](https://github.com/basho/yokozuna/pull/42)

* [yokozuna/issues/469](https://github.com/basho/yokozuna/issues/469): Fix YZ stats name typo from throughput to throughput.
  * [yokozuna/pull/470](https://github.com/basho/yokozuna/pull/470)

* [riak_core/issues/698](https://github.com/basho/riak_core/issues/698): SSL connections from peers with matching wildcard name are always rejected.
  * [riak_core/pull/718](https://github.com/basho/riak_core/pull/718)

## Known Issues

* [yokozuna/issues/481](https://github.com/basho/yokozuna/issues/481) - Search loses entries when Search AAE trees expire. We are currently investigating this issue.
* [riak/issues/727](https://github.com/basho/riak/issues/727) - Users upgrading from 1.4.x to 2.1.x that choose to use the traditional `app.config` for configuration should be aware that the default settings for `allow_mult` and `dvv_enabled` have changed from `false` to `true` between these versions.  If your application depends on these being set to 'false', you must explicitly define this by adding `{default_bucket_props, [{allow_mult, false}, {dvv_enabled, false}]},` to the `riak_core` section of your `app.config` file to maintain backwards compatibility.


## Notes on upgrading

* Write-Once Buckets
  * If you make use of write-once buckets, you must upgrade all of your Riak nodes in a cluster to 2.1.0.  There is no support for write-once buckets in a cluster with mixed Riak versions.
  * Riak Enterprise: There is no support for real-time replication with write-once buckets.  Full Synchronization is supported with `write_once` bucket types.
  * Pre/Post-commit hooks are not supported.
  * Large object warnings and limits are not enforced
