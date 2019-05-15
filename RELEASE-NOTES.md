# Riak KV 2.9.0 Release Notes

See [here for notes on 2.9.0](doc/Release 2.9 Series - Overview.md)

# Riak KV 2.2.5 Release Notes

> This release is dedicated to the memory of Andy Gross. Thank you and RIP.

## Overview

This is the first full community release of Riak, post-Basho's
collapse into bankruptcy. A lot has happened, in particular [bet365](https://twitter.com/bet365Tech) bought Basho's
assets and donated the code to the community. They kept the Basho
website running, the documents site, the mailing list (after [TI Tokyo](https://www.tiot.jp/)
had helpfully mirrored the docs in the interim), and have done a huge amount to
provide continuity to the community.

The development work on this release of Riak has received significant
funding from [NHS Digital](https://twitter.com/NHSDigital), who depend on Riak for Spine II, and other
critical services. Thanks also to [ESL](https://twitter.com/ErlangSolutions), [TI Tokyo](https://www.tiot.jp/), and all the other
individuals and organisations involved.

This release of Riak is based on the last known-good release of Riak,
riak-2.2.3. There is good work in the `develop` branches of many Basho
repos, but since much of it was unfinished, unreleased, untested, or
just status-unknown, we decided as a community to go forward based on
riak-2.2.3.

This is the first release with open source multi-data-centre
replication. The rest of the changes are fixes ([riak-core claim](#core-claim-fixes),
repl), new features ([gsets](#gsets), [participate in coverage](#participate-in-2i), [node-confirms](#node-confirms)),
and [fixes to tests](#developer-improvements) and the build/development process.

[Improvements](#improvements)

[Known Issues](#known-issues) - please read **before upgrading** from a previous Riak release

[Log of Changes](#change-log-for-this-release)

[Previous Release Notes](RELEASE-NOTES-2_2_4.md)

## Improvements

#### Multi Datacentre Replication

Previously a paid for enterprise addition to Riak as part of the Riak
EE product, this release includes Multi-Datacentre Replication
(MDC). There is no longer a Riak EE product. All is now Open
Source. Please consult the existing documentation for
[MDC](http://docs.basho.com/riak/kv/2.2.3/configuring/v3-multi-datacenter/). Again,
many thanks to bet365 Technology for this addition. See also
[Known Issues](#known-issues) below.

#### Core Claim Fixes

Prior to this release, in some scenarios, multiple partitions from the
same preflist could be stored on the same node, potentially leading to
data loss. [This write up](https://github.com/basho/riak_core/blob/c9c924ef006af1121b7eec04c7e1eefe54f4cf26/docs/claim-fixes.md)
explains the fixes in detail, and links to
[another post](https://github.com/infinityworks/riak_core/blob/ada7030a2b2c3463d6584f1d8b20e2c4bc5ac3d8/docs/ring_claim.md)
that gives a deep examination of riak-core-ring and the issues fixed
in this release.

#### Node Confirms

This feature adds a new bucket property, and write-option of
`node_confirms`. Unlike `w` and `pw` that are tunables for
consistency, `node_confirms` is a tunable for durability. When
operating in a failure state, Riak will store replicas in fallback
vnodes, and in some case multiple fallbacks may be on the same
physical node. `node_confirms` is an option that specifies how many
distinct physical nodes must acknowledge a write for it to be
considered successful. There is a
[detailed write up here](https://github.com/ramensen/riak_kv/blob/30b0e50374196d9a8cfef37871955a5f5b2bb472/docs/Node-Diversity.md),
and more in the documentation.

#### Participate In 2i

This feature was added to bring greater consistency to 2i query
results. When a node has just been joined to a riak cluster it may not
have any, or at least up-to-date, data. However the joined node is
immediately in the ring and able to take part in coverage queries,
which can lead to incomplete results. This change adds an operator
flag to a node's configuration that will exclude it from coverage
plans. When all transfers are complete, the operator can remove the
flag. See documentation for more details.

#### GSets

This release adds another Riak Data Type, the GSet CRDT. The GSet is a
grow only set, and has simpler semantics and better merge performance
than the existing Riak Set. See documentation for details.

#### Developer Improvements

The tests didn't pass. Now they do. More details
[here](https://github.com/russelldb/russelldb.github.io/blob/b228eacd4fd3246b4eb7f8d0b98c6bed747e2514/make_test.md)

## Known Issues

#### Advanced.config changes

With the inclusion of Multi-Datacentre Replication in riak-2.2.5 there
are additional `advanced.config` parameters. If you have an existing
`advanced.config` you must merge it with the new one from the install
of riak-2.2.5. Some package installs will simply replace the old with
new (e.g. .deb), others may leave the old file unchanged. YOU MUST
make sure that the `advanced.config` contains valid `riak_repl`
entries.

Example default entries to add to your existing advanced.config:

```
{riak_core,
  [
   {cluster_mgr, {"0.0.0.0", 9080 } }
  ]},
 {riak_repl,
  [
   {data_root, "/var/lib/riak/riak_repl/"},
   {max_fssource_cluster, 5},
   {max_fssource_node, 1},
   {max_fssink_node, 1},
   {fullsync_on_connect, true},
   {fullsync_interval, 30},
   {rtq_max_bytes, 104857600},
   {proxy_get, disabled},
   {rt_heartbeat_interval, 15},
   {rt_heartbeat_timeout, 15},
   {fullsync_use_background_manager, true}
  ]},
```

Read more about configuring
[MDC](http://docs.basho.com/riak/kv/2.2.3/configuring/v3-multi-datacenter/)
replication.

More details about the issue can be found in riak\_repl/782: [2.2.5 - \[enoent\] - riak_repl couldn't create log dir
"data/riak_repl/logs"](https://github.com/basho/riak/issues/940)

## Change Log for This Release

* riak_pipe/113: [Some flappy test failure fixes](https://github.com/basho/riak_pipe/pull/113)
* riak_kv/1657: [Intermittent test failure fixes](https://github.com/basho/riak_kv/pull/1657)
* riak_kv/1658: [ Move schedule_timeout to execute in put_fsm](https://github.com/basho/riak_kv/pull/1658)
* riak_kv/1663: [Add bucket property `node_confirms` for physical diversity](https://github.com/basho/riak_kv/pull/1663)
* riak_kv/1664: [Add  option 'participate_in_2i_coverage' with default 'enabled'](https://github.com/basho/riak_kv/pull/1664)
* riak_kv/1665: [enable gset support](https://github.com/basho/riak_kv/pull/1665)
* riak_kv/1666: [Fix schema paths for make test](https://github.com/basho/riak_kv/pull/1666)
* eleveldb/243: [Add a 10% fuzz factor to the resident memory calc (intermittent test failure "fixes")](https://github.com/basho/eleveldb/pull/243)
* riak_core/911: [Fix brops intermittent test failures](https://github.com/basho/riak_core/pull/911)
* riak_core/913: [ Fix claim tail violations and unbalanced rings](https://github.com/basho/riak_core/pull/913)
* riak_core/915: [Add `node_confirms` default bucket props](https://github.com/basho/riak_core/pull/915)
* riak_core/917: [Add participate_in_2i_coverage riak option](https://github.com/basho/riak_core/pull/917)
* sidejob/18: [Address some intermittent test failures](https://github.com/basho/sidejob/pull/18)
* riak_pb/228: [Add `node_confirms` option to write messages](https://github.com/basho/riak_pb/pull/228)
* riak_pb/229: [add gsets support](https://github.com/basho/riak_pb/pull/229)
* basho_stats/13: [Non-deterministic test needs a little ?SOMETIMES](https://github.com/basho/basho_stats/pull/13)
* basho_stats/4: [Add Makefile](https://github.com/basho/basho_stats/pull/4)
* exometer_core/17: [Fix failing test with correct tuple entry](https://github.com/basho/exometer_core/pull/17)
* yokozuna/741: [Fix broken eqc test](https://github.com/basho/yokozuna/pull/741)
* yokozuna/746: [remove -XX:+UseStringCache](https://github.com/basho/yokozuna/pull/746)
* yokozuna/747: [Remove jvm directive from test too](https://github.com/basho/yokozuna/pull/747)
* clique/81: [Fix failing test on some environments](https://github.com/basho/clique/pull/81)
* riak_dt/121: [doc related fix & explanation](https://github.com/basho/riak_dt/pull/121)
* riak_dt/127: [bring develop-2.2 up-to-date with develop](https://github.com/basho/riak_dt/pull/127)
* riak_dt/129: [Add gset support](https://github.com/basho/riak_dt/pull/129)
* riak_dt/135: [Fix `equal/2` bug around unordered dict usage](https://github.com/basho/riak_dt/pull/135)
* riak_repl/776: [Fix bug when passing utc timestamps into httpd_util:rfc1123/1.](https://github.com/basho/riak_repl/pull/776)
* riak_repl/777: [Fix badarg in binary construction for args to ebloom](https://github.com/basho/riak_repl/pull/777)
* riak_repl/779: [Sticking plaster fix for basho/riak_repl#772](https://github.com/basho/riak_repl/pull/779)
* riak_repl/780: [Fix sometime failing test](https://github.com/basho/riak_repl/pull/780)
* riak_repl/782: [Change ETS queue table permissions to protected](https://github.com/basho/riak_repl/pull/782)

## Previous Release NOTES

[Full history of Riak 2.0+ release notes](RELEASE-NOTES-2_2_4.md)
