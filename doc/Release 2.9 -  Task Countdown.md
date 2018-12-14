# Task Countdown

Count of tasks, known to be required to progress towards a final release 2.9.0 of Riak.  The release phase is split into four parts:

- Private Release Candidate - preparing a branch which can be subject to non-production testing by the primary Riak customers involved in the development cycle.  This will allow for further testing under different constraints, and work to commence on exploiting the required features.

- Public Release Candidate - provide a release candidate that has passed sufficient testing to allow for proving in non-production environments by a broader customer base.

- Release 2.9.0 - a quality-assured first release of the series.

- Subsequent Releases - any features deferred from 2.9.0, or fixes discovered post the Release of 2.9.0 may lead to subsequent releases.


## Private Release Candidate


Task | Status
:-------------------------|:-------------------------:
Complete first round of riak functional test | complete - mas
Risk assessment following riak functional test | complete - mas
Confirm eqc tests | complete - rdb
Merge in hot_backup work and test | complete - mas
Configuration guidance for exposed Leveled and Tictac AAE features | complete - mas
Tagged release of `leveled` and `kv_index_tictactree` repos | complete - mas
Provide preview of overview notes for release | complete - mas
Final test before declaring private RC ready | complete - mas

To access the private release candidate use the branch `develop-2.9.0-PRC0` after cloning the riak repo at `https://github.com/martinsumner/riak.git`.  Locked dependencies should *NOT* be used - use `make deps` followed by `make rel` to build.


## Public Release Candidate

Task | Status
:-------------------------|:-------------------------:
Access to A-D replication fixes | ongoing - bet365
Add enterprise leveldb into the build for Riak (with hot backup feature) | complete - mas
Creation of actual develop 2.9 branch | ongoing - mas
Tag release candidate (runs from make locked-deps) | unassigned
Code and test review of get_fsm changes | ongoing - rdb
Volume test showing bitcask comparison (not just leveled vs leveldb) | pending - mas
Testing in more live-like Spine environment (with larger disks) | ongoing - nhs
Testing in more live-like SuS environment | pending - nhs
Completion of HTTP API for Cluster-wide AAE | complete - rdb
Verify previous volume test results on actual release candidate | pending - mas
Two rounds of riak functional test on actual release candidate | unassigned
Compatibility testing with Riak CS | unassigned
Publish preview of release notes | pending - TI Tokyo
Writing of migration guidance, with publication of volume test results for migration | pending - mas
Leveled configuration when in multi-backend mode | ongoing - mas
Investigate Yokozuna [abstraction PR](https://github.com/basho/riak_kv/pull/1571) | unassigned
Leveled snapshot timeouts investigation | ongoing - mas
Multi-backend testing | unassigned

## Release 2.9.0


Task | Status
:-------------------------|:-------------------------:|
Additional independent property-based testing of leveled (focused on file-management actors) | pending - quviq
Publication of release notes and documentation updates |  unassigned
Acceptance of PRs into basho repo | unassigned

## Subsequent Releases

To be determined.
