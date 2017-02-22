This doc contains the internal Basho policy for building, testing, and releasing Riak.

# Scheduling
All public releases must conform to the following schedule:
* An Execution phase, where the work is actually completed by Engineering. At the beginning of the Execution phase, all features / bugfixes / backports must be scoped and prioritized for release. Given demands of users/business/product, a Release from Engineering date will be chosen (which may some day be on a regular cadence). While that release date should not be changed, the final scope of that release may be increased or reduced based on actual work completed, tested, and merged before the cut-off determined by Engineering for new features to be merged into this release.
* A Functional Complete date scheduled on a Wednesday at least two weeks before the expected Release from Engineering date. At this point:
  * All features must be complete, and all PRs must be triaged and merged
  * At least one nightly build has undergone a Giddyup run and triage
  * At least one nightly build has been smoke tested on all platforms  
  * At least one nightly build has been perf tested  
  * Predocs have been submitted to Tech Pubs team

 The Functional Complete date will effectively begin the Release Candidate phase. During this time, only bugfixes to address issues in any features required for the GA release will be added, and Release Candidates will be cut on an as-needed basis.
* An expected Release from Engineering date from engineering on a Wednesday. The Release from Engineering will be the final package that is distributed to the public, and will end the Release Candidate phase. Each delivery date must have at least one fallback date that is no earlier than one week after the expected delivery date. If the Release from Engineering date is missed, the fallback date becomes the primary Release from Engineering date, and a new fallback date must be scheduled.
* A Go/No-Go scheduled on the Thursday after the Release from Engineering date or fallback date to ensure that the General Availability (GA) release can proceed as planned. During the Go/No-Go we must confirm:
  * Giddyup has been triaged, and there are no major issues
  * Smoke testing has completed successfully on all supported platforms
  * Perf testing has been completed
  * Documentation content has been merged and triaged if necessary, and release notes are ready to ship
  * Packages for all supported platforms have been built, reviewer, and are ready to stage
  * Any marketing announcements have been reviewed and are ready to ship
* A final GA staging date on the Friday after the Go/No-Go. During this time, all packages will be pushed to their final download locations, the `External Release` docs will be sent around internally to the Product Management and Documentation teams for review, and the indexes for the download pages will be generated.
* A GA date on the Tuesday after the final staging date. This date cannot overlap with another public release.

### Effective Schedule
* Execution Phase (During this phase, the schedule for GA will be determined)
* [Functional Complete](#functional-complete) (Release from Engineering -2 Weeks)
* [Release from Engineering](#release-from-engineering) (GA -6 Days)
* [Go/No-Go](#gono-go) (GA -5 Days)
* [GA Staging](#ga-staging) (GA -4 Days)
* [GA](#ga)

# Pre-Release
All releases of Riak will have at least one `Release Candidate` before the final package build. These RCs will contain all of the `features, backports, and bugfixes` required to build the Final Release, but will not have an updated link to the `Release Notes`. The Release Notes link update will be the only difference between RCs and the final version that will be released to the public.

## Pre-release Procedure
All Release Candidates will undergo the following release stages to ensure quality:
  * Package Build
  * Internal Publishing
  * riak_test (giddyup) runs
  * Smoke Testing (on at least the final RC)

The following procedure will cover all of these steps:  
* Ensure `riak_test` is configured to automatically run after the package building steps:
  * If there is no branch in `riak_test` that corresponds to `riak`, create it
  * `basho_builds.yml` must exist in the top level of the repo, and contain the list of devrels that will be used to test various upgrade / downgrade scenarios during riak_test runs. The contents of the file will be similar to:
 ```
docker_rt_riak_test_config:
  - version: current
    product_version: current
    product: riak_ee
  - version: previous
    product_version: 2.2.0
    product: riak_ee
  - version: legacy
    product_version: 2.0.8
    product: riak_ee
  - version: 2.0.2
    product_version: 2.0.2
    product: riak_ee
  - version: 2.0.4
    product_version: 2.0.4
    product: riak_ee
  - version: 2.0.5
    product_version: 2.0.5
    product: riak_ee
```
  * Tag riak_test with the same `annotated tag` that you will use for riak
    * If you are running tests against 2.0 series, see below for special instructions
  * Push the tag to github  
* Bundle Riak
  * Lock the deps: `make lock`
  * Tag riak with an `annotated tag`:
    * Tags must follow the convention `product_name-semantic.version<xyz123>`. Only 1 dash can be used, or this will break the results in giddyup. For example:
      - `riak_ee-2.2.0`
      - `riak-2.2.1rc2`
      - `riak_ts-1.6.0b`
      - `riak_ee-2.2.0somekindacrazytest1`
      - `riak-1.4.8`  
    * Tag the product using: `git tag -a <tagname> -m "<message>"`
* Giddyup runs will be kicked off automatically after the package builds complete successfully, but results must be triaged after each run. Giddyup runs usually complete 4.5 hours after the tag is initially received by the system.  
* Inform the `Release Management` team that a new version of riak is being built, and schedule some time for Smoke testing.  
  
  
*Note*: Remember to follow the aforementioned procedure for EE as well, since these products are always releases in tandem.

### Special considerations for 2.0 series
There is one key difference in 2.0.x and later versions that must be accounted for when setting up riak_test: PB is locked to a specific version in Riak itself. This would cause a conflict in rebar, since `basho_bench` (built as part of riak_test) typically pulls the latest PB. To avoid this scenario in 2.0.x builds, a special branch has been created in b_b that locks PB to the same version as Riak: `riak/2.0`. An override must be created in basho-builds by setting `riak_test_bb_version` to `riak/2.0` in `basho_builds.yml` in the riak_test repo. `basho_builds.yml` for 2.0.x in riak_test should look something like this:
```
riak_test_bb_version: 'riak/2.0'
docker_rt_riak_test_config:
  - version: current
    product_version: current
    product: riak_ee
  - version: previous
    product_version: 2.0.7
    product: riak_ee
  - version: legacy
    product_version: 1.4.12
    product: riak_ee
  - version: 2.0.2
    product_version: 2.0.2
    product: riak_ee
  - version: 2.0.4
    product_version: 2.0.4
    product: riak_ee
  - version: 2.0.5
    product_version: 2.0.5
    product: riak_ee
```

# Checklists
## Planning
- [ ] Expected final-package delivery date has been scheduled
- [ ] Fallback final-package delivery date has been scheduled
- [ ] Go/No-Go has been scheduled
- [ ] Fallback Go/No-Go has been scheduled
- [ ] Public release date has been scheduled
- [ ] Fallback Public release date has been scheduled
- [ ] Product Management has issues a supported platforms list to DevOps and RelEng

## Functional Complete
- [ ] All functionality defined for this release is code complete
- [ ] All PRs required to be included have been reviewed and merged
- [ ] At least one nightly has gone through Giddyup triage
- [ ] At least one nightly has gone through Smoke Testing for all supported platforms
- [ ] At least one nightly has been perf tested
- [ ] Predocs have been submitted to Tech Pubs team

## Release from Engineering
- [ ] Final Packaging can begin as the following criteria have been met:
  - [ ] At least one RC has been generated:
    - [ ] Packages for all supported platforms have been built
    - [ ] Giddyup has been triaged and vetted by engineering
    - [ ] Smoke testing has been run against all supported platforms
    - [ ] Perf testing has been completed
- [ ] Link to the Release Notes has been added
- [ ] Packages for all supported platforms have been built
- [ ] Smoke testing has been run against all supported platforms

## Go/No-Go
- [ ] Packages have been built and delivered by Engineering
- [ ] Giddyup has been triaged and vetted by Engineering
- [ ] Smoke testing has been completed successfully for all supported platforms
- [ ] Perf testing has been completed
- [ ] Documentation has been merged, release notes are ready to ship
- [ ] Marketing announcements have been reviewed and are ready to ship

## GA Staging
- [ ] Docs pushed to staging
- [ ] Packages have been put in their final download locations and the `External Release` doc has been generated
- [ ] `External Release` doc has been sent to Product Management and Documentation for review
- [ ] Indexes for the Downloads Page have been generated

## GA
- [ ] Docs have been pushed live
- [ ] Packages have been uploaded to `packagecloud`
- [ ] EE package links published in Zendesk
- [ ] Marketplace AMI has been created and sent to Amazon

