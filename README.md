# Riak - a distributed, decentralised data storage system.

To build riak, Erlang OTP 22 or higher is required.

`make rel` will build a release which can be run via `rel/riak/bin/riak start`.  Riak is primarily configured via `rel/riak/etc/riak.conf`

To make a package, install appropriate build tools for your operating system and run `make package`.

To create a local multi-node build environment use `make devclean; make devrel`.

To test Riak use [Riak Test](https://github.com/basho/riak_test/blob/develop-3.0/doc/SIMPLE_SETUP.md).

[legacy documentation](https://docs.riak.com/riak/kv/latest/index.html) is still generally relevant.

Issues and PRs can be tracked via [Riak Github](https://github.com/basho/riak/issues) or [Riak KV Github](https://github.com/basho/riak_kv/issues).
