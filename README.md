# Riak - a distributed, decentralised data storage system.

To build riak, Erlang OTP 22 or higher is required.

To install and run on Ubuntu 22

1. sudo apt upgrade
2. sudo apt update
3. sudo apt install build-essential
4. sudo apt install libpam0g-dev
5. sudo apt install erlang
6. sudo apt install cmake
7. Go to the riak GitHub repo and clone the repo (https://github.com/basho/riak)
8.  cd riak
9. sudo ./rebar3 get-deps
10. sudo make rel
11. Edit riak/rel/riak/etc/riak.config following the user  [documentation](https://www.tiot.jp/riak-docs/riak/kv/2.9.10/) or the [legacy documentation](https://docs.riak.com/riak/kv/latest/index.html)
12. sudo riak daemon

`make rel` will build a release which can be run via `rel/riak/bin/riak start`.  Riak is primarily configured via `rel/riak/etc/riak.conf`

To make a package, install appropriate build tools for your operating system and run `make package`.

To create a local multi-node build environment use `make devclean; make devrel`.

To test Riak use [Riak Test](https://github.com/basho/riak_test/blob/develop-3.0/doc/SIMPLE_SETUP.md).

Up to date documentation is not available, but work on [documentation](https://www.tiot.jp/riak-docs/riak/kv/2.9.10/) is ongoing and the core information available in the [legacy documentation](https://docs.riak.com/riak/kv/latest/index.html) is still generally relevant.

Issues and PRs can be tracked via [Riak Github](https://github.com/basho/riak/issues) or [Riak KV Github](https://github.com/basho/riak_kv/issues).
