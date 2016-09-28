#!/usr/bin/env bash

set -o nounset
set -o errexit

declare -r travis_build_dir="$1"
declare -r riak_dev_dir="$travis_build_dir/dev"

if [[ ! -d $riak_dev_dir ]]
then
    echo "Expected to find $riak_dev_dir directory, exiting!" 1>&2
    exit 1
fi

declare -r riak_erlang_client_dir="$HOME/build/riak-erlang-client"

git clone --depth=50 https://github.com/basho/riak-erlang-client.git "$riak_erlang_client_dir"

cd "$riak_erlang_client_dir"

git submodule update --init

./tools/devrel/setup-dev-cluster -p "$riak_dev_dir"

if ! hash escript
then
    set +o nounset
    set +o errexit
    source "$HOME/otp-basho/activate"
    set -o nounset
    set -o errexit
fi

export RIAK_TEST_HOST_1=127.0.0.1
export RIAK_TEST_NODE_1=dev1@127.0.0.1
export RIAK_TEST_PBC_1=10017
make
make test
