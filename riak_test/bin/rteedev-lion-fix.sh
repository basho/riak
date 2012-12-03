#!/bin/bash

# Fixes riak_ee-1.0.3 on lion/mountain lion

wget http://s3.amazonaws.com/downloads.basho.com/riak/1.0/1.0.3/riak-1.0.3-osx-x86_64.tar.gz
mkdir erlang_js-1.0.0 && tar xvzf riak-1.0.3-osx-x86_64.tar.gz -C erlang_js-1.0.0 riak-1.0.3/lib/erlang_js-1.0.0

rm -rf riak_ee-1.0.3/dev/dev1/lib/erlang_js-1.0.0
rm -rf riak_ee-1.0.3/dev/dev2/lib/erlang_js-1.0.0
rm -rf riak_ee-1.0.3/dev/dev3/lib/erlang_js-1.0.0
rm -rf riak_ee-1.0.3/dev/dev4/lib/erlang_js-1.0.0
cp -r erlang_js-1.0.0/riak-1.0.3/lib/erlang_js-1.0.0 riak_ee-1.0.3/dev/dev1/lib/.
cp -r erlang_js-1.0.0/riak-1.0.3/lib/erlang_js-1.0.0 riak_ee-1.0.3/dev/dev2/lib/.
cp -r erlang_js-1.0.0/riak-1.0.3/lib/erlang_js-1.0.0 riak_ee-1.0.3/dev/dev3/lib/.
cp -r erlang_js-1.0.0/riak-1.0.3/lib/erlang_js-1.0.0 riak_ee-1.0.3/dev/dev4/lib/.
cp -r erlang_js-1.0.0/riak-1.0.3/lib/erlang_js-1.0.0 riak_ee-1.0.3/dev/dev5/lib/.
cp -r erlang_js-1.0.0/riak-1.0.3/lib/erlang_js-1.0.0 riak_ee-1.0.3/dev/dev6/lib/.
