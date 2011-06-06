#!/bin/sh

TEST_DIR=`pwd`

cd ../../rel/riak

for x in $TEST_DIR/*_test;
do
    echo $x
    bin/search-cmd test $x 2>&1 | grep -e FAIL -e ERROR
done
