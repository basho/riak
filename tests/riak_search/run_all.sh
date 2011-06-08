#!/bin/sh

TEST_DIR=`pwd`
LOG=$TEST_DIR/test.log

cd ../../rel/riak

for x in $TEST_DIR/*_test;
do
    echo RUN: $x
    if ! bin/search-cmd test $x > $LOG 2>&1
    then
        printf "\nFAIL: %s\nCheck $LOG\n\n" $x
        exit 1
    fi
done
