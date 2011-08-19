#!/bin/sh
#
#> Usage:
#>     run_all.sh [--all] [-b BIN_DIR] [TEST_DIR]

error()
{
    echo "$@" 1>&2
    exit 1
}

usage() {
    grep '#>' $0 | tr -d "#>" | sed '$d'
}

LOG=`pwd`/test.log
BIN_DIR=../../rel/riak/bin
TESTS=`pwd`/*_test
all=no

while [ $# -gt 0 ]
do
    case $1 in
        -h)
            usage
            exit 0
            ;;
        --all)
            all=yes
            ;;
        -b)
            if [ $# -lt 2 ]
            then
                usage
                error "Missing BIN_DIR arg"
            else
                BIN_DIR=$2
                shift
            fi
            ;;
        -*)
            usage
            error "Unrecognized option: $1"
            ;;
        *)
            break
            ;;
    esac
    shift
done

if [ $# -gt 0 ]
then
    all=no
    TESTS=`pwd`/$1
    shift
fi

for x in $TESTS
do
    if [ "${x##/*/}" == "replication_test" ] && [ "$all" == "no" ]
    then
        continue
    else
        echo RUN: $x
        if ! $BIN_DIR/search-cmd test $x > $LOG 2>&1
        then
            printf "\nFAIL: %s\nCheck $LOG\n\n" $x
            error
        fi
    fi
done
