#!/usr/bin/env bash
# ./start-restore.sh<node> <cookie> <filename>
# This will:
#  1. Join riak cluster of which <node> is a member, using <cookie>.
#  2. Overwrite cluster data with data contained in <filename>

if [ $# -lt 3 ]; then
    echo Usage: 1>&2
    echo "    `basename $0` <node> <cookie> <filename>" 1>&2
    exit 1
fi
. riak-env.sh
NODE=$1
COOKIE=$2
FILENAME=$3
erl -noshell -pa deps/*/ebin -pa ebin -name riak_restore -setcookie $COOKIE -eval "riak_backup:restore('$NODE', \"$FILENAME\")." -run init stop
