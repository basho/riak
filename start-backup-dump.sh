#!/usr/bin/env bash
# ./start-backup-dump.sh <clustername> <node> <cookie> <filename>
#
# This will:
#  1. Join riak cluster <clustername> at <node> using <cookie>
#  2. Dump the entire cluster's contents to <filename>
if [ $# -lt 4 ]; then
    echo Usage: 1>&2
    echo "    `basename $0` <clustername> <node> <cookie> <filename>" 1>&2
    exit 1
fi
. riak-env.sh
CLUSTERNAME=$1
NODE=$2
COOKIE=$3
FILENAME=$4
erl -noshell -pa deps/*/ebin -pa ebin -name backup_dumper -setcookie $COOKIE -run riak_backup dump_config $CLUSTERNAME -run riak start -run riak_backup do_dump $NODE $FILENAME -run init stop
