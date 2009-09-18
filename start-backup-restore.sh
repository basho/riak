#!/usr/bin/env bash
# ./start-backup-restore.sh <clustername> <node> <cookie> <filename>
# This will:
#  1. Join riak cluster <clustername> at <node> using <cookie>
#  2. Overwrite cluster data with data contained in <filename>
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
erl -noshell -pa deps/*/ebin -pa ebin -name backup_restore -setcookie $COOKIE -run riak_backup restore_config $CLUSTERNAME -run riak start -run riak_backup do_restore $NODE $FILENAME -run init stop

