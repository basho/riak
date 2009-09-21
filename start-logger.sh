#!/usr/bin/env bash
# ./start-eventer.sh <clustername> <cookie> <ip> <port> <nodename> <eventmodname> <eventmodarg> <matchspec>
# This will:
#  Join riak cluster <clustername> using erlcookie <cookie>
#  via the node listening at <ip>:<port>
#  and register a riak event handler
if [ $# -lt 3 ]; then
    echo Usage: 1>&2
    echo "    `basename $0` <node> <cookie> <filename>"
    exit 1
fi
. riak-env.sh
NODE=$1
COOKIE=$2
FILENAME=$3
erl -noshell -pa deps/*/ebin -pa ebin -name riak_logger -setcookie $COOKIE -eval "riak_event_logger:start('$NODE', \"$FILENAME\")."
