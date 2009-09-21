#!/usr/bin/env bash
# ./start-eventer.sh <clustername> <cookie> <ip> <port> <nodename> <eventmodname> <eventmodarg> <matchspec>
# This will:
#  Join riak cluster <clustername> using erlcookie <cookie>
#  via the node listening at <ip>:<port>
#  and register a riak event handler
if [ $# -lt 2 ]; then
    echo Usage: 1>&2
    echo "    Log to console: `basename $0` <node> <cookie>"  1>&2
    echo "    Log to file:    `basename $0` <node> <cookie> <filename>"  1>&2
    exit 1
fi
. riak-env.sh
NODE=$1
COOKIE=$2
FILENAME=$3
erl -noshell -pa deps/*/ebin -pa ebin -name riak_logger -setcookie $COOKIE -eval "riak_event_logger:start('$NODE', \"$FILENAME\")."
