#!/usr/bin/env bash
# ./start-eventer.sh <clustername> <cookie> <ip> <port> <nodename> <eventmodname> <eventmodarg>
# This will:
#  Join riak cluster <clustername> using erlcookie <cookie>
#  via the node listening at <ip>:<port>
#  and register a riak event handler
. riak-env.sh
erl -noshell -pa deps/*/ebin -pa ebin -name $5 -run riak_eventer eventer_config $1 $2 -run riak start -run riak_eventer do_eventer $3 $4 $6 $7
