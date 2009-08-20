#!/usr/bin/env bash
# start-join <configfile> <ip> <port>
# join an existing riak ring via a node known to be listening on ip:port
if [ $# -le 3 ]; then
    echo Usage: 1>&2
    echo "    `basename $0` <config file> <ip of node in ring> <doorbell port>" 1>&2
    exit 1
fi
. riak-env.sh
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~p~n\",[riak:get_app_env(riak_nodename)])" -run init stop)
RHOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_hostname)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_heart_command)])" -run init stop)
if [ "$NODENAME" = "no_riak_nodename_undefined" ]; then
    echo "riak_nodename not set in config file, cannot start";
else
    exec erl -heart -detached -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME}@${RHOSTNAME} -run riak start $1 -run riak_startup join_cluster $2 $3
fi
