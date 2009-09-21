#!/usr/bin/env bash
# start-join <configfile> <ip> <port>
# join an existing riak ring via a node known to be listening on ip:port
if [ $# -lt 2 ]; then
    echo Usage: 1>&2
    echo "    `basename $0` <config file> Node" 1>&2
    exit 1
fi
. riak-env.sh
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_nodename)])" -run init stop)
HOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_hostname)])" -run init stop)
COOKIE=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_cookie)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_heart_command)])" -run init stop)
exec erl -heart -detached -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME}@${HOSTNAME} -setcookie ${COOKIE} -run riak start $1 -run riak_startup join_cluster $2