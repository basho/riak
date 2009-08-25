#!/usr/bin/env bash
# debug-restart <configfile>
# assuming we've run a node from here before, start back up in debug mode
. riak-env.sh
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~p~n\",[riak:get_app_env(riak_nodename)])" -run init stop)
RHOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_hostname)])" -run init stop)
if [ "$NODENAME" = "no_riak_nodename_undefined" ]; then
    echo "riak_nodename not set in config file, cannot start";
else
    exec erl -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME}@${RHOSTNAME} -run riak start $1 -run riak_startup rejoin
fi
