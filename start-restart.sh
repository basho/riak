#!/usr/bin/env bash
# start-restart <configfile>
# assuming we've run a node from here before, start back up
. riak-env.sh
echo $ERL_MAX_ETS_TABLES
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~p~n\",[riak:get_app_env(riak_nodename)])" -run init stop)
RHOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_hostname)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_heart_command)])" -run init stop)
if [ "$NODENAME" = "no_riak_nodename_undefined" ]; then
    echo "riak_nodename not set in config file, cannot start";
else
    exec erl -heart -detached -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME}@${RHOSTNAME} -run riak start $1 -run riak_startup rejoin
fi
