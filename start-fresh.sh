#!/usr/bin/env bash
# start-fresh <configfile>
# create a brand new riak ring, using configfile as app config params
. riak-env.sh
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~p~n\",[riak:get_app_env(riak_nodename)])" -run init stop)
RHOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_hostname)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_heart_command)])" -run init stop)
if [ "$NODENAME" = "no_riak_nodename_undefined" ]; then
    echo "riak_nodename not set in config file, cannot start";
else
    exec erl -heart -detached -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME}@${RHOSTNAME} -run riak start $1
fi
