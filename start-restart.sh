#!/usr/bin/env bash
# start-restart <configfile>
# assuming we've run a node from here before, start back up
. riak-env.sh
echo $ERL_MAX_ETS_TABLES
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(erlang_nodename)])" -run init stop)
COOKIE=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(erlang_cookie)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_heart_command)])" -run init stop)
exec erl -heart -detached -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME} -setcookie ${COOKIE} -run riak start $1 -run riak_startup rejoin