#!/usr/bin/env bash
# start-fresh <configfile>
# create a brand new riak ring, using configfile as app config params
. riak-env.sh
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(erlang_nodename)])" -run init stop)
COOKIE=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(erlang_cookie)])" -run init stop)
export HEART_COMMAND=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_heart_command)])" -run init stop)
exec erl -heart -detached -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME} -setcookie ${COOKIE} -run riak start $1