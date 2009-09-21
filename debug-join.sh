#!/usr/bin/env bash
# debug-join <configfile> <ip> <port>
# join an existing riak ring via a node known to be listening on ip:port
# run in interactive debug shell mode
. riak-env.sh
NODENAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_nodename)])" -run init stop)
HOSTNAME=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_hostname)])" -run init stop)
COOKIE=$(erl -noshell -pa ebin -eval "error_logger:tty(false), riak_app:read_config(\"$1\"), io:format(\"~s~n\",[riak:get_app_env(riak_cookie)])" -run init stop)
exec erl -connect_all false -pa deps/*/ebin -pa ebin -name ${NODENAME}@${HOSTNAME} -setcookie ${COOKIE} -run riak start $1 -run riak_startup join_cluster $2
