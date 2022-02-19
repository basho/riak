#!/bin/sh

# $FreeBSD$
#
# PROVIDE: riak
# REQUIRE: LOGIN
# KEYWORD: shutdown

. /etc/rc.subr

name=riak
command=/usr/local/lib/riak/%ERTS_PATH%/bin/beam.smp
rcvar=riak_enable
start_cmd="/usr/local/bin/riak start"
stop_cmd="/usr/local/bin/riak stop"
pidfile="/var/run/riak/riak.pid"

load_rc_config $name
run_rc_command "$1"
