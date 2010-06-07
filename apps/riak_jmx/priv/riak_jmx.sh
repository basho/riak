#!/bin/bash

##
## Use exec so that the PID doesn't change when invoked
##
exec java -server -cp riak_jmx.jar com.basho.riak.jmx.Main $1 $2
