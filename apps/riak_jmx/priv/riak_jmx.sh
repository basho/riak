#!/bin/bash

##
## Use exec so that the PID doesn't change when invoked
##
exec java -server -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.port=41123 -cp riak_jmx.jar com.basho.riak.jmx.Main $1 $2
