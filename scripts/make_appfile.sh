#!/bin/bash
prefix=""
modules=$(
    for f in $(ls src/*.erl); do 
        echo -n "$prefix'$(basename -s .erl $f)'"
        prefix=", "
    done)
sed "s/__MODULES__/$modules/g" <src/riak.app.src 