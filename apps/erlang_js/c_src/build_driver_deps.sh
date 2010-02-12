#!/bin/sh

set -e

if [ `basename $PWD` != "src" ]; then
    cd c_src
fi

unset CFLAGS LDFLAGS

make $1
