#!/bin/bash

set -e

if [ `basename $PWD` != "src" ]; then
    pushd c_src
fi

unset CFLAGS LDFLAGS

make $1
