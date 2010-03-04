#!/bin/bash

NAME=$1
DESTDIR=$2

if [ -z $NAME ] || [[ $NAME =~ ^[\.\~\/] ]]; then
    echo "usage: new_webmachine.sh name [destdir]"
    exit 1
fi

if [ -z $DESTDIR ]; then
    DESTDIR="."
elif [[ $DESTDIR =~ /${NAME}$ ]]; then
    DESTDIR=${DESTDIR%/*}
fi

if [ ! -e $DESTDIR ]; then
    $(mkdir -p $DESTDIR)
fi

ABSDEST=$(cd $DESTDIR && pwd)

cd ${0%/*}/../priv

../rebar create template=wmskel appid=$NAME prefix=$ABSDEST/$NAME
