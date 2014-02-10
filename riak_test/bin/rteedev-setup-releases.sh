#!/usr/bin/env bash

# Creates a mixed-version directory structure for running riak_test
# using rteedev-mixed.config settings. Should be run inside a directory
# that contains devrels for prior Riak EE releases. Easy way to create this
# is to use the rteedev-build-releases.sh script

set -e

: ${RTEE_DEST_DIR:="$HOME/rt/riak_ee"}
mkdir -p $RTEE_DEST_DIR

echo "Setting up releases from $(pwd):"
echo " - Creating $RTEE_DEST_DIR"

rm -rf $RTEE_DEST_DIR
mkdir $RTEE_DEST_DIR

count=$(ls */dev 2> /dev/null | wc -l)
if [ "$count" -ne "0" ]
then
    for rel in */dev; do
        vsn=$(dirname "$rel")
        echo " - Initializing $RTEE_DEST_DIR/$vsn"
        mkdir -p "$RTEE_DEST_DIR/$vsn"
        cp -p -P -R "$rel" "$RTEE_DEST_DIR/$vsn"
    done
else
    # This is useful when only testing with 'current'
    # The repo still needs to be initialized for current
    # and we don't want to bomb out if */dev doesn't exist
    touch $RTEE_DEST_DIR/.current_init
    echo "No devdirs found. Not copying any releases."
fi

cd $RTEE_DEST_DIR
git init 

## Some versions of git and/or OS require these fields
git config user.name "Riak Test"
git config user.email "dev@basho.com"

git add .
git commit -a -m "riak_test init" > /dev/null
echo " - Successfully completed initial git commit of $RTEE_DEST_DIR"
