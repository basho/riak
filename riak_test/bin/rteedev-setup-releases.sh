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
for rel in */dev; do
    vsn=$(dirname "$rel")
    echo " - Initializing $RTEE_DEST_DIR/$vsn"
    mkdir "$RTEE_DEST_DIR/$vsn"
    cp -p -P -R "$rel" "$RTEE_DEST_DIR/$vsn"
done
cd $RTEE_DEST_DIR
echo " - Creating the git repository"
git init > /dev/null 2>&1

## Some versions of git and/or OS require these fields
git config user.name "Riak Test"
git config user.email "dev@basho.com"

git add .
git commit -a -m "riak_test init" > /dev/null 2>&1
