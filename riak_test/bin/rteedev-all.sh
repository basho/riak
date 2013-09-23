#!/usr/bin/env bash

ORIGDIR=`pwd`
pushd `dirname $0` > /dev/null
SCRIPT_DIR=`pwd`
popd > /dev/null
CURRENT_OTP=${CURRENT_OTP:-$HOME/erlang-R16B02}

if [ -n "$DEBUG_RTDEV" ]; then
    echo "= Configuration ================================================="
    echo "Build dir:         $ORIGDIR"
    echo "rteedev-* scripts: $SCRIPT_DIR"
    echo "Erlang:            $CURRENT_OTP"
    echo
fi

echo "================== riak_test Omnibus Installer =================="
echo
echo "This is an omnibus script that builds all the necessary versions "
echo "of Erlang and Riak EE (including the latest from Github) for running"
echo "riak_test and installs them into $HOME/rt/riak_ee.   "
echo
echo -n "Are you sure you want to continue? [Y|n] "
read continue
if [[ $continue == n || $continue == N ]]; then
    echo
    echo "Aborting install!"
    exit 1
fi

echo
echo "= Building Riak EE Releases ========================================"
echo

source $SCRIPT_DIR/rteedev-build-releases.sh


echo "= Installing Riak EE Releases ======================================"
echo
source $SCRIPT_DIR/rteedev-setup-releases.sh

echo
echo "= Building and Installing Riak from Git ========================="
echo

cd $ORIGDIR
build "current" $CURRENT_OTP "" "git@github.com:/basho/riak_ee.git"
echo
cd current
source $SCRIPT_DIR/rteedev-current.sh

cd $ORIGDIR
echo
echo "= Build complete! ==============================================="
