#!/usr/bin/env bash

# You need to use this script once to build a set of devrels for prior
# releases of Riak EE (for mixed version / upgrade testing). You should
# create a directory and then run this script from within that directory.
# I have ~/test-releases that I created once, and then re-use for testing.
#
# See rteedev-setup-releases.sh as an example of setting up mixed version layout
# for testing.

# Different versions of Riak EE were released using different Erlang versions,
# make sure to build with the appropriate version.

# This is based on my usage of having multiple Erlang versions in different
# directories. If using kerl or whatever, modify to use kerl's activate logic.
# Or, alternatively, just substitute the paths to the kerl install paths as
# that should work too.

R14B03=${R14B03:-$HOME/erlang-R14B03}
R14B04=${R14B04:-$HOME/erlang-R14B04}
R15B01=${R15B01:-$HOME/erlang-R15B01}

checkbuild()
{
    ERLROOT=$1

    if [ ! -d $ERLROOT ]; then
        echo -n " - $ERLROOT cannot be found, install with kerl? [Y|n]: "
        read ans
        if [[ $ans == n || $ans == N ]]; then
            echo
            echo " [ERROR] Can't build $ERLROOT without kerl, aborting!"
            exit 1
        else
            if [ ! -x kerl ]; then
                echo "   - Fetching kerl."
                curl -O https://raw.github.com/spawngrid/kerl/master/kerl > /dev/null 2>&1; chmod a+x kerl
            fi
        fi
    fi
}

kerl()
{
    RELEASE=$1
    BUILDNAME=$2

    echo " - Building Erlang $RELEASE (this could take a while)"
    ./kerl build $RELEASE $BUILDNAME  > /dev/null 2>&1
    echo " - Installing $RELEASE into $HOME/$BUILDNAME"
    ./kerl install $BUILDNAME $HOME/$BUILDNAME  > /dev/null 2>&1
}

build()
{
    SRCDIR=$1
    ERLROOT=$2
    DOWNLOAD=$3
    GITURL=$4

    echo "Building $SRCDIR:"

    checkbuild $ERLROOT
    if [ ! -d $ERLROOT ]; then
        BUILDNAME=`basename $ERLROOT`
        RELEASE=`echo $BUILDNAME | awk -F- '{ print $2 }'`
        kerl $RELEASE $BUILDNAME
    fi

    if [ -n "$DOWNLOAD" ]; then
        echo " - Fetching $DOWNLOAD"
        s3cmd get $DOWNLOAD

        TARBALL=`basename $DOWNLOAD`
        echo " - Expanding $TARBALL"
        tar xzf $TARBALL > /dev/null 2>&1
    fi

    if [ -n "$GITURL" ]; then
        echo " - Cloning $GITURL"
        git clone $GITURL $SRCDIR > /dev/null 2>&1
    fi

    echo " - Building devrel in $SRCDIR (this could take a while)"
    cd $SRCDIR

    RUN="env PATH=$ERLROOT/bin:$ERLROOT/lib/erlang/bin:$PATH \
             C_INCLUDE_PATH=$ERLROOT/usr/include \
             LD_LIBRARY_PATH=$ERLROOT/usr/lib"
    $RUN make all devrel > /dev/null 2>&1
    cd ..
    echo " - $SRCDIR built."
}

build "riak_ee-1.1.4" $R14B04 s3://builds.basho.com/riak_ee/1.1/1.1.4/riak_ee-1.1.4.tar.gz
echo
build "riak_ee-1.2.1" $R15B01 s3://builds.basho.com/riak_ee/1.2/1.2.1/riak_ee-1.2.1.tar.gz
echo
