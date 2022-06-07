fmt() {
    echo -n "[["
    local n=$#
    if [ $n -ne 0 ]; then
        echo -n "\"$1\""
        shift
        n=$((n-1))
    fi
    while [ $n -ne 0 ]; do
        echo -n ", \"$1\""
        shift
        n=$((n-1))
    done
    echo "]]"
}

rpc() {
    local mod=$1
    local fun=$2
    shift 2
    if [ $# -gt 0 ]; then
        "${PLATFORM_BIN_DIR}/riak" rpc $mod $fun `fmt "$@"`
    else
        "${PLATFORM_BIN_DIR}/riak" rpc $mod $fun "[[]]"
    fi
}

rpc_raw() {
    local mod=$1
    local fun=$2
    "${PLATFORM_BIN_DIR}/riak" rpc $mod $fun "$3"
}

## Example usage:
#
# #!/bin/sh
#
# # Load the functions
# . path/to/app_epath.sh
#
# # Build the path info
# epaths=`make_app_epaths path/to/app.config`
#
# # View all of the settings. Quotes are important.
# echo "$epaths"
#
# # Grep around in the paths for various items.
# echo "$epaths" | grep 'riak_core ring_creation_size'
# echo "$epaths" | grep "lager handlers lager_file_backend" | grep info
#
# # Use the epath function to get directly at settings
# epath 'riak_core ring_creation_size' "$epaths"
# epath 'riak_core platform_bin_dir' "$epaths"
# epath 'riak_kv storage_backend' "$epaths"
#
# # Use epath to view all of the riak_core settings
# epath riak_core
#
## End example

## Here is a command you can put in your path for general cli use.
#
# #!/bin/sh
#
# # $1 quoted eterm path for search: 'riak_core ring_creation_size'
# # $2 path/to/app.config
#
# . path/to/app_epath.sh
#
# epaths=`make_app_epaths "$2"`
# epath "$1" "$epaths"
#
## End epath command

# make_app_epaths takes a path to an app.config file as its argument and
# and returns (prints) a flattened text structure of configuration settings.

make_app_epaths () {
    ERTS_VER=$(cd ${PLATFORM_BASE_DIR} && ls -d erts-*)
    ERTS_PATH="${PLATFORM_BASE_DIR}/$ERTS_VER/bin"
    $ERTS_PATH/escript $riak_lib_dir/app_epath.escript "$1"
}

epath () {
    # arg1   - a pattern to search for
    # arg2   - output of make_app_epaths, passed in quoted
    # output - search of arg2 for arg1, trimming arg1 from the beginning
    #          Note: there may be multiple lines of output.
    pat=$1
    shift
    echo "$*" | grep "$pat " | sed "s/^${pat} *//"
}
