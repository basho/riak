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
