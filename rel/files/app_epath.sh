#!/bin/sh

## ---------------------------------------------------------------------
##
## app_epath.sh: Parse Erlang app.config with POSIX tools for automation
##
## Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
##
## This file is provided to you under the Apache License,
## Version 2.0 (the "License"); you may not use this file
## except in compliance with the License.  You may obtain
## a copy of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing,
## software distributed under the License is distributed on an
## "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
## KIND, either express or implied.  See the License for the
## specific language governing permissions and limitations
## under the License.
##
## ---------------------------------------------------------------------

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
    # Remove all lines containing comments
    # Remove all blank lines
    # Remove the first [
    # Remove the last ].
    # Remove all blank lines again (just in case)
    appconfig=`cat $1 \
                   | sed '/^[ \t]*%/d' \
                   | sed '/^[ \t]*$/d' \
                   | sed -e '/\[/{s///;:a' -e '$!N;$!ba' -e '}' \
                   | sed '$ s/\]\.//g' \
                   | sed '/^[ \t]*$/d'`

    STACK=
    INQUOTE=0
    # The awk puts each char, spaces included, on their own line for parsing.
    echo "$appconfig" | awk '{gsub(//,"\n");print}' | while read i; do
        case "x${i}x" in
            "x\"x")
                # Flip the INQUOTE state and echo the quote
                if [ 1 -eq $INQUOTE ]; then
                    INQUOTE=0
                else
                    INQUOTE=1
                fi
                STACK="${STACK}${i}"
                ;;
            "xx")
                if [ 1 -eq $INQUOTE ]; then
                    # If in quotes, keep this space
                    STACK="${STACK} "
                fi
                ;;
            "x,x")
                if [ 1 -eq $INQUOTE ]; then
                    # If in quotes, keep this comma
                    STACK="${STACK},"
                else
                    # Commas outside quotes means a new item is next
                    STACK="${STACK} "
                fi
                ;;
            "x{x")
                if [ 1 -eq $INQUOTE ]; then
                    # If in quotes, keep this bracket
                    STACK="${STACK}{"
                else
                    # Add brace to the stack; will pop off from here on next }
                    STACK="${STACK} __EBRACE__ "
                fi
                ;;
            "x}x")
                if [ 1 -eq $INQUOTE ]; then
                    # If in quotes, keep this bracket
                    STACK="${STACK}}"
                else
                    # We're only interested in printing leaves, not
                    # intermediates like 'riak_core http', which contain lists.
                    # See if the current stack ends with ] (end of list).
                    echo $STACK | grep -E "__EBRACKET__$" >/dev/null 2>&1
                    if [ 1 -eq $? ]; then
                        # If not, print the stack without all of the mess.
                        echo "$STACK" | \
                            sed 's/ *__EBRACE__//g;s/ *__EBRACKET__//g;s/^ *//'
                    fi
                    # Pop off everything from the last brace on.
                    STACK=`echo "$STACK" | sed 's/\(.*\) __EBRACE__.*/\1/g'`
                fi
                ;;
            "x[x")
                if [ 1 -eq $INQUOTE ]; then
                    # If in quotes, keep this bracket
                    STACK="${STACK}["
                else
                    # Add a placeholder to aid in determining whether or not to
                    # print. That is, we don't want to print 'riak_core http'.
                    STACK="${STACK} __EBRACKET__ "
                fi
                ;;
            "x]x")
                if [ 1 -eq $INQUOTE ]; then
                    # If in quotes, keep this bracket
                    STACK="${STACK}]"
                fi
                # Don't actually do anything with ], as the starting brackets
                # are instead removed with }.
                ;;
            *)
                # Anything else is just pushed.
                STACK="${STACK}${i}"
                ;;
        esac
    done
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

