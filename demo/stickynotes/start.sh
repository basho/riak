#!/bin/sh
cd `dirname $0`
exec erl -name stickynotes@127.0.0.1 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s stickynotes
