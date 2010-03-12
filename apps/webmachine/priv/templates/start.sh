#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/webmachine/ebin $PWD/deps/webmachine/deps/mochiweb/ebin -boot start_sasl -s reloader -s {{appid}}
