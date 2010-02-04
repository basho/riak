#!/bin/bash

export ERL_LIBS=`cd ..;pwd`;
erl -noshell -eval 'edoc:application(erlang_js, [{dir, "docs"}]), init:stop().'