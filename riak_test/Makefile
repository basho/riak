.PHONY: deps

all: clean deps compile

deps:
	../rebar get-deps

compile: deps
	../rebar compile

clean:
	@../rebar clean
