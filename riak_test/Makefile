.PHONY: deps

deps:
	../rebar get-deps

compile: deps
	../rebar compile
	$(MAKE) -C deps/riak_test -f Makefile

clean:
	@../rebar clean

