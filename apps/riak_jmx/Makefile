all: compile

compile:
	./rebar get-deps
	./rebar compile
	@cd java_src;make

test: compile
	./rebar eunit

clean:
	./rebar clean
	@cd java_src;make clean