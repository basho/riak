all:
	./rebar compile

clean:
	./rebar clean

test: all
	@cd tests;erl -make
	@erl -noshell -boot start_sasl -pa ebin -s erlang_js -eval 'test_suite:test().' -s init stop
	@rm -f ebin/test_* ebin/*_tests.erl
