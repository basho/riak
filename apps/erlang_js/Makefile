all:
	./rebar compile

verbose:
	./rebar compile verbose=1

clean:
	rm -rf tests_ebin docs
	./rebar clean
	cd c_src;make clean

test: all
	@mkdir -p tests_ebin
	@cd tests;erl -make
	@erl -noshell -boot start_sasl -pa ebin -pa tests_ebin -s erlang_js -eval 'test_suite:test().' -s init stop
	@rm -f ebin/test_* ebin/*_tests.erl

docs: all
	@mkdir -p docs
	@./build_docs.sh
