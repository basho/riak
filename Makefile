
all: compile

compile:
	./rebar compile

clean:
	./rebar clean

# docs:
# 	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
# 	@cp -r doc/* www/edoc
# 	@cp README www/
# 	@cp LICENSE www/
# 	@cp TODO www/

# reldocs: docs
# 	@cd client_lib/java && make javadoc && \
# 		cp -r javadoc/* ../../www/java_client_api



# dialyzer: compile
# 	@dialyzer -Wno_return -c ebin/ | tee priv/log/dialyzer.log

# test: compile
# 	scripts/run_tests.escript ebin | tee test.log

