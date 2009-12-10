.PHONY: rel

all: compile

compile:
	./rebar compile

clean:
	./rebar clean

test: 
	./rebar eunit

rel:
	./rebar generate force=1 dump_spec=1
	cp -R rel/overlay/* rel/riak

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


