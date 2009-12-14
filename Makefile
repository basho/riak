.PHONY: rel

all: compile

compile:
	./rebar compile

clean:
	./rebar clean

test: 
	./rebar eunit

rel:
	./rebar compile generate force=1 dump_spec=1
	cp -R rel/overlay/* rel/riak

devrel: rel
	cp -Rp rel/riak rel/riak2
	perl -pi -e 's/name riak/name riak2/g' rel/riak2/etc/vm.args
	perl -pi -e 's/riak_web_port, \d+/riak_web_port, 8099/g' rel/riak2/etc/app.config
	cp -Rp rel/riak rel/riak3
	perl -pi -e 's/name riak/name riak3/g' rel/riak3/etc/vm.args
	perl -pi -e 's/riak_web_port, \d+/riak_web_port, 8100/g' rel/riak3/etc/app.config

devclean: clean
	rm -rf rel/riak*

docs:
	@erl -noshell -run edoc_run application riak '"apps/riak"' '[]' 
	@cp -r doc/* www/edoc
	@cp README www/
	@cp LICENSE www/
	@cp TODO www/

reldocs: docs
	@mkdir -p www/java_client_api
	@cd client_lib/java && make javadoc && \
            cp -R javadoc/* ../../www/java_client_api


dialyzer: compile
	@dialyzer -Wno_return -c apps/riak/ebin


