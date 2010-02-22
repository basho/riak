.PHONY: rel

all: compile

compile:
	./rebar compile

clean:
	./rebar clean

distclean: clean devclean relclean
	@cd apps/erlang_js;make realclean

test: 
	./rebar eunit

##
## Release targets
##
rel:
	./rebar compile generate 

relclean:
	rm -rf rel/riak

##
## Developer targets
##

devrel: dev1 dev2 dev3

dev: 
	mkdir dev
	cp -R rel/overlay rel/reltool.config dev
	./rebar compile && cd dev && ../rebar generate

dev1 dev2 dev3: dev
	cp -Rn dev/riak dev/$@
	rm -rf dev/$@/data
	mkdir -p dev/$@/data/ring
	$(foreach app,$(wildcard apps/*), rm -rf dev/$@/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$@/lib;)
	perl -pi -e 's/name riak/name $@/g' dev/$@/etc/vm.args
	perl -pi -e 's/riak_web_port, \d+/riak_web_port, 809$(subst dev,,$@)/g' \
                    dev/$@/etc/app.config
	perl -pi -e 's/riak_handoff_port, \d+/riak_handoff_port, 810$(subst dev,,$@)/g' \
                    dev/$@/etc/app.config

devclean: clean
	rm -rf dev

##
## Doc targets
##
docs:
	@erl -noshell -run edoc_run application riak '"apps/riak"' '[]' 
	@cp -R apps/riak/doc doc/riak

reldocs: docs
	@mkdir -p www/java_client_api
	@cd client_lib/java && make javadoc && \
            cp -R javadoc/* ../../www/java_client_api

orgs: orgs-doc orgs-README

orgs-doc:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-dir \"doc\" 'html)"

orgs-README:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-file \"README.org\" 'ascii)"
	@mv README.txt README

dialyzer: compile
	@dialyzer -Wno_return -c apps/riak/ebin


