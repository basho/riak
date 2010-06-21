RIAK_TAG		= $(shell hg identify -t)

.PHONY: rel deps

all: deps compile

compile:
	./rebar compile
	make -C apps/riak_jmx/java_src

deps:
	./rebar get-deps

clean:
	./rebar clean
	make -C apps/riak_jmx/java_src clean

distclean: clean devclean relclean ballclean
	./rebar delete-deps

test: 
	./rebar eunit

##
## Release targets
##
rel: deps
	make -C apps/riak_jmx/java_src
	./rebar compile generate 

relclean:
	rm -rf rel/riak

##
## Developer targets
##

devrel: dev1 dev2 dev3 dev4 dev5 dev6

dev: 
	mkdir dev
	cp -R rel/overlay rel/reltool.config dev
	make -C apps/riak_jmx/java_src
	./rebar compile && cd dev && ../rebar generate

dev1 dev2 dev3 dev4 dev5 dev6: dev
	yes n | cp -Ri dev/riak dev/$@
	mkdir -p dev/$@/data/ring
	mkdir -p dev/$@/data/snmp/agent/db
	$(foreach app,$(wildcard apps/*), rm -rf dev/$@/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$@/lib;)
	$(foreach dep,$(wildcard deps/*), rm -rf dev/$@/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$@/lib;)
	perl -pi -e 's/name riak/name $@/g' dev/$@/etc/vm.args
	perl -pi -e 's/web_port, \d+/web_port, 809$(subst dev,,$@)/g' \
                    dev/$@/etc/app.config
	perl -pi -e 's/pb_port, \d+/pb_port, 808$(subst dev,,$@)/g' \
                    dev/$@/etc/app.config
	perl -pi -e 's/handoff_port, \d+/handoff_port, 810$(subst dev,,$@)/g' \
                    dev/$@/etc/app.config
	perl -pi -e 's/intAgentUDPPort, \d+/intAgentUDPPort, 400$(subst dev,,$@)/g' \
			    dev/$@/etc/snmp/agent/conf/agent.conf


devclean: clean
	rm -rf dev

stage : rel
	cd rel/riak/lib && \
	rm -rf riak_core-* riak_kv-* && \
	ln -s ../../../apps/riak_core && \
	ln -s ../../../apps/riak_kv

##
## Doc targets
##
docs:
	@erl -noshell -run edoc_run application luke '"apps/luke"' '[]' 
	@cp -R apps/luke/doc doc/luke
	@erl -noshell -run edoc_run application riak_core '"apps/riak_core"' '[]' 
	@cp -R apps/riak_core/doc doc/riak_core
	@erl -noshell -run edoc_run application riak_kv '"apps/riak_kv"' '[]' 
	@cp -R apps/riak_kv/doc doc/riak_kv

orgs: orgs-doc orgs-README

orgs-doc:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-dir \"doc\" 'html)"

orgs-README:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-file \"README.org\" 'ascii)"
	@mv README.txt README

dialyzer: compile
	@dialyzer -Wno_return -c apps/riak/ebin

# Release tarball creation
# Generates a tarball that includes all the deps sources so no checkouts are necessary

distdir:
	$(if $(findstring tip,$(RIAK_TAG)),$(error "You can't generate a release tarball from tip"))
	mkdir distdir
	hg clone . distdir/riak-clone
	cd distdir/riak-clone; \
	hg archive ../$(RIAK_TAG); \
	mkdir ../$(RIAK_TAG)/deps; \
	make deps; \
	for dep in deps/*; do cd $${dep} && hg archive ../../../$(RIAK_TAG)/$${dep}; cd ../..; done

dist $(RIAK_TAG).tar.gz: distdir
	cd distdir; \
	tar czf ../$(RIAK_TAG).tar.gz $(RIAK_TAG)

ballclean:
	rm -rf $(RIAK_TAG).tar.gz distdir

