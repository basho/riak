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
	./rebar skip_deps=true eunit

##
## Release targets
##
rel: deps
	./rebar compile generate

relclean:
	rm -rf rel/riak

##
## Developer targets
##

devrel: dev1 dev2 dev3 

dev1 dev2 dev3:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)


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
	./rebar skip_deps=true doc
	@cp -R apps/luke/doc doc/luke
	@cp -R apps/riak_core/doc doc/riak_core
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
	hg clone -u $(RIAK_TAG) . distdir/riak-clone
	cd distdir/riak-clone; \
	hg update -r $(RIAK_TAG)
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

