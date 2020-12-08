REPO            ?= riak
HEAD_REVISION   ?= $(shell git describe --tags --exact-match HEAD 2>/dev/null)
PKG_REVISION    ?= $(shell git describe --tags 2>/dev/null)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl 2>/dev/null) 2>/dev/null)
REBAR           ?= $(BASE_DIR)/rebar3
OVERLAY_VARS    ?=
TEST_IGNORE     ?= lager riak basho_bench
TEST_DEPS_DIR   ?= _build/test/lib
REL_DIR         ?= _build/default/rel
DEPS             = $(patsubst $(TEST_DEPS_DIR)/%, %, $(wildcard $(TEST_DEPS_DIR)/*))
TEST_DEPS        = $(filter-out $(TEST_IGNORE), $(DEPS))

RIAK_CORE_STAT_PREFIX = riak
export RIAK_CORE_STAT_PREFIX

EXOMETER_PACKAGES = "(basic)"
export EXOMETER_PACKAGES

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

.PHONY: rel stagedevrel deps

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(if $(HEAD_REVISION),$(warning "Warning: you have checked out a tag ($(HEAD_REVISION)) and should use the compile target"))
	$(REBAR) upgrade

clean: testclean
	$(REBAR) clean

distclean: clean devclean relclean ballclean
	@rm -rf _build

##
## Test targets
##
TEST_LOG_FILE := $(shell pwd)/eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

eunit: compile
	$(REBAR) eunit

# Tricking rebar3 to use the dependencies from the top-level _build directory.
# One needs to have ran `make eunit` before we can make test-deps to find
# directories in _build/test
testdep-% :
	@echo "--- Running EUnit tests for $* ---"
	@rm -rf _build/deptest+test _build/deptest
	@(cd $(TEST_DEPS_DIR)/$* && \
	  $(REBAR) eunit && \
	  (echo "Eunit: $* PASSED" >> $(TEST_LOG_FILE) ) ) || echo "Eunit: $* FAILED" >> $(TEST_LOG_FILE)

## need to make test before to get TEST_DEPS dir established
test-deps : compile testclean $(patsubst %, testdep-%, $(TEST_DEPS))
	echo Tested the dependencies: $(TEST_DEPS)

# Test each dependency individually in its own VM
test : testclean eunit test-deps
	@if test -s $(TEST_LOG_FILE) ; then \
             cat $(TEST_LOG_FILE) && \
             exit `cat $(TEST_LOG_FILE) | grep FAILED | wc -l`; \
        fi


##
## Release targets
##
rel: compile
	$(REBAR) as rel release
	cp -a _build/rel/rel/riak rel/

rel-rpm: compile
	$(REBAR) as rpm release
	cp -a _build/rpm/rel/riak rel/

rel-deb: compile
	$(REBAR) as deb release
	cp -a _build/deb/rel/riak rel/

relclean:
	rm -rf $(REL_DIR)
	rm -rf rel/riak

##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 8

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	rel/gen_dev dev$* rel/vars/dev_vars.config.src rel/vars/$*_vars.config
	$(REBAR) as dev release -o dev/dev$* --overlay_vars rel/vars/$*_vars.config

stagedev% : all
	rel/gen_dev dev$* rel/vars/dev_vars.config.src rel/vars/$*_vars.config
	$(REBAR) as dev release -o dev/dev$* --overlay_vars rel/vars/$*_vars.config

perfdev : all
	perfdev/bin/riak stop || :
	rm -rf perfdev
	mkdir -p perfdev
	rel/gen_dev $@ rel/vars/perf_vars.config.src rel/vars/perf_vars.config
	(cd rel && ../rebar generate target_dir=../perfdev overlay_vars=vars/perf_vars.config)
	$(foreach dep,$(wildcard deps/*), rm -rf perfdev/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) perfdev/lib;)

perf:
	perfdev/bin/riak stop || :
	perfdev/bin/riak start
	perfdev/bin/riak-admin wait-for-service riak_kv 'perfdev@127.0.0.1'
	escript apps/riak/src/riak_perf_smoke || :
	perfdev/bin/riak stop

devclean: clean
	rm -rf dev

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) rel/riak/lib;)

##
## Doc targets
##
docs:
	$(REBAR) edoc
	@cp -R apps/riak_core/doc doc/riak_core
	@cp -R apps/riak_kv/doc doc/riak_kv

orgs: orgs-doc orgs-README

orgs-doc:
	@emacs -l misc/orgbatch.el -batch --eval="(riak-export-doc-dir \"doc\" 'html)"

orgs-README:
	@emacs -l misc/orgbatch.el -batch --eval="(riak-export-doc-file \"README.org\" 'ascii)"
	@mv README.txt README

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: build_plt
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		_build/default/lib/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		_build/default/lib/*/ebin

dialyzer: check_plt
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) _build/default/lib/*/ebin

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)


## Create a dependency graph png
depgraph: graphviz
	@echo "Note: If you have nothing in deps/ this might be boring"
	@echo "Creating dependency graph..."
	@misc/mapdeps.erl | dot -Tpng -oriak.png
	@echo "Dependency graph created as riak.png"
graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))

##
## Version and naming variables for distribution and packaging
##

# Tag from git with style <tagname>-<commits_since_tag>-<current_commit_hash>
# Ex: When on a tag:            riak-1.0.3   (no commits since tag)
#     For most normal Commits:  riak-1.1.0pre1-27-g1170096
#                                 Last tag:          riak-1.1.0pre1
#                                 Commits since tag: 27
#                                 Hash of commit:    g1170096
REPO_TAG 	:= $(shell git describe --tags)

# Split off repo name
# Changes to 1.0.3 or 1.1.0pre1-27-g1170096 from example above
REVISION = $(shell echo $(REPO_TAG) | sed -e 's/^$(REPO)-//')

# Primary version identifier, strip off commmit information
# Changes to 1.0.3 or 1.1.0pre1 from example above
MAJOR_VERSION	?= $(shell echo $(REVISION) | sed -e 's/\([0-9.]*\)-.*/\1/')

# Name resulting directory & tar file based on current status of the git tag
# If it is a tagged release (PKG_VERSION == MAJOR_VERSION), use the toplevel
#   tag as the package name, otherwise generate a unique hash of all the
#   dependencies revisions to make the package name unique.
#   This enables the toplevel repository package to change names
#   when underlying dependencies change.
NAME_HASH = $(shell git hash-object distdir/$(CLONEDIR)/$(MANIFEST_FILE) 2>/dev/null | cut -c 1-8)
PKG_ID := $(REPO_TAG)

##
## Packaging targets
##

# Yes another variable, this one is repo-<generatedhash
# which differs from $REVISION that is repo-<commitcount>-<commitsha>
PKG_VERSION = $(shell echo $(PKG_ID) | sed -e 's/^$(REPO)-//')

package:
	mkdir rel/pkg/out/riak-$(PKG_ID)
	git archive --format=tar HEAD | gzip >rel/pkg/out/$(PKG_ID).tar.gz
	$(MAKE) -f rel/pkg/Makefile

packageclean:
	rm -rf rel/pkg/out/*


.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

# Package up a devrel to save time later rebuilding it
pkg-devrel: devrel
	echo -n $(PKG_REVISION) > VERSION
	tar -czf $(PKG_ID)-devrel.tar.gz dev/ VERSION
	rm -rf VERSION

pkg-rel: rel
	tar -czf $(PKG_ID)-rel.tar.gz -C rel/ .
