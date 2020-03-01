REPO            ?= riak
HEAD_REVISION   ?= $(shell git describe --tags --exact-match HEAD 2>/dev/null)
PKG_REVISION    ?= $(shell git describe --tags 2>/dev/null)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl 2>/dev/null) 2>/dev/null)
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=
SPECIAL_DEPS	?= meck riak_ensemble webmachine
TEST_IGNORE     ?= rebar_lock_deps_plugin

RIAK_CORE_STAT_PREFIX = riak
export RIAK_CORE_STAT_PREFIX

EXOMETER_PACKAGES = "(basic)"
export EXOMETER_PACKAGES

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

.PHONY: rel stagedevrel deps

all: deps compile

compile:
	./rebar compile

deps:
	$(if $(HEAD_REVISION),$(warning "Warning: you have checked out a tag ($(HEAD_REVISION)) and should use the locked-deps target"))
	./rebar get-deps

clean: testclean
	./rebar clean

distclean: clean devclean relclean ballclean
	./rebar delete-deps

generate:
	./rebar generate $(OVERLAY_VARS)


##
## Lock Targets
##
##  see https://github.com/seth/rebar_lock_deps_plugin
lock: deps compile
	./rebar lock-deps

locked-all: locked-deps compile

locked-deps:
	@echo "Using rebar.config.lock file to fetch dependencies"
	./rebar -C rebar.config.lock get-deps

##
## Test targets
##
TEST_LOG_FILE := eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

# Test each dependency individually in its own VM
test : deps compile testclean
	@$(foreach dep, \
		$(filter-out $(TEST_IGNORE), \
			$(filter-out $(SPECIAL_DEPS), $(patsubst deps/%, %, $(wildcard deps/*)))), \
			(cd deps/$(dep) && REBAR_DEPS_DIR=$(BASE_DIR)/deps/ ../../rebar eunit deps_dir=.. skip_deps=true)  \
			|| echo "Eunit: $(dep) FAILED" >> $(TEST_LOG_FILE);)
	@$(foreach special, \
		$(filter-out $(TEST_IGNORE), $(SPECIAL_DEPS)), \
			(cd deps/$(special) && make test)  \
			|| echo "Eunit: $(special) FAILED" >> $(TEST_LOG_FILE);)
	./rebar eunit skip_deps=true
	@if test -s $(TEST_LOG_FILE) ; then \
             cat $(TEST_LOG_FILE) && \
             exit `wc -l < $(TEST_LOG_FILE)`; \
        fi

##
## Release targets
##
rel: locked-deps compile generate

relclean:
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
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

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

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)

devclean: clean
	rm -rf dev

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) rel/riak/lib;)

##
## Doc targets
##
docs:
	./rebar skip_deps=true doc
	@cp -R apps/riak_core/doc doc/riak_core
	@cp -R apps/riak_kv/doc doc/riak_kv

orgs: orgs-doc orgs-README

orgs-doc:
	@emacs -l misc/orgbatch.el -batch --eval="(riak-export-doc-dir \"doc\" 'html)"

orgs-README:
	@emacs -l misc/orgbatch.el -batch --eval="(riak-export-doc-file \"README.org\" 'ascii)"
	@mv README.txt README

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin | \
	    fgrep -v -f ./dialyzer.ignore-warnings

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


##
## Release tarball creation
## Generates a tarball that includes all the deps sources so no checkouts are necessary
##

# Use git archive make a clean copy of a repository at a current
# revision and copy to a new directory
archive_git = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

# Alternative to git archive to remove .git directory, but not any
# other files outside of the source tree (used for eleveldb which
# brings in leveldb)
clean_git = cp -R ../../$(1) $(2)/deps/ && find $(2)/$(1) -name .git -type d | xargs rm -rf

# Determines which function to call.  eleveldb is treated as a special case
archive = if [ "$(1)" = "deps/eleveldb" ]; then \
              $(call clean_git,$(1),$(2)); \
          else \
              $(call archive_git,$(1),$(2)); \
          fi


# Checkout tag, fetch deps (so we don't have to do it multiple times) and collect
# the version of all the dependencies into the MANIFEST_FILE
CLONEDIR ?= riak-clone
MANIFEST_FILE ?= dependency_manifest.git
get_dist_deps = mkdir distdir && \
                git clone . distdir/$(CLONEDIR) && \
                cd distdir/$(CLONEDIR) && \
                git checkout $(REPO_TAG) && \
                $(MAKE) locked-deps && \
                echo "- Dependencies and their tags at build time of $(REPO) at $(REPO_TAG)" > $(MANIFEST_FILE) && \
                for dep in deps/*; do \
                    cd $${dep} && \
                    printf "$${dep} version `git describe --long --tags 2>/dev/null || git rev-parse HEAD`\n" >> ../../$(MANIFEST_FILE) && \
                    cd ../..; done && \
                LC_ALL=POSIX && export LC_ALL && sort $(MANIFEST_FILE) > $(MANIFEST_FILE).tmp && mv $(MANIFEST_FILE).tmp $(MANIFEST_FILE);


# Name resulting directory & tar file based on current status of the git tag
# If it is a tagged release (PKG_VERSION == MAJOR_VERSION), use the toplevel
#   tag as the package name, otherwise generate a unique hash of all the
#   dependencies revisions to make the package name unique.
#   This enables the toplevel repository package to change names
#   when underlying dependencies change.
NAME_HASH = $(shell git hash-object distdir/$(CLONEDIR)/$(MANIFEST_FILE) 2>/dev/null | cut -c 1-8)
ifeq ($(REVISION), $(MAJOR_VERSION))
PKG_ID := $(REPO_TAG)
else
PKG_ID = $(REPO)-$(MAJOR_VERSION)-$(NAME_HASH)
endif

# To ensure a clean build, copy the CLONEDIR at a specific tag to a new directory
#  which will be the basis of the src tar file (and packages)
# The vsn.git file is required by rebar to be able to build from the resulting
#  tar file
build_clean_dir = cd distdir/$(CLONEDIR) && \
                  $(call archive_git,$(PKG_ID),..) && \
                  cp $(MANIFEST_FILE) ../$(PKG_ID)/ && \
                  mkdir ../$(PKG_ID)/deps && \
                  for dep in deps/*; do \
                      cd $${dep} && \
                           $(call archive,$${dep},../../../$(PKG_ID)) && \
                           mkdir -p ../../../$(PKG_ID)/$${dep}/priv && \
                           printf "`git describe --long --tags 2>/dev/null || git rev-parse HEAD`" > ../../../$(PKG_ID)/$${dep}/priv/vsn.git && \
                           cd ../..; \
                  done


distdir/$(CLONEDIR)/$(MANIFEST_FILE):
	$(if $(REPO_TAG), $(call get_dist_deps), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

distdir/$(PKG_ID): distdir/$(CLONEDIR)/$(MANIFEST_FILE)
	$(call build_clean_dir)

distdir/$(PKG_ID).tar.gz: distdir/$(PKG_ID)
	tar -C distdir -czf distdir/$(PKG_ID).tar.gz $(PKG_ID)

dist: distdir/$(PKG_ID).tar.gz
	cp distdir/$(PKG_ID).tar.gz .

ballclean:
	rm -rf $(PKG_ID).tar.gz distdir

pkgclean: ballclean
	rm -rf package

##
## Packaging targets
##

# Yes another variable, this one is repo-<generatedhash
# which differs from $REVISION that is repo-<commitcount>-<commitsha>
PKG_VERSION = $(shell echo $(PKG_ID) | sed -e 's/^$(REPO)-//')

package: distdir/$(PKG_ID).tar.gz
	ln -s distdir package
	$(MAKE) -C package -f $(PKG_ID)/deps/node_package/Makefile

.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

# Package up a devrel to save time later rebuilding it
pkg-devrel: locked-deps devrel
	echo -n $(PKG_REVISION) > VERSION
	tar -czf $(PKG_ID)-devrel.tar.gz dev/ VERSION
	rm -rf VERSION

pkg-rel: locked-deps rel
	tar -czf $(PKG_ID)-rel.tar.gz -C rel/ .
