OS		= $(shell uname -s)
KERNEL		= $(shell uname -r)
ifeq ($(OS),Linux)
PKGER		= $(shell cat /etc/redhat-release 2> /dev/null)
ifeq ($(PKGER),)
PKGER		= debuild
PKGERDIR	= deb
else
PKGER		= rpmbuild
PKGERDIR	= rpm
endif
endif
ifeq ($(OS),SunOS)
PKGER		= make
PKGERDIR	= solaris
DISTRO		= $(shell awk '{ if (NR==1) print $$1; };' /etc/release)
endif

APP		 = $(shell echo "$(REPO)" | sed -e 's/_/-/g')
# Assumes CURDIR is .../$(APP)/package/$(APP)/
RIAK_PATH	?= ..
RELEASE		?= 

$(APP)-$(REVISION).tar.gz: ../$(RIAK_TAG).tar.gz
	ln -s $< $@

pkgclean: $(PKGERDIR)/pkgclean
	rm -rf $(APP)-$(REVISION).tar.gz working rpmbuild debuild packages

pkgcheck:
	$(if $(RIAK_TAG),,$(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))
	$(if $(RELEASE),,$(error "You must provide a package release number via RELEASE= on the command line"))
	@echo "Packaging \"$(RIAK_TAG)\""

# The heavy lifting is done by the individual packager Makefiles
package: pkgcheck build

include $(PKGERDIR)/Makefile
