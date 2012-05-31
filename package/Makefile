OS		= $(shell uname -s)
KERNEL		= $(shell uname -r)
ERLANG_BIN	= $(shell dirname $(shell which erl))
ifeq ($(OS),Linux)
ARCH		= $(shell uname -m)
PKGER		= $(shell cat /etc/redhat-release 2> /dev/null)
ifeq ($(PKGER),)
OSNAME		= Debian
PKGERDIR	= deb
else
OSNAME		= RedHat
PKGERDIR	= rpm
endif
endif
ifeq ($(OS),SunOS)
ARCH		= $(shell uname -p)
PKGERDIR	= solaris
DISTRO		= $(shell head -1 /etc/release|awk \
                   '{if ($$1 == "OmniOS") {print $$1} else {print "Solaris"}}')
OSNAME		= ${DISTRO}
endif
ifeq ($(OS),Darwin)
OSNAME		= OSX
ARCH		= $(shell uname -m)
PKGERDIR	= osx
BUILDDIR	= osxbuild
endif
ifeq ($(OS),FreeBSD)
OSNAME          = $(OS)
ARCH            = $(shell uname -p)
PKGERDIR        = freebsd
BUILDDIR        = bsdbuild
endif

DATE		= $(shell date +%Y-%m-%d)
VERSIONSTRING	= $(APP) ($(PKG_VERSION) $(DATE)) $(OSNAME) $(ARCH)

APP		 = $(shell echo "$(REPO)" | sed -e 's/_/-/g')
# Assumes CURDIR is .../$(APP)/package/$(APP)/
RIAK_PATH	?= ..
RELEASE		?=

$(APP)-$(PKG_VERSION).tar.gz: ../$(DISTNAME).tar.gz
	ln -s $< $@

pkgclean: $(PKGERDIR)/pkgclean
	rm -rf $(APP)-*.tar.gz working rpmbuild debuild packages osxbuild bsdbuild

pkgcheck:
	$(if $(DISTNAME),,$(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))
	$(if $(RELEASE),,$(error "You must provide a package release number via RELEASE= on the command line"))
	@echo "Packaging \"$(DISTNAME)\""

# The heavy lifting is done by the individual packager Makefiles
package: pkgcheck build

include $(PKGERDIR)/Makefile
