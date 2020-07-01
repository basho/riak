##
## Export all variables to sub-invocation
##
export

OS            = $(shell uname -s)
ERLANG_BIN   ?= $(shell dirname $(shell which erl))
DEPS_DIR     ?= deps

##
## Support RPM and Debian based linux systems
##
ifeq ($(OS),Linux)
ARCH          = $(shell uname -m)
ISRPM         = $(shell cat /etc/redhat-release 2> /dev/null)
ISDEB         = $(shell cat /etc/debian_version 2> /dev/null)
ISSLES        = $(shell cat /etc/SuSE-release 2> /dev/null)
ifneq ($(ISRPM),)
OSNAME        = RedHat
PKGERDIR      = rpm
BUILDDIR      = rpmbuild
else
ifneq ($(ISDEB),)
OSNAME        = Debian
PKGERDIR      = deb
BUILDDIR      = debuild
else
ifneq ($(ISSLES),)
OSNAME        = SLES
PKGERDIR      = rpm
BUILDDIR      = rpmbuild
endif  # SLES
endif  # deb
endif  # rpm
endif  # linux

ifeq ($(OS),Darwin)          # OSX
OSNAME        = OSX
ARCH          = $(shell file `which erlc` | grep -c x86_64 2> /dev/null | awk \
                       '{if ($$1 == "0") {print "i386"} else {print "x86_64"}}')
PKGERDIR      = osx
BUILDDIR      = osxbuild
endif

ifeq ($(OS),FreeBSD)
OSNAME        = FreeBSD
ARCH          = $(shell uname -m)
BUILDDIR      = fbsdbuild
PKGNG         = $(shell uname -r | awk -F. '{ print ($$1 > 9) ? "true" : "false" }')
ifeq ($(PKGNG),true)        # FreeBSD 10.0 or greater
PKGERDIR      = fbsdng
else                        # Older FreeBSD pkg_add
PKGERDIR      = fbsd
endif
endif

ifeq ($(OS),SunOS)         # Solaris flavors
KERNELVER     = $(shell uname -v | grep -c joyent 2> /dev/null)
ARCH          = $(shell file `which erlc` | grep -c 64-bit 2> /dev/null | awk \
                    '{if ($$1 == "0") {print "i386"} else {print "x86_64"}}')

ifneq ($(KERNELVER),0)       # SmartOS
OSNAME        = SmartOS
PKGERDIR      = smartos
BUILDDIR      = smartosbuild
else                         # Solaris / OmniOS
DISTRO        = $(shell head -1 /etc/release|awk \
                   '{if ($$1 == "OmniOS") {print $$1} else {print "Solaris"}}')
OSNAME        = ${DISTRO}
PKGERDIR      = solaris
BUILDDIR      = solarisbuild
endif

endif

DATE          = $(shell date +%Y-%m-%d)

# Default the package build version to 1 if not already set
PKG_BUILD    ?= 1

.PHONY: ostype varcheck

## Call platform dependent makefile
ostype: varcheck
	$(if $(PKGERDIR),,$(error "Operating system '$(OS)' not supported by node_package"))
	$(MAKE) -C $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID) -f $(BASE_DIR)/rel/pkg/$(PKGERDIR)/Makefile

## Check required settings before continuing
varcheck:
	$(if $(PKG_VERSION),,$(error "Variable PKG_VERSION must be set and exported, see basho/node_package readme"))
	$(if $(PKG_ID),,$(error "Variable PKG_ID must be set and exported, see basho/node_package readme"))
