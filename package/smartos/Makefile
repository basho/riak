BUILD_RIAK_PATH := $(BUILDDIR)/$(REPO)-$(PKG_VERSION)
BUILD_STAGE_DIR := $(BUILDDIR)/$(APP)

# Where we install things (based on vars.config)
# /opt/local based dirs
PMAN_DIR         := $(BUILD_STAGE_DIR)/man
PBIN_DIR         := $(BUILD_STAGE_DIR)/sbin
PETC_DIR         := $(BUILD_STAGE_DIR)/etc/riak
PLIB_DIR         := $(BUILD_STAGE_DIR)/lib/riak
PSMF_DIR         := $(BUILD_STAGE_DIR)/share/smf/riak
ESMF_DIR         := $(BUILD_STAGE_DIR)/share/smf/riak-epmd

# Recursive assignment of ERTS version
# We will know this after building the rel
ERTS_PATH        = $(shell ls $(BUILD_RIAK_PATH)/rel/riak | egrep -o "erts-.*")

# /var based dirs are handled in the +INSTALL file

TARNAME := $(APP)-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tar
PKGNAME := $(APP)-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tgz

# pkg_add on Smartos requires that the tar file
# and +CONTENTS files are in the exact same order
# so we need to pack up the tar in a very
# particular order to not anger pkg_add.
# This is done by piping the file listings into tar
#  in the same manner we did with the +CONTENTS file
build: packing_list_files
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILD_STAGE_DIR) && \
	tar -cf $(TARNAME) +CONTENTS +COMMENT +DESC +INSTALL +DEINSTALL +DISPLAY +BUILD_INFO
	cd $(BUILD_STAGE_DIR) && \
	find man -type f | tar -rf $(TARNAME) -T - && \
	find etc -type f | tar -rf $(TARNAME) -T - && \
	find lib -type f | tar -rf $(TARNAME) -T - && \
	find sbin -type f | tar -rf $(TARNAME) -T - && \
	find share -type f | tar -rf $(TARNAME) -T -

	cd $(BUILD_STAGE_DIR) && \
	gzip $(TARNAME) && \
	mv $(TARNAME).gz ../../packages/$(PKGNAME)

	cd packages && \
		for tarfile in `ls *.tgz`; do \
		shasum -a 256 $${tarfile} > $${tarfile}.sha \
	; done

# Where most of the magic (horror?) happens
# Create a packing list according to pkg_create(1)
# Write initial settings to a local plist then copy
#   to the destination folder where we use 'find'
#   to populate the files
packing_list_files: $(BUILD_STAGE_DIR) templates
	@echo "Adding to packaging list $(APP)-$(PKG_VERSION)"
	echo "@name $(APP)-$(PKG_VERSION)" >> plist
	echo "@pkgcfl riak-*" >> plist
	echo "@pkgdep sudo-[0-9]*" >> plist
	echo "@pkgdep openssl-0.9.8*" >> plist

	@echo "Copying staging package listing to +CONTENTS"
	mv plist $(BUILD_STAGE_DIR)/+CONTENTS

	@echo "Packaging /opt/local files"
	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /opt/local" >> +CONTENTS && \
	   echo "@owner root" >> +CONTENTS && \
	   echo "@group root" >> +CONTENTS && \
	   find man -type f >> +CONTENTS

	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /opt/local" >> +CONTENTS && \
	   echo "@owner root" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS && \
	   find etc -type f >> +CONTENTS && \
	   echo "@exec chmod o-r /opt/local/etc/riak/vm.args" >> +CONTENTS && \
	   echo "@exec chown -R root:riak /opt/local/etc/riak" >> +CONTENTS && \
	   echo "@exec chmod -R g+r+X /opt/local/etc/riak/" >> +CONTENTS

	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /opt/local" >> +CONTENTS && \
	   echo "@owner riak" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find lib -type f >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /opt/local/lib/riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@owner riak" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS && \
	   echo "@mode 0755" >> +CONTENTS && \
	   find sbin -type f >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@owner riak" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS && \
	   find share -type f >> +CONTENTS

	cd $(BUILD_STAGE_DIR) && \
	   echo "@display +DISPLAY" >> +CONTENTS


# These are static files that should not have to change often
templates: $(BUILD_STAGE_DIR)
	@echo "Copying metadata files to package"
	cp $(PKGERDIR)/+DESC $(PKGERDIR)/+COMMENT $(PKGERDIR)/+DEINSTALL \
           $(PKGERDIR)/+INSTALL $(PKGERDIR)/+BUILD_INFO \
	   $(PKGERDIR)/+DISPLAY $(BUILD_STAGE_DIR)
	mkdir -p $(PSMF_DIR)
	cp $(PKGERDIR)/manifest.xml $(PSMF_DIR)
	mkdir -p $(ESMF_DIR)
	sed -e "s/%ERTS_PATH%/${ERTS_PATH}/" < \
		$(PKGERDIR)/riak-epmd > $(ESMF_DIR)/riak-epmd
	cp $(PKGERDIR)/epmd-manifest.xml $(ESMF_DIR)/manifest.xml


# Copy the app rel directory to the staging directory to build our
# package structure and move the directories into the right place
# for the package, see the vars.config file for destination
# directories
# The data and log directories need to go into /var, but the
# smartos pkg_add doesn't allow this to be in the +CONTENTS
# file so the directories are created in the +INSTALL script
$(BUILD_STAGE_DIR): buildrel
	@echo "Copying rel directory to staging directory"
	mkdir -p $@
	cp -R $(BUILD_RIAK_PATH)/rel/riak $(BUILD_STAGE_DIR)
	mkdir -p $(PBIN_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/bin/* $(PBIN_DIR)
	mkdir -p $(PETC_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/etc/* $(PETC_DIR)
	mkdir -p $(PLIB_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/lib $(PLIB_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/erts-* $(PLIB_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/releases $(PLIB_DIR)
	echo "Copying man pages to staging directory"
	mkdir -p $(PMAN_DIR)
	cp -R $(BUILD_RIAK_PATH)/doc/man/man1 $(PMAN_DIR)



# Build the release we need to package
#  * Patch rel/files/riak with our version
#  * Ensure all binaries are executable
#  * copy the vars.config over for build config
buildrel: $(BUILD_RIAK_PATH)
	cp $(BUILD_RIAK_PATH)/rel/files/riak $(BUILD_RIAK_PATH)/rel/files/riak.tmp
	sed -e "s/^RIAK_VERSION.*$$/RIAK_VERSION=\"${VERSIONSTRING}\"/" < \
	       $(BUILD_RIAK_PATH)/rel/files/riak.tmp > $(BUILD_RIAK_PATH)/rel/files/riak
	cp $(PKGERDIR)/patches/* $(BUILD_RIAK_PATH)/rel/files/
	cd $(BUILD_RIAK_PATH)/rel/files && \
		patch -p3 < rel-files.patch
	cp -f $(PKGERDIR)/vars.config $(BUILD_RIAK_PATH)/rel/vars.config
	$(MAKE) -C $(BUILD_RIAK_PATH) deps rel
	chmod 0755 $(BUILD_RIAK_PATH)/rel/riak/bin/* $(BUILD_RIAK_PATH)/rel/riak/erts-*/bin/*



$(BUILD_RIAK_PATH): $(BUILDDIR) $(APP)-$(PKG_VERSION).tar.gz
	tar xz -C $(BUILDDIR) -f $(APP)-$(PKG_VERSION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	rm -rf $(BUILD_STAGE_DIR) $(BUILDDIR)
