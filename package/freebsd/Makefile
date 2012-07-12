BUILD_RIAK_PATH = $(BUILDDIR)/$(APP)-$(PKG_VERSION)
BUILD_STAGE_DIR = $(BUILDDIR)/$(APP)

# Where we install things (based on vars.config)
# /usr/local based dirs
PMAN_DIR         = $(BUILD_STAGE_DIR)/man
PBIN_DIR         = $(BUILD_STAGE_DIR)/sbin
PETC_DIR         = $(BUILD_STAGE_DIR)/etc/riak
PLIB_DIR         = $(BUILD_STAGE_DIR)/lib/riak
# /var based dirs
PDATA_DIR        = $(BUILD_STAGE_DIR)/db/riak
PLOG_DIR         = $(BUILD_STAGE_DIR)/log/riak

TARNAME = $(APP)-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tar
PKGNAME = $(APP)-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tbz

build: packing_list_files templates
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILD_STAGE_DIR) && \
	tar -cf $(TARNAME) +CONTENTS +COMMENT +DESC +MTREE_DIRS +DEINSTALL +DISPLAY
	cd $(BUILD_STAGE_DIR) && \
	find man -type f | tar -rf $(TARNAME) -T - && \
	find sbin -type f | tar -rf $(TARNAME) -T - && \
	find lib -type f | tar -rf $(TARNAME) -T - && \
	find etc -type f | tar -rf $(TARNAME) -T - && \
	find db -type f | tar -rf $(TARNAME) -T - && \
	find log -type f | tar -rf $(TARNAME) -T -

	cd $(BUILD_STAGE_DIR) && \
	gzip $(TARNAME) && \
	mv $(TARNAME).gz ../../packages/$(PKGNAME)

	cd packages && \
		for tarfile in `ls *.tbz`; do \
		shasum -a 256 $${tarfile} > $${tarfile}.sha \
	; done

# Where most of the magic (horror?) happens
# Create a packing list according to pkg_create(1)
# Write initial settings to a local file then copy
#   to the destination folder where we use 'find'
#   to populate the files and directories
# Regarding @owner and @group callouts
#   They work on files, but not on directories.
#   Also, the last callout should be blank to revert
#   back to the default so the /var/db/pkg directory
#   doesn't get set to something bad (which is a 
#   horrible bug in pkg_add).
packing_list_files: $(BUILD_STAGE_DIR)
	@echo "Adding to packaging list $(APP)-$(PKG_VERSION)"
	echo "@name $(APP)-$(PKG_VERSION)" >> plist
	echo "@conflicts riak-*" >> plist
	echo "@exec if ! pw groupshow riak 2>/dev/null; then pw groupadd riak; fi" >> plist
	echo "@exec if ! pw usershow riak 2>/dev/null; then pw useradd riak -g riak -h - -d /usr/local/lib/riak -s /bin/sh -c \"Riak Server\"; fi" >> plist
	echo "@pkgdep openssl-1.0.0_7" >> plist
	echo "@comment ORIGIN:basho/riak" >> plist

	@echo "Copying Man pages to staging directory"
	mkdir -p $(PMAN_DIR)
	cp -R $(BUILD_RIAK_PATH)/doc/man/man1 $(PMAN_DIR)
	echo "@cwd /usr/local" >> plist
	echo "@owner root" >> plist
	echo "@group wheel" >> plist
	echo "man/man1/riak-admin.1.gz" >> plist
	echo "man/man1/riak.1.gz" >> plist
	echo "man/man1/search-cmd.1.gz" >> plist

	@echo "Copying staging package listing to +CONTENTS"
	mv plist $(BUILD_STAGE_DIR)/+CONTENTS

	@echo "Packaging /usr/local files"
	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /usr/local" >> +CONTENTS && \
	   echo "@owner riak" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS && \
	   echo "@mode 0755" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find sbin -type f >> +CONTENTS && \
           find sbin -d -type d -exec echo "@dirrm {}" \; >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find lib -type f >> +CONTENTS && \
	   find lib -d -type d -exec echo "@dirrm {}" \; >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /usr/local/lib/riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@owner root" >> +CONTENTS && \
	   echo "@group wheel" >> +CONTENTS && \
	   echo "@mode 0644" >> +CONTENTS && \
	   find etc -type f >> +CONTENTS && \
	   find etc -d -type d -exec echo "@dirrm {}" \; >> +CONTENTS

	@echo "Packaging /var files"
	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /var" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@owner riak" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find db -type f >> +CONTENTS && \
           find db -d -type d -exec echo "@dirrm {}" \; >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /var/db/riak" >> +CONTENTS && \
	   echo "@exec chmod 700 /var/db/riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find log -type f >> +CONTENTS && \
           find log -d -type d -exec echo "@dirrm {}" \; >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /var/log/riak" >> +CONTENTS && \
	   echo "@exec chmod 700 /var/log/riak" >> +CONTENTS

	cd $(BUILD_STAGE_DIR) && \
	   echo "@owner" >> +CONTENTS && \
	   echo "@group" >> +CONTENTS && \
	   echo "@mode"  >> +CONTENTS && \
	   echo "@display +DISPLAY" >> +CONTENTS


templates: $(BUILD_STAGE_DIR)
	@echo "Copying metadata files to package"
	cp $(PKGERDIR)/+DESC $(PKGERDIR)/+COMMENT $(PKGERDIR)/+DEINSTALL \
           $(PKGERDIR)/+MTREE_DIRS $(PKGERDIR)/+DISPLAY \
           $(BUILD_STAGE_DIR)


# Copy the app rel directory to the staging directory to build our
# package structure and move the directories into the right place
# for the package, see the vars.config file for destination
# directories
# The data and log directories are by default empty, so create
# an empty file in them so the file listing makes the directories
$(BUILD_STAGE_DIR): buildrel
	@echo "Copying rel directory to staging directory"
	mkdir -p $@
	mkdir -p $(PBIN_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/bin/* $(PBIN_DIR)
	mkdir -p $(PETC_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/etc/* $(PETC_DIR)
	mkdir -p $(PLIB_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/lib $(PLIB_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/erts-* $(PLIB_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/releases $(PLIB_DIR)
	mkdir -p $(PDATA_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/data/* $(PDATA_DIR)
	touch $(PDATA_DIR)/.empty
	mkdir -p $(PLOG_DIR)
	touch $(PLOG_DIR)/.empty


# Build the release we need to package
#  * Ensure all binaries are executable
#  * copy the vars.config over for build config
buildrel: $(BUILD_RIAK_PATH)
	cp $(BUILD_RIAK_PATH)/rel/files/riak $(BUILD_RIAK_PATH)/rel/files/riak.tmp
	sed -e "s/^RIAK_VERSION.*$$/RIAK_VERSION=\"${VERSIONSTRING}\"/" < \
	       $(BUILD_RIAK_PATH)/rel/files/riak.tmp > $(BUILD_RIAK_PATH)/rel/files/riak
	cp -f $(PKGERDIR)/vars.config $(BUILD_RIAK_PATH)/rel/vars.config
	$(MAKE) -C $(BUILD_RIAK_PATH) deps rel
	chmod 0755 $(BUILD_RIAK_PATH)/rel/riak/bin/* $(BUILD_RIAK_PATH)/rel/riak/erts-*/bin/*



$(BUILD_RIAK_PATH): $(BUILDDIR) $(APP)-$(PKG_VERSION).tar.gz
	tar xz -C $(BUILDDIR) -f $(APP)-$(PKG_VERSION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	rm -rf $(BUILD_STAGE_DIR) $(BUILDDIR)
