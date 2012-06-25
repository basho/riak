BUILD_RIAK_PATH = $(BUILDDIR)/$(APP)-$(PKG_VERSION)
BUILD_STAGE_DIR = $(BUILDDIR)/$(APP)

# Where we install things (based on vars.config)
# /usr/local based dirs
PMAN_DIR         = $(BUILD_STAGE_DIR)/man/
PBIN_DIR         = $(BUILD_STAGE_DIR)/sbin/
PETC_DIR         = $(BUILD_STAGE_DIR)/etc/riak/
PLIB_DIR         = $(BUILD_STAGE_DIR)/lib/riak/
# /var based dirs
PDATA_DIR        = $(BUILD_STAGE_DIR)/db/riak/
PLOG_DIR         = $(BUILD_STAGE_DIR)/log/riak/

PKGNAME = $(APP)-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tbz

build: packing_list_files
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILD_STAGE_DIR) && \
	tar -cjf ../../packages/$(PKGNAME) *
	cd packages && \
		for tarfile in `ls *.tbz`; do \
		shasum -a 256 $${tarfile} > $${tarfile}.sha \
	; done

# Where most of the magic happens
# Create a packing list according to pkg_create(1)
# Write initial settings to a local file then copy
#   to the destination folder where we use 'find'
#   to populate the files and directories
packing_list_files: var_plist usr_local_plist
	cd $(BUILD_STAGE_DIR) && \
	   echo "@display +DISPLAY" >> +CONTENTS

var_plist: build_stage_dir_complete
	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /var" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find db -type f >> +CONTENTS && \
	   find db -type d -exec echo "@dirrm {}" \; >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /var/db/riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find log -type f >> +CONTENTS && \
	   find log -type d -exec echo "@dirrm {}" \; >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /var/log/riak" >> +CONTENTS

usr_local_plist: build_stage_dir_complete
	cd $(BUILD_STAGE_DIR) && \
	   echo "@cwd /usr/local" >> +CONTENTS && \
	   echo "@owner riak" >> +CONTENTS && \
	   echo "@group riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find sbin -type f >> +CONTENTS && \
           find sbin -type d -exec echo "@dirrm {}" \; >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@owner" >> +CONTENTS && \
	   echo "@group" >> +CONTENTS && \
	   find etc -type f >> +CONTENTS && \
           find etc -type d -exec echo "@dirrm {}" \; >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find lib -type f >> +CONTENTS && \
           find lib -type d -exec echo "@dirrm {}" \; >> +CONTENTS && \
	   echo "@exec chown -R riak:riak /usr/local/lib/riak" >> +CONTENTS



# Meta-target for all staging directory tasks to be completed
build_stage_dir_complete: $(BUILD_STAGE_DIR) plist_header manpages templates
	mv plist $(BUILD_STAGE_DIR)/+CONTENTS

templates: $(BUILD_STAGE_DIR)
	@echo "Copying metadata files to package"
	cp $(PKGERDIR)/+DESC $(PKGERDIR)/+COMMENT $(PKGERDIR)/+DEINSTALL \
           $(PKGERDIR)/+MTREE_DIRS $(PKGERDIR)/+DISPLAY \
           $(BUILD_STAGE_DIR)

# Install man pages and add to package listing
manpages: $(BUILD_STAGE_DIR) plist_header
	@echo "Copying Man pages to staging directory"
	mkdir -p $(MAN_DIR)
	cp -R $(BUILD_RIAK_PATH)/doc/man/man1 $(MAN_DIR)
	echo "man/man1/riak-admin.1.gz" >> plist
	echo "man/man1/riak.1.gz" >> plist
	echo "man/man1/search-cmd.1.gz" >> plist

# Create the header of the plist
# Anything listed dynamically should be done in another makefile target
plist_header: $(BUILD_STAGE_DIR)
	@echo "Adding to packaging list $(APP)-$(PKG_VERSION)"
	echo "@name $(APP)-$(PKG_VERSION)" >> plist
	echo "@conflicts riak-*" >> plist
	echo "@exec if ! pw groupshow riak 2>/dev/null; then pw groupadd riak; fi" >> plist
	echo "@exec if ! pw usershow riak 2>/dev/null; then pw useradd riak -g riak -h - -d /usr/local/riak -s /bin/sh -c \"Riak Server\"; fi" >> plist
	echo "@owner riak" >> plist
	echo "@group riak" >> plist
	echo "@pkgdep openssl-1.0.0_7" >> plist
	echo "@comment ORIGIN:basho/riak" >> plist

# Copy the app rel directory to the staging directory to build our
# package structure and move the directories into the right place
# for the package, see the vars.config file for destination
# directories
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
	mkdir -p $(PDATA_DIR)
	cp -R $(BUILD_RIAK_PATH)/rel/riak/data/* $(PDATA_DIR)
	mkdir -p $(PLOG_DIR)


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
