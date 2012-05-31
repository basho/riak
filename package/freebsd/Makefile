BUILD_RIAK_PATH = $(BUILDDIR)/$(REPO)-$(PKG_VERSION)
BUILD_STAGE_DIR = $(BUILDDIR)/$(APP)
PKGNAME = $(APP)-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tbz

build: packing_list templates
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
packing_list: $(BUILD_STAGE_DIR)
	@echo "Adding to packaging list $(APP)-$(PKG_VERSION)"
	echo "@name $(APP)-$(PKG_VERSION)" >> plist
	echo "@cwd /usr/local" >> plist
	echo "@conflicts riak-*" >> plist
	echo "@conflicts riak-ee-*" >> plist
	echo "@pkgdep openssl-1.0.1_2" >> plist
	echo "@exec if ! pw groupshow riak 2>/dev/null; then pw groupadd riak; fi" >> plist
	echo "@exec if ! pw usershow riak 2>/dev/null; then pw useradd riak -g riak -h - -d /usr/local/riak -s /bin/sh -c \"Riak Server\"; fi" >> plist
	echo "@owner riak" >> plist
	echo "@group riak" >> plist
	mv plist $(BUILD_STAGE_DIR)/+CONTENTS
	cd $(BUILD_STAGE_DIR) && \
           find riak -type f >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   find riak -type d -exec echo "@dirrm {}" \; >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@exec chown -R riak:riak /usr/local/riak" >> +CONTENTS
	cd $(BUILD_STAGE_DIR) && \
	   echo "@display +DISPLAY" >> +CONTENTS

templates: $(BUILD_STAGE_DIR)
	@echo "Copying metadata files to package"
	cp $(PKGERDIR)/+DESC $(PKGERDIR)/+COMMENT $(PKGERDIR)/+DEINSTALL \
           $(PKGERDIR)/+MTREE_DIRS $(PKGERDIR)/+DISPLAY \
           $(BUILD_STAGE_DIR)

# Copy the app rel directory to the staging directory to build our
# package structure
$(BUILD_STAGE_DIR): buildrel
	@echo "Copying rel directory to staging directory"
	mkdir -p $@
	cp -r $(BUILD_RIAK_PATH)/rel/riak $(BUILD_STAGE_DIR)
	mkdir -p $(BUILD_STAGE_DIR)/riak/data/snmp/agent/db
	touch $(BUILD_STAGE_DIR)/riak/data/snmp/agent/db/empty

# Build the release we need to package
#  Also ensure all binaries are executable
#  and copy the vars.config over for build config
buildrel: $(BUILD_RIAK_PATH)
	cp $(BUILD_RIAK_PATH)/rel/files/riak $(BUILD_RIAK_PATH)/rel/files/riak.tmp
	sed -e "s/^RIAK_VERSION.*$$/RIAK_VERSION=\"${VERSIONSTRING}\"/" < \
	       $(BUILD_RIAK_PATH)/rel/files/riak.tmp > $(BUILD_RIAK_PATH)/rel/files/riak
	cp -f $(PKGERDIR)/vars.config $(BUILD_RIAK_PATH)/rel/vars.config
	$(MAKE) -C $(BUILD_RIAK_PATH) deps compile
	cp $(BUILD_RIAK_PATH)/deps/riak_snmp/mibs/* \
		$(BUILD_RIAK_PATH)/deps/riak_snmp/priv/mibs/
	$(MAKE) -C $(BUILD_RIAK_PATH) rel
	chmod 0755 $(BUILD_RIAK_PATH)/rel/riak/bin/* $(BUILD_RIAK_PATH)/rel/riak/erts-*/bin/*



$(BUILD_RIAK_PATH): $(BUILDDIR) $(APP)-$(PKG_VERSION).tar.gz
	tar xz -C $(BUILDDIR) -f $(APP)-$(PKG_VERSION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	rm -rf $(BUILD_STAGE_DIR) $(BUILDDIR)
