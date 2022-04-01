export

BUILDDIR        = $(shell pwd)
BUILD_STAGE_DIR = $(BUILDDIR)/riak

# Where we install things (based on vars.config)
# /usr/local based dirs
PMAN_DIR         = $(BUILD_STAGE_DIR)/usr/local/man
PBIN_DIR         = $(BUILD_STAGE_DIR)/usr/local/lib/riak/bin
PETC_DIR         = $(BUILD_STAGE_DIR)/usr/local/etc/riak
PLIB_DIR         = $(BUILD_STAGE_DIR)/usr/local/lib/riak
PUSRBIN_DIR      = $(BUILD_STAGE_DIR)/usr/local/bin
# /var based dirs
PDATA_DIR        = $(BUILD_STAGE_DIR)/usr/local/var/lib/riak
PLOG_DIR         = $(BUILD_STAGE_DIR)/var/log/riak

PKGNAME = riak-$(PKG_VERSION)-$(OSNAME)-$(ARCH).tbz


# Recursive assignment of ERTS version
# We will know this after building the rel
ERTS_PATH = $(shell ls $(BUILDDIR)/rel/riak | egrep -o "erts-.*")

build: packing_list_files
	@echo "Building package $(PKGNAME)"

	cd $(BUILD_STAGE_DIR) && \
	mkdir ../../../out/packages && \
	pkg create -m . -r . -o ../../../out/packages

	cd ../../out/packages && \
		for f in *.pkg; do \
		shasum -a 256 $${f} > $${f}.sha \
	; done

packing_list_files: $(BUILD_STAGE_DIR)
	@mv ${BUILDDIR}/rel/pkg/fbsdng/+MANIFEST ${BUILD_STAGE_DIR}
	sed -e "s/%ERTS_PATH%/${ERTS_PATH}/" < \
	        ${BUILDDIR}/rel/pkg/fbsdng/rc.d > \
	        ${BUILD_STAGE_DIR}/usr/local/etc/rc.d/riak
	chmod -w ${BUILD_STAGE_DIR}/usr/local/etc/rc.d/riak
	chmod +x ${BUILD_STAGE_DIR}/usr/local/etc/rc.d/riak
	@cd $(BUILD_STAGE_DIR) && \
	        echo "version: \"${PKG_VERSION}\"" >> +MANIFEST && \
	        echo "files: {" >> +MANIFEST

	@echo "Copying Man pages to staging directory"
	@cd $(BUILDDIR) && \
	if [ -d doc/man/man1 ]; then \
		mkdir -p $(PMAN_DIR) && \
		cp -R doc/man/man1 $(PMAN_DIR); fi

	@echo "Packaging /usr/local files"
	@cd $(BUILD_STAGE_DIR) && \
	find usr -type f | while read file ; do \
	    mode=$$(stat -f%p "$$file" | cut -c 3-) && \
	    sum=$$(sha256 -q $$file) && \
			echo "  \"/$$file\": { sum: \"$$sum\", perm: \"$$mode\", uname: \"root\", gname: \"wheel\" }," >> +MANIFEST; done && \
		sed -i .bak '$$s/,$$//' +MANIFEST && \
		rm -- +MANIFEST.bak && \
		echo " }" >> +MANIFEST

	@cd $(BUILD_STAGE_DIR) && \
		echo "directories: {" >> +MANIFEST && \
		echo "  /usr/local/lib/riak: \"y\"," >> +MANIFEST && \
		echo "  /usr/local/var/lib/riak: {uname: \"riak\", gname: \"riak\", perm: \"0700\" }," >> +MANIFEST && \
		echo "  /usr/local/etc/riak: \"y\"" >> +MANIFEST && \
		echo " }" >> +MANIFEST

# Copy the app rel directory to the staging directory to build our
# package structure and move the directories into the right place
# for the package, see the vars.config file for destination
# directories
$(BUILD_STAGE_DIR): buildrel
	@echo "Copying rel directory to staging directory"
	mkdir -p $@
	mkdir -p $(PBIN_DIR) $(PUSRBIN_DIR)
	for f in riak-admin riak-debug riak-repl riak-chkconfig; do \
	    cp -R rel/riak/bin/$$f $(PUSRBIN_DIR); \
	done
	cp -R rel/riak/usr/bin/riak $(PUSRBIN_DIR)
	cp -R rel/riak/bin $(PLIB_DIR);
	mkdir -p $(PETC_DIR)
	cp -R rel/riak/etc/* $(PETC_DIR)
	mkdir -p $(PLIB_DIR)
	cp -R rel/riak/lib $(PLIB_DIR)
	cp -R rel/riak/share $(PLIB_DIR)
	cp -R rel/riak/erts-* $(PLIB_DIR)
	(cd $(PLIB_DIR) && ln -s erts-* erts)
	cp -R rel/riak/releases $(PLIB_DIR)
	mkdir -p $(PDATA_DIR)
	cp -R rel/riak/data/* $(PDATA_DIR)
	mkdir -p ${BUILD_STAGE_DIR}/usr/local/etc/rc.d

# Build the release we need to package
#  * Ensure all binaries are executable
#  * copy the vars.config over for build config
buildrel:
	tar -xf $(BASE_DIR)/rel/pkg/out/$(PKG_ID).tar.gz -C $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID)
	$(MAKE) -C $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID) rel-fbsdng
	chmod 0755 rel/riak/bin/* rel/riak/erts-*/bin/*

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	rm -rf $(BUILD_STAGE_DIR) $(BUILDDIR)
