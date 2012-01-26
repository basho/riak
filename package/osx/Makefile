PKGNAME     = $(APP)-$(PKG_VERSION)-osx-$(ARCH).tar.gz

# simply tar up the rel directory and sha the file
build: buildrel 
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILDDIR)/$(APP)-$(PKG_VERSION) && \
		cp -R rel/riak $(APP)-$(PKG_VERSION) && \
		tar -czf ../../packages/$(PKGNAME) $(APP)-$(PKG_VERSION)
	cd packages && \
		for tarfile in `ls *.gz`; do \
			shasum -a 256 $${tarfile} > $${tarfile}.sha \
		; done

# Build the release we need to package
buildrel: $(BUILDDIR)/$(APP)-$(PKG_VERSION)
	cd $^ && \
	cp rel/files/riak rel/files/riak.tmp && \
	sed -e "s/^RIAK_VERSION.*$$/RIAK_VERSION=\"${VERSIONSTRING}\"/" < rel/files/riak.tmp > rel/files/riak && \
	$(MAKE) deps compile rel

$(BUILDDIR)/$(APP)-$(PKG_VERSION): $(BUILDDIR) $(APP)-$(PKG_VERSION).tar.gz
	tar xz -C $(BUILDDIR) -f $(APP)-$(PKG_VERSION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	@echo
