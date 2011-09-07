ARCH        = $(shell uname -m)
PKGNAME     = $(APP)-$(REVISION)-osx-$(ARCH).tar.gz

# simply tar up the rel directory and sha the file
build: buildrel 
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILDDIR)/$(APP)-$(REVISION) && \
		cp -R rel/riak $(APP)-$(REVISION) && \
		tar -czf ../../packages/$(PKGNAME) $(APP)-$(REVISION)
	cd packages && \
		for tarfile in `ls *.gz`; do \
			shasum -a 256 $${tarfile} > $${tarfile}.sha \
		; done

# Build the release we need to package
buildrel: $(BUILDDIR)/$(APP)-$(REVISION)
	cd $^ && $(MAKE) deps compile rel

$(BUILDDIR)/$(APP)-$(REVISION): $(BUILDDIR) $(APP)-$(REVISION).tar.gz
	mkdir -p $@
	tar xz -C $(BUILDDIR) -f $(APP)-$(REVISION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	@echo
