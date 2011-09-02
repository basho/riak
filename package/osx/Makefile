ARCH        = $(shell uname -m)
PKGNAME     = $(APP)-$(REVISION)-osx-$(ARCH).tar.gz

# simply tar up the rel directory and sha the file
build: buildrel 
	@echo "Building package $(PKGNAME)"
	mkdir -p packages
	cd $(BUILDDIR)/$(REPO)-$(REVISION) && \
		cp -R rel/riak $(REPO)-$(REVISION) && \
		tar -czf ../../packages/$(PKGNAME) $(REPO)-$(REVISION)
	cd packages && \
		for tarfile in `ls *.gz`; do \
			shasum -a 256 $${tarfile} > $${tarfile}.sha \
		; done

# Build the release we need to package
buildrel: $(BUILDDIR)/$(REPO)-$(REVISION)
	cd $^ && $(MAKE) deps compile rel

# confusingly enough for riak_ee APP=riak-ee REPO=riak_ee
# and the APP.tar untars to a REPO directory
$(BUILDDIR)/$(REPO)-$(REVISION): $(BUILDDIR) $(APP)-$(REVISION).tar.gz
	tar xz -C $(BUILDDIR) -f $(APP)-$(REVISION).tar.gz

$(BUILDDIR):
	mkdir -p $@

$(PKGERDIR)/pkgclean:
	@echo
