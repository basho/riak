BUILDPATH	= debuild/$(APP)-$(REVISION)

build:  $(BUILDPATH)/debian \
	debuild/$(APP)_$(REVISION).orig.tar.gz
	export DEBFULLNAME="Basho Buildbot Packager"; \
	export DEBEMAIL="support@basho.com"; \
	dch  --noquery -c $(BUILDPATH)/debian/changelog \
		-b -v "$(REVISION)-$(RELEASE)" "pants on head"
	cd $(BUILDPATH) && debuild --no-lintian \
		-e REVISION=$(REVISION) \
		-e RELEASE=$(RELEASE) \
		-uc -us
	mkdir -p packages
	mv debuild/$(APP)_$(REVISION)-$(RELEASE)_*.deb packages

$(BUILDPATH): $(APP)-$(REVISION).tar.gz
	mkdir -p debuild
	tar xz -C debuild -f $^

$(BUILDPATH)/debian: $(BUILDPATH)
	cp -a $(PKGERDIR) $@
	rm -rf $@/.hg $@/Makefile $@/.*.swp

debuild/$(APP)_$(REVISION).orig.tar.gz: $(APP)-$(REVISION).tar.gz
	cp $^ $@

$(PKGERDIR)/pkgclean:
	@echo
