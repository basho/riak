BUILDPATH	= debuild/$(APP)-$(PKG_VERSION)

build:  $(BUILDPATH)/debian \
	debuild/$(APP)_$(PKG_VERSION).orig.tar.gz
	export DEBFULLNAME="Basho Buildbot Packager"; \
	export DEBEMAIL="support@basho.com"; \
	dch  --noquery -c $(BUILDPATH)/debian/changelog \
		-b -v "$(PKG_VERSION)-$(RELEASE)" "pants on head"
	cd $(BUILDPATH) && debuild --no-lintian \
	        --prepend-path=$(ERLANG_BIN) \
		-e REVISION=$(PKG_VERSION) \
		-e RELEASE=$(RELEASE) \
		-e VERSIONSTRING="$(VERSIONSTRING)" \
		-uc -us
	mkdir -p packages
	mv debuild/$(APP)_$(PKG_VERSION)-$(RELEASE)_*.deb packages
	cd packages && \
		for debfile in `ls *.deb`; do \
			sha256sum $${debfile} > $${debfile}.sha \
		; done

$(BUILDPATH): $(APP)-$(PKG_VERSION).tar.gz
	mkdir -p debuild
	tar xz -C debuild -f $^

$(BUILDPATH)/debian: $(BUILDPATH)
	cp -a $(PKGERDIR) $@
	rm -rf $@/.hg $@/Makefile $@/.*.swp

debuild/$(APP)_$(PKG_VERSION).orig.tar.gz: $(APP)-$(PKG_VERSION).tar.gz
	cp $^ $@

$(PKGERDIR)/pkgclean:
	@echo
