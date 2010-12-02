
build: $(PKGERDIR)/SOURCES/$(APP)-$(REVISION).tar.gz rpmbuild
	@echo "RIAK_TAG = $(RIAK_TAG)"
	@echo "REVISION = $(REVISION)"
	@echo "RELEASE  = $(RELEASE)"
	rpmbuild --define '_topdir $(CURDIR)/rpmbuild' \
		 --define '_sourcedir $(CURDIR)/$(PKGERDIR)/SOURCES' \
		 --define '_specdir $(CURDIR)/$(PKGERDIR)/SPECS' \
		 --define '_rpmdir $(CURDIR)/packages' \
		 --define '_srcrpmdir $(CURDIR)/packages' \
		 --define "_revision $(REVISION)" \
		 --define "_version $(PKG_VERSION)" \
		 --define "_release $(RELEASE)" \
		 -ba $(PKGERDIR)/SPECS/$(APP).spec
	mv packages/*/$(APP)-$(PKG_VERSION)-$(RELEASE)*.rpm packages
	rm -rf packages/i?86 packages/x86_64

rpmbuild:
	@mkdir -p rpmbuild/BUILD
	@mkdir -p packages

# In case it doesn't exist because there aren't any patches to apply
$(PKGERDIR)/SOURCES:
	@mkdir -m 0755 -p $(PKGERDIR)/SOURCES

$(PKGERDIR)/SOURCES/$(APP)-$(REVISION).tar.gz:  $(APP)-$(REVISION).tar.gz \
						$(PKGERDIR)/SOURCES
	cp $(APP)-$(REVISION).tar.gz $(PKGERDIR)/SOURCES

$(PKGERDIR)/pkgclean:
	@echo
