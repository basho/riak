
DISTRO 			= $(shell head -1 /etc/redhat-release| awk \
                          '{if ($$0 ~ /CentOS release 5/) { print ".el5."} else { print "." }} ')

PKG_VERSION_NO_H	?= $(shell echo $(PKG_VERSION) | tr - .)

build: $(PKGERDIR)/SOURCES/$(APP)-$(PKG_VERSION).tar.gz rpmbuild
	@echo "DISTRO = $(DISTRO)"
	@echo "RIAK_TAG = $(RIAK_TAG)"
	@echo "REVISION = $(PKG_VERSION)"
	@echo "RELEASE  = $(RELEASE)"
	rpmbuild --define "_rpmfilename %%{NAME}-%%{VERSION}-%%{RELEASE}$(DISTRO)%%{ARCH}.rpm" \
		 --define '_topdir $(CURDIR)/rpmbuild' \
		 --define '_sourcedir $(CURDIR)/$(PKGERDIR)/SOURCES' \
		 --define '_specdir $(CURDIR)/$(PKGERDIR)/SPECS' \
		 --define '_rpmdir $(CURDIR)/packages' \
		 --define '_srcrpmdir $(CURDIR)/packages' \
		 --define "_revision $(PKG_VERSION)" \
		 --define "_repo $(REPO)" \
		 --define "_version $(PKG_VERSION_NO_H)" \
		 --define "_release $(RELEASE)" \
		 --define "_versionstring $(VERSIONSTRING)" \
		 -ba $(PKGERDIR)/SPECS/$(APP).spec
	cd packages && \
		for rpmfile in `ls *.rpm`; do \
			sha256sum $${rpmfile} > $${rpmfile}.sha \
		; done

rpmbuild:
	@mkdir -p rpmbuild/BUILD
	@mkdir -p packages

# In case it doesn't exist because there aren't any patches to apply
$(PKGERDIR)/SOURCES:
	@mkdir -m 0755 -p $(PKGERDIR)/SOURCES

$(PKGERDIR)/SOURCES/$(APP)-$(PKG_VERSION).tar.gz:  $(APP)-$(PKG_VERSION).tar.gz \
						   $(PKGERDIR)/SOURCES
	cp $(APP)-$(PKG_VERSION).tar.gz $(PKGERDIR)/SOURCES

$(PKGERDIR)/pkgclean:
	@echo
