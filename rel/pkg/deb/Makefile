
default:
	ln -sf $(PKG_ID).tar.gz ../{{package_name}}_$(PKG_VERSION).orig.tar.gz
	export DEBFULLNAME="{{vendor_contact_name}}"; \
	export DEBEMAIL="{{vendor_contact_email}}"; \
	dch  --create --package {{package_name}} -v "$(PKG_VERSION)-$(PKG_BUILD)" \
             "Build from $(PKG_VERSION)";\
	debuild --prepend-path=$(ERLANG_BIN) \
		-e REVISION=$(PKG_VERSION) \
		-e RELEASE=$(PKG_BUILD) \
                -e REBAR=$(REBAR) \
		{{debuild_extra_options}} \
		-uc -us
	mkdir -p ../packages
	cd .. && mv *$(PKG_VERSION)-$(PKG_BUILD)_*.deb packages
	cd ../packages && \
		for debfile in *.deb; do \
			sha256sum $${debfile} > $${debfile}.sha \
		; done
