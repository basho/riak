export

TAR_VERSION = $(shell git describe --tags | sed -e 's/\([0-9.]*\-[0-9]*\)-.*/\1/')

DEBEMAIL = $(shell git config user.email)

default:
	mkdir -p $(BASE_DIR)/rel/pkg/out/$(PKG_ID)/debian
	cp -R $(BASE_DIR)/rel/pkg/deb/debian/* $(BASE_DIR)/rel/pkg/out/$(PKG_ID)/debian

	tar -xf $(BASE_DIR)/rel/pkg/out/$(PKG_ID).tar.gz -C $(BASE_DIR)/rel/pkg/out/$(PKG_ID)/
	ln -sf $(BASE_DIR)/rel/pkg/out/$(PKG_ID).tar.gz $(BASE_DIR)/rel/pkg/out/$(TAR_VERSION).orig.tar.gz

	cd $(BASE_DIR)/rel/pkg/out/$(PKG_ID); \
	(mkdir -p _build/default && cd _build/default && for d in lib; do ln -fs $(BASE_DIR)/_build/default/$$d; done); \
	dch  --create --package riak -v "$(PKG_VERSION)" \
             "Build from $(PKG_ID)";\
	debuild --prepend-path=$(ERLANG_BIN) \
			-e REVISION=$(PKG_VERSION) \
			-e RELEASE=$(PKG_BUILD) \
		-i -uc -us -b

	mkdir -p $(BASE_DIR)/rel/pkg/packages
	cd $(BASE_DIR)/rel/pkg/out && mv *.deb ../packages
	cd $(BASE_DIR)/rel/pkg/packages && \
		for debfile in *.deb; do \
			sha256sum $${debfile} > $${debfile}.sha \
		; done
