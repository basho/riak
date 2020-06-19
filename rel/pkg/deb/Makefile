export

default:
	mkdir -p $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID)/debian
	cp -R $(BASE_DIR)/rel/pkg/deb/debian/* $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID)/debian
	cd $(BASE_DIR)/rel/pkg/out
	echo $(PWD)
	tar -xf $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID).tar.gz -C $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID)/
	ln -sf $(BASE_DIR)/rel/pkg/out/riak-$(PKG_ID).tar.gz $(BASE_DIR)/rel/pkg/out/riak_$(PKG_ID).orig.tar.gz
	export DEBFULLNAME="riak"; \
	export DEBEMAIL="packaging@riak.com"; \
	dch  --create --package riak -v "$(PKG_ID)" \
             "Build from $(PKG_ID)";\
	debuild --prepend-path=$(ERLANG_BIN) \
			-e REVISION=$(PKG_VERSION) \
			-e RELEASE=$(PKG_BUILD) \
            -e REBAR=$(REBAR) \
		-uc -us
	mkdir -p ../packages
	cd .. && mv *$(PKG_VERSION)-$(PKG_BUILD)_*.deb packages
	cd ../packages && \
		for debfile in *.deb; do \
			sha256sum $${debfile} > $${debfile}.sha \
		; done
