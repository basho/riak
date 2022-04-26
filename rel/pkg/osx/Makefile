BUILDDIR        = $(shell pwd)
BUILD_STAGE_DIR = $(BUILDDIR)/riak

default: buildrel
	mkdir ../packages
	tar -czf ../packages/$(PKG_ID).tar.gz -C rel riak && \
	(cd ../packages && shasum -a 256 $(PKG_ID).tar.gz > $(PKG_ID).tar.gz.sha)

buildrel:
	tar -xf $(BASE_DIR)/rel/pkg/out/$(PKG_ID).tar.gz -C $(BASE_DIR)/rel/pkg/out/$(PKG_ID)
	$(MAKE) -C $(BASE_DIR)/rel/pkg/out/$(PKG_ID) rel-osx
	chmod 0755 rel/riak/bin/* rel/riak/erts-*/bin/*
