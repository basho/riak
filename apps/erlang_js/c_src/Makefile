# This Makefile builds the dependencies (libjs and libnspr) needed by
# spidermonkey_drv.so

SMONKEY_VER	:= 1.8.0-rc1
NSPR_VER	:= 4.8

# NSPR_SIXTYFOUR, JS_SIXTYFOUR, and JS_TOILET are all conditionally defined in
# erlang_js/rebar.config

js: libjs.a

libnspr4.a: deps/nspr_release/dist/lib/libnspr4.a
	@cp deps/nspr_release/dist/lib/libnspr4.a .

libjs.a: libnspr4.a deps/js/src/libjs.a
	@cp deps/js/src/libjs.a .

deps/nspr_release/dist/lib/libnspr4.a: deps/nsprpub deps/nspr_release
	@echo "Building NSPR... this can take a bit"
	@cd deps/nspr_release ; \
	../nsprpub/configure --disable-debug --enable-optimize \
		$(NSPR_SIXTYFOUR) ; \
	make

deps:
	@mkdir -p $(@)

deps/nsprpub: deps
	@mkdir -p $(@)
	@tar -C deps -xzf nsprpub-$(NSPR_VER).tar.gz

deps/nspr_release: deps
	@mkdir -p $(@)

deps/js/src/libjs.a: deps/js
	@echo "Building Spidermonkey... this can take a bit"
	@for I in patches/deps-js-*.patch; do \
		patch -p1 < $${I}; \
	done
	@make -C deps/js/src BUILD_OPT=1 JS_DIST=../../nspr_release/dist \
		JS_THREADSAFE=1 \
		XCFLAGS="-DHAVE_VA_COPY -DVA_COPY=va_copy $(JS_SIXTYFOUR)" \
		XLDFLAGS="$(JS_TOILET)" \
		-f Makefile.ref
	@cp deps/js/src/*_OPT.OBJ/libjs.a deps/js/src
	@mkdir -p include/js
	@cp deps/js/src/*.h include/js
	@cp deps/js/src/*.tbl include/js
	@cp deps/js/src/*_OPT.OBJ/*.h include/js

deps/js: deps
	@mkdir $(@)
	@tar -C deps -xzf js-$(SMONKEY_VER).tar.gz

clean:
	@rm -f libjs.a libnspr4.a
	@rm -f *flymake*
	@rm -rf include
	@rm -rf deps

nspryoink: deps
	@cd deps ; \
	@cvs -q -d :pserver:anonymous@cvs-mirror.mozilla.org:/cvsroot \
		co -r NSPR_4_8_RTM -d nsprpub mozilla/nsprpub

.EXPORT_ALL_VARIABLES:
