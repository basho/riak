# This Makefile builds the dependencies (libjs and libnspr) needed by
# spidermonkey_drv.so

js: libjs.a

libnspr4.a: deps/nspr_release/dist/lib/libnspr4.a
	@cp deps/nspr_release/dist/lib/libnspr4.a .

libjs.a: libnspr4.a js/src/libjs.a
	@cp deps/js/src/libjs.a .

deps/nspr_release/dist/lib/libnspr4.a: deps/mozilla deps/nspr_release
	@cd deps/nspr_release;../mozilla/nsprpub/configure --disable-debug --enable-optimize;make

deps/mozilla:
	@mkdir -p $(@)
	@cvs -q -d :pserver:anonymous@cvs-mirror.mozilla.org:/cvsroot co -r NSPR_4_8_RTM mozilla/nsprpub
	@mv mozilla deps

deps/nspr_release:
	@mkdir -p $(@)

js/src/libjs.a: deps/js
	@cd deps/js/src;make BUILD_OPT=1 JS_DIST=../../nspr_release/dist JS_THREADSAFE=1 XCFLAGS="-DHAVE_VA_COPY -DVA_COPY=va_copy" -f Makefile.ref
	@cp deps/js/src/*_OPT.OBJ/libjs.a deps/js/src
	@mkdir -p include/js;
	@cp deps/js/src/*.h include/js
	@cp deps/js/src/*.tbl include/js
	@cp deps/js/src/*_OPT.OBJ/*.h include/js

deps/js:
	@tar -C deps -xzf js-1.8.0-rc1.tar.gz

dist:
	@rm -f libjs.a libnspr4.a
	@rm -f *flymake*
	@rm -rf include
	@rm -rf deps

jsclean:
	@rm -rf deps
	@rm -f *flymake*
	@rm -f libjs.a libnspr4.a
	@rm -rf include
