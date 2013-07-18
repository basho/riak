# Riak Enterprise

## Erlang VM patch

**Purpose**: To prevent scheduler collapse by disabling scheduler sleep periods.

Patch file to add +sfwi tuning to an R15B01 Erlang VM. Should be present without patch in R16B01+. It's possible that this patch will apply to other releases. Use at your own risk.

This repository contains the file `collapse.patch`. To apply this to the Erlant R15B01 source tree, download:
	
	wget http://www.erlang.org/download/otp_src_R15B01.tar.gz
	tar xvzf otp_src_R15B01.tar.gz
	cd otp_src_R15B01
	patch -p1 < collapse.patch
	# On OSX
	./configure --prefix=/wherever/you/want/erlang -enable-vm-probes --with-dynamic-trace=dtrace --enable-darwin-64bit
	
	make && make install
	 
The new flag, "+zdss", must be used with "+scl false" flag to be successful.
See the erl.xml document for a description.  Example use:
 
  erl +scl false +zdss 500:500
 
Note that this flag used to be called "+zdnfgtse"
 
See also:
  * https://gist.github.com/evanmcc/a599f4c6374338ed672e
  * https://gist.github.com/slfritchie/5624609 
  * https://github.com/slfritchie/otp/compare/a70d09b6e...disable-scheduler-sleeps
  * http://erlang.org/pipermail/erlang-questions/2013-April/073490.html
  * http://erlang.org/pipermail/erlang-bugs/2013-May/003529.html
 
---

© 2013 Basho Technologies