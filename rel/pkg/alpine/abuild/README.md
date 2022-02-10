# Packaging Riak for Alpine Linux

Alpine Linux is a minimalistic Gentoo-inspired, source-based distribution.

Packaging instructions for Alpine cannot be placed in
rel/pkg/alpine/Makefile without bending too many rules and
conventions. Rather, following their
[wiki](https://wiki.alpinelinux.org/wiki/Creating_an_Alpine_package),
the idea is to place the contents of this directory in your abuild
dir, and execute `abuild` in it.

Riak could not be built with the version of OTP currently available in
Alpine (from the "community" repository) is 24. You will need to build
and install version 22 manually (other versions may or may not work),
or write an abuild for it.
