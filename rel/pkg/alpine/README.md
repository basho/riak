# Packaging Riak for Alpine Linux

Alpine Linux is a minimalistic, Gentoo-inspired, source-based distribution.

Packaging instructions for Alpine cannot be placed in
rel/pkg/alpine/Makefile without bending too many rules and
conventions.

Instead, TI Tokyo [builds](https://github.com/TI-Tokyo/alpine-builds)
apks for x86_64 and aarch64 and maintains an external Alpine
repository at https://files.tiot.jp/alpine/.
