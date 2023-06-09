#
# This Makefile is not called from Opam but only used for
# convenience during development
#

DUNE 	= dune

.PHONY: all install test clean uninstall format

switch:
	opam switch create . 5.0.0 --deps-only
	opam pin -y https+git@github.com:koonwen/domainslib.git#upstream

all:
	$(DUNE) build

install: all
	$(DUNE) install hello

uninstall:
	$(DUNE) uninstall

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

utop:
	$(DUNE) utop

fmt:
	$(DUNE) build --auto-promote @fmt
	opam lint --normalise obatcher.opam > obatcher.opam.tmp
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8
