#
# This Makefile is not called from Opam but only used for
# convenience during development
#

.PHONY: all install test clean uninstall format utop

all:
	dune build

install: all
	dune install fit

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean

utop:
	dune utop

format:
	dune build --auto-promote @fmt
	dune format-dune-file dune-project > $$$$ && mv $$$$ dune-project
	opam lint
	opam lint --normalise fit.opam > tmp.opam && mv tmp.opam fit.opam
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8

%.mli: %.ml
	dune exec -- ocaml-print-intf $<

release:
	dune-release tag
	dune-release distrib
	dune-release opam pkg
	echo 'use "dune-release opam submit" to release on Opam'

# vim:ts=8:noet:
