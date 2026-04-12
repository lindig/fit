#
# This Makefile is not called from Opam but only used for
# convenience during development
#

export OCAML_LANDMARKS=output=landmarks.txt,time,allocation
F = ./_build/default/bin/main.exe

.PHONY: all install test clean uninstall format utop

all:
	dune build --instrument-with landmarks

install: all
	dune install fit

uninstall:
	dune uninstall

test:
	dune runtest

perf:
	$F ./data/xpress-4x-2022-08-03.fit > /dev/null

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

changes:
	git log $$(git describe --tags --abbrev=0)..HEAD --pretty=format:"* %s"

opam: 	all
	cp fit.opam opam
	./url.sh $$(git describe --tags --abbrev=0) >> opam


# vim:ts=8:noet:
