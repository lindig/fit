(lang dune 2.0)

(generate_opam_files true)

(source
 (github lindig/fit))

(license MIT)

(authors "Christian Lindig <lindig@gmail.com>")

(maintainers "Christian Lindig <lindig@gmail.com>")

(package
 (name fit)
 (synopsis "A parser for Garmin FIT data files")
 (description
  "Fit is library for reading FIT files as they are produced by Garmin and\nother fitness devices. It comes with a small command-line tool to emit\nsome of that information as JSON, mostly for debugging. Fit is not\ncomprehensive but reads the most important records from a FIT file that\ncontains the actual periodic measurements.")
 (depends
  (ocaml
   (>= 4.08.0))
  (dune
   (>= 2.0))
  (cmdliner
   (>= 1.1.0))
  (angstrom
   (>= 0.15.0))
  (yojson
   (>= 2.1.0))
  (rresult
   (>= 0.6.0))
  (ISO8601
   (>= 0.2.6))))
