(lang dune 3.2)

(generate_opam_files true)

(name fit)

(source
 (github lindig/fit))

(license MIT)

(authors "Christian Lindig <lindig@gmail.com>")

(maintainers "Christian Lindig <lindig@gmail.com>")

(package
 (name fit)
 (synopsis "A parser for Garmin FIT data files")
 (description
  "Fit is a library for reading FIT files as they are produced by Garmin and other fitness devices. It comes with a small command-line tool to emit some of that information as JSON, mostly for debugging. Fit is not comprehensive but reads the most important records from a FIT file that contains the actual periodic measurements.")
 (depends
  (ocaml
   (>= 4.08.0))
  (cmdliner
   (>= 1.1.0))
  (angstrom
   (>= 0.15.0))
  (yojson
   (>= 2.1.0))
  (ptime
   (>= 1.1.0))))
