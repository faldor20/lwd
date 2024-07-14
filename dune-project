(lang dune 3.5)

(generate_opam_files true)

(formatting
 (enabled_for dune))

(name lwd)

(source
 (github let-def/lwd))

(license MIT)

(authors "Frédéric Bour")

(maintainers "fred@tarides.com")

(package
 (name lwd)
 (synopsis "Lightweight reactive documents")
 (documentation "https://let-def.github.io/lwd/doc")
 (depends
  dune
  seq
  (ocaml
   (>= "4.03"))
  (qtest :with-test)
  (qcheck :with-test)))
