(lang dune 3.1)

(name rows)

(version 0.0.1)

(authors "Keigo Imai")

(maintainers "keigo.imai@gmail.com")

(source
 (github keigoi/rows-ocaml))

(documentation "https://github.com/keigoi/rows-ocaml")

(generate_opam_files true)

(package
 (name rows)
 (synopsis "First-class variant tags and object methods")
 (description "First-class variant tags and object methods.")
 (depends
  (ocaml
   (>= 4.12.0))
  (ppxlib
   (and (>= 0.19.0) (< 0.27.0)))
  ocaml-compiler-libs
  cppo
  (ounit :with-test)))
