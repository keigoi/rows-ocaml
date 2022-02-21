(lang dune 1.12)

(name rows)

(version 0.0.1)

(allow_approximate_merlin)

(using fmt 1.1)

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
 (depends stdlib-shims))
