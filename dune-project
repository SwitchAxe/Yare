(lang dune 3.15)

(name Yare)

(generate_opam_files true)

(source
 (github SwitchAxe/Yare))

(authors "Sofia Cerasuoli")

(maintainers "Sofia Cerasuoli")

(license GPL-3.0-or-later)

(package
 (name Yare)
 (synopsis "yare")
 (description "A shell and a programming language")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
