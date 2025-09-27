(lang dune 3.20)

(name rubiks)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rubiks)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (>= 5.0.0))
  (ounit2 (>= 2.2.0)))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
