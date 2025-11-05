(lang dune 3.20)

(name rubiks)

(generate_opam_files true)

(source
 (github gertab/Cube))

(authors "gerard <gerardtabone@yahoo.com>")

(maintainers "gerard <gerardtabone@yahoo.com>")

(license "GPL-3.0-or-later")

(documentation "https://github.com/gertab/Cube/blob/main/README.md")

(package
 (name rubiks)
 (synopsis "Rubik's Cube simulator and CFOP-based solver written in OCaml")
 (description "This project is a Rubik's Cube simulator and solver implemented natively in OCaml.  It supports defining, scrambling, and solving cubes programmatically, using the CFOP solving method (Cross, F2L, OLL, PLL). The project includes visualisation utilities and can export solutions for use with Twizzle or other cube viewers.")
 (depends 
  (ocaml (>= 5.0.0))
  (ounit2 (>= 2.2.0)))
 (tags
  ("rubiks-cube" "ocaml" "solver" "cfop" "algorithm" "simulator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
