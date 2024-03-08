(lang dune 3.0)
(using menhir 2.0)
(using dune_site 0.1)
(generate_opam_files true)

(name binsec)

(version "0.8.2")
(maintainers "BINSEC <binsec@saxifrage.saclay.cea.fr>")
(authors
  "Adel Djoudi"
  "Benjamin Farinier"
  "Chakib Foulani"
  "Dorian Lesbre"
  "Frédéric Recoules"
  "Guillaume Girol"
  "Josselin Feist"
  "Lesly-Ann Daniel"
  "Manh-Dung Nguyen"
  "Mathéo Vergnolle"
  "Mathilde Ollivier"
  "Matthieu Lemerre"
  "Olivier Nicole"
  "Richard Bonichon"
  "Robin David"
  "Sébastien Bardin"
  "Soline Ducousso"
  "Ta Thanh Dinh"
  "Yaëlle Vinçont"
)
(license LGPL-2.1-or-later)
(homepage "https://binsec.github.io")
(source (github binsec/binsec))
(bug_reports "mailto:binsec@saxifrage.saclay.cea.fr")

(package
 (name binsec)
 (sites (share utils) (lib plugins))
 (synopsis "Semantic analysis of binary executables")
 (description "
BINSEC aims at developing an open-source platform filling the gap between formal
methods over executable code and binary-level security analyses currently used
in the security industry.

The project targets the following applicative domains:

    vulnerability analyses
    malware comprehension
    code protection
    binary-level verification

BINSEC is developed at CEA List in scientfic collaboration with Verimag and LORIA.

An overview of some BINSEC features can be found in our SSPREW'17 tutorial.")
 (depends
  (ocaml (>= 4.11))
  (menhir (and :build (>= 20181113)))
  (ocamlgraph (>= 1.8.5))
  (zarith (>= 1.4))
  dune-site
  grain_dypgen
  (toml (>= 6))
  (ounit2 (and :with-test (>= 2)))
  (qcheck (and :with-test (>= 0.7)))
  (ocamlformat (and :with-dev-setup (= 0.26.1))))
 (depopts
  curses
  llvm
  unisim_archisec
  bitwuzla)
 (conflicts
  (llvm (or (< 6.0.0) (>= 16.0.0)))
  (bitwuzla (< 1.0.4))
  (unisim_archisec (< 0.0.6)))
 (tags
  ("binary code analysis"
    "symbolic execution"
    "deductive"
    "program verification"
    "formal specification"
    "automated theorem prover"
    "plugins"
    "abstract interpretation"
    "dataflow analysis"
    "linking"
    "disassembly")))
