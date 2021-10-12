(lang dune 2.8)
(using menhir 2.0)
(using dune_site 0.1)
(generate_opam_files true)

(name binsec)
(version "0.4.0")
(maintainers "BINSEC <binsec@saxifrage.saclay.cea.fr>")
(authors
  "Adel Djoudi"
  "Benjamin Farinier"
  "Frédéric Recoules"
  "Josselin Feist"
  "Lesly-Ann Daniel"
  "Manh-Dung Nguyen"
  "Mathilde Ollivier"
  "Matthieu Lemerre"
  "Olivier Nicole"
  "Richard Bonichon"
  "Robin David"
  "Ta Thanh Dinh"
  "Yaëlle Vinçont"
  "Guillaume Girol"
)
(license LGPL-2.1-or-later)
(homepage "https://binsec.github.io")
(source (github binsec/binsec))
(bug_reports "mailto:binsec@saxifrage.saclay.cea.fr")

(package
 (name binsec)
 (sites (share utils))
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
  (ocaml (and :build (>= 4.08) (< 4.14)))
  (menhir (and :build (>= 20181113)))
  (ocamlgraph (>= 1.8.5))
  (zarith (>= 1.4))
  dune-site
  (ounit2 (and :with-test (>= 2)))
  (qcheck (and :with-test (>= 0.7))))
 (depopts
  unisim_archisec)
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
