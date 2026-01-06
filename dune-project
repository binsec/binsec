(lang dune 3.18)

(using menhir 2.0)

(using dune_site 0.1)

(generate_opam_files true)

(name binsec)

(version "0.11.0")

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
 "Mahmudul Faisal Al Ameen"
 "Manh-Dung Nguyen"
 "Mathéo Vergnolle"
 "Mathilde Ollivier"
 "Matthieu Lemerre"
 "Nicolas Bellec"
 "Olivier Nicole"
 "Richard Bonichon"
 "Robin David"
 "Sébastien Bardin"
 "Soline Ducousso"
 "Ta Thanh Dinh"
 "Yaëlle Vinçont"
 "Yanis Sellami")

(license LGPL-2.1-or-later)

(homepage "https://binsec.github.io")

(source
 (github binsec/binsec))

(bug_reports "mailto:binsec@saxifrage.saclay.cea.fr")

(package
 (name binsec)
 (sites
  (share utils)
  (lib plugins))
 (synopsis "Semantic analysis of binary executables")
 (description
  "\nBINSEC aims at developing an open-source platform filling the gap between formal\nmethods over executable code and binary-level security analyses currently used\nin the security industry.\n\nThe project targets the following applicative domains:\n\n    vulnerability analyses\n    malware comprehension\n    code protection\n    binary-level verification\n\nBINSEC is developed at CEA List in scientfic collaboration with Verimag and LORIA.\n\nAn overview of some BINSEC features can be found in our SSPREW'17 tutorial.")
 (depends
  (ocaml
   (>= 4.14))
  (menhir
   (and
    :build
    (>= 20181113)))
  (ocamlgraph
   (>= 1.8.5))
  (zarith
   (>= 1.13))
  dune-site
  dypgen
  (toml
   (>= 6))
  (ounit2
   (and
    :with-test
    (>= 2)))
  (qcheck
   (and
    :with-test
    (>= 0.90)))
  (ppx_inline_test :with-test)
  (unionFind :with-test)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.26.2))))
 (depopts curses llvm unisim_archisec bitwuzla bitwuzla-cxx z3)
 (conflicts
  (llvm
   (or
    (< 6.0.0)
    (>= 16.0.0)))
  (bitwuzla
   (< 1.0.4))
  (bitwuzla-cxx
   (< 0.4))
  (z3
   (< 4.8.13))
  (unisim_archisec
   (< 0.0.6)))
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
   "disassembly"))
 (maintenance_intent "(latest)"))
