# Installing BINSEC from sources

The latest public revision of `binsec` is available on GitHub:
https://github.com/binsec/binsec.

#### (optional) OCaml "environment"

:information_source: **Local opam switch**
Similarly to Python's venv, `opam` offers a mechanism for isolated OCaml environments, called switches.
In particular, local switches are activated when inside the specified directory.

A local switch makes the installation of dependencies, including OCaml supported version, not impacting the system wide OCaml configuration.
*Doing so, everything installed will be readily available but only inside the BINSEC directory.*

---
If `opam` is available, using the following command will create a new OCaml switch inside the BINSEC tree.
```bash
OCAML_COMPILER=4.14.3 make switch
```
Use `OCAML_COMPILER=4.14.3 make switch-dev` to also set up editor support (`merlin` and `user-setup`).

---
If you require a local switch in a parent directory to BINSEC's, look into:
```bash
opam switch create . 4.14.3
```

:warning: You might have to load the new switch, as indicated by `opam` at the end of the creation, or after a `opam switch`.
```bash
eval $(opam env)
```

#### Dependencies

Dependencies can be automatically installed via
[*opam*](https://opam.ocaml.org/doc/Install.html).  
```bash
$ opam install dune dune-site menhir grain_dypgen ocamlgraph zarith toml mula
$ opam install bitwuzla bitwuzla-cxx z3 # optional -- for native SMT solver bindings
$ opam install unisim_archisec # optional -- for x86-64, ARMv7 and ARMv8
$ opam install curses # optional -- for real time summary window
```

##### System

- [GMP v6.1 (GNU Multi-Precision arithmetic library)](https://gmplib.org)
- [OCaml >= 4.14](https://github.com/ocaml/ocaml)

##### OCaml

- [dune >= 3.18](https://github.com/ocaml/dune)
- [menhir](https://gitlab.inria.fr/fpottier/menhir)
- [toml](https://github.com/ocaml-toml/To.ml)
- [ocamlgraph >= 1.8.5](https://github.com/backtracking/ocamlgraph)
- [zarith >= 1.4](https://github.com/ocaml/Zarith)
- *[unisim_archisec](https://github.com/binsec/unisim_archisec) (optional)*
- *[bitwuzla](https://github.com/bitwuzla/ocaml-bitwuzla) (optional)*
- *[z3](https://github.com/Z3Prover/z3) (optional)*
- *[curses](https://github.com/mbacarella/curses) (optional)*
- [odoc](https://github.com/ocaml/odoc) (*documentation*)
- [qcheck](https://github.com/c-cube/qcheck) (*test*)
- [ounit2](https://github.com/gildor478/ounit) (*test*)

## Build instructions

#### With `make`

[Makefile](Makefile) is a wrapper around `dune` build system.  
When `opam` is available, using the command `make` will automatically install the missing dependencies.

---

Run the following in order to build the `binsec` executable:
```bash
make
```
Then run the following in order to install `binsec` in the current switch:
```bash
make install
```

#### With `dune`

Make sure the above dependencies are available.

Run the following in order to build `binsec` executable:
```bash
dune build @install
```

`binsec` executable can be found in
`_build/install/default/bin`.

Run the following in order to install `binsec` in the current switch:
```bash
dune install
```
Or use it locally with:
```bash
dune exec -- binsec [...]
```
