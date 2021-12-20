## 0.4.1 (2021-12-20)

** Features

   - Reworked Backward Bounded Symbolic Execution
     (together with some [documentation](doc/bbsse/))

** Misc

   - Support native OCaml
     [bitwuzla binding](https://github.com/bitwuzla/ocaml-bitwuzla)

** Bug

   - Fix an issue with 64-bit kernel virtual addresses

## 0.4.0 (2021-10-12)

** Features

   - New architecture support : ARMv7 Thumb mode
     (requires [unisim_archisec](https://github.com/binsec/unisim_archisec))
   - New architecture support : AARCH64
     (requires [unisim_archisec](https://github.com/binsec/unisim_archisec))
   - New architecture support : AMD64
     (requires [unisim_archisec](https://github.com/binsec/unisim_archisec))
   - Backward Bounded Symbolic Execution (experimental)
   - Reworked Static Symbolic Execution
     (together with some [documentation](doc/sse/))
   
** Dropped features (until rework)

   - Static Abstract Interpretation
   - Dynamic Symbolic Execution
   
** Misc

   - Use Dune build system
   - Remove several system dependencies (PIQI, ZMQ)

## 0.3.0 (2020-01-21)

** Features

   - New architecture support : RISC-V 32 bits
   - Support for DWARF-4 debug instruction format
   - Support to import IDA control-flow graph
   - Add documented plugin creation example : mnemonic count [mcount]
   - New Makefile 'library' to ease plugin creation

** Fixes

   - Fix (vectorized instructions) x86 decoder

** Misc

   - Detach PINSEC to own repository (support to be deprecated in later version)

## 0.2.0 (2018-10-01)

  - New symbolic execution engine
  - New interpreter for binary code
  - Improved logical representation for formulas
  - New internal control-flow-graph representation
  - Directive language for symbolic execution control
  - Support for new PIN tool xtrasec
  - Improved x86 decoder
    - Fixed bugs reported by KAIST
  - Docker support
    - includes Unisim-vp ARM v7 decoder
    - includes new PIN tool xtrasec



## 0.1.0 (2017-03-01)

  First release
