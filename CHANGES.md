## 0.6.1 (2022-09-23)

** Bugs

  - Fix the model extraction for newer versions of `Bitwuzla`
  - Fix the timeout handler for `ocaml-bitwuzla` when `4.09 <= ocaml < 4.13`
  - Fix SSE not properly resetting the screen when an exception occurs

## 0.6.0 (2022-09-22)

** Features

   - New architecture support : RISC V 64bit
   - Catch interrupt signal (`CTRL-C`) in SSE in order to
     print exploration summary gracefully
   - Switch between log and monitor screen in SSE by pressing `space`
     (require `curses`)

** Documentation

   - Broaden the SSE [manual reference](doc/sse/references.md)
   - Add the write-up
	 ["How to read the SSE exploration board"](doc/sse/exploration_board.md)

** Bugs

   - Fix bitvector canonical representation
   - Fix compatibility issues with `unisim-archisec.0.0.3`
   - Fix issues with new experimental SSE engine

## 0.5.0 (2022-04-18)

** Features

   - Alternative experimental SSE engine
     (enabled with `-sse-alternative-engine`)
   - Core dump support in SSE initialization
   - Self-modifying code support in SSE
     (enabled with `-sse-self-written-enum N`)
	 
** Examples

   - Add SSE FlareOn 2021 challenge 2
   - Add SSE `gugus` challenge from
     [crackmes.one](https://crackmes.one/user/bueb810)
   - Add SSE `hidden_password` challenge from
     [crackmes.one](https://crackmes.one/user/pjenik@seznam.cz)
	 with dedicated [write-up](doc/sse/advanced_users.md)
   - Add SSE `license_checker_3` challenge from
     [crackmes.one](https://crackmes.one/user/NomanProdhan)
   - Add SSE `trycrackme` challenge from
	 [crackmes.one](https://crackmes.one/user/MrEmpy)
	 with dedicated [write-up](doc/sse/intermediates_2.md)

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
