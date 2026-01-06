## 0.11.0 (2026-01-06)

** Feature

- Coredump support improvement:
  - add ARM, RISCV and PPC architectures
  - reworked the `make_coredump.sh` script to use QEMU for foreign architectures
  - add the `archive_sysroot.sh` script to make a snapshot of the file system
    used by a coredump, in pair with the `-sse-sysroot` option to set the system
    root for the program being executed, like gdb `set sysroot`
    (see `coreutils` [true](./examples/sse/coreutils/true/README.md) example)
- SSE plugin API overhaul (**code-breaking changes**):
  - add a new *continuation* mechanism for path management in builtins
    (e.g fork and merge requests)
  - add new callbacks to create hooks from plugins
  - improve module interface documentation (`State`, `Path` and `Types`)

** Documentation

- Split and update plugin tutorial ([part 1](doc/sse/plugins_1.md), [part 2](doc/sse/plugins_2.md))
- Add a [write-up](doc/sse/quick_merge.md) about how to use the new plugin API for path merging

** Misc (**code-breaking changes**)

- Merge `Lreader` and `Loader_buf` into `Reader` with a more type safe interface
  (**BINSEC** can now be used safely on 32-bit architectures)

- Refactor source code in main sub-libraries:
  - `Binsec_base` -- base types and structures (without dependencies)
  - `Binsec_kernel` -- DBA intermediate language, loaders and architectures
  - `Binsec_smtlib` -- SMTlib intermediate language and solvers
  - `Binsec_symbolic` -- symbolic state and path predicate
  - `Binsec_sse` -- core static symbolic execution engine

- Reorganize command line tool:
  - `Binsec` -- core command line library
  - `Binsec_disasm` -- legacy disassembly utilities
  - `Binsec_bbsse` -- legacy backward bounded symbolic execution
  - `Binsec_sse` -- static symbolic execution

## 0.10.1 (2025-06-30)

** Misc

- Minor changes in the DBA printer (sync operator names with `sse` script)
- Experimental support for extensible *custom* entries in formulas

** Bugs

- Fix some issues with x86 and RISCV disassembly

## 0.10.0 (2025-02-16)

** Features

- Add a new configuration option to tune the `checkct` instrumentation
  (`-checkct-features control-flow,memory-access,multiplication,dividend,divisor`)
- Add the secret-erasure check command in `checkct`
  (`check secret erasure over` *symbol*)
- Add an experimental *hook function return* command in SE

** Misc

- Add support for `AARCH64` core dump in SE
- Improve debug with symbol offset annotation (`-sse-debug-level 2`)

** Bugs

- Fix some ARM and x86 disassembly issues
- Fix some rewriting rule issues
- Fix compilation issues with latest OCaml compilers
- Refactor parsers and suppress conflicts


## 0.9.1 (2024-05-21)

** Misc

- Support native OCaml
  [z3 binding](https://github.com/Z3Prover/z3/tree/master/src/api/ml)
  (`-smt-solver z3:builtin`)

** Bugs

- Fix SMTlib formula printer not always flushing the definitions before use

## 0.9.0 (2024-05-01)

** Features

- Add a new SSE engine (`-sse-engine multi-checks`) that tries to reuse the
  previous SMT solver session (incremental mode)
- Add a common sub-expression elimination pass in SSE (`-sse-cse`)

** Documentation

- Add some comments in the SSE plugin interface

** Misc

- Reworked SMT solver interface
- Support for latest version of the `bitwuzla` solver
- Add some formula rewriting rules

** Bugs

- Fix solver queries taking several order times the given timeout
- Fix some x86 and RISCV disassembly issues
- Fix some script parser issues

## 0.8.2 (2024-03-08)

** Misc

  - Best effort handling of `<`*symbol*`@plt>` in SSE script (`x86` for now)
  - Add an option to ignore or simply warn when trying to `replace`
    a symbol absent of the binary (`-sse-missing-symbol`)
  - Warn for different threats to completeness at the end of SSE analysis

** Bugs

  - Fix several issues in `checkct` analysis
  - Fix *choice* option not showing the alternatives
  - Fix several parsing issues
  - Fix some download links in examples
  - Fix several issues in architecture handling
  - Fix compilation issues with OCaml 5

## 0.8.1 (2023-10-31)

** Misc

  - Add basic opcode replacement and address hook in SSE script
  - Add a registration mechanism for symbolic state
  - Add an option to disable the monitor screen when `curses` is installed
  - Small code improvements
  - Upgrade `ocamlformat` to `0.26.1`

** Bugs

  - Fix some uncatched exceptions
  - Fix a bug in `checkct` preprocessing

## 0.8.0 (2023-07-14)

** Features

- Add symbolic execution monitoring mechanism

** Documentation

  - Add the tutorial ["Checking *constant-time* security property"](doc/sse/relse.md)
  - Add the tutorial ["Monitoring the symbolic execution with a custom plugin"](doc/sse/plugins.md)
  
** Examples

  - Add a [shadow stack](src/sse/plugin/shadow_stack.ml) SSE plugin
  - Add a re-implementation of the [relational symbolic engine](src/sse/plugin/checkct.ml)

## 0.7.4 (2023-05-12)

** Bugs

  - Fix infinite loop on arm64 basic bloc disassembly

## 0.7.3 (2023-05-05)

** Bugs

  - Fix operator precedence issues in DBA parser
  - Expected fix for a hard to reproduce overlapping text issue
    at the end of SSE exploration
  - Fix issues with SSE intermediate representation

## 0.7.2 (2023-04-22)

** Bugs

  - Backport fixes for SSE intermediate representation
  
## 0.7.1 (2023-02-14)

** Features
  - New architecture support : Z80
  - New quick merging strategy in SSE
  - Support for custom array in SSE stubs

** Documentation

  - Add the write-up ["FCSC 2022: Licorne"](doc/sse/fcsc_licorne.md)

** Examples

  - Add SSE [`prechall` challenge](examples/sse/fcsc/2022.prechall) from FCSC 2022
  - Add SSE [`souk` challenge](examples/sse/fcsc/2022.souk) from FCSC 2022
  - Add SSE [`licorne` challenge](examples/sse/fcsc/2022.licorne) from FCSC 2022

## 0.6.3 (2022-12-08)

** Misc

  - Restore SSE timeout option
  - Enable non ELF nor PE file loading as a single contiguous bytes section

** Bugs

  - Fix rare issues with SMT solvers

## 0.6.2 (2022-11-09)

** Misc

  - Improve SSE SMT-LIB printer

** Bugs

  - Fix SSE screen not properly releasing the terminal
  - Fix SSE screen forget some pending logs
  - Correct typo from #17

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
