(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type kind =
  | Fallthrough  (** The instruction jumps to its immediate follower. *)
  | Branch  (** The instruction branchs to another one. *)
  | Call  (** The instruction calls a function. *)
  | Return of Virtual_address.t  (** The instruction returns to the caller. *)
  | Presumed
      (** The instruction calls a function that may not return properly.
          Its immediate follower is taken as successor. *)

include
  Graph.Sig.I
    with type V.t = Virtual_address.t
     and type E.t = Virtual_address.t * kind * Virtual_address.t

val parse_cache : path:string -> t * string Virtual_address.Htbl.t
(** [parse_cache ~path]
    build a new graph from the saved textual output of a previously
    Ghidra run.

    @param path The path of the Ghidra log.
    @return     A new imperative graph with its mnemonic mapping.
*)

val run_ghidra :
  ?temp_dir:string ->
  ?cache:string ->
  runner:string ->
  string ->
  t * string Virtual_address.Htbl.t
(** [run_ghidra ?cache ~runner binary]
    run Ghidra disassembly on the binary file and extract its control
    flow graph.

    @param temp_dir The path of the workspace in which Ghidra will be run
                    (default is /dev/shm).
    @param cache    If any, save in this file the textual output of Ghidra
                    for later use.
    @param runner   The path of the the analyzeHeadless Ghidra script.
    @param binary   The path of the binary under study.
    @return         A new imperative graph with its mnemonic mapping.
*)

val import : unit -> t * string Virtual_address.Htbl.t
(** [import ()]
    calls [run_ghidra] or [parse_cache] on the executatble file
    ([Kernel_options.ExecFile]) according to the global options
    [Ghidra_options.Runner] and [Ghidra_options.Cache].

    @return         A new imperative graph with its mnemonic mapping.
*)
