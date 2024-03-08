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

type state = {
  cfg : Ghidra_cfg.t;  (** The Ghidra cfg *)
  ims : string Virtual_address.Htbl.t;  (** Instruction mnemonic storage *)
  ctp : Virtual_address.Set.t;  (** Call to precess *)
  dis : Dba.Instr.t Dba_types.Caddress.Htbl.t;  (** Dba instruction storage *)
  dap : Dba_types.Caddress.t list Dba_types.Caddress.Htbl.t;
      (** Dba address predecessors *)
  opa : bool Dba_types.Caddress.Htbl.t;  (** Opaque predicate addresses *)
}

val find_condition :
  state -> Virtual_address.t -> Dba_types.Caddress.t * Dba.Expr.t
(** [find_condition state addr] searches the full address and the test
    expression of a conditional branch instruction.
    It will fill the instruction storage and address predecessors if
    the address was not yet encountered.

    @param state The global environment.
    @param addr The address to search for conditional test.

    @return The address and the test of the conditional statement.
    @raise Not_found Instruction at [addr] is not recognized as a
           conditional branch.
*)

val enumerate_path :
  state ->
  int ->
  Dba_types.Caddress.t ->
  (Dba_types.Caddress.t * bool list * Virtual_address.t list) list
(** [enumerate_path state n addr] returns the list of all paths of
    [n] basic blocks that reach the address [addr].

    @param state The global environment.
    @param n The number of basic blocks to search backward.
    @param addr The address to start the backward search.

    @return The list of all path that reach [addr] in [n] basic blocks.
*)
