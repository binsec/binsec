(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

open Trace_type
open Dba
open Instr

let is_cond_jump inst =
  List.exists (fun x ->
      match Dba_types.Statement.instruction x with
      | If (_, JOuter _ ,_) -> true
      | Assign (_, _, _)|SJump (_, _)|Stop _|Assert (_, _) | DJump (_, _)
      | Nondet (_, _, _)|Undef (_, _)|Malloc (_, _, _) |Free (_, _)|Print (_, _)
      | Assume (_, _)|NondetAssume (_, _, _) | _ -> false) inst.dbainstrs

let is_dyn_jump inst =
  List.exists (fun x -> match  Dba_types.Statement.instruction x  with
      | DJump _ -> true
      | Assign (_, _, _)|SJump (_, _)|Stop _|Assert (_, _) | If (_, _, _)
      | Nondet (_, _, _)|Undef (_, _)|Malloc (_, _, _) |Free (_, _)|Print (_, _)
      | Assume (_, _)|NondetAssume (_, _, _) -> false) inst.dbainstrs

let _is_cond_or_dyn_jump inst =
  List.exists (fun x -> match  Dba_types.Statement.instruction x with
      | If _  | DJump _ -> true
      | Assign (_, _, _)|SJump (_, _)|Stop _|Assert (_, _)
      | Nondet (_, _, _)|Undef (_, _)|Malloc (_, _, _) |Free (_, _)|Print (_, _)
      | Assume (_, _)|NondetAssume (_, _, _) -> false) inst.dbainstrs

let is_call inst =
  match String.sub inst.mnemonic 1 5 with
  | "call " | "dcall" -> true
  | _any -> false
  | exception Invalid_argument _ -> false

let is_ret inst =
  match String.sub inst.mnemonic 1 3 with
  | "ret" -> true
  | _any -> false
  | exception Invalid_argument _ -> false

(*    List.exists (fun x -> match snd x with | SJump _ -> true | _ -> false) inst.dbainstrs*)

let is_libcall inst =
  List.exists (fun x ->
      match x with
      | Libcall _ -> true
      | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)
      | MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false
    ) inst.concrete_infos
