(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

let is_cond_jump inst =
  List.exists (fun x ->
      match Dba_types.Statement.instruction x with
      | IkIf (_, JOuter _ ,_) -> true
      | IkAssign (_, _, _)|IkSJump (_, _)|IkStop _|IkAssert (_, _) | IkDJump (_, _)
      | IkNondet (_, _, _)|IkUndef (_, _)|IkMalloc (_, _, _) |IkFree (_, _)|IkPrint (_, _)
      | IkAssume (_, _)|IkNondetAssume (_, _, _) | _ -> false) inst.dbainstrs

let is_dyn_jump inst =
  List.exists (fun x -> match  Dba_types.Statement.instruction x  with
      | IkDJump _ -> true
      | IkAssign (_, _, _)|IkSJump (_, _)|IkStop _|IkAssert (_, _) | IkIf (_, _, _)
      | IkNondet (_, _, _)|IkUndef (_, _)|IkMalloc (_, _, _) |IkFree (_, _)|IkPrint (_, _)
      | IkAssume (_, _)|IkNondetAssume (_, _, _) -> false) inst.dbainstrs

let _is_cond_or_dyn_jump inst =
  List.exists (fun x -> match  Dba_types.Statement.instruction x with
      | IkIf _  | IkDJump _ -> true
      | IkAssign (_, _, _)|IkSJump (_, _)|IkStop _|IkAssert (_, _)
      | IkNondet (_, _, _)|IkUndef (_, _)|IkMalloc (_, _, _) |IkFree (_, _)|IkPrint (_, _)
      | IkAssume (_, _)|IkNondetAssume (_, _, _) -> false) inst.dbainstrs

let is_call inst =
  try
    match String.sub inst.opcode 1 5 with
    | "call " | "dcall" -> true
    | _ -> false
  with _ -> false

let is_ret inst =
  try
    match String.sub inst.opcode 1 3 with
    | "ret" ->true
    | _ -> false
  with _ -> false

(*    List.exists (fun x -> match snd x with | IkSJump _ -> true | _ -> false) inst.dbainstrs*)

let is_libcall inst =
  List.exists (fun x ->
      match x with
      | Libcall _ -> true
      | NextAddr _|RegRead (_, _)|RegWrite (_, _)|Syscall _|MemLoad (_, _)
      | MemStore (_, _)|Not_retrieved|Comment _|Wave _ -> false
    ) inst.concrete_infos
