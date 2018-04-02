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

(* The DBA types without their successors, that will be added at instruction chaining (see chain_insns) *)
(* For static jumps. Tags may indicate a probable call / return instruction *)
type t =
  | Assign of Dba.lhs * Dba.expr
  | SJump of Dba.jump_target * Dba.tag option
  | DJump of Dba.expr * Dba.tag option
  | If of Dba.cond * Dba.jump_target
  | Undef of Dba.lhs
  | Nondet of Dba.lhs * Dba.region
  | Stop of Dba.state


let assign lval e = Assign (lval, e)

let static_jump ?(tag=None) jt = SJump (jt, tag)

let dynamic_jump ?(tag=None) e = DJump (e, tag)

let jif c jt = If (c, jt)

let undefined lval = Undef lval

let non_deterministic lval r = Nondet (lval, r)

let stop s = Stop s
  
let needs_termination = function
  | Dba.IkDJump _
  | Dba.IkSJump (Dba.JOuter _, _)
  | Dba.IkStop _ -> false
  | Dba.IkAssume _ | Dba.IkAssert _
  | Dba.IkNondetAssume _ | Dba.IkMalloc _
  | Dba.IkFree _ | Dba.IkPrint _
  | Dba.IkAssign _
  | Dba.IkUndef _
  | Dba.IkIf _
  | Dba.IkNondet _
  | Dba.IkSJump _ -> true


let to_dba_instruction next_id = function
  | If (cond, thn) -> Dba_types.Instruction.ite cond thn next_id
  | Assign (lhs, expr) -> Dba_types.Instruction.assign lhs expr next_id
  | Undef lhs -> Dba_types.Instruction.undefined lhs next_id
  | Nondet (lhs, region) -> Dba_types.Instruction.non_deterministic lhs region next_id
  | SJump (dst, tag) -> Dba_types.Instruction.static_jump dst ~tag
  | DJump (dst, tag) -> Dba_types.Instruction.dynamic_jump dst ~tag
  | Stop st -> Dba_types.Instruction.stop (Some st)

(* [block_addr] is the physical address of the current DBA block.
   [next_addr] is where the next block is in the sequence.
   [instrucitons] is the list of instructions for this block.
*)
let blockify next_addr instructions =
  let end_jump = Dba_types.Instruction.static_jump (Dba.JOuter next_addr) in
  (* The chained list is constructed in reverse order in acc *)
  let rec aux n acc = function
    | [] ->
      begin
        match acc with
        | [] -> [end_jump]
        | instr :: _ ->
          if needs_termination instr then end_jump :: acc else acc
      end
    | instr :: instructions ->
      let id = n + 1 in
      let dba_instr = to_dba_instruction id instr in
      aux id (dba_instr :: acc) instructions
  in aux 0 [] instructions |> List.rev |> Dba_types.Block.of_list 
