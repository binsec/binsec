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

(* The DBA types without their successors.
 * That will be added at instruction chaining (see chain_insns)
 * For static jumps. Tags may indicate a probable call / return instruction
 *)

open X86_options

type 'a t =
  | Assign of Dba.LValue.t * Dba.Expr.t
  | SJump of 'a Dba.jump_target * Dba.tag option
  | DJump of Dba.Expr.t * Dba.tag option
  | Assert of Dba.Expr.t
  | If of Dba.Expr.t * 'a Dba.jump_target
  | Undef of Dba.LValue.t
  | Nondet of Dba.LValue.t
  | Stop of Dba.state

let assign lval e =
  Logger.debug ~level:5 "@[<hov 0>Assigning %a (%d) with %a (%d)@]"
    Dba_printer.Ascii.pp_lhs lval (Dba.LValue.size_of lval)
    Dba_printer.Ascii.pp_bl_term e (Dba.Expr.size_of e);

  assert (Dba.LValue.size_of lval = Dba.Expr.size_of e);
  Assign (lval, e)

let ( <<- ) = assign
let static_jump ?tag jt = SJump (jt, tag)
let dynamic_jump ?tag e = DJump (e, tag)
let dynamic_assert e = Assert e
let conditional_jump c jt = If (c, jt)
let undefined lval = Undef lval
let non_deterministic lval = Nondet lval
let stop s = Stop s

let needs_termination = function
  | Dba.Instr.DJump _ | Dba.Instr.SJump (Dba.JOuter _, _) | Dba.Instr.Stop _ ->
      false
  | Dba.Instr.Assume _ | Dba.Instr.Assert _ | Dba.Instr.Assign _
  | Dba.Instr.Undef _ | Dba.Instr.If _ | Dba.Instr.Nondet _ | Dba.Instr.SJump _
    ->
      true

let to_dba_instruction next_id = function
  | Assert cond -> Dba.Instr._assert cond next_id
  | If (cond, thn) -> Dba.Instr.ite cond thn next_id
  | Assign (lhs, expr) -> Dba.Instr.assign lhs expr next_id
  | Undef lhs -> Dba.Instr.undefined lhs next_id
  | Nondet lhs -> Dba.Instr.non_deterministic lhs next_id
  | SJump (dst, tag) -> Dba.Instr.static_jump dst ?tag
  | DJump (dst, tag) -> Dba.Instr.dynamic_jump dst ?tag
  | Stop st -> Dba.Instr.stop (Some st)

(* [block_addr] is the physical address of the current DBA block.
   [next_addr] is where the next block is in the sequence.
   [instrucitons] is the list of instructions for this block.
*)
let blockify next_addr instructions =
  let end_jump = Dba.Instr.static_jump (Dba.JOuter next_addr) in
  (* The chained list is constructed in reverse order in acc *)
  let rec aux n acc = function
    | [] -> (
        match acc with
        | [] -> [ end_jump ]
        | instr :: _ -> if needs_termination instr then end_jump :: acc else acc
        )
    | instr :: instructions ->
        let id = n + 1 in
        let dba_instr = to_dba_instruction id instr in
        aux id (dba_instr :: acc) instructions
  in
  aux 0 [] instructions |> List.rev |> Dhunk.of_list
