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

open Generic_decoder_sig

module Decode_Expr (I : Expr_Input) = struct
  open Dba
  open Binary_op
  open I

  let ( >>= ) = M.bind

  let rec expr : Dba.Expr.t -> I.binary M.m =
   fun e ->
    (* Logger.result "Expressions %a" Dba_printer.Ascii.pp_bl_term e; *)
    match e with
    | Expr.Cst bv ->
        let size = Bitvector.size_of bv in
        I.Binary.biconst ~size (Bitvector.value_of bv)
    | Expr.Binary (bop, e1, e2) -> (
        let size = Dba.Expr.size_of e1 in
        expr e1 >>= fun v1 ->
        expr e2 >>= fun v2 ->
        match bop with
        (* Binary operations. *)
        | Plus -> I.Binary.biadd ~size v1 v2
        | Minus -> I.Binary.bisub ~size v1 v2
        | Mult -> I.Binary.bimul ~size v1 v2
        | DivU -> I.Binary.biudiv ~size v1 v2
        | DivS -> I.Binary.bisdiv ~size v1 v2
        | ModU -> I.Binary.biurem ~size v1 v2
        | ModS -> I.Binary.bisrem ~size v1 v2
        | Or -> I.Binary.bor ~size v1 v2
        | And -> I.Binary.band ~size v1 v2
        | Xor -> I.Binary.bxor ~size v1 v2
        | Concat ->
            I.Binary.bconcat ~size1:(Dba.Expr.size_of e1) v1
              ~size2:(Dba.Expr.size_of e2) v2
        | LShift -> I.Binary.bshl ~size v1 v2
        | RShiftU -> I.Binary.blshr ~size v1 v2
        | RShiftS -> I.Binary.bashr ~size v1 v2
        | LeftRotate -> I.Binary.bv_left_rotate ~size v1 v2
        | RightRotate -> I.Binary.bv_right_rotate ~size v1 v2
        (* Predicates. *)
        | Eq -> I.Binary.beq ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
        | Diff ->
            I.Binary.beq ~size v1 v2 >>= fun bool ->
            I.Boolean.not bool >>= fun nbool -> I.bin_of_bool nbool
        | LeqU -> I.Binary.biule ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
        | GeqU -> I.Binary.biule ~size v2 v1 >>= fun bool -> I.bin_of_bool bool
        | LeqS -> I.Binary.bisle ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
        | GeqS -> I.Binary.bisle ~size v2 v1 >>= fun bool -> I.bin_of_bool bool
        | LtU -> I.Binary.biult ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
        | GtU -> I.Binary.biult ~size v2 v1 >>= fun bool -> I.bin_of_bool bool
        | LtS -> I.Binary.bislt ~size v1 v2 >>= fun bool -> I.bin_of_bool bool
        | GtS -> I.Binary.bislt ~size v2 v1 >>= fun bool -> I.bin_of_bool bool)
    | Expr.Unary (op, e1) as e -> (
        let size = Dba.Expr.size_of e in
        expr e1 >>= fun v1 ->
        match op with
        | Unary_op.UMinus ->
            I.Binary.biconst ~size Z.zero >>= fun vz ->
            I.Binary.bisub ~size vz v1
        | Unary_op.Not ->
            I.Binary.biconst ~size Z.minus_one >>= fun ffff ->
            I.Binary.bxor ~size ffff v1
        | Unary_op.Uext n ->
            I.Binary.buext ~size:n ~oldsize:(Dba.Expr.size_of e1) v1
        | Unary_op.Sext n ->
            I.Binary.bsext ~size:n ~oldsize:(Dba.Expr.size_of e1) v1
        | Unary_op.Restrict { Interval.lo; Interval.hi } ->
            I.Binary.bextract ~lo ~hi ~oldsize:(Dba.Expr.size_of e1) v1)
    | Expr.Var { name = var; size; _ } -> I.get_var ~size var
    | Expr.Load (size, endianness, e, None) ->
        expr e >>= fun address -> I.load ~size:(size * 8) endianness address
    | Expr.Load _ -> assert false
    | Expr.Ite (c, e1, e2) ->
        cond c >>= fun vc ->
        expr e1 >>= fun v1 ->
        expr e2 >>= fun v2 -> I.ite vc v1 v2

  and cond : Dba.Expr.t -> I.boolean M.m =
   fun e ->
    assert (Dba.Expr.size_of e == 1);
    let open Dba.Expr in
    match e with
    | Cst x when Bitvector.is_one x -> I.Boolean.true_
    | Cst x when Bitvector.is_zero x -> I.Boolean.false_
    | Cst _ -> assert false
    | Unary (Unary_op.Not, x) -> cond x >>= fun v -> I.Boolean.not v
    | Unary (Unary_op.UMinus, x) -> cond x
    | Binary (And, a, b) ->
        cond a >>= fun va ->
        cond b >>= fun vb -> I.Boolean.( && ) va vb
    | Binary (Or, a, b) ->
        cond a >>= fun va ->
        cond b >>= fun vb -> I.Boolean.( || ) va vb
    | e -> expr e >>= fun v -> I.bool_of_bin v
end

module Decode_Instr (S : Instr_Input) : sig
  val instruction :
    S.State.t ->
    Dba.Instr.t ->
    (S.boolean, S.binary) Generic_decoder_sig.jump_kind * S.State.t
end = struct
  module EDecode = Decode_Expr (struct
    include S
    module M = State_Monad (S.State)
  end)

  open Dba

  let write_lhs state value = function
    | LValue.Var { name; size; _ } -> S.set_var ~size name value state
    | LValue.Restrict ({ name; size; _ }, { Interval.lo; Interval.hi }) ->
        let value_size = size in
        let old, state = S.get_var ~size name state in
        let written_size = 1 + hi - lo in
        let v, state =
          if lo == 0 then (value, state)
          else
            let pold, state =
              S.Binary.bextract ~oldsize:value_size ~lo:0 ~hi:(lo - 1) old state
            in
            S.Binary.bconcat ~size1:written_size ~size2:lo value pold state
        in
        let v, state =
          if hi == size - 1 then (v, state)
          else
            let pold, state =
              S.Binary.bextract ~oldsize:value_size ~lo:(hi + 1) ~hi:(size - 1)
                old state
            in
            S.Binary.bconcat ~size1:(size - hi) ~size2:hi pold v state
        in
        S.set_var ~size name v state
    | LValue.Store (size, endianness, address, None) ->
        let vaddress, state = EDecode.expr address state in
        S.store ~size:(size * 8) endianness vaddress value state
    | LValue.Store _ -> assert false

  let instruction state instr =
    let open! Generic_decoder_sig in
    let open Instr in
    (* Logger.result "Instruction %a" Dba_printer.Ascii.pp_instruction instr; *)
    match instr with
    | Assign (lhs, expr, id) ->
        let v, state = EDecode.expr expr state in
        let state = write_lhs state v lhs in
        (JKJump (Static (JInner id)), state)
    | SJump (target, _) -> (JKJump (Static target), state)
    | DJump (target, _) ->
        let v, state = EDecode.expr target state in
        (JKJump (Dynamic v), state)
    | If (Dba.Expr.Cst bv, target, id) ->
        if Bitvector.is_one bv then (JKJump (Static target), state)
        else (JKJump (Static (JInner id)), state)
    | If (cond, target, id) ->
        let v, state = EDecode.cond cond state in
        (JKIf (v, Static target, Static (JInner id)), state)
    | Stop _ -> (JKStop, state)
    | Assume (cond, id) | Assert (cond, id) ->
        let v, state = EDecode.cond cond state in
        (JKAssume (v, Static (JInner id)), state)
    | Nondet (lhs, id) ->
        let size = assert false in
        let v, state = S.unknown ~size state in
        let state = write_lhs state v lhs in
        (JKJump (Static (JInner id)), state)
    | Undef (lhs, id) ->
        let size = Dba_types.LValue.unsafe_bitsize lhs in
        let v, state = S.undef ~size state in
        let state = write_lhs state v lhs in
        (JKJump (Static (JInner id)), state)
end
