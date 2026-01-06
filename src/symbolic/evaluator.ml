(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Make
    (Value : State.VALUE)
    (State : State.DATA with type value := Value.t) : sig
  val eval : Dba.Expr.t -> State.t -> Value.t
end = struct
  let uop e (o : Dba.Unary_op.t) : Term.unary Term.operator =
    match o with
    | Not -> Not
    | UMinus -> Minus
    | Sext n -> Sext (n - Dba.Expr.size_of e)
    | Uext n -> Uext (n - Dba.Expr.size_of e)
    | Restrict interval -> Restrict interval

  let bop (op : Dba.Binary_op.t) : Term.binary Term.operator =
    match op with
    | Plus -> Plus
    | Minus -> Minus
    | Mult -> Mul
    | DivU -> Udiv
    | DivS -> Sdiv
    | RemU -> Urem
    | RemS -> Srem
    | Eq -> Eq
    | Diff -> Diff
    | LeqU -> Ule
    | LtU -> Ult
    | GeqU -> Uge
    | GtU -> Ugt
    | LeqS -> Sle
    | LtS -> Slt
    | GeqS -> Sge
    | GtS -> Sgt
    | Xor -> Xor
    | And -> And
    | Or -> Or
    | Concat -> Concat
    | LShift -> Lsl
    | RShiftU -> Lsr
    | RShiftS -> Asr
    | LeftRotate -> Rol
    | RightRotate -> Ror

  let rec eval (e : Dba.Expr.t) state =
    match e with
    | Cst bv | Var { info = Symbol (_, (lazy bv)); _ } -> Value.constant bv
    | Var var -> State.lookup var state
    | Load (len, dir, addr, None) ->
        fst (State.read ~addr:(eval addr state) len dir state)
    | Load (len, dir, addr, Some name) ->
        fst (State.select name ~addr:(eval addr state) len dir state)
    | Unary (f, x) -> Value.unary (uop x f) (eval x state)
    | Binary (f, x, y) -> Value.binary (bop f) (eval x state) (eval y state)
    | Ite (c, r, e) -> Value.ite (eval c state) (eval r state) (eval e state)
end
