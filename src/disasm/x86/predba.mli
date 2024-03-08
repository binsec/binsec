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

(** First IL before producing DBA *)

type 'a t = private
  | Assign of Dba.LValue.t * Dba.Expr.t
  | SJump of 'a Dba.jump_target * Dba.tag option
  | DJump of Dba.Expr.t * Dba.tag option
  | Assert of Dba.Expr.t
  | If of Dba.Expr.t * 'a Dba.jump_target
  | Undef of Dba.LValue.t
  | Nondet of Dba.LValue.t
  | Stop of Dba.state

val assign : Dba.LValue.t -> Dba.Expr.t -> 'a t
val ( <<- ) : Dba.LValue.t -> Dba.Expr.t -> 'a t
val static_jump : ?tag:Dba.tag -> 'a Dba.jump_target -> 'a t
val dynamic_jump : ?tag:Dba.tag -> Dba.Expr.t -> 'a t
val dynamic_assert : Dba.Expr.t -> 'a t
val conditional_jump : Dba.Expr.t -> 'a Dba.jump_target -> 'a t
val undefined : Dba.LValue.t -> 'a t
val non_deterministic : Dba.LValue.t -> 'a t
val stop : Dba.state -> 'a t

val blockify : Dba.address -> Dba.id t list -> Dhunk.t
(** [blockify next_addr predbas]
    @return a full DBA block considering it continues to [next_addr]
*)
