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

(** First IL before producing DBA *)

type t = private
  | Assign of Dba.lhs * Dba.expr
  | SJump of Dba.jump_target * Dba.tag option
  | DJump of Dba.expr * Dba.tag option
  | If of Dba.cond * Dba.jump_target
  | Undef of Dba.lhs
  | Nondet of Dba.lhs * Dba.region
  | Stop of Dba.state

val assign : Dba.lhs -> Dba.expr -> t

val static_jump : ?tag:Dba.tag option -> Dba.jump_target -> t

val dynamic_jump : ?tag:Dba.tag option -> Dba.expr -> t

val jif : Dba.cond -> Dba.jump_target -> t

val undefined : Dba.lhs -> t

val non_deterministic : Dba.lhs -> Dba.region -> t

val stop : Dba.state -> t

val blockify : Dba.address -> t list -> Dba_types.Block.t
(** [blockify next_addr predbas] 
    @returns a full DBA block considering it continues to [next_addr]
*)
