(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

val incr_address : Dba.address -> unit
val cur_address  : unit -> int

val patch_expr_size : Dba.Expr.t -> int -> Dba.Expr.t

val expr_of_name : string -> Dba.Expr.t

module Initialization : sig
  type interval_or_set =
    | SignedInterval of Dba.Expr.t * Dba.Expr.t
    | UnsignedInterval of Dba.Expr.t * Dba.Expr.t
    | Set of Dba.Expr.t list

  type t =
    | Assignment of Dba.LValue.t * Dba.Expr.t
    | MemLoad of Int64.t * int
    | NondeterministicAssignment of Dba.LValue.t * interval_or_set


  val from_store: Dba.LValue.t -> t
  val from_assignment: Dba.Instr.t -> t
end

module Message : sig
  module Value : sig
    type t =
      | Hex of int
      | Bin of int
      | Int of int
      | Str of string

    val vstr : string -> t
    val vhex : string -> t
    val vbin : string -> t
    val vint : string -> t
  end

end


module Declarations : sig
  val add : string -> Dba.size -> Dba.VarTag.t option -> unit
end

module Mk : sig
  val filemode :
    'a ->
    bool ->
    bool ->
    bool ->
    ('a * (Dba_types.read_perm * Dba_types.write_perm * Dba_types.exec_perm)) *
      (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t)

  val checked_localized_instruction:
    Dba_types.Caddress.t -> Dba.Instr.t -> Dba_types.Caddress.t * Dba.Instr.t

  val checked_cond_expr: Dba.Expr.t -> Dba.Expr.t

  val program :
    (Dba_types.permissions list Dba_types.Region.Map.t *
       Dba.Expr.t Dba_types.Rights.t)
      option ->
    Dba.Instr.t list ->
    Dba.address ->
    Dba.LValue.t list ->
    (Dba_types.Caddress.Map.key * Dba.Instr.t) list -> 'a Dba_types.program

  module Predicates : sig
    val of_list :
      ('a * (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t)) list ->
      'a list * (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t)
  end

  module Permissions : sig
    val of_list :
      (Dba_types.Region.Map.key * 'a list * (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t))
        list -> 'a list Dba_types.Region.Map.t * Dba.Expr.t Dba_types.Rights.t
  end
end

val mk_patches :
  (int * 'a) list -> 'a Virtual_address.Map.t
