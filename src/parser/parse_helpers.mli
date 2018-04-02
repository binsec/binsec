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

val expr_of_name : string -> Dba.expr

val expr_size : Dba.expr -> int

val patch_expr_size : Dba.expr -> Dba.size -> Dba.expr

val cur_address : int ref
val incr_address : Dba.address -> unit

module Mk : sig
  (** General helper module for construction of Dba elements *)

  (* NOTE: This should probably be merged into Dba_types *)
  val filemode :
    Dba.cond -> bool -> bool -> bool ->
    (Dba.cond * (Dba_types.read_perm * Dba_types.write_perm * Dba_types.exec_perm)) *
    (Dba.cond * Dba.cond * Dba.cond)

  val checked_localized_instruction :
    Dba.address -> Dba.instruction -> Dba.address * Dba.instruction

  val program :
    (Dba_types.permissions list Dba_types.Region.Map.t *
     Dba.cond Dba_types.Rights.t) option ->
    Dba.instruction list ->
    Dba.address ->
    Dba.lhs list ->
    (Dba_types.Caddress.Map.key * Dba.instruction) list -> 'a Dba_types.program

  val checked_cond_expr : Dba.expr -> Dba.cond

  module Region : sig
    val malloc : int -> [> `Malloc of (int * Dba.address) * Bigint.t ]
  end

  module Predicates : sig
    val of_list :
      ('a * (Dba.cond * Dba.cond * Dba.cond)) list ->
      'a list * (Dba.cond * Dba.cond * Dba.cond)
  end


  module Permissions : sig
    val of_list :
      (Dba_types.Region.Map.key * 'a list * (Dba.cond * Dba.cond * Dba.cond)) list ->
      'a list Dba_types.Region.Map.t * Dba.cond Dba_types.Rights.t

  end

  module Lhs : sig
    val store : Dba.expr -> string -> Dba.endianness option -> Dba.lhs
    val declared : string -> Dba.lhs
    val declared_restricted : string -> int * int -> Dba.lhs
  end

  module Expr : sig
    val load : Dba.expr -> Dba.size -> Dba.endianness option -> Dba.expr
    val sized_region : Dba.region -> string * int -> Dba.expr
    val restricted : Dba.expr -> int * int -> Dba.expr
    val constant : string * int -> Dba.expr
    val declared_id : string -> Dba.expr
  end

end

module Declarations : sig
  val add : string -> int -> Dba.vartag option -> unit
end

module Message : sig
  module Value : sig
    type t = private
      | Hex of int
      | Int of int
      | Str of string
    val vhex : string -> t
    val vint : string -> t
    val vstr : string -> t
  end
end

val mk_patches : (int * Basic_types.Binstream.t) list -> Basic_types.Binstream.t Dba_types.Virtual_address.Map.t
