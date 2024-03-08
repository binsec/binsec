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

val incr_address : Dba.address -> unit
val cur_address : unit -> int
val patch_expr_size : Dba.Expr.t -> int -> Dba.Expr.t
val expr_of_name : string -> Dba.Expr.t

module Initialization : sig
  type rvalue =
    | Nondet
    | Signed_interval of Dba.Expr.t * Dba.Expr.t
    | Unsigned_interval of Dba.Expr.t * Dba.Expr.t
    | Set of Dba.Expr.t list
    | Singleton of Dba.Expr.t

  type identifier = string

  type operation =
    | Assignment of Dba.LValue.t * rvalue * identifier option
    | Mem_load of Dba.Expr.t * int
    | Assumption of Dba.Expr.t
    | Universal of Dba.LValue.t

  type t = { controlled : bool; operation : operation }

  val assume : Dba.Expr.t -> t

  val assign :
    ?identifier:string -> ?controlled:bool -> Dba.LValue.t -> rvalue -> t

  val universal : Dba.LValue.t -> t
  (** Mark l-value as universally quantified *)

  val from_store : ?controlled:bool -> Dba.LValue.t -> t

  val from_assignment :
    ?identifier:string -> ?controlled:bool -> Dba.Instr.t -> t

  val set_control : bool -> t -> t
end

module Message : sig
  module Value : sig
    type t = Int of Z.t | Str of string

    val vstr : string -> t
    val vint : string -> t
  end
end

module Declarations : sig
  val add : string -> Dba.size -> Dba.Var.Tag.t -> unit
end

module Mk : sig
  val filemode :
    'a ->
    bool ->
    bool ->
    bool ->
    ('a * (Dba_types.read_perm * Dba_types.write_perm * Dba_types.exec_perm))
    * (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t)

  val checked_localized_instruction :
    Dba_types.Caddress.t -> Dba.Instr.t -> Dba_types.Caddress.t * Dba.Instr.t

  val checked_cond_expr : Dba.Expr.t -> Dba.Expr.t

  val program :
    Dba.Instr.t list ->
    Dba.address ->
    Dba.LValue.t list ->
    (Dba_types.Caddress.Map.key * Dba.Instr.t) list ->
    Dba_types.program

  module Predicates : sig
    val of_list :
      ('a * (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t)) list ->
      'a list * (Dba.Expr.t * Dba.Expr.t * Dba.Expr.t)
  end
end

val mk_patches : (int * 'a) list -> 'a Virtual_address.Map.t
