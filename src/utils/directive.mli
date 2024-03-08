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

(** Directives are general located goals that one would like to achieve during an
 *  analysis
 *)

module Action : sig
  type format = Bin | Dec | Hex | Ascii

  type t =
    | Print_formula of (Dba.Expr.t * string) list option
    | Print_model
    | Print_value of format * Dba.Expr.t
    | Print_stream of string
end

module Choice : sig
  type side = Consequent | Alternative
  type t

  val do_alternate : t -> unit
  val is_alternative : t -> bool
  val is_consequent : t -> bool
end

module Count : sig
  type t = private Unlimited | Count of int

  val pp : Format.formatter -> t -> unit

  val once : t
  (** [once] is [Count 1] *)

  val count : int -> t
  val unlimited : t
  val decr : t -> t
  val is_zero : t -> bool
end

type d = private
  | Reach of Count.t * Dba.Expr.t * Action.t list
  | Enumerate of int * Dba.Expr.t
  | Cut of Dba.Expr.t
  | Assume of Dba.Expr.t
  | Assert of Dba.Expr.t
  | Choice of Choice.t

type t

val check_and_decr : t -> t option
val directive : t -> d
val loc : t -> Dba.Expr.t
val addr : t -> Virtual_address.t
val pp : Format.formatter -> t -> unit

(** {2 Constructors} *)

val reach :
  ?n:int ->
  ?guard:Dba.Expr.t ->
  ?actions:Action.t list ->
  loc:Dba.Expr.t ->
  unit ->
  t

val reach_all :
  ?guard:Dba.Expr.t -> ?actions:Action.t list -> loc:Dba.Expr.t -> unit -> t

val enumerate : ?n:int -> Dba.Expr.t -> loc:Dba.Expr.t -> unit -> t
val enumerate_all : Dba.Expr.t -> loc:Dba.Expr.t -> unit -> t
val cut : ?guard:Dba.Expr.t -> loc:Dba.Expr.t -> unit -> t
val assume : Dba.Expr.t -> loc:Dba.Expr.t -> unit -> t
val dynamic_assert : Dba.Expr.t -> loc:Dba.Expr.t -> unit -> t
val choose : alternate:bool -> side:Choice.side -> d
val choose_alternative : ?alternate:bool -> loc:Dba.Expr.t -> unit -> t
val choose_consequent : ?alternate:bool -> loc:Dba.Expr.t -> unit -> t
val is_choice : t -> bool
val reloc : Dba.Expr.t -> t -> t
