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

(** Bitvectors with low-level regions *)

type 'a symbol

module rec SubSymb : sig
  type t = (int SymbMap.t) symbol
  val compare : t -> t -> int
end

and SymbMap : Map.S with type key = (int SymbMap.t) symbol

type t =
  [
     Smt_bitvectors.basic_value
    | `Symb of (int SymbMap.t) symbol
    | `SymbSmt of Smt_bitvectors.smtBvExprAlt
  ]

val region_equal : Dba.region -> Dba.region -> bool
val equal : t -> t -> bool


(** {2 Construction functions} *)

val create_constant : Bigint.t -> int -> t
(** [create_constant v sz] creates a bitvector of value [bv] with size
    [sz] in region [`Constant]
*)

val zeros : int -> t
(** [zeros size] creates a bitvector of value 0 with size [size] in region
    [`Constant]*)

val undefined : int -> t
(** [undefined n] creates an undefined value of bitsize [n] *)

(** {7 Pretty-printing} *)
val pp : Format.formatter -> t -> unit
val to_string : t -> string


(** {2 Accessors} *)
val region_of : t -> Dba.region
val value_of : t -> Bigint.t
val bitvector_of : t -> Bitvector.t
val size_of : t -> int
val st_of : Bitvector.t -> string (*FIXME *)
val append : t -> t -> t
val non_deterministic: Dba.region -> int -> t
val restrict : t -> int -> int -> t

val succ : t -> t

include Sigs.Arithmetic with type t := t


val lshift : t -> t -> t
val rshiftS : t -> t -> t
val rshiftU : t -> t -> t

val rotate_left : t -> t -> t
val rotate_right : t -> t -> t

val extension : t -> int -> t
val signed_extension : t -> int -> t
val eq : t -> t -> t


val diff : t -> t -> t

val leqU : t -> t -> t
val leqS : t -> t -> t
val ltU : t -> t -> t
val ltS : t -> t -> t
val gtU : t -> t -> t
val gtS : t -> t -> t
val geqU : t -> t -> t
val geqS : t -> t -> t

val lognot : t -> t
val logxor : t -> t -> t
val logor : t -> t -> t
val logand : t -> t -> t

val is_zero : t -> bool

val display_statistics : Format.formatter -> unit -> unit

val get_value :
  Smt_bitvectors.smtBvExprAlt -> int -> Smt_bitvectors.smtBvExprAlt list -> Dba_types.Caddress.Set.t
  -> t

val get_expr :
  Smt_bitvectors.smtBvExprAlt -> int -> Smt_bitvectors.smtBvExprAlt list -> Dba_types.Caddress.Set.t
  -> Dba.expr


val get_byte_region_at : Bigint.t -> t
(** [get_byte_region_at addr] returns the value read at the lone byte cell
    [addr].
    @throws [Invalid_address msg] if [addr] is out of range
*)

val default_get_byte_region_at : Bigint.t -> t
(** [default_get_byte_region_at addr] is [get_byte_region_at addr] but catches
    the possible exception and returns an undefined byte instead.
*)
