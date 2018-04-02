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

(** Taint types DSE *)

type taint =
  | NoTaint  (** No taint *)
  | TaintI   (** Tainted as input (directly controllable *)
  | TaintP   (** Tainted by propagation *)
  | TaintMix of taint list (** Mix of taint (bit-level) *)

type taint_strategy =
  | May      (** Taint all memory in case of symbolic load/store *)
  | Must     (** Do not taint all the memory in case of symbolic load/store (just forget about the it) → (break soundness?) *)
  | MustConc (** In case of symbolic load/store concretise the address → (be careful: the c/s policy should be in accordance with that) *)

(** Keep the taint on the various infos (deprecated) *)
type taint_infos =
  | Load of taint
  | Store of taint
  | LoadAddr of taint
  | StoreAddr of taint
  | Variable of string * int * int * taint
  | LhsVar of string * int * int * taint

(** Set of taint infos *)
module TaintInfoSet : Set.S with type elt = taint_infos

(** return the string associated to the given taint *)
val taint_to_string : taint -> string