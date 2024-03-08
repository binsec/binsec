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

(** Definitions of non-DBA types *)

type 'a interval = { lo : 'a; hi : 'a }
type u8 = int

(** {2 Maps & Sets on base types } *)

(* HINT:
   Always use fully-qualified names when using these modules in order not to
   clash with the standard library
*)
module String : Sigs.Collection with type t = string
module Int : Sigs.Collection with type t = int
module BigInt : Sigs.Collection with type t = Z.t
module Float : Sigs.Collection with type t = float

module Int64 : sig
  include Sigs.Collection with type t = Int64.t

  val max : t -> t -> t

  val is_int_int64 : t -> bool
  (** [is_int_int64 n] returns [true] if the value of [n] is also representable
      as an OCaml [int] on your machine
  *)
end

module Addr64 = Int64

(** {2 Functors } *)

module Collection_make : sig
  module Default (C : Sigs.COMPARABLE) : Sigs.Collection with type t = C.t
  module Auto (C : Sigs.COMPARABLE_EXT) : Sigs.Collection with type t = C.t
  module Hashed (C : Sigs.HASHABLE) : Sigs.Collection with type t = C.t
end

(** {2 Specific modules & types} *)

module Constants : sig
  val bytesize : Natural.t
end

(** {2 Ternary logic} *)
module Ternary : sig
  type t = True | False | Unknown

  val of_bool : bool -> t

  val to_bool : ?unknown:bool -> t -> bool
  (** [to_bool t] translates a ternary value to its boolean equivalent.
      For [Unknown] the value is given by [unknown] and defaults to [false].
  *)

  (** {3 Operations} *)

  include Sigs.LOGICAL with type t := t
end
