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

(** Definitions of non-DBA types *)

type 'a interval = { lo : 'a; hi : 'a }
type endianness = LittleEndian | BigEndian

(** {2 Integers} *)

module Integers : sig
  type uint8 = private int
  type uint16 = private int
  type uint32 = private int32
  type uint64 = private int64
  type int8 = private int
  type int16 = private int

  module type S = sig
    type t

    val to_uint8 : t -> uint8
    val to_uint16 : t -> uint16
    val to_uint32 : t -> uint32
    val to_uint64 : t -> uint64
    val to_int8 : t -> int8
    val to_int16 : t -> int16
    val to_int32 : t -> int32
    val to_int64 : t -> int64
    val to_int : t -> int
    val to_bigint : t -> Z.t
  end

  module Uint8 : sig
    include S with type t = uint8

    external to_char : t -> char = "%identity"
    external to_int : t -> int = "%identity"
  end

  module Uint16 : sig
    include S with type t = uint16

    external to_int : t -> int = "%identity"
  end

  module Uint32 : S with type t = uint32
  module Uint64 : S with type t = uint64
  module Int8 : S with type t = int8
  module Int16 : S with type t = int16

  module Int32 : sig
    include module type of Int32
    include S with type t = int32
  end

  module Int64 : sig
    include module type of Int64
    include S with type t = int64
  end

  module Int : sig
    include Collection.S with type t = int
    include S with type t := int

    external unsafe_to_uint8 : t -> uint8 = "%identity"
  end

  module Bigint : sig
    include module type of Z
    include Collection.S with type t := Z.t
    include S with type t := Z.t
  end
end

(** {2 Maps & Sets on base types } *)

(* HINT:
   Always use fully-qualified names when using these modules in order not to
   clash with the standard library
*)
module String : Collection.S with type t = string

(* module BigInt : Collection.S with type t = Z.t *)
module Float : Collection.S with type t = float

(** {2 Functors } *)

module Collection_make : sig
  module Default (C : Sigs.COMPARABLE) : Collection.S with type t = C.t
  module Auto (C : Sigs.COMPARABLE_EXT) : Collection.S with type t = C.t
  module Hashed (C : Sigs.HASHABLE) : Collection.S with type t = C.t
end

(** {2 Specific modules & types} *)

module Constants : sig
  val bytesize : Natural.t
end

(** {2 Ternary logic} *)
module Ternary : sig
  type t = Domains.trilean = False | True | Unknown

  val of_bool : bool -> t

  val to_bool : ?unknown:bool -> t -> bool
  (** [to_bool t] translates a ternary value to its boolean equivalent.
      For [Unknown] the value is given by [unknown] and defaults to [false].
  *)

  (** {3 Operations} *)

  include Sigs.LOGICAL with type t := t
end
