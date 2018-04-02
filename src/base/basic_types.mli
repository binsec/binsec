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

(** Definitions of non-DBA types *)

(** {2 Maps & Sets on base types } *)

(* HINT:
   Always use fully-qualified names when using these modules in order not to
   clash with the standard library
*)
module String : sig
  include Sigs.Collection with type t = string
  module Hashtbl : Hashtbl.S with type key = string
end

module Int : Sigs.Collection with type t = int

module BigInt : Sigs.Collection with type t = Bigint.t

module Int64 : sig
  include Sigs.Collection with type t = Int64.t
  module Hashtbl : Hashtbl.S with type key = Int64.t
  val max : t -> t -> t
end

module Addr64 = Int64

(** {2 Functors } *)

module MapSetMaker(C:Sigs.Comparable) : Sigs.Collection with type t = C.t

(** {2 Specific modules & types} *)
module Natural : sig
  type t = private int
  val create : int -> t
  val add : t -> t -> t
  val add_int : t -> int -> t
  val sub : t -> t -> t
  val sub_int : t -> int -> t
  val eq : t -> t -> bool
  val gt : t -> t -> bool
  val ge : t -> t -> bool
end

module Constants : sig
  val bytesize : Natural.t
end


(** {2 Size} *)
module type Size = sig
  type t = Natural.t
  val create : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val pp_hex : Format.formatter -> t -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  val pred : t -> t
  val is_zero : t -> bool
end

module BitSize : sig
  include Size

  val bits1   : t
  val bits8   : t
  val bits16  : t
  val bits32  : t
  val bits64  : t
  val bits128 : t
end

module ByteSize : sig
  include Size
  val to_bitsize : t -> BitSize.t
  val of_bitsize : BitSize.t -> t
end

(** {2 Binary stream} *)

module Binstream : sig
  type t

  (** {3 Constructors } *)

  val empty : t


  val of_nibbles : string -> t
  (** [of_nibbles s] converts a string [s] of hexadecimal characters.

      @assumes each character is in the range [0-9a-f].
  *)

  val of_bytes : string -> t
  (** [of_bytes s] converts a byte stream [s].
      Each character stands for its own byte.
  *)

  val of_list : int list -> t
  (** [of_list l] converts a list of integers.

      @assumes: each integer n is in the range 0 <= n <= 255.
  *)

  (** {3 Operations} *)

  val append_int    : int -> t -> t
  val append_int64  : int64 -> t -> t
  val prepend_int   : int -> t -> t
  val prepend_int64 : int64 -> t -> t

  (** {3 Accessors} *)

  val length : t -> int

  val get_byte : t -> int -> int
  (** [get_byte b n] retrieves byte number [n] from [b]

      The byte sequence is 0-indexed.

      @raise Invalid_argument if the index is not between [0] and [length b - 1]
  *)

  val to_string : t -> string

  (** {3 Printers} *)
  include Sigs.Printable with type t := t
end

(** {2 Ternary logic} *)
module Ternary : sig
  type t =
    | True
    | False
    | Unknown

  val of_bool : bool -> t

  val to_bool : ?unknown:bool -> t -> bool
  (** [to_bool t] translates a ternary value to its boolean equivalent.
      For [Unknown] the value is given by [unknown] and defaults to [false].
  *)

  (** {3 Operations} *)
  include Sigs.Logical with type t := t
end
