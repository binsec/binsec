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

(** Loader utility functions *)

val find_section_by_name : string -> Loader.Img.t -> Loader.Section.t

val section_slice_by_name : string -> Loader.Img.t -> int * int
(** [section_slice section_name img] returns the interval [lo, hi] of virtual
    addresses defining the section [section_name].
*)

val find_section_by_address :
  address:int -> Loader.Img.t -> Loader.Section.t option

val find_section_by_address_exn :
  address:int -> Loader.Img.t -> Loader.Section.t
(** @raise Failure exception if no such section exists *)

val section_slice_by_address : address:int -> Loader.Img.t -> int * int

val find_section :
  p:(Loader.Section.t -> bool) -> Loader.Img.t -> Loader.Section.t option

(** { Manipulation of symbols } **)

(** Functions that are of the form [f_by_name] call the function
    [symbol_by_name] which is costly because it compares all the symbols in the
    image to the requested [name]. Use them with parcimony and cache the symbols
    if they are requested multiple times. *)

val symbol_by_name : name:String.t -> Loader.Img.t -> Loader.Symbol.t option
(** [symbol_by_name ~name img] Returns [Some] symbol [name] in [img].
    If [img] contains no symbol [name], returns [None]. *)

val address_of_symbol : Loader.Symbol.t -> int
(** [address_of_symbol symbol] finds [Some address] where the symbole
    is defined. Otherwise returns [None]. *)

val address_of_symbol_by_name : name:string -> Loader.Img.t -> int option
val size_of_symbol : Loader.Symbol.t -> int
val size_of_symbol_by_name : name:string -> Loader.Img.t -> int option

val symbol_interval : Loader.Symbol.t -> Virtual_address.t * Virtual_address.t
(** [symbol_interval symbol] Returns the address range corresponding
    to [symbol] *)

val symbol_interval_by_name :
  name:string -> Loader.Img.t -> (Virtual_address.t * Virtual_address.t) option

val belongs_to_symbol : Loader.Symbol.t -> Virtual_address.t -> bool
(** [belongs_to_symbol symbol addr] Returns [true] if the address
    [addr] is locate in the [symbol] (i.e. in the range
    [address_of_symbol symbol] (included) and [address_of_symbol symbol
    + size_of_symbol symbol_interval] (excluded)). *)

val belongs_to_symbol_by_name :
  name:string -> Loader.Img.t -> Virtual_address.t -> bool

val address_of_symbol_or_section_by_name :
  name:string -> Loader.Img.t -> int option

val size_of_symbol_or_section_by_name :
  name:string -> Loader.Img.t -> int option

val interval_of_symbol_or_section_by_name :
  name:string -> Loader.Img.t -> (Virtual_address.t * Virtual_address.t) option

(* { End of Manipulation of symbols } *)

val get_byte_at : Loader.Img.t -> Bitvector.t -> int
val entry_point : Loader.Img.t -> Virtual_address.t

module Binary_loc : sig
  (** An abstract representation for binary locations *)
  type t = private
    | Address of Virtual_address.t
    | Name of string
    | Offset of t * int

  val of_string : string -> t
  (** {6 Constructors} *)

  val name : string -> t
  val address : Virtual_address.t -> t
  val offset : int -> t -> t
  val pp : Format.formatter -> t -> unit

  (** {6 Accessors} *)

  val to_virtual_address_from_file :
    filename:string -> t -> Virtual_address.t option
  (** [virtual_address_from_file file t] resolves the name [Name name] w.r.t to the
    loaded binary from [file] if needed.
   *)

  val to_virtual_address : img:Loader.Img.t -> t -> Virtual_address.t option
  (** [virtual_address img t] resolves the name [Name name] w.r.t to the
    loaded [img] binary if needed.
 *)
end
