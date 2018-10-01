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

(** An abstract representation for binary locations *)
type t = private
  | Address of Virtual_address.t
  | Name of string
  | Offset of t * int

(** {6 Constructors} *)
val of_string : string -> t

val name : string -> t

val address : Virtual_address.t -> t

val offset : int -> t -> t

val pp: Format.formatter -> t -> unit

(** {6 Accessors} *)

val to_virtual_address_from_file :
  filename:string -> t -> Virtual_address.t option
(** [virtual_address_from_file file t] resolves the name [Name name] w.r.t to the
    loaded binary from [file] if needed.
*)

val to_virtual_address :
  img:Loader.Img.t -> t -> Virtual_address.t option
(** [virtual_address img t] resolves the name [Name name] w.r.t to the
    loaded [img] binary if needed.
*)
