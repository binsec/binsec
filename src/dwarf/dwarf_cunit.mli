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

type t

val load : Loader.Img.t -> t list

val dir : t -> string
(** [dir cunit] return the path from where the compiler have proceed *)

val file : t -> string
(** [file cunit] return the path of the proceesed file
    of the compilation unit *)

module Type : sig
  type t

  val name : t -> string
  (** [name type] return the name of the type declaration *)

  include Sigs.PRINTABLE with type t := t
end

module Var : sig
  type t

  val name : t -> string
  (** [name var] return the name of the variable var *)

  val line : t -> int
  (** [line var] return the line of the declaration of the variable var *)

  val typ : t -> Type.t
  (** [typ var] return the type of the declaration of the variable var *)

  val loc : t -> (int -> Dba.Expr.t option) -> int -> Dba.Expr.t
  (** [loc var cfa addr] return the location where the compiler store the
      variable var at the given program point addr *)

  include Sigs.PRINTABLE with type t := t
end

module Func : sig
  type func

  val find : t -> string -> func
  (** [find cunit func_name] return the function named func_name
      of the compilation unit
      @raise Exception Not_found *)

  type t = func

  val name : t -> string
  (** [name func] return the name of the declaration of the function func *)

  val line : t -> int
  (** [line func] return the line of the declaration of the function func *)

  val typ : t -> Type.t
  (** [typ func] return the type of the declaration of the function func *)

  val cfa : t -> Dwarf_expr.t
  (** [cfa func] return the Canonical Frame Address of the function func *)

  val vars : t -> Var.t list
  (** [vars func] return the list of local variables
      declared in the function func *)
end

module Global : sig
  val vars : t -> Var.t list
  (** [vars cunit] return the list of global variables
      declared in the compilation unit *)
end

include Sigs.PRINTABLE with type t := t
