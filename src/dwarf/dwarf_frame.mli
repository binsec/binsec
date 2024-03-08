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

type rule = Undef | Same | Value of Dba.Expr.t
type entry

val addresses : entry -> int Interval.t
(** [address entry] return the address range of the entry *)

val cfa : entry -> Dba.Expr.t
(** [cfa entry] return the canonical frame address of the entry *)

val rule : int -> entry -> rule
(** [rule n entry] return the rule of the n'th column of the entry *)

type t

val load : Loader.Img.t -> t
(** [load img] extract and interpret the content
    of either .debug_frame or .eh_frame section *)

val fold :
  ('a -> return_address:int -> columns:int array -> entry -> 'a) ->
  'a ->
  t ->
  'a
(** [fold f frame] iterate through the frame matrix
    columns is the list of valid column indexes of the given entry
    return_address is the column index of the return address of the function *)

val iter :
  (return_address:int -> columns:int array -> entry -> unit) -> t -> unit
(** [iter frame] same as fold but without return *)

include Sigs.PRINTABLE with type t := t
