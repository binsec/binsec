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

type entry = {
  addresses : int Interval.t;
  path : string;
  line : int;
  column : int;
  is_stmt : bool;
  basic_block : bool;
  discriminator : int;
}
(** represent one or more rows of the addresse / line matrix
    [addresses]     the range of virtual addresses of the entry
    [path]          the path of the processed file
    [line]          the line of the source (starting from 1)
    [column]        the column (non reliable, old compilers do not produce it)
    [is_stmt]       if the entry correspond to a statement in the source
    [basic_block]   if the entry is the start of a basic block
    [discriminator] an integer identifying the block to which the entry belong
*)

type t

val load : Loader.Img.t -> t
(** [load img] extract and interpret the content of .debug_line section *)

val fold : ('a -> entry -> 'a) -> 'a -> t -> 'a
(** [fold f line] iterate through the line matrix *)

val iter : (entry -> unit) -> t -> unit
(** [iter f line] same as fold but without return *)

include Sigs.PRINTABLE with type t := t
