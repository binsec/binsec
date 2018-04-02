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

(** General color definitions for outputs *)

type color = int

val rgb : int -> int -> int -> color

val pp_with_prefix : string -> Format.formatter -> color -> unit
val pp : Format.formatter -> color -> unit
(** [pp ppf color] is [pp_with_prefix "#" ppf color] *)

module FlatUI : sig
  val turquoise    : color 
  val greensea     : color 
  val emerland     : color 
  val nephritis    : color 
  val peterriver   : color 
  val belizehole   : color 
  val amethyst     : color 
  val wisteria     : color 
  val wetasphalt   : color 
  val midnightblue : color 
  val sunflower    : color 
  val orange       : color 
  val carrot       : color 
  val pumpkin      : color 
  val alizarin     : color 
  val pomegranate  : color 
  val clouds       : color 
  val silver       : color 
  val concrete     : color 
  val asbestos     : color 
end
