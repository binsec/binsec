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

open Types

module type DRIVER = sig
  type path
  type outcome

  val step : path -> ([ `Step ], path) fiber -> outcome
  val assign : path -> ([ `Assign ], path) fiber -> outcome
  val clobber : path -> ([ `Clobber ], path) fiber -> outcome
  val load : path -> ([ `Load ], path) fiber -> outcome
  val store : path -> ([ `Store ], path) fiber -> outcome
  val symbolize : path -> ([ `Symbolize ], path) fiber -> outcome
  val apply : path -> ([ `Apply ], path) fiber -> outcome
  val assume : path -> ([ `Assume ], path) fiber -> outcome
  val check : path -> ([ `Assert ], path) fiber -> outcome
  val ite : path -> ([ `Branch ], path) fiber -> outcome
  val goto : path -> ([ `Goto ], path) fiber -> outcome
  val jump : path -> ([ `Jump ], path) fiber -> outcome
  val call : path -> ([ `Call ], path) fiber -> outcome
  val tail_call : path -> ([ `Tail_call ], path) fiber -> outcome
end

type ('path, 'outcome) driver =
  (module DRIVER with type path = 'path and type outcome = 'outcome)

val dispatch :
  'path -> ([ `All ], 'path) fiber -> ('path, 'outcome) driver -> 'outcome

module Concrete
    (P : PATH)
    (_ : DRIVER with type path = P.t and type outcome = P.t continuation) :
  DRIVER with type path = P.t and type outcome = P.t continuation
