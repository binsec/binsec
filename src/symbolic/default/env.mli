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
include State.S with type Value.t = Expr.t and type Model.t = Model.t

type 'a State.value_kind += Term : Expr.t State.value_kind

type ('value, 'state, 'cookie, 'a) State.feature +=
  | VisibleSymbols :
      ( 'value,
        'state,
        'cookie,
        'state -> 'value Dba_types.Var.Map.t )
      State.feature
  | VisibleMemory : ('value, 'state, 'cookie, 'state -> Memory.t) State.feature
  | VisibleArrays :
      ( 'value,
        'state,
        'cookie,
        'state -> Memory.t Basic_types.String.Map.t )
      State.feature
  | Downcast : ('value, 'state, 'cookie, 'state -> t) State.feature
  | Upcast : ('value, 'state, 'cookie, t -> 'state) State.feature
