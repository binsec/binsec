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

module Bv = Types.Bv
module Chunk = Types.Chunk
module Store = Types.Store

module Expr = struct
  include Types.Expr
  module Set = Types.BvSet
  module Map = Types.BvMap
  module Hmap = Types.BvHmap
  module Tbl = Types.BvTbl
end

module Memory = struct
  include Types.Memory

  type 'a node = 'a Types.Memory.node = private
    | None : [< `None | `Any ] node
    | Symbol : {
        id : int;
        name : string;
        index : int;
      }
        -> [< `Some | `Symbol | `Any ] node
    | Layer : {
        id : int;
        over : t;
        addr : Expr.t;
        store : Store.t;
      }
        -> [< `Some | `Any ] node

  module Tbl = Types.AxTbl
end

module Symbol = struct
  type t = Memory.symbol

  module Tbl = Types.AsTbl
end

module Model = Types.Model
module Solver = Solver
module Printer = Printer
module ToFormula = To_formula

type 'a State.value_kind += Term = Env.Term

module State = Env
