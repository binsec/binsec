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

type warnerror = Error | Warn | Quiet
type output = (Dba.Var.t, Dba.Expr.t) Output.t
type Ir.builtin += Print of output | Signal of status

type Ir.builtin +=
  private
  | Enumerate of {
      id : int;
      n : int;
      enum : Dba.Expr.t;
      format : Output.format;
    }
  | Reach of {
      id : int;
      n : int;  (** number of time to reach; -1 for infinite *)
      guard : Dba.Expr.t;
      actions : output list;
    }

module type CONFIG = sig
  val filename : string
  val isa : Machine.isa option
  val img : Loader.Img.t
  val fs : string -> Loader_types.buffer
  val assembler : (module Compiler.ASSEMBLER)
  val trace : Compiler.trace
  val transient_enum : int
  val max_depth : int
  val enumeration_limit : int
  val smt_backend : Symbolic.Smtlib.Solver.backend
  val smt_timeout : float option
  val smt_multichecks : bool
  val smt_dumpdir : string option
  val missing_symbols : warnerror
  val timeout : int option
  val entry : Virtual_address.t option

  val script :
    ( unit,
      Binsec_script.obj,
      unit,
      unit,
      Binsec_script.obj Dyp.dyplexbuf )
    Dyp.dyp_action
    list
    list ->
    Script.Ast.t list

  val plugins : (module PLUGIN) list
end

exception Halt

module Run (_ : CONFIG) (_ : STATE) (_ : Worklist.S) () : sig
  val unit : unit
end
