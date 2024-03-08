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

(** Represent the concrete state of a running program. *)
module Env : sig
  type t

  exception Ox8BADF00D of Dba.Expr.t

  val empty : t
  val load_memory_file : t -> string -> t
  val load_init_file : t -> string -> t
  val eval : t -> Dba.Expr.t -> Bitvector.t
  val assign : t -> Dba.LValue.t -> Bitvector.t -> t
  val kill : t -> Dba.LValue.t -> t
end

module type Program = sig
  type t

  exception Ox8BADF00D of Dba.address

  val fetch : t -> Dba.address -> Dba.Instr.t
end

(** **)
module Interpreter (P : Program) : sig
  exception AssertFailure of Dba.address
  exception EndOfTrace of Env.t

  val step : Env.t -> Dba.address -> Dba.Instr.t -> Env.t * Dba.address
  val fetch : P.t -> Dba.address -> Dba.Instr.t
end

module Image : Program with type t = unit
module Dba_program : Program with type t = Dba_types.program

module Instr_list : sig
  include Program

  val init : Dba.Instr.t list -> t
end
