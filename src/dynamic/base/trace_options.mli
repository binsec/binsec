(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

include Cli.S

module Summary: Cli.BOOLEAN

module AsCFG : Cli.BOOLEAN

(** {3 Guidance  Options }*)

module Score_file : Cli.STRING
module Score_alloc : Cli.STRING
module Score_free : Cli.STRING
module Score_use : Cli.STRING
module L_evt : Cli.STRING

(** {4 Path exploration options} *)

module Nth_to_invert: Cli.INTEGER
module Nth_alloc: Cli.INTEGER
module Nth_free: Cli.INTEGER
module Nth_use: Cli.INTEGER
module Check_init: Cli.BOOLEAN

(** {3 Path_predicate_formula generation options} *)

(** Either to apply the backward pruning phase when generating
    a formula (default [true]) *)
module Pruning: Cli.BOOLEAN

(** Enable lifting low-level condition predicates to higher
    level predicates on conditional jumps *)
module Hlp: Cli.BOOLEAN

module Policy_file : Cli.STRING_OPT

module Type : Cli.GENERIC with type t = Common_piqi.analysis_direction_t
(** Direction of analysis *)

module K : Cli.INTEGER
module Trace_file : Cli.STRING_OPT
module Config_file : Cli.STRING_OPT
module Name : Cli.STRING
module Call_convention : Cli.GENERIC with type t = Common_piqi.call_convention_t
module Trace_format : Cli.GENERIC with type t = Trace_config.trace_format
module Default_action : Cli.GENERIC with type t = Common_piqi.action
module Read_all : Cli.BOOLEAN
module Incremental_solving : Cli.BOOLEAN

(** Optimizations on formulas *)
module Optimizations : sig
  module Constant_propagation : Cli.BOOLEAN
  module Rebase : Cli.BOOLEAN
  module RoW : Cli.BOOLEAN
  module RoW_plus : Cli.BOOLEAN
  module Equality_propagation : Cli.BOOLEAN
end
