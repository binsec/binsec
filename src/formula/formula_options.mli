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

include Cli.S

module Flatten_memory : Cli.BOOLEAN
(** Remove the array theory from formula generated.

    {b Warning:
    only works when providing a full concrete memory addressing
    as concretization policy}
*)

module No_stitching : Cli.BOOLEAN

module OptimAll : Cli.BOOLEAN
(** {2 Formula optimizations} *)

module OptimCst : Cli.BOOLEAN
module OptimItv : Cli.BOOLEAN
module OptimPrn : Cli.BOOLEAN
module OptimRbs : Cli.BOOLEAN
module OptimRow : Cli.BOOLEAN
module OptimSsa : Cli.BOOLEAN
module OptimLst : Cli.INTEGER

type solver = Boolector | Bitwuzla | Z3 | CVC4 | Yices

module Solver : sig
  include Cli.GENERIC with type t = solver

  module Timeout : Cli.INTEGER
  (** Default timeout for solver queries *)

  module Options : Cli.STRING_OPT
  (** Set solver options -- ignore default ones *)
end
