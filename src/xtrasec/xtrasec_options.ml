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

include Cli.Make (struct
  let name = "Follows a trace generated using pin"
  let shortname = "xtrasec"
end)

module Trace_file = Builder.String_option (struct
  let name = "trace"
  let doc = "Input trace as output by the xtrasec tool"
end)

module Output_smt = Builder.String_option (struct
  let name = "output-smt"
  let doc = "If set, output a SMT formula to this file"
end)

module Concretize_regs = struct
  include Builder.Variant_list (struct
    type t = [ `All | `Stack | `Register of string ]

    let name = "concretize-regs"

    (* let default = [`Stack] *)
    let doc =
      "List of registers to concretize; use <stack> for stack and frame \
       pointers; and <all> for all"

    let of_string = function
      | "all" -> `All
      | "stack" -> `Stack
      | r -> `Register r
  end)
end

module Concretize_mem = struct
  include Builder.Variant_choice (struct
    type t = [ `No | `Exact | `Approximate of int ]

    let name = "concretize-mem"
    let default = `Exact

    let doc =
      "How to add assertions regarding the memory addresses. Use <no> for no \
       assertion; <exact> to provide the exact address; or a number to state \
       that the address is in some interval."

    let of_string = function
      | "no" -> `No
      | "exact" -> `Exact
      | s -> `Approximate (int_of_string s)

    let to_string = function
      | `No -> "no"
      | `Exact -> "exact"
      | `Approximate i -> string_of_int i

    let choices = [ "no"; "exact"; "<number>" ]
  end)
end
