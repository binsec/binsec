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

include
  Cli.Make_from_logger
    (Logger)
    (struct
      let name = "Constant time checker"
      let shortname = "checkct"
    end)

module LeakInfo = Builder.Variant_choice_assoc (struct
  type t = leak_info

  let name = "leak-info"

  let doc =
    "Select the information that is reported about leakage.\n"
    ^ "\t\t- halt: halts at first leak\n"
    ^ "\t\t- instr: reports leaky instructions (instructions are reported only \
       once)\n"

  let default = InstrLeak
  let assoc_map = [ ("halt", HaltLeak); ("instr", InstrLeak) ]
end)

module Taint = Builder.No (struct
  let name = "taint"
  let doc = "Disable taint analysis (prove that instruction can not leak)"
end)

module ChosenValues = Builder.No (struct
  let name = "cv"
  let doc = "Disable chosen value sampling (prove that instruction may leak)"
end)

module Relse = Builder.No (struct
  let name = "relse"
  let doc = "Disable relational symbolic engine to answer security queries"
end)

module StatsFile = Builder.String_option (struct
  let name = "stats-file"
  let doc = "set file for dumping staistics"
end)

module Features = Builder.Variant_list (struct
  let name = "features"

  let doc =
    "Set of CT check points : control-flow, memory-access, multiplication, \
     dividend and divisor. Multiplication and division checks are \
     experimental."

  let accept_empty = true

  type t = Kind.t

  let of_string = Kind.of_string
end)

let make_options : unit -> (module OPTIONS) =
 fun () : (module OPTIONS) ->
  (module struct
    let leak_info = LeakInfo.get ()
    let taint = Taint.get ()
    let cv = ChosenValues.get ()
    let relse = Relse.get ()
    let stats_file = StatsFile.get_opt ()

    let features =
      if Features.is_set () then Features.get ()
      else [ Control_flow; Memory_access ]

    let check_branch = List.memq Kind.Control_flow features
    let check_memory = List.memq Kind.Memory_access features
    let check_mult = List.memq Kind.Multiplication features
    let check_dividend = List.memq Kind.Dividend features
    let check_divisor = List.memq Kind.Divisor features
  end)

let () =
  Sse.register_plugin ~is_enabled (fun () ->
      (module Plugin ((val make_options ()))))
