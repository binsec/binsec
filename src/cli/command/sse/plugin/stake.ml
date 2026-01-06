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
      let name =
        "A stake to support the exploration and sanitize the unwanted \
         non-determinism from missing initializations."

      let shortname = shortname
    end)

module Fix = Builder.Variant_list (struct
  let name = "fix"

  let doc =
    "Fixed elements will be enforced to have a single value along the \
     execution (rvalue, load, address, test, jump-target)."

  let accept_empty = true

  type t = watchpoint

  let of_string = Watchpoint.of_string
end)

module Interrupt = Builder.Variant_list (struct
  let name = "interrupt"

  let doc =
    "Interrupt the program when non-determinism is found (rvalue, load, \
     address, test, jump-target)."

  let accept_empty = true

  type t = watchpoint

  let of_string = Watchpoint.of_string
end)

module RegInit = Builder.No (struct
  let name = "reg-init"
  let doc = "Initialize undefined registers (including flags) to zero."
end)

module MemInit = Builder.No (struct
  let name = "mem-init"

  let doc =
    Format.sprintf
      "Try to initialize memory cells with either the value from the process \
       image or zero. May fail if the address is non-deterministic (use \
       '-%s-fix address')"
      shortname
end)

let make_options : unit -> (module OPTIONS) =
 fun () ->
  (module struct
    let check = if Interrupt.is_set () then Interrupt.get () else []

    let fix =
      if Fix.is_set () then Fix.get ()
      else [ Rvalue; Load; Address; Test; Target ]

    let rval =
      if List.memq Rvalue check then Check
      else if List.mem Rvalue fix then Fix
      else Ignore

    let load =
      if List.memq Load check then Check
      else if List.mem Load fix then Fix
      else Ignore

    let addr =
      if List.memq Address check then Check
      else if List.mem Address fix then Fix
      else Ignore

    let test =
      if List.memq Test check then Check
      else if List.mem Test fix then Fix
      else Ignore

    let target =
      if List.memq Target check then Check
      else if List.mem Target fix then Fix
      else Ignore

    let reg_init = RegInit.get ()
    let mem_init = MemInit.get ()
  end)

let () =
  Sse.register_plugin ~is_enabled (fun () ->
      (module Plugin ((val make_options ()))))
