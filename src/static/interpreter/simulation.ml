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
  let shortname = "sim2"
  let name = "Simulation"
end)

module MemoryFile = Builder.String_option (struct
  let name = "memory"
  let doc = "set file containing the initial (concrete) memory state"
end)

module InitFile = Builder.String_option (struct
  let name = "init"
  let doc = "set dba file containing arbitrary initialisation instructions"
end)

module Directives = Builder.Any (struct
  type t = Directive.t list

  let name = "goals"
  let doc = "Set simulation goals"
  let default = []

  let of_string s =
    let lexbuf = Lexing.from_string s in
    Parser.directives Lexer.token lexbuf

  let default_str = None
end)
