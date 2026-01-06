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

include Cli.Make (struct
  let name = "Shadow stack protection"
  let shortname = "shadow-stack"
end)

type mode =
  | Inline  (** Use standard DBA assertions *)
  | Builtin  (** Use new push and pop builtins *)

module Mode = Builder.Variant_choice_assoc (struct
  type t = mode

  let name = "mode"

  let doc =
    "Use standard DBA assertions [Inline] or new push and pop builtins \
     [Builtin]"

  let default = Inline
  let assoc_map = [ ("inline", Inline); ("builtin", Builtin) ]
end)

let () =
  Sse.register_plugin ~is_enabled (fun () ->
      match Mode.get () with
      | Inline -> (module Inline_plugin)
      | Builtin -> (module Builtin_plugin))
