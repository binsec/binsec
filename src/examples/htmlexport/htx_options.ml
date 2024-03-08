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
  let shortname = "htx"
  let name = "Plugin example (HTML export)"
end)

module Directory = Builder.String (struct
  let name = "html-directory"
  let default = "binsec_cfg_html"
  let doc = "Set HTML directory for HTML export"
end)

module Level = Builder.Variant_choice_assoc (struct
  let name = "export-level"
  let doc = " Set level of details for HTML export"

  type t = [ `Callgraph | `Function | `Mnemonic ]

  let assoc_map =
    [
      ("callgraph", `Callgraph); ("function", `Function); ("mnemonic", `Mnemonic);
    ]

  let default = `Callgraph
end)
