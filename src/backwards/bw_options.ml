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

include Cli.Make(
struct
  let shortname = "bw"
  let name = "Backwards reasoners"
end
)

module Opaque_predicates =
  Builder.False(
      struct
        let name = "opaque"
        let doc = "Check all predicates from entry point section for opacity"
      end
    )

module Opaque_addresses =
  Builder.Integer_list(
      struct
        let name = "opaque-at"
        let doc = "Check address list for opaque predicates"
      end
    )

module Opaque_sections =
  Builder.String_list (
      struct
        let name = "opaque-sections"
        let doc  = "Check predicates of section list for opacity"
      end
    )

module K =
  Builder.Integer(
      struct
        let name = "k"
        let doc = "Set size of backward trace"
        let default = 16
      end
  )
