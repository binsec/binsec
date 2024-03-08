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
  let shortname = "ida"
  let name = "IDA Pro interface"
end)

module IdaOutputFile = Builder.String (struct
  let name = "o-ida"
  let default = "out.ida"
  let doc = " Set IDA output file"
end)

module IdaCfg = Builder.False (struct
  let name = "cfg-dot"
  let doc = " Generate CFGs in dot format"
end)

module IdaSimpleCfg = Builder.No (struct
  let name = "simple"
  let doc = "Consider CFG at instruction level (instead of basic block level)"
end)
