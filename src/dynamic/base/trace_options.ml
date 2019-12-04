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
  let name = "DSE with trace"
  let shortname = "tr"
end
)

module Summary =
  Builder.False(
  struct
    let name = "summary"
    let doc = " show trace summary only"
  end
  )

module AsCFG =
  Builder.False(
  struct
    let name = "as-cfg"
    let doc = "view trace as CFG"
  end
  )

module Pruning =
  Builder.True(
      struct
        let name = "no-pruning"
        let doc = "Deactivate pruning in path predicates"
      end
)

module Hlp = Builder.False(
struct
  let name = "nat-cond"
  let doc = "Enable transformation of conditions to natural conditions"
end
)

module Policy_file = Builder.String_option(
struct
  let name = "policy"
  let doc = "use given concretization/symbolization policy file"
end
)

(* FIXME: these elements seem never used ... *)
module Score_file = Builder.String(
struct
 let default = "score"
 let doc = ""
 let name = "sc-file"
end
)

module Score_alloc = Builder.String(
struct
 let default = "score-alloc"
 let doc = ""
 let name = "sc-alloc"
end
)

module Score_free = Builder.String(
struct
 let default = "score-free"
 let doc = ""
 let name = "sc-free"
end
)

module Score_use = Builder.String(
struct
 let default = "score-use"
 let doc = ""
 let name = "sc-use"
end
)

module L_evt = Builder.String(
struct
 let default = "l_event"
 let doc = ""
 let name = "levt"
end
)


module Nth_to_invert = Builder.Zero(
struct
  let name = "nth"
  let doc = "nth instruction to invert"
end
)

module Nth_alloc = Builder.Zero(
struct
  let name = "nth-alloc"
  let doc = "nth instruction allocation"
end
)
module Nth_free = Builder.Zero(
struct
  let name = "nth-free"
  let doc = "nth instruction free"
end
)
module Nth_use = Builder.Zero(
struct
  let name = "nth-use"
  let doc = "nth instruction use"
end
)

module Check_init = Builder.False(
struct
  let name = "check-init"
  let doc = "check if input depends on initial values"
end
)


module Type = Builder.Variant_choice_assoc(
struct
  open Common_piqi
  type t = analysis_direction_t
  let default = `forward
  let assoc_map = [
      "forward", `forward;
      "backward", `backward;
    ]

  let name = "type"
  let doc = "Type of trace analyis to perform"
end
)

module K = Builder.Integer(
struct
  let name = "ksteps"
  let doc = "Number of steps to perform before stopping the analysis"
  let default = 32
end)

module Trace_file = Builder.String_option(
struct
  let name = "trace"
  let doc = "Set trace file"
end
)

module Name = Builder.String(
struct
  let name = "analysis-name" (* was -name *)
  let doc = "Name of analysis to perform"
  let default = ""
end
)

module Config_file = Builder.String_option(
struct
  let name = "config"
  let doc =
    "Use this configuration file [command line takes precedence]"
end
)

module Call_convention =
  Builder.Variant_choice_assoc(
  struct
    type t = Common_piqi.call_convention_t
    let name = "callcvt"
    let doc = "Set call convention used in binary"
    let assoc_map = [
        "cdecl"    , `cdecl;
        "stdcall"  , `stdcall;
        "fastcall" , `fastcall;
        "thiscall" , `thiscall;
      ]

    let default = `cdecl
  end
)

module Trace_format =
  Builder.Variant_choice_assoc(
  struct
    type t = Trace_config.trace_format
    let name = "format"
    let doc = " Set trace format"
    let default = Trace_config.Chunked(Pervasives.stdin, false)
    let assoc_map = [
        "chunked", default;
        "stream", Trace_config.Stream ""
      ]
  end
)

module Default_action =
  Builder.Variant_choice_assoc(
  struct
    type t = Common_piqi.action
    let name = "default-action"
    let doc = "Define default action in policy"
    let assoc_map = [
        "symb"  ,`symb;
        "conc"  , `conc;
        "ignore",`ignore;
      ]
    let default = `symb
  end
)

module Read_all = Builder.False(
struct
  let name = "read-all"
  let doc = "load full trace in memory (at once)"
end
)

module Incremental_solving = Builder.False(
struct
  let name = "incremental"
  let doc = "Use incremental solving -- if available"
end
)

module Optimizations = struct
  module Constant_propagation =
    Builder.False(
        struct
          let name = "Ocstprop"
          let doc = "Enable constant propagation on formulas"
        end
      )

  module Rebase =
    Builder.False(
        struct
          let name = "Orbs"
          let doc = "Enable rebasing on formulas"
        end
      )

  module RoW =
    Builder.False(
        struct
          let name = "ORoW"
          let doc = "Enable read-over-write optimization on formulas"
        end
      )

  module RoW_plus =
    Builder.False(
        struct
          let name = "ORoW+"
          let doc = "Enable enhanced read-over-write optimization on formulas"
        end
      )

    module Equality_propagation =
    Builder.False(
        struct
          let name = "Oeqprop"
          let doc = "Enable equality propagation on formulas"
        end
      )
end
