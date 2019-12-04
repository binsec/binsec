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
  let shortname = "sim"
  let name = "Simulate"
end
)

type strategy =
  | Branch_if
  | Branch_else
  | Fail

module Conditional_strategy =
  Builder.Variant_choice_assoc(
      struct
        type t = strategy
        let name = "on-cond"
        let doc = "Set exploration strategy on conditionals"
        let default = Branch_if
        let assoc_map = [
          "fail", Fail;
          "if",   Branch_if;
          "else", Branch_else;
          ]
      end
    )

module StepByStep =
  Builder.False(
  struct
    let name = "step"
    let doc = "Set instruction by instruction simulation"
  end
  )

module FuzzerIterations =
  Builder.Integer(
  struct
    let name = "fuzz"
    let default = 1
    let doc = "Set number of fuzzing iteration"
  end
  )

type semantic_mode =
  | Flat
  | Region
  | Region_load_store
  | Logic
  | Rewrite

module Semantic_mode =
  Builder.Variant_choice_assoc(
    struct
      type t = semantic_mode
      let name = "memory-model"
      let assoc_map = [
          "flat", Flat;
          "region", Region;
          "region-load-store", Region_load_store;
          "logic", Logic ;
          "rewrite", Rewrite;
        ]

      let default = Flat

      let doc = "Set semantic memory model"
    end
  )
