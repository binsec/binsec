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

type domain =
    | TaintedKset
    | Kset
    | Interval

include Cli.Make(
struct
  let shortname = "ai"
  let name = "Abstract interpretation"
end
)

module Domain =
  Builder.Variant_choice_assoc(
      struct
        type t = domain
        let name = "domain"
        let default = Kset
        let doc = "Set domain for abstract interpretation"
        let assoc_map = [
            "kset", Kset;
            "interval", Interval;
            "taintedkset", TaintedKset;
          ]
      end
    )



module FailSoftMode =
  Builder.False(struct
    let name = "failsoft-mode"
    let doc =
      " Allow analysis to switch to unsound mode when stumbling on jump T"
  end)


module X86FlagPatterns =
  Builder.False(
  struct
    let name = "x86-flag-patterns"
    let doc =
      " Apply x86 flag patterns to recover natural predicates from conditionals"
  end
  )


module KSetSize =
  Builder.Integer(struct
    let default = 100
    let name = "kmax"
    let doc = " Sets maximum kset cardinality (use with -abs-domain kset)"
  end
  )

let time_simpl = ref 0.
let time_disas = ref 0.
let time_nat_flag_recovery = ref 0.
let nb_conditional_cache_uses = ref 0
let nb_recovered_nat_predicates = ref 0
let nb_nat_predicate_recovery_tries = ref 0
let nb_failed_nat_predicate_recoveries = ref 0
let finalsize = ref 0
let initsize = ref 0
let itemps = ref 0
let iflags = ref 0
let ftemps = ref 0
let fflags = ref 0
(* --------------- *)


(* Static analysis options *)
let nb_equalities_names = ref 0
let nb_equalities_classes = ref 0
let time_analysis = ref 0.
let time_equalities = ref 0.
let time_redundant_evals = ref 0.
let nb_equalities_refinement = ref 0
let nb_refined_lhs = ref 0
(* ------- *)
