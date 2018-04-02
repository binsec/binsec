(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

module Domain = struct
  type t =
    | TaintedKset
    | Kset
    | Interval

  let values = ["kset"; "interval"; "taintedkset"]

  let set, get =
    let dom = ref Kset in
    (fun d -> dom := d),
    (fun () -> !dom)

  let of_string s = 
    match String.lowercase s with
    | "kset" -> Kset
    | "interval" -> Interval
    | "taintedkset" -> TaintedKset
    | _ -> assert false

  let cli_handler = Arg.Symbol (values, (fun s -> set (of_string s)))
      
end


module FailSoftMode =
  Parameters.Builder.False(struct
    let name = "failsoft-mode"
    let doc =
      " Allow analysis to switch to unsound mode when stumbling on jump T"
  end)


module X86FlagPatterns =
  Parameters.Builder.False(
  struct
    let name = "x86-flag-patterns"
    let doc =
      " Apply x86 flag patterns to recover natural predicates from conditionals"
  end
  )


module KSetSize =
  Parameters.Builder.Integer(struct
    let default = 100
    let name = "kmax"
    let doc = " Sets maximum kset cardinality (use with -abs-domain kset)"
  end
  )
