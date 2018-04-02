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

(** Trace postprocessing operations *)

(** {2 Natural conditions lifting}

  This post processing operation, translates low-level jump conditional
  condition into more natural ones (<, >, >= ..)
*)

val get_merge_stats: unit ->
  (int * Basic_types.Addr64.Set.t * int * Basic_types.Addr64.Set.t * int)
(** @return stats of the merge of operations that took place during the 
    analysis execution. Values are:
    - number of conditional jumps for which no natural predicate was found
    - the set of address where no natural predicates where found
    - number of conditional jumps where the test was neither "test" neither "cmp"
    - the set of addresses where the test was not test/cmp
    - number of conditional jumps replaced
     *)

val merge_natural_conditions: Trace_type.trace_inst_map -> Trace_type.trace_inst_map
(** apply the post processing computation replacing low-level
    condition by higher-level conditions *)