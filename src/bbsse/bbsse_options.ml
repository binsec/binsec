(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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
  let shortname = "bbsse"

  let name = "Backward Bounded Static Symbolic Execution"
end)

module MaxCondition = Builder.Integer_option (struct
  let name = "max_condition"

  let doc = "Set maximal number of conditions to meet"
end)

module MaxConditionCycle = Builder.Integer_option (struct
  let name = "max_condition_cycle"

  let doc = "Set maximal number of conditions to go backwards to prevent cycles"
end)

module GenGroundTruth = Builder.False (struct
  let name = "gen-ground-truth"

  let doc = "Give all jumps that are opaque"
end)

module ProcessAllJumps = Builder.False (struct
  let name = "process-all-jumps"

  let doc = "Process all jumps or not"
end)

module FindJumps = Builder.False (struct
  let name = "find-jumps"

  let doc = "Find automatically jumps and run BB-SSE on each"
end)

module Address_counter = struct
  type t = { address : Virtual_address.t; counter : int }

  let of_string s =
    match String.split_on_char ':' s with
    | [ address; counter ] ->
        {
          address = Virtual_address.of_string address;
          counter = int_of_string counter;
        }
    | _ -> assert false

  let decr c = { c with counter = c.counter - 1 }

  let init addr c = { address = addr; counter = c }

  let check_and_decr c = if c.counter > 0 then Some (decr c) else None
end

module Visit_address_counter = Builder.Variant_list (struct
  include Address_counter

  let name = "visit-until"

  let doc =
    "Specify a the maximum number of times [n] an address [vaddr] is visited \
     by SE (format: <vaddr>:<n>)"
end)

module OPFile = Builder.String_option (struct
  let name = "op-file"

  let doc =
    "file containing opaque predicates returned by the ground truth for a \
     program"
end)

module IgnoreAddr = Builder.String_option (struct
  let name = "ignore-addr"

  let doc = "addresses to ignore (take next)"
end)

module MaxDepth = Builder.Integer (struct
  let name = "depth"

  let default = 1000

  let doc = "Set exploration maximal depth"
end)
