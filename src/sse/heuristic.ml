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

open Types

module Dfs : WORKLIST = struct
  type 'a t = 'a list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push e w = e :: w
  let singleton e = [ e ]
  let pop = function e :: w -> (e, w) | [] -> raise Not_found
  let length = List.length
end

module Bfs : WORKLIST = struct
  type 'a t = 'a Sequence.t

  let length = Sequence.length
  let is_empty q = Sequence.length q = 0
  let empty = Sequence.empty
  let push p q = Sequence.push_back p q

  let pop q =
    match Sequence.peek_front q with
    | None -> raise Not_found
    | Some v -> (
        match Sequence.pop_front q with
        | None -> assert false
        | Some seq -> (v, seq))

  let singleton p = push p empty
end

module Nurs : WORKLIST = struct
  (* This is actually a fairly classical heap.
     The priority added to the date is just generated at random.
  *)
  module I = Basic_types.Int.Map

  type 'a t = 'a I.t

  let rec gen_priority t =
    let p = Utils.random_max_int () in
    if I.mem p t then gen_priority t else p

  let length = I.cardinal
  let is_empty = I.is_empty
  let empty = I.empty

  let push e t =
    let p = gen_priority t in
    I.add p e t

  let pop t =
    let (_, e), t' = I.pop t in
    (e, t')

  let singleton p = push p empty
end
