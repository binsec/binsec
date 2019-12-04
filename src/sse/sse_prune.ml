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

open Sse_graph

let is_djump v =
  match G.V.inst v with
  | Some (Dba.Instr.DJump _) -> true
  | None
  | Some _ -> false

let v_to_intvaddr v =
  (G.V.addr v).Dba.base |> Virtual_address.to_int

module Distance = struct
  type t = Finite of int | Infinite

  let add a b = match (a,b) with
    | Finite a, Finite b -> Finite (a+b)
    | _ -> Infinite

  let lt a b = match (a,b) with
    | Finite x, Finite y -> x < y
    | Finite _, Infinite -> true
    | Infinite, _ -> false

  let min a b = match (a,b) with
    | Finite a, Finite b -> Finite (min a b)
    | Finite _ as x, Infinite -> x
    | Infinite, (Finite _ as y) -> y
    | Infinite, Infinite -> Infinite

  let pp fmt = function
    | Finite x -> Format.fprintf fmt "%d" x
    | Infinite -> Format.fprintf fmt "\\infty"
end

let initial default v =
  let open Distance in
  let goals = Sse_options.GoalAddresses.get ()
  and avoids = Sse_options.AvoidAddresses.get ()
  and addr = v_to_intvaddr v in
  if Basic_types.Int.Set.mem addr avoids then Infinite
  else if is_djump v || Basic_types.Int.Set.mem addr goals then Finite 0
  else default

module Analysis =
  G.Fixpoint
    (struct
      type data = Distance.t
      let direction = Cfg.Backward
      let join = Distance.min
      let equal = (=)
      let analyze edge incoming =
        initial Distance.(add incoming (Finite 1)) (G.E.src edge)
    end)

let get_distances_to_goals cfg _branch_root _depth =
  let analysis = Analysis.analyze (initial Distance.Infinite) cfg in
  fun caddr -> G.V.of_addr caddr |> analysis
