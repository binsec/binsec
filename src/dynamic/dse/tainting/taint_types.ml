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

type taint =
  | NoTaint
  | TaintI
  | TaintP
  | TaintMix of taint list

type taint_strategy =
  | May      (* May: Taint all memory in case of symbolic load/store *)
  | Must     (* Must: Do not taint all the memory in case of symbolic load/store (just forget about the it) → (break soundness?) *)
  | MustConc (* MustConc: In case of symbolic load/store concretise the address → (be careful: the c/s policy should be in accordance with that) *)

type taint_infos =
  | Load of taint
  | Store of taint
  | LoadAddr of taint
  | StoreAddr of taint
  | Variable of string * int * int * taint
  | LhsVar of string * int * int * taint
  (* ------------------- *)

module TaintInfoSet = Set.Make (
  struct
    type t = taint_infos
    let compare a b =
      match (a,b) with
      | (Load _, Load _) -> 0 | (Load _, _) -> -1
      |  (_, Load _) -> 1 | (Store _, Store _) -> 0
      | (Store _, _) -> -1 | (_, Store _) -> 1
      | (StoreAddr _, StoreAddr _) -> 0
      | (StoreAddr _, _) -> -1 | (_, StoreAddr _) -> 1
      | (LoadAddr _, LoadAddr _) -> 0
      | (LoadAddr _, _) -> -1 | (_, LoadAddr _) -> 1
      | (Variable(s1,l1,h1,_),Variable(s2,l2,h2,_)) ->
        begin
          match compare s1 s2 with
          | 0 -> begin match compare l1 l2 with
                0-> compare h1 h2 | x-> x  end
          | x -> x end
      | (Variable(_,_,_,_), _) -> -1
      | (_, Variable(_,_,_,_)) -> 1
      | (LhsVar(s1,l1,h1,_),LhsVar(s2,l2,h2,_)) ->
        begin
          match compare s1 s2 with
          | 0 -> begin match compare l1 l2 with
                0-> compare h1 h2 | x-> x  end
          | x -> x end
  end)

let rec taint_to_string = function
  | NoTaint -> "No"
  | TaintI -> "Ti"
  | TaintP -> "Tp"
  | TaintMix(l) -> (List.fold_left (fun acc i -> acc ^ (taint_to_string i)^" " ) "[ " l) ^ "]"