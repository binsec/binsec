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

module type S = sig
  type elt
  type t

  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool

  val add : elt -> t -> t
  val remove : t -> t

  val pop : t -> elt * t
  val peek : t -> elt

  val merge : t -> t -> t

  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> t
end


module Make(X: Sigs.Comparable) = struct

  type t =
    | Empty
    | Heap of X.t * t list

  type elt = X.t

  let empty = Empty

  let singleton x = Heap (x, [])
  let is_empty h = h = Empty

  let merge h1 h2 =
    match h1, h2 with
    | Empty, _ -> h2
    | _, Empty -> h1
    | Heap (x1, l1), Heap(x2, l2) ->
      if X.compare x1 x2 > 0
      then Heap (x1, h2 :: l1)
      else Heap (x2, h1 :: l2)

  let rec merge_pairs l =
    match l with
    | [] -> Empty
    | [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (merge_pairs hs)


  let add x h = merge (Heap (x, [])) h ;;


  let remove h =
    match h with
    | Empty -> failwith "remove"
    | Heap (_, l) -> merge_pairs l

  let peek h =
    match h with
    | Empty -> failwith "peek"
    | Heap (x, _) -> x

  let pop h =
    try
      let v = peek h in
      v, remove h
    with Failure _ -> failwith "pop"


  let rec fold f acc = function
    | Empty -> acc
    | Heap (x, l) ->
      List.fold_left (fold f) (f acc x) l

  let rec map f = function
    | Empty -> Empty
    | Heap (x, l) ->
      let l' = List.map (map f) l in
      add (f x) (merge_pairs l')

  let rec iter f = function
    | Empty -> ()
    | Heap (x, l) ->
      f x;
      List.iter (iter f) l
end


module CMake (X: Sigs.Any) = struct

  module H =Make (
    struct
      type t = X.t * int
      let compare (_, n1) (_, n2) = Pervasives.compare n1 n2
    end)

  type elt = X.t
  type t = H.t

  let front_idx = ref 0
  let rear_idx = ref 0

  let front x = incr front_idx; x, !front_idx

  let rear x = decr rear_idx; x, !rear_idx

  let empty = H.empty

  let is_empty = H.is_empty

  let add x = H.add (front x)

  let remove = H.remove

  let cons x = add x
  let singleton x = add x empty

  let snoc x = H.add (rear x)

  let pop h =
    let (v, _), l = H.pop h in
    v, l

  let peek h = fst (H.peek h)

  let fold f acc h =
    H.fold (fun acc (e, _) -> f acc e) acc h

  let iter f h = H.iter (fun (e, _) -> f e) h

  let map f h = H.map (fun (e, n) -> (f e, n)) h

  let merge = H.merge

end
