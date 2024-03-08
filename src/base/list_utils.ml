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

let is_empty = function [] -> true | _ :: _ -> false

let take n l =
  assert (n >= 0);
  let rec aux acc left l =
    if left <= 0 then List.rev acc
    else
      match l with
      | [] -> List.rev acc
      | x :: xs -> aux (x :: acc) (left - 1) xs
  in
  aux [] n l

let take_while p l =
  let rec aux acc i l =
    match l with
    | [] -> List.rev acc
    | x :: xs -> if p i x then aux (x :: acc) (i + 1) xs else List.rev acc
  in
  aux [] 0 l

let drop n l =
  assert (n >= 0);
  let rec loop n l =
    if n = 0 then l else match l with [] -> [] | _ :: xs -> loop (n - 1) xs
  in
  loop n l

let rec last = function [] -> failwith "last" | [ e ] -> e | _ :: l -> last l

let drop_last l =
  let rec loop acc = function
    | [] -> failwith "drop_last"
    | [ _ ] -> List.rev acc
    | x :: xs -> loop (x :: acc) xs
  in
  loop [] l

let rev_flatten =
  let rec loop r = function
    | [] -> r
    | x :: l -> loop (List.rev_append x r) l
  in
  fun l -> loop [] l

let flat_map f l =
  List.fold_left (fun r x -> List.rev_append (f x) r) [] l |> List.rev

let hd_hd = function
  | hd1 :: hd2 :: _ -> (hd1, hd2)
  | [] | [ _ ] -> failwith "hd_hd: empty or singleton list"

let pop = function [] -> failwith "pop" | hd :: tl -> (hd, tl)

let make n x =
  assert (n >= 0);
  let rec aux n acc = if n > 0 then aux (n - 1) (x :: acc) else acc in
  aux n []

let filter_map p f l =
  List.fold_left
    (fun acc e ->
      let e' = f e in
      if p e' then e' :: acc else acc)
    [] l
  |> List.rev

let map_if p f l =
  List.fold_left (fun acc e -> if p e then f e :: acc else acc) [] l |> List.rev

let rec eq_length l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _ | _, [] -> false
  | _ :: xs, _ :: ys -> eq_length xs ys

let compare f l1 l2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | h1 :: t1, h2 :: t2 ->
        let c = f h1 h2 in
        if c <> 0 then c else aux t1 t2
  in
  aux l1 l2
