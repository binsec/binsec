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

let is_empty = function
  | [] -> true
  | _ :: _ -> false

let take n l =
  assert (n >= 0);
  let rec aux acc left l =
    if left <= 0 then List.rev acc
    else
      match l with
      | [] -> List.rev acc
      | x :: xs -> aux (x :: acc) (left - 1) xs
  in aux [] n l


let drop n l =
  assert (n >= 0);
  let rec loop n l =
    if n = 0 then l
    else
      match l with
      | [] -> []
      | _ :: xs -> loop (n - 1) xs
  in loop n l


let rec last = function
  | [] -> failwith "last"
  | [e] -> e
  | _ :: l -> last l


let rev_flatten l =
  let rec loop acc = function
    | [] -> acc
    | l :: ll ->
      loop (List.rev_append l acc) ll
  in loop [] l


let flat_map f l = List.rev_map f l |> rev_flatten

let hd_hd = function
  | hd1 :: hd2 :: _ -> hd1, hd2
  | [] | [_] -> failwith "hd_hd: empty or singleton list"
