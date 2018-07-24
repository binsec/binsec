(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

type t =
  | Address of Virtual_address.t
  | Name of string
  | Offset of t * int


let name s = Name s
let address a = Address a
let offset n t = Offset(t, n)


let rec of_string s =
  match s.[0] with
  | '<' ->
    let pos_end = String.rindex s '>' in
    let pos_plus = String.index s '+' in
    let base = of_string (String.sub s 1 (pos_plus - 1))
    and int_off =
      let start = pos_plus + 1 in
      let len = pos_end - start in
      int_of_string (String.sub s start len)
    in offset int_off base
  | '0' ->
    if s.[1] = 'x' || s.[1] = 'X' then
      Address (Virtual_address.create (int_of_string s))
    else Name s
  | _ -> Name s


(* match int_of_string s with
 * | addr -> Address (Virtual_address.create addr)
 * | exception Failure "int_of_string" -> Name s *)


let (>>) g f =
  match g with
  | None -> None
  | Some v -> Some (f v)

let address_from_img name img =
  match Loader_utils.address_of_symbol img ~name with
  | None -> None
  | Some i -> Some (Virtual_address.create i)

let to_virtual_address_from_file ~filename t =
  let rec eval = function
    | Address addr -> Some addr
    | Name name ->
      let img = Loader.load_file filename in
      address_from_img name img
    | Offset (t, n) ->
      eval t >> Virtual_address.add_int n
  in eval t

let to_virtual_address ~img t =
  let rec loop = function
    | Address addr -> Some addr
    | Name name -> address_from_img name img
    | Offset (t, n) -> loop t  >> Virtual_address.add_int n
  in loop t
