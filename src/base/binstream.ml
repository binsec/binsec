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

module Logger = Base_logger

type t = int Sequence.t
(* The list is stored in reversed order *)

let length = Sequence.length

let empty = Sequence.empty



let is_byte_value n = n >= 0 && n < 256

let iteri f s =
  let i = ref 0 in
  Sequence.iter_forward
    (fun x -> f !i x; incr i) s

let get_byte_exn t n =
  let exception Byte_found of int in
  if n < 0 || n > length t then raise Not_found
  else
    let find m x = if m = n then raise (Byte_found x) in
    try iteri find t; raise Not_found
    with
    | Byte_found x -> x

let get_byte t n =
  match get_byte_exn t n with
  | by -> Some by
  | exception Not_found -> None
;;

let value_of c =
  let cc = Char.code c in
  if c <= '9' then cc - Char.code '0'
  else 10 + cc - Char.code 'a'

let add_zeros hs =
  let len = String.length hs in
  if len mod 2 = 0 then hs
  else
    let b = Buffer.create 8 in
    let rec add_zero len =
      if len mod 2 = 0 then Buffer.contents b ^ hs
      else begin
        Buffer.add_char b '0';
        add_zero (len + 1)
      end
    in add_zero len

let append_int n h =
  assert (is_byte_value n);
  Sequence.push_back n h

let append_int64 n64 h =
  let n = Int64.to_int n64 in append_int n h

let prepend_int n h =
  assert (is_byte_value n);
  Sequence.push_front n h

let prepend_int64 n64 h = prepend_int (Int64.to_int n64) h

let prepend_char c = prepend_int (Char.code c)
let append_char c = append_int (Char.code c)

let of_list l =
  List.fold_left (fun seq e -> prepend_int e seq) empty l

let of_bytes s =
  Logger.debug ~level:5 "Binstream.oXf_bytes %s" s;
  let len = String.length s in
  let rec loop acc idx =
    if idx >= len then acc
    else
      let byte = s.[idx] in
      let ascii = Char.code byte in
      let acc = prepend_int ascii acc in
      loop acc (idx + 1)
  in loop empty 0

let clean_string s =
  let len = String.length s in
  let b = Buffer.create len in (* len is the upper bound *)
  for i = 0 to len - 1 do
    let c = s.[i] in
    if c <> ' ' then Buffer.add_char b (Char.lowercase_ascii c);
  done;
  Buffer.contents b

let of_nibbles s =
  Logger.debug ~level:5 "Binstream.of_string %s" s;
  let s = clean_string s in
  let s = add_zeros s in
  let len = String.length s in
  assert (len mod 2 = 0);
  let rec loop acc idx =
    if idx >= len then acc
    else
      let byte_str = String.sub s idx 2 in
      let v = 16 * value_of byte_str.[0] + value_of byte_str.[1] in
      let acc = prepend_int v acc in
      loop acc (idx + 2)
  in loop empty 0


let iter = Sequence.iter_forward
let map = Sequence.map_forward
let fold = Sequence.fold_forward

let rev sq = fold Sequence.push_back sq empty ;;

let pp ppf t =
  let open Format in
  pp_open_hbox ppf ();
  iter
    (fun by -> Format.fprintf ppf "%02x@ " by) t;
  pp_close_box ppf ()


let to_string t = Format.asprintf "%a" pp t
