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

type t = string

external to_string : t -> string = "%identity"

let pp = Format.pp_print_string
let zero = "!0"

let incr =
  let set t' i c =
    Bytes.set t' i c;
    Bytes.unsafe_to_string t'
  in
  (* to keep it compact, transition rules are impacted by the ASCII encoding:
        0-9a-zA-Z!$%&*+./<>?@^_~ *)
  let rec incr t' i =
    match Bytes.get t' i with
    | '9' -> set t' i 'a'
    | 'z' -> set t' i 'A'
    | 'Z' -> set t' i '!'
    | '!' -> set t' i '$'
    | '&' -> set t' i '*'
    | '+' -> set t' i '.'
    | '/' -> set t' i '<'
    | '<' -> set t' i '>'
    | '@' -> set t' i '^'
    | '_' -> set t' i '~'
    | '~' when i = 0 ->
        (* overflow *)
        let t' = Bytes.(make (length t' + 1) '0') in
        Bytes.set t' 0 '!';
        Bytes.unsafe_to_string t'
    | '~' ->
        Bytes.set t' i '0';
        incr t' (i - 1) (* carry *)
    | x -> set t' i Char.(unsafe_chr (code x + 1))
  in
  fun t ->
    let t' = Bytes.of_string t in
    incr t' (Bytes.length t' - 1)

let compare = String.compare
