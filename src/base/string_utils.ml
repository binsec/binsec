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

let replace_chars f s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_string b (f c)) s;
  Buffer.contents b


let reverse s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])


let filter p s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> if p c then Buffer.add_char b c) s;
  Buffer.contents b


let fold f acc s =
  let acc_ref = ref acc in
  String.iter (fun c -> acc_ref := f !acc_ref c) s;
  !acc_ref


let remove_char c s =
  filter (fun c' -> c <> c') s


let for_all p s =
  let len = String.length s in
  let rec loop i = i >= len || p s.[i] && loop (i + 1) in loop 0 


let exists p s =
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else p s.[i] || loop (i + 1)
  in loop 0 


let remove_newline = remove_char '\n'


let lchop n s =
  assert (n >= 0);
  let len = String.length s in
  if len = 0 || n >= len then ""
  else String.sub s n (len - n)


let left n s =
  assert (n <= String.length s);
  String.sub s 0 n


let size_of_hexstring s =
  assert (s.[0] = '0' && s.[1] = 'x');
  (* The pattern in the lexer is '0x' + hexadigits *)
  let nibble_size = 4 in
  nibble_size * (String.length s - 2)
