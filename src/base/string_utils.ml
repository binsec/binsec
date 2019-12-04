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

let char_codes s =
  Array.init (String.length s) (fun i -> Char.code s.[i])
;;

let remove_char c s =
  filter (fun c' -> c <> c') s

let remove_newline = remove_char '\n'

let for_all p s =
  let len = String.length s in
  let rec loop i = i >= len || p s.[i] && loop (i + 1) in loop 0


let exists p s =
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else p s.[i] || loop (i + 1)
  in loop 0


let split ~sep s =
  let rexp = Str.regexp sep in
  Str.split rexp s

let cli_split s = split ~sep:"," s

let lchop n s =
  assert (n >= 0);
  let len = String.length s in
  if len = 0 || n >= len then ""
  else String.sub s n (len - n)


let left n s =
  assert (n <= String.length s);
  String.sub s 0 n


let right n s =
  let len = String.length s in
  assert (n <= len);
  let idx = n - len in
  String.sub s idx n

let size_of_hexstring s =
  assert (s.[0] = '0' && s.[1] = 'x');
  (* The pattern in the lexer is '0x' + hexadigits *)
  let nibble_size = 4 in
  nibble_size * (String.length s - 2)


let lfindi s p =
  let len = String.length s in
  let rec loop i =
    if i >= len then None
    else if p s.[i] then Some i
    else loop (i + 1)
  in loop 0

let contains subs s =
  let ls = String.length s and lsubs = String.length subs in
  (lsubs = 0) || ((lsubs <= ls) && (
      let rec loop = function
        | -1 -> false
        | n ->
          ((String.sub s n lsubs |> String.compare subs) = 0
           || loop (n-1))
      in
      loop (ls-lsubs)
    ))

let is_hex_char c =
  c >= '0' && c <= '9'
  || c >= 'a' && c <= 'f'
  || c >= 'A' && c <= 'F'


let is_char_printable c =
  (* Printable ASCII characters are all between 33 [!] & 126 [~] included *)
  c >= '!' && c <= '~'

let to_hex, pp_hex =
  let lookup = "0123456789abcdef" in
  (fun s ->
    Bytes.unsafe_to_string
      (Bytes.init (2 * String.length s)
         (fun i ->
           let j = Char.code (String.get s (i / 2)) in
           String.get lookup (j lsr (4 * (1 - i mod 2)) land 0xf)))),
  (fun ppf s ->
    String.iter
      (fun c ->
        let i = Char.code c in
        Format.pp_print_char ppf (String.get lookup (i lsr 4));
        Format.pp_print_char ppf (String.get lookup (i land 0xf))) s)
