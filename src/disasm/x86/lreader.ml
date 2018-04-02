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

module type Accessor = sig
  type t
  val u8  : t -> int
  val u16 : t -> int
  val u32 : t -> int
  val u64 : t -> int
end

type bytestream =
  | LoaderImg of Loader.Img.t
  | BinStream of int * Basic_types.Binstream.t

type t = {
  content : bytestream ;
  mutable cursor : int;
}

let of_img ?(cursor=0) img = { content = LoaderImg img; cursor; }

let of_binstream ?(cursor=0) ?(base=0) hstr =
  { content = BinStream (base, hstr); cursor; }

let of_nibbles ?(cursor=0) ?(base=0) str =
  Logger.debug "lreader of nibbles %s" str;
  let hstr = Basic_types.Binstream.of_nibbles str in
  of_binstream ~cursor ~base hstr

let of_bytes ?(cursor=0) ?(base=0) str =
  Logger.debug "%s" str;
  let hstr = Basic_types.Binstream.of_bytes str in
  of_binstream ~cursor ~base hstr

let pp ppf r = Format.fprintf ppf "%@%x" r.cursor

let set_virtual_cursor r addr =
  match r with
  | { content = LoaderImg _; _ } -> r.cursor <- addr
  | { content = BinStream (base, _); _ } ->
      r.cursor <- addr - base

let get_virtual_cursor = function
  | { content = LoaderImg _; cursor } -> cursor
  | { content = BinStream (base, _); cursor } -> base + cursor

let rewind r n = assert (n >= 0); r.cursor <- r.cursor - n

let advance r n = assert (n >= 0); r.cursor <- r.cursor + n

(* [to_int bytes] returns the contents of the big-endian [bytes] as an int
   value.
   @warning: the computed value may not be correct if the bytes list needs more
   than 63 bits (ocaml integer).
*)
let to_int bytes =
  assert (List.length bytes <= 8);
  List.fold_left (fun acc n -> (acc lsl 8) lor n ) 0 bytes

let read_cell r =
  match r.content with
  | LoaderImg img -> Loader.read_address img r.cursor
  | BinStream (_, hstr) ->
    Basic_types.Binstream.get_byte hstr r.cursor

module Read = struct
(* [read r n] reads [n] bytes from reader [r].
   @returns a big-endian ordered byte list: the lowest address is at the end of
   the list.
 *)
let read r n =
  let limit = r.cursor + n - 1 in
  let rec loop acc =
    if r.cursor > limit then acc (* ? *)
    else begin
      let acc = read_cell r :: acc in
      advance r 1;
      loop acc
    end
  in loop []

let u8 r =
  match read r 1 with
  | [x] -> x
  | _ -> assert false

let u16 r = to_int (read r 2)

let u32 r = to_int (read r 4)

let u64 r = to_int (read r 8)
(* This function is not correct:
   It returns a 63-bits long caml integer while the read value is potentially 64 bits long.
*)

end

(** Peeking functions *)
module Peek = struct
let peek r n =
  let b = to_int (Read.read r n) in
  rewind r n;
  b

let u8 r  = peek r 1

let u16 r = peek r 2

let u32 r = peek r 4

let u64 r = peek r 8
end

let get_slice r start_address end_address =
  assert (start_address < end_address);
  let saved_address = get_virtual_cursor r in
  let rec loop acc =
    if get_virtual_cursor r = end_address then List.rev acc
    else loop (Read.u8 r :: acc)
  in
  set_virtual_cursor r start_address;
  let bytes = loop [] in
  set_virtual_cursor r saved_address;
  bytes
