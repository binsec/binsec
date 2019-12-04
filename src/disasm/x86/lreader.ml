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

open X86_options

module type Accessor = sig
  type t
  val u8  : t -> int
  val u16 : t -> int
  val u32 : t -> int
  val u64 : t -> int
  val i8  : t -> int
  val i16 : t -> int
  val i32 : t -> int
  val bv8: t -> Bitvector.t
  val bv16: t -> Bitvector.t
  val bv32: t -> Bitvector.t
  val bv64: t -> Bitvector.t
end

type bytestream =
  | LoaderImg of Loader.Img.t
  | BinStream of int * Binstream.t


type t = {
  content : bytestream ;
  mutable cursor : int;
  mutable endianness : Machine.endianness;
}

(* Default value for endianness *)
let little = Machine.LittleEndian ;;

let of_img ?(endianness=little) ?(cursor=0) img =
  { content = LoaderImg img; cursor; endianness; }
;;

let of_binstream ?(endianness=little) ?(cursor=0) ?(base=0) hstr =
  { content = BinStream (base, hstr); cursor; endianness; }
;;

let of_nibbles ?(endianness=little) ?(cursor=0) ?(base=0) str =
  Logger.debug "lreader of nibbles %s" str;
  let hstr = Binstream.of_nibbles str in
  of_binstream ~endianness ~cursor ~base hstr
;;

let of_bytes ?(endianness=little) ?(cursor=0) ?(base=0) str =
  Logger.debug "%s" str;
  let hstr = Binstream.of_bytes str in
  of_binstream ~endianness ~cursor ~base hstr
;;

let pp ppf r = Format.fprintf ppf "%@%x" r.cursor

let set_endianness t e = t.endianness <- e ;;

let set_virtual_cursor r addr =
  match r with
  | { content = LoaderImg _; _ } -> r.cursor <- addr
  | { content = BinStream (base, _); _ } ->
    r.cursor <- addr - base

let get_virtual_cursor = function
  | { content = LoaderImg _; cursor; _ } -> cursor
  | { content = BinStream (base, _); cursor; _ } -> base + cursor

let rewind r n = assert (n >= 0); r.cursor <- r.cursor - n

let advance r n = assert (n >= 0); r.cursor <- r.cursor + n

type bigendian = int ;;

(* [to_int bytes] returns the contents of the big-endian [bytes] as an int
   value.
   @warning: the computed value may not be correct if the bytes list needs more
   than 63 bits (ocaml integer).

   In this case, use [to_bv] to get the correct value
*)
let to_int e (bytes:bigendian list) =
  assert (List.length bytes <= 8);
  match e with
  | Machine.LittleEndian ->
     List.fold_left (fun acc n -> (acc lsl 8) lor n ) 0 bytes
  | Machine.BigEndian ->
     List.fold_left (fun acc n -> (acc lsl 8) lor n ) 0 (List.rev bytes)
;;

let to_bv e bytes =
  assert (List.length bytes <= 8);
  match e with
  | Machine.LittleEndian ->
     Bitvector.concat @@ List.map (Bitvector.of_int ~size:8) bytes
  | Machine.BigEndian ->
     Bitvector.concat @@ List.rev_map (Bitvector.of_int ~size:8) bytes
;;

let read_cell r =
  match r.content with
  | LoaderImg img -> Loader.read_address img r.cursor
  | BinStream (_, hstr) ->
     Binstream.get_byte_exn hstr r.cursor
;;

module Read = struct


  (* [read r n] reads [n] bytes from reader [r].
     @return a big-endian ordered byte list: the lowest address is at the end of
     the list.
  *)
  let read r n : bigendian list =
    let limit = r.cursor + n - 1 in
    let rec loop acc =
      if r.cursor > limit then acc (* ? *)
      else begin
        let acc = read_cell r :: acc in
        advance r 1;
        loop acc
      end
    in loop []

  (* unsigned 8 bits int *)
  let u8 r =
    match read r 1 with
    | [x] -> x
    | _ -> assert false

  let u16 r = to_int r.endianness (read r 2)

  let u32 r = to_int r.endianness (read r 4)

  (* This function is not correct:
     It returns a 63-bits long caml integer while the read value is potentially 64 bits long.
  *)
  let u64 r = to_int r.endianness (read r 8)

  (* signed 8 bits int *)
  let i8 r =
    let x = u8 r in
    if x >= 0x80 then x - 0xff - 1 else x

  let i16 r =
    let x = u16 r in
    if x >= 0x8000 then x - 0xffff - 1 else x

  let i32 r =
    let x = u32 r in
    if x >= 0x80000000 then x - 0xffffffff - 1 else x

  let bv8  r = to_bv r.endianness (read r 1)
  let bv16 r = to_bv r.endianness (read r 2)
  let bv32 r = to_bv r.endianness (read r 4)
  let bv64 r = to_bv r.endianness (read r 8)
end

(** Peeking functions *)
module Peek = struct
  let peek f r n =
    let a = get_virtual_cursor r in
    match f r.endianness (Read.read r n) with
    | v -> set_virtual_cursor r a; v
    | exception e -> set_virtual_cursor r a; raise e
  ;;

  let peek_int = peek to_int
  let peek_bv  = peek to_bv

  let u8 r  = peek_int r 1

  let u16 r = peek_int r 2

  let u32 r = peek_int r 4

  let u64 r = peek_int r 8

  (* signed 8 bits int *)
  let i8 r =
    let x = u8 r in
    if x >= 0x80 then x - 0xff - 1 else x

  let i16 r =
    let x = u16 r in
    if x >= 0x8000 then x - 0xffff - 1 else x

  let i32 r =
    let x = u32 r in
    if x >= 0x80000000 then x - 0xffffffff - 1 else x


  let bv8  r = peek_bv r 1
  let bv16 r = peek_bv r 2
  let bv32 r = peek_bv r 4
  let bv64 r = peek_bv r 8

  let peek = peek_bv
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
