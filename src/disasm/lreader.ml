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

module type Accessor = sig
  type t

  val u8 : t -> int
  val i8 : t -> int
  val u16 : t -> int
  val i16 : t -> int
  val i32 : t -> int32
  val i64 : t -> int64
  val bv8 : t -> Bitvector.t
  val bv16 : t -> Bitvector.t
  val bv32 : t -> Bitvector.t
  val bv64 : t -> Bitvector.t
  val read : t -> int -> Bitvector.t
end

type byte = int

type t =
  | Cursor : {
      content : 'a;
      get : 'a -> int -> byte;
      mutable pos : int;
      mutable endianness : Machine.endianness;
    }
      -> t

let create ?(endianness = Machine.LittleEndian) ?(at = 0) get content =
  Cursor { content; get; pos = at; endianness }

let of_img ?endianness ?at img = create ?endianness ?at Loader.read_address img

let of_zero_extend_buffer =
  let get b x =
    if Bigarray.Array1.dim b <= x then 0 else Bigarray.Array1.unsafe_get b x
  in
  fun ?endianness ?at buffer -> create ?endianness ?at get buffer

let of_binstream ?endianness ?at hstr =
  create ?endianness ?at Binstream.get_byte_exn hstr

let of_bytes =
  let get s x = Char.code (String.get s x) in
  fun ?endianness ?at str -> create ?endianness ?at get str

let of_nibbles =
  let value_of c =
    let cc = Char.code c in
    if c <= '9' then cc - Char.code '0' else 10 + cc - Char.code 'a'
  in
  let get s x =
    let hi = String.get s (2 * x) and lo = String.get s ((2 * x) + 1) in
    (value_of hi lsl 4) lor value_of lo
  in
  fun ?endianness ?at str -> create ?endianness ?at get str

let pp ppf (Cursor { pos; _ }) = Format.fprintf ppf "%@%x" pos
let set_endianness (Cursor t) e = t.endianness <- e
let get_pos (Cursor t) = t.pos

let rewind (Cursor t) n =
  assert (n >= 0);
  t.pos <- t.pos - n

let advance (Cursor t) n =
  assert (n >= 0);
  t.pos <- t.pos + n

module Peek : Accessor with type t := t = struct
  let read (Cursor { get; content; pos; endianness }) n : bytes =
    match endianness with
    | LittleEndian ->
        Bytes.init n (fun i -> Char.unsafe_chr (get content (pos + i)))
    | BigEndian ->
        Bytes.init n (fun i -> Char.unsafe_chr (get content (pos + n - 1 - i)))

  (* unsigned 8 bits int *)
  let u8 (Cursor { get; content; pos; _ }) = get content pos
  let u16 t = Bytes.get_uint16_le (read t 2) 0
  let i8 t = (u8 t lxor 0x80) - 0x80
  let i16 t = (u16 t lxor 0x8000) - 0x8000
  let i32 t = Bytes.get_int32_le (read t 4) 0
  let i64 t = Bytes.get_int64_le (read t 8) 0
  let bv8 t = Bitvector.of_int ~size:8 (u8 t)
  let bv16 t = Bitvector.of_int ~size:16 (u16 t)

  let bv32 t =
    Bitvector.create (Z.of_bits (Bytes.unsafe_to_string (read t 4))) 32

  let bv64 t =
    Bitvector.create (Z.of_bits (Bytes.unsafe_to_string (read t 8))) 64

  let read t n =
    Bitvector.create (Z.of_bits (Bytes.unsafe_to_string (read t n))) (8 * n)
end

module Read : Accessor with type t := t = struct
  let read t n =
    let r = Peek.read t n in
    advance t n;
    r

  let advance f n t =
    let r = f t in
    advance t n;
    r

  (* unsigned 8 bits int *)
  let u8 = advance Peek.u8 1
  let u16 = advance Peek.u16 2
  let i8 = advance Peek.i8 1
  let i16 = advance Peek.i16 2
  let i32 = advance Peek.i32 4
  let i64 = advance Peek.i64 8
  let bv8 = advance Peek.bv8 1
  let bv16 = advance Peek.bv16 2
  let bv32 = advance Peek.bv32 4
  let bv64 = advance Peek.bv64 8
end

let get_slice (Cursor { get; content; _ }) ~lo ~hi =
  Bytes.init (hi - lo + 1) (fun i -> Char.unsafe_chr (get content (lo + i)))
