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

open Loader_types

exception Invalid_format of string
let invalid_format msg = raise (Invalid_format msg)

let assert_format b msg =
  if not b then
    invalid_format msg

module type S =
sig
  type t

  val dim : t -> int

  type cursor = private {
    buffer: t;
    endian: endian;
    mutable position: int;
  }

  val cursor  : ?at:int -> endian -> t -> cursor
  val seek    : cursor -> int -> unit
  val ensure  : cursor -> int -> string -> unit
  val advance : cursor -> int -> unit
  val at_end  : cursor -> bool

  module Peek : sig
    val u8  : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64

    val fixed_string : cursor -> int -> string
    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
  end

  module Read : sig
    val u8  : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64

    val fixed_string : cursor -> int -> string
    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
  end
end

module type Bufferable =
sig
  type t

  val get : t -> int -> int
  val dim : t -> int
end

module Make (B: Bufferable) =
struct
  type t = B.t

  let dim t = B.dim t

  type cursor = {
    buffer: t;
    endian: endian;
    mutable position: int;
  }

  let cursor ?(at=0) endian buffer =
    { buffer; endian; position = at }

  let seek t position =
    t.position <- position

  let ensure t count msg =
    if t.position + count > dim t.buffer then
      invalid_format msg

  let advance t count = t.position <- t.position + count

  let at_end t = dim t.buffer = t.position

  (* All endian and bit-width dependent code starts here *)
  let little_u16 t : u16 =
    B.get t.buffer t.position
    lor B.get t.buffer (t.position + 1) lsl 8

  let little_u32 t : u32 =
    B.get t.buffer t.position
    lor B.get t.buffer (t.position + 1) lsl 8
    lor B.get t.buffer (t.position + 2) lsl 16
    lor B.get t.buffer (t.position + 3) lsl 24

  let little_u64 t : u64 =
    B.get t.buffer t.position
    lor B.get t.buffer (t.position + 1) lsl 8
    lor B.get t.buffer (t.position + 2) lsl 16
    lor B.get t.buffer (t.position + 3) lsl 24
    lor B.get t.buffer (t.position + 4) lsl 32
    lor B.get t.buffer (t.position + 5) lsl 40
    lor B.get t.buffer (t.position + 6) lsl 48
    lor B.get t.buffer (t.position + 7) lsl 56

  let big_u16 t : u16 =
    B.get t.buffer (t.position + 1)
    lor B.get t.buffer  t.position lsl 8

  let big_u32 t : u32 =
    B.get t.buffer (t.position + 3)
    lor B.get t.buffer (t.position + 2) lsl 8
    lor B.get t.buffer (t.position + 1) lsl 16
    lor B.get t.buffer  t.position lsl 24

  let big_u64 t : u64 =
    B.get t.buffer (t.position + 7)
    lor B.get t.buffer (t.position + 6) lsl 8
    lor B.get t.buffer (t.position + 5) lsl 16
    lor B.get t.buffer (t.position + 4) lsl 24
    lor B.get t.buffer (t.position + 3) lsl 32
    lor B.get t.buffer (t.position + 2) lsl 40
    lor B.get t.buffer (t.position + 1) lsl 48
    lor B.get t.buffer  t.position lsl 56

  let rec scan_0 (b : t) ofs l i =
    if i >= l then None
    else if B.get b (ofs + i) = 0 then Some i
    else scan_0 b ofs l (i + 1)

  let string_init t length =
    let buffer = t.buffer in
    let position = t.position in
    String.init length (fun i -> Char.chr (B.get buffer (position + i)))

  module Peek = struct
    let u8 t : u8 =
      B.get t.buffer (t.position)

    let u16 t : u16 =
      match t.endian with
      | LittleEndian -> little_u16 t
      | BigEndian    -> big_u16 t

    let u32 t : u32 =
      match t.endian with
      | LittleEndian -> little_u32 t
      | BigEndian    -> big_u32 t

    let u64 t : u64 =
      match t.endian with
      | LittleEndian -> little_u64 t
      | BigEndian    -> big_u64 t

    let fixed_string t length =
      let buffer = t.buffer in
      let position = t.position in
      let l =
        match scan_0 buffer position length 0 with
        | None -> length
        | Some length -> length
      in
      string_init t l

    let zero_string msg t ?maxlen () =
      let buffer = t.buffer in
      let position = t.position in
      let maxlen = match maxlen with
        | None -> dim t.buffer - t.position
        | Some maxlen -> maxlen
      in
      let length =
        match scan_0 buffer position maxlen 0 with
        | None -> invalid_format msg
        | Some length -> length
      in
      fixed_string t length
  end

  module Read = struct
    let u8 t : u8 =
      let result = Peek.u8 t in
      advance t 1;
      result

    let u16 t : u16 =
      let result = Peek.u16 t in
      advance t 2;
      result

    let u32 t : u32 =
      let result = Peek.u32 t in
      advance t 4;
      result

    let u64 t : u64 =
      let result = Peek.u64 t in
      advance t 8;
      result

    let fixed_string t length =
      let result = Peek.fixed_string t length in
      advance t length;
      result

    let zero_string msg t ?maxlen () =
      let maxlen = match maxlen with
        | None -> dim t.buffer - t.position
        | Some maxlen -> maxlen
      in
      let result = Peek.zero_string msg t ~maxlen () in
      advance t (String.length result + 1);
      result
  end
end

include Make
    (struct
      open Bigarray
      type t = (int, int8_unsigned_elt, c_layout) Array1.t

      let get t i = Array1.get t i
      let dim t   = Array1.dim t
    end)

let sub t length =
  let result = cursor t.endian (Bigarray.Array1.sub t.buffer t.position length) in
  advance t length;
  result

