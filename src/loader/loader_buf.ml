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
    endian: Machine.endianness;
    mutable position: int;
  }

  val cursor  : ?at:int -> Machine.endianness -> t -> cursor
  val seek    : cursor -> int -> unit
  val ensure  : cursor -> int -> string -> unit
  val advance : cursor -> int -> unit
  val at_end  : cursor -> bool

  module Peek : sig
    val u8  : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64

    val s8  : cursor -> s8
    val s16 : cursor -> s16
    val s32 : cursor -> s32
    val s64 : cursor -> s64

    val uleb128 : cursor -> u64
    val sleb128 : cursor -> s64

    val fixed_string : cursor -> int -> string
    val zero_string : string -> cursor -> ?maxlen:int -> unit -> string
  end

  module Read : sig
    val u8  : cursor -> u8
    val u16 : cursor -> u16
    val u32 : cursor -> u32
    val u64 : cursor -> u64

    val s8  : cursor -> s8
    val s16 : cursor -> s16
    val s32 : cursor -> s32
    val s64 : cursor -> s64

    val uleb128 : cursor -> u64
    val sleb128 : cursor -> s64

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
    endian: Machine.endianness;
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

  let uleb128 t : u64 * int =
    let rec loop i shift value =
      match B.get t.buffer (t.position + i) with
      | x when 0x80 land x = 0x00 ->
        if shift > 56 then failwith (Format.sprintf
            "unsafe uleb128 into int conversion at 0x%x" t.position);
        x lsl shift lor value, i + 1
      | x -> loop (i + 1) (shift + 7) ((0x7f land x) lsl shift lor value) in
    loop 0 0 0

  let sleb128 t : s64 * int =
    let rec loop i shift value =
      match B.get t.buffer (t.position + i) with
      | x when 0x80 land x = 0x00 ->
        if shift > 56 then
          failwith (Format.sprintf
            "unsafe sleb128 into int conversion at 0x%x" t.position);
        if 0x40 land x = 0x00 then x lsl shift lor value, i + 1
        else x lsl shift lor value lor lnot 0x00 lsl (shift + 7), i + 1
      | x -> loop (i + 1) (shift + 7) ((0x7f land x) lsl shift lor value) in
    loop 0 0 0

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

  let little_s16 t : s16 =
    let v = B.get t.buffer (t.position + 0) in
    let s = B.get t.buffer (t.position + 1) in
    match 0x80 land s > 0 with
    | false -> v lor s lsl 8
    | true  -> v lor s lsl 8 lor lnot 0xffff

  let little_s32 t : s32 =
    let v = B.get t.buffer (t.position + 0) lsl 0
        lor B.get t.buffer (t.position + 1) lsl 8
        lor B.get t.buffer (t.position + 2) lsl 16 in
    let s = B.get t.buffer (t.position + 3) in
    match 0x80 land s > 0 with
    | false -> v lor s lsl 24
    | true  -> v lor s lsl 24 lor lnot 0xffffffff

  let little_s64 t : s64 =
    let v = B.get t.buffer (t.position + 0) lsl 0
        lor B.get t.buffer (t.position + 1) lsl 8
        lor B.get t.buffer (t.position + 2) lsl 16
        lor B.get t.buffer (t.position + 3) lsl 24
        lor B.get t.buffer (t.position + 4) lsl 32
        lor B.get t.buffer (t.position + 5) lsl 40
        lor B.get t.buffer (t.position + 6) lsl 48 in
    let s = B.get t.buffer (t.position + 7) in
    if 0xc0 land s = 0x80 || 0xc0 land s = 0x40 then
      failwith (Format.sprintf
        "unsafe s64 into int conversion at 0x%x : 0x%x%x -> %d"
        t.position s v (v lor s lsl 56));
    v lor s lsl 56


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

  let big_s16 t : s16 =
    let v = B.get t.buffer (t.position + 1) in
    let s = B.get t.buffer (t.position + 0) in
    match 0x80 land s > 0 with
    | false -> v lor s lsl 8
    | true  -> v lor s lsl 8 lor lnot 0xffff

  let big_s32 t : s32 =
    let v = B.get t.buffer (t.position + 3) lsl 0
        lor B.get t.buffer (t.position + 2) lsl 8
        lor B.get t.buffer (t.position + 1) lsl 16 in
    let s = B.get t.buffer (t.position + 0) in
    match 0x80 land s > 0 with
    | false -> v lor s lsl 24
    | true  -> v lor s lsl 24 lor lnot 0xffffffff

  let big_s64 t : s64 =
    let v = B.get t.buffer (t.position + 7) lsl 0
        lor B.get t.buffer (t.position + 6) lsl 8
        lor B.get t.buffer (t.position + 5) lsl 16
        lor B.get t.buffer (t.position + 4) lsl 24
        lor B.get t.buffer (t.position + 3) lsl 32
        lor B.get t.buffer (t.position + 2) lsl 40
        lor B.get t.buffer (t.position + 1) lsl 48 in
    let s = B.get t.buffer (t.position + 0) in
    if 0xc0 land s = 0x80 || 0xc0 land s = 0x40 then
      failwith (Format.sprintf
        "unsafe s64 into int conversion at 0x%x : 0x%x%x -> %d"
        t.position s v (v lor s lsl 56));
    v lor s lsl 56


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
      | Machine.LittleEndian -> little_u16 t
      | Machine.BigEndian    -> big_u16 t

    let u32 t : u32 =
      match t.endian with
      | Machine.LittleEndian -> little_u32 t
      | Machine.BigEndian    -> big_u32 t

    let u64 t : u64 =
      match t.endian with
      | Machine.LittleEndian -> little_u64 t
      | Machine.BigEndian    -> big_u64 t

    let s8 t : s8 =
      let s = B.get t.buffer t.position in
      match 0x80 land s > 0 with
      | false -> s
      | true  -> s lor lnot 0xff

    let s16 t : s16 =
      match t.endian with
      | Machine.LittleEndian -> little_s16 t
      | Machine.BigEndian    -> big_s16 t

    let s32 t : s32 =
      match t.endian with
      | Machine.LittleEndian -> little_s32 t
      | Machine.BigEndian    -> big_s32 t

    let s64 t : s64 =
      match t.endian with
      | Machine.LittleEndian -> little_s64 t
      | Machine.BigEndian    -> big_s64 t

    let uleb128 t : u64 = let v, _ = uleb128 t in v

    let sleb128 t : s64 = let v, _ = sleb128 t in v

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

    let s8 t : s8 =
      let result = Peek.s8 t in
      advance t 1;
      result

    let s16 t : s16 =
      let result = Peek.s16 t in
      advance t 2;
      result

    let s32 t : s32 =
      let result = Peek.s32 t in
      advance t 4;
      result

    let s64 t : s64 =
      let result = Peek.s64 t in
      advance t 8;
      result

    let uleb128 t : u64 =
      let v, s = uleb128 t in
      advance t s;
      v

    let sleb128 t : s64 =
      let v, s = sleb128 t in
      advance t s;
      v

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

module type W =
sig
  include S

  module Write : sig
    val u8  : cursor -> u8 -> unit
    val u16 : cursor -> u16 -> unit
    val u32 : cursor -> u32 -> unit
    val u64 : cursor -> u64 -> unit

    val s8  : cursor -> s8 -> unit
    val s16 : cursor -> s16 -> unit
    val s32 : cursor -> s32 -> unit
    val s64 : cursor -> s64 -> unit
  end
end

module type Writable =
sig
  include Bufferable

  val set : t -> int -> u8 -> unit
end

module Wake (W: Writable) = struct
  include Make (W)

  module Write = struct
    let u8 t v : unit =
      W.set t.buffer t.position v;
      advance t 1
    let u16 t v : unit =
      begin match t.endian with
      | Machine.LittleEndian ->
         W.set t.buffer (t.position + 0) (0xff land v lsr 0);
         W.set t.buffer (t.position + 1) (0xff land v lsr 8)
      | Machine.BigEndian    ->
         W.set t.buffer (t.position + 1) (0xff land v lsr 0);
         W.set t.buffer (t.position + 0) (0xff land v lsr 8)
      end;
      advance t 2
    let u32 t v : unit =
      begin match t.endian with
      | Machine.LittleEndian ->
         W.set t.buffer (t.position + 0) (0xff land v lsr 0);
         W.set t.buffer (t.position + 1) (0xff land v lsr 8);
         W.set t.buffer (t.position + 2) (0xff land v lsr 16);
         W.set t.buffer (t.position + 3) (0xff land v lsr 24)
      | Machine.BigEndian    ->
         W.set t.buffer (t.position + 3) (0xff land v lsr 0);
         W.set t.buffer (t.position + 2) (0xff land v lsr 8);
         W.set t.buffer (t.position + 1) (0xff land v lsr 16);
         W.set t.buffer (t.position + 0) (0xff land v lsr 24)
      end;
      advance t 4
    let u64 t v : unit =
      begin match t.endian with
      | Machine.LittleEndian ->
         W.set t.buffer (t.position + 0) (0xff land v lsr 0);
         W.set t.buffer (t.position + 1) (0xff land v lsr 8);
         W.set t.buffer (t.position + 2) (0xff land v lsr 16);
         W.set t.buffer (t.position + 3) (0xff land v lsr 24);
         W.set t.buffer (t.position + 4) (0xff land v lsr 32);
         W.set t.buffer (t.position + 5) (0xff land v lsr 40);
         W.set t.buffer (t.position + 6) (0xff land v lsr 48);
         W.set t.buffer (t.position + 7) (0xff land v lsr 56)
      | Machine.BigEndian    ->
         W.set t.buffer (t.position + 7) (0xff land v lsr 0);
         W.set t.buffer (t.position + 6) (0xff land v lsr 8);
         W.set t.buffer (t.position + 5) (0xff land v lsr 16);
         W.set t.buffer (t.position + 4) (0xff land v lsr 24);
         W.set t.buffer (t.position + 3) (0xff land v lsr 32);
         W.set t.buffer (t.position + 2) (0xff land v lsr 40);
         W.set t.buffer (t.position + 1) (0xff land v lsr 48);
         W.set t.buffer (t.position + 0) (0xff land v lsr 56)
      end;
      advance t 8

    let s8 = u8
    let s16 = u16
    let s32 = u32
    let s64 = u64
  end
end

include Wake
    (struct
      open Bigarray
      type t = (int, int8_unsigned_elt, c_layout) Array1.t

      let get = Array1.get
      let set = Array1.set
      let dim = Array1.dim
    end)

let sub t length =
  let result = cursor t.endian (Bigarray.Array1.sub t.buffer t.position length) in
  advance t length;
  result

let read ?(signed=false) bitwidth = match signed, bitwidth with
  | false, `x32 -> Read.u32
  | false, `x64 -> Read.u64
  | true, `x32 -> Read.s32
  | true, `x64 -> Read.s64
