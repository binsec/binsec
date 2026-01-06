(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

(** Basic stream reader *)

open Basic_types.Integers

type nonrec uint8 = uint8
type nonrec uint16 = uint16
type nonrec uint32 = uint32
type nonrec uint64 = uint64
type nonrec int8 = int8
type nonrec int16 = int16
type nonrec int32 = int32
type nonrec int64 = int64
type endianness = Basic_types.endianness = LittleEndian | BigEndian

type 'a t =
  | Cursor : {
      offset : 'a -> int -> 'a;
      get : 'b -> 'a -> char;
      content : 'b;
      start : 'a;
      mutable pos : 'a;
      stop : 'a;
      mutable endianness : endianness;
      buffer : Bytes.t;
    }
      -> 'a t

let create :
    offset:('a -> int -> 'a) ->
    get:('b -> 'a -> char) ->
    ?endianness:endianness ->
    start:'a ->
    ?pos:'a ->
    stop:'a ->
    'b ->
    'a t =
 fun ~offset ~get ?(endianness = LittleEndian) ~start ?(pos = start) ~stop
     content ->
  Cursor
    {
      offset;
      get;
      content;
      start;
      pos;
      stop;
      endianness;
      buffer = Bytes.create 8;
    }

let rebase :
    offset:('a -> int -> 'a) ->
    distance:('a -> 'a -> int) ->
    'a ->
    int t ->
    'a t =
 fun ~offset ~distance base
     (Cursor { get; content; start; pos; stop; endianness; buffer; _ }) ->
  Cursor
    {
      offset;
      get = (fun content idx -> get content (distance idx base));
      content;
      start = offset base start;
      pos = offset base pos;
      stop = offset base stop;
      endianness;
      buffer;
    }

let of_bigarray :
    ?pos:int ->
    ?endianness:endianness ->
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int t =
  let get :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      int ->
      char =
   fun b i -> Char.unsafe_chr (Bigarray.Array1.unsafe_get b i)
  in
  fun ?pos ?endianness content ->
    create ~offset:( + ) ~get ?endianness ~start:0 ?pos
      ~stop:(Bigarray.Array1.dim content)
      content

let of_zero_extend_bigarray :
    ?pos:int ->
    ?endianness:endianness ->
    dim:int ->
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int t =
  let get :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      int ->
      char =
   fun b i ->
    if Bigarray.Array1.dim b <= i then '\x00'
    else Char.unsafe_chr (Bigarray.Array1.unsafe_get b i)
  in
  fun ?pos ?endianness ~dim content ->
    create ~offset:( + ) ~get ?endianness ~start:0 ?pos ~stop:dim content

let of_nibbles : ?pos:int -> ?endianness:endianness -> string -> int t =
  let value_of c =
    if c <= '9' then Char.code c - Char.code '0'
    else 10 + Char.code c - Char.code 'a'
  in
  let get s x =
    let hi = String.unsafe_get s (2 * x)
    and lo = String.unsafe_get s ((2 * x) + 1) in
    Char.unsafe_chr ((value_of hi lsl 4) lor value_of lo)
  in
  fun ?pos ?endianness content ->
    create ~offset:( + ) ~get ?endianness ~start:0 ?pos
      ~stop:(String.length content / 2)
      content

let of_bytes : ?pos:int -> ?endianness:endianness -> string -> int t =
 fun ?pos ?endianness content ->
  create ~offset:( + ) ~get:String.unsafe_get ?endianness ~start:0 ?pos
    ~stop:(String.length content) content

let of_binstream : ?pos:int -> ?endianness:endianness -> Binstream.t -> int t =
  let get t i = Char.unsafe_chr (Binstream.get_byte_exn t i) in
  fun ?pos ?endianness content ->
    create ~offset:( + ) ~get ?endianness ~start:0 ?pos
      ~stop:(Binstream.length content) content

let get_pos : int t -> int = function Cursor { start; pos; _ } -> pos - start

let check_pos : 'a t -> 'a -> unit =
 fun (Cursor { start; stop; _ }) pos ->
  if pos < start || stop < pos then
    raise (Invalid_argument "index out of bounds")

let sub : 'a t -> int -> int t =
 fun (Cursor { offset; get; content; pos; endianness; buffer; _ } as t) len ->
  check_pos t (offset pos len);
  Cursor
    {
      offset = ( + );
      get = (fun content idx -> get content (offset pos idx));
      content;
      start = 0;
      pos = 0;
      stop = len;
      endianness;
      buffer;
    }

let set_pos : 'a t -> int -> unit =
 fun (Cursor r as t) delta ->
  let pos = r.offset r.start delta in
  check_pos t pos;
  r.pos <- pos

let move : 'a t -> 'a -> unit =
 fun (Cursor r as t) pos ->
  check_pos t pos;
  r.pos <- pos

let advance : 'a t -> int -> unit =
 fun (Cursor { offset; pos; _ } as t) n -> move t (offset pos n)

let rewind : 'a t -> int -> unit =
 fun (Cursor { offset; pos; _ } as t) n -> move t (offset pos (-n))

let get_endianness : 'a t -> endianness =
 fun (Cursor { endianness; _ }) -> endianness

let set_endianness : 'a t -> endianness -> unit =
 fun (Cursor r) endianness -> r.endianness <- endianness

let dim : int t -> int = fun (Cursor { start; stop; _ }) -> stop - start
let at_end : 'a t -> bool = fun (Cursor { pos; stop; _ }) -> pos = stop

let ensure : 'a t -> int -> bool =
 fun (Cursor { offset; pos; stop; _ }) n -> offset pos n <= stop

module type ACCESS = sig
  val u8 : 'a t -> uint8
  val u16 : 'a t -> uint16
  val u32 : 'a t -> uint32
  val u64 : 'a t -> uint64
  val i8 : 'a t -> int8
  val i16 : 'a t -> int16
  val i32 : 'a t -> int32
  val i64 : 'a t -> int64
  val uleb128 : 'a t -> Z.t
  val sleb128 : 'a t -> Z.t
  val bv8 : 'a t -> Bitvector.t
  val bv16 : 'a t -> Bitvector.t
  val bv32 : 'a t -> Bitvector.t
  val bv64 : 'a t -> Bitvector.t
  val read : 'a t -> int -> Bitvector.t

  val bytes : 'a t -> int -> string
  (** [bytes t len] gets a string of exactly [len] bytes from [t] *)

  val fixed_string : 'a t -> int -> string
  (** [fixed_string t len] gets a string of maximum [len] bytes from [t] *)

  val zero_string : string -> 'a t -> ?maxlen:int -> unit -> string
  (** [zero_string msg t ?maxlen ()] gets a zero-terminated string from [t],
      stopping at the first zero or when [maxlen] is reached, if it was
      provided. *)

  val sub : 'a t -> int -> 'a t
  (** [sub t len] returns a fresh cursor pointing to the beginning of a sub-buffer
      of size [len] starting from [t], and advances [t] by [len]. *)
end

let uleb128 : 'a t -> Z.t * int =
  let rec loop : 'a t -> int -> int -> Z.t -> Z.t * int =
   fun (Cursor { offset; get; pos; content; _ } as t) i shift value ->
    check_pos t (offset pos i);
    let byte = Char.code (get content (offset pos i)) in
    if 0x80 land byte = 0x00 then
      (Z.logor (Z.shift_left (Z.of_int byte) shift) value, i + 1)
    else
      loop t (i + 1) (shift + 7)
        (Z.logor (Z.shift_left (Z.of_int (0x7f land byte)) shift) value)
  in
  fun t -> loop t 0 0 (Z.of_int 0)

let sleb128 : 'a t -> Z.t * int =
  let rec loop : 'a t -> int -> int -> Z.t -> Z.t * int =
   fun (Cursor { offset; get; pos; content; _ } as t) i shift value ->
    check_pos t (offset pos i);
    let byte = Char.code (get content (offset pos i)) in
    if 0x80 land byte = 0x00 then
      if 0x40 land byte = 0x00 then
        (Z.logor (Z.shift_left (Z.of_int byte) shift) value, i + 1)
      else
        ( Z.signed_extract
            (Z.logor (Z.shift_left (Z.of_int byte) shift) value)
            0 (shift + 7),
          i + 1 )
    else
      loop t (i + 1) (shift + 7)
        (Z.logor (Z.shift_left (Z.of_int (0x7f land byte)) shift) value)
  in
  fun t -> loop t 0 0 (Z.of_int 0)

module Peek : ACCESS = struct
  external unsafe_char_to_int8 : char -> int8 = "%identity"
  external unsafe_int_to_int16 : int -> int16 = "%identity"
  external unsafe_int8_to_uint8 : int8 -> uint8 = "%identity"
  external unsafe_int16_to_uint16 : int16 -> uint16 = "%identity"
  external unsafe_int32_to_uint32 : int32 -> uint32 = "%identity"
  external unsafe_int64_to_uint64 : int64 -> uint64 = "%identity"

  let load : 'a t -> int -> unit =
   fun (Cursor { offset; get; pos; content; buffer; _ } as t) n ->
    check_pos t (offset pos n);
    for i = 0 to n - 1 do
      Bytes.unsafe_set buffer i (get content (offset pos i))
    done

  let i8 : 'a t -> int8 =
   fun (Cursor { offset; get; pos; content; _ } as t) ->
    check_pos t (offset pos 1);
    unsafe_char_to_int8 (get content pos)

  let i16 : 'a t -> int16 =
   fun (Cursor { endianness; buffer; _ } as t) ->
    load t 2;
    let value =
      match endianness with
      | LittleEndian -> Bytes.get_uint16_le buffer 0
      | BigEndian -> Bytes.get_uint16_be buffer 0
    in
    unsafe_int_to_int16 value

  let i32 : 'a t -> int32 =
   fun (Cursor { endianness; buffer; _ } as t) ->
    load t 4;
    match endianness with
    | LittleEndian -> Bytes.get_int32_le buffer 0
    | BigEndian -> Bytes.get_int32_be buffer 0

  let i64 : 'a t -> int64 =
   fun (Cursor { endianness; buffer; _ } as t) ->
    load t 8;
    match endianness with
    | LittleEndian -> Bytes.get_int64_le buffer 0
    | BigEndian -> Bytes.get_int64_be buffer 0

  let u8 : 'a t -> uint8 = fun t -> unsafe_int8_to_uint8 (i8 t)
  let u16 : 'a t -> uint16 = fun t -> unsafe_int16_to_uint16 (i16 t)
  let u32 : 'a t -> uint32 = fun t -> unsafe_int32_to_uint32 (i32 t)
  let u64 : 'a t -> uint64 = fun t -> unsafe_int64_to_uint64 (i64 t)

  let bv8 : 'a t -> Bitvector.t =
   fun t -> Bitvector.of_int ~size:8 (Uint8.to_int (u8 t))

  let bv16 : 'a t -> Bitvector.t =
   fun t -> Bitvector.of_int ~size:16 (Uint16.to_int (u16 t))

  let bv32 : 'a t -> Bitvector.t = fun t -> Bitvector.of_int32 (i32 t)
  let bv64 : 'a t -> Bitvector.t = fun t -> Bitvector.of_int64 (i64 t)
  let uleb128 : 'a t -> Z.t = fun t -> fst (uleb128 t)
  let sleb128 : 'a t -> Z.t = fun t -> fst (sleb128 t)

  let bytes : 'a t -> int -> String.t =
   fun (Cursor { offset; get; pos; content; endianness; _ }) n ->
    match endianness with
    | LittleEndian -> String.init n (fun i -> get content (offset pos i))
    | BigEndian -> String.init n (fun i -> get content (offset pos (n - 1 - i)))

  let read : 'a t -> int -> Bitvector.t =
   fun t n -> Bitvector.create (Z.of_bits (bytes t n)) (8 * n)

  let rec scan_0 : 'a t -> 'a -> int -> int option =
   fun (Cursor { offset; get; pos; content; _ } as t) l i ->
    let idx = offset pos i in
    if idx >= l then None
    else if get content idx = '\x00' then Some i
    else scan_0 t l (i + 1)

  let fixed_string : 'a t -> int -> string =
   fun (Cursor { offset; get; pos; content; _ } as t) len ->
    String.init
      (match scan_0 t (offset pos len) 0 with None -> len | Some len -> len)
      (fun i -> get content (offset pos i))

  let zero_string : string -> 'a t -> ?maxlen:int -> unit -> string =
   fun msg (Cursor { offset; pos; stop; _ } as t) ?maxlen () ->
    let limit =
      match maxlen with None -> stop | Some maxlen -> offset pos maxlen
    in
    let length =
      match scan_0 t limit 0 with
      | None -> raise (Invalid_argument msg)
      | Some length -> length
    in
    fixed_string t length

  let sub : 'a t -> int -> 'a t =
   fun (Cursor { offset; get; content; pos; endianness; _ } as t) len ->
    let stop = offset pos len in
    check_pos t stop;
    create ~offset ~get ~start:pos ~stop ~endianness content
end

module Read : ACCESS = struct
  let apply : 'a t -> int -> ('a t -> 'b) -> 'b =
   fun t n f ->
    let r = f t in
    advance t n;
    r

  let u8 : 'a t -> uint8 = fun t -> apply t 1 Peek.u8
  let u16 : 'a t -> uint16 = fun t -> apply t 2 Peek.u16
  let u32 : 'a t -> uint32 = fun t -> apply t 4 Peek.u32
  let u64 : 'a t -> uint64 = fun t -> apply t 8 Peek.u64
  let i8 : 'a t -> int8 = fun t -> apply t 1 Peek.i8
  let i16 : 'a t -> int16 = fun t -> apply t 2 Peek.i16
  let i32 : 'a t -> int32 = fun t -> apply t 4 Peek.i32
  let i64 : 'a t -> int64 = fun t -> apply t 8 Peek.i64

  let uleb128 : 'a t -> Z.t =
   fun t ->
    let r, n = uleb128 t in
    advance t n;
    r

  let sleb128 : 'a t -> Z.t =
   fun t ->
    let r, n = sleb128 t in
    advance t n;
    r

  let bv8 : 'a t -> Bitvector.t = fun t -> apply t 1 Peek.bv8
  let bv16 : 'a t -> Bitvector.t = fun t -> apply t 2 Peek.bv16
  let bv32 : 'a t -> Bitvector.t = fun t -> apply t 4 Peek.bv32
  let bv64 : 'a t -> Bitvector.t = fun t -> apply t 8 Peek.bv64

  let read : 'a t -> int -> Bitvector.t =
   fun t n ->
    let r = Peek.read t n in
    advance t n;
    r

  let bytes : 'a t -> int -> string =
   fun t n ->
    let r = Peek.bytes t n in
    advance t n;
    r

  let fixed_string : 'a t -> int -> string =
   fun t len ->
    let r = Peek.fixed_string t len in
    advance t len;
    r

  let zero_string : string -> 'a t -> ?maxlen:int -> unit -> string =
   fun msg t ?maxlen () ->
    let r = Peek.zero_string msg t ?maxlen () in
    advance t (String.length r + 1);
    r

  let sub : 'a t -> int -> 'a t =
   fun t len ->
    let r = Peek.sub t len in
    advance t len;
    r
end
