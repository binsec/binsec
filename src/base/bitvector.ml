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

(*
   A bitvector can be:
   - either an ocaml int with the 10 least significand bits encoding the size;
   - or a boxed record with size, signed and unsigned values.
*)
type t
type boxed = { size : int; unsigned : Z.t; signed : Z.t }

external is_unboxed : t -> bool = "%obj_is_int"
external unsafe_to_unboxed : t -> Z.t = "%identity"
external unsafe_to_boxed : t -> boxed = "%identity"
external unsafe_of_unboxed : int -> t = "%identity"
external unsafe_of_boxed : boxed -> t = "%identity"
external is_small_int : Z.t -> bool = "%obj_is_int"
external unsafe_to_int : Z.t -> int = "%identity"

let limits = Array.init 128 (fun i -> Z.shift_left Z.one i)
let masks = Array.map Z.pred limits

let create value size =
  if size <= 0 then invalid_arg "Negative bitvector size";
  if size < Sys.int_size - 10 then
    unsafe_of_unboxed
      ((unsafe_to_int (Z.logand value (Z.pred (Z.shift_left Z.one size))) lsl 10)
      lor size)
  else if
    size < 1024 && is_small_int value
    &&
    let ival = unsafe_to_int value in
    (ival lsl 10) asr 10 = ival
  then unsafe_of_unboxed ((unsafe_to_int value lsl 10) lor size)
  else
    let ulimit, slimit, umask =
      if size < 128 then
        ( Array.unsafe_get limits size,
          Array.unsafe_get limits (size - 1),
          Array.unsafe_get masks size )
      else
        let ulimit = Z.shift_left Z.one size in
        (ulimit, Z.shift_left Z.one (size - 1), Z.pred ulimit)
    in
    let unsigned = Z.logand value umask in
    let signed =
      if Z.geq unsigned slimit then Z.sub unsigned ulimit else unsigned
    in
    if
      size < 1024 && is_small_int signed
      &&
      let ival = unsafe_to_int signed in
      (ival lsl 10) asr 10 = ival
    then unsafe_of_unboxed ((unsafe_to_int signed lsl 10) lor size)
    else unsafe_of_boxed { size; unsigned; signed }

let size_of t =
  if is_unboxed t then unsafe_to_int (unsafe_to_unboxed t) land 0x3ff
  else
    let { size; _ } = unsafe_to_boxed t in
    size

let value_of t =
  if is_unboxed t then
    let size = unsafe_to_int (unsafe_to_unboxed t) land 0x3ff in
    if size < Sys.int_size - 10 then
      Z.of_int (unsafe_to_int (unsafe_to_unboxed t) asr 10)
    else
      let value = unsafe_to_int (unsafe_to_unboxed t) asr 10 in
      if value < 0 then Z.extract (Z.of_int value) 0 size else Z.of_int value
  else
    let { unsigned; _ } = unsafe_to_boxed t in
    unsigned

let signed_of t =
  if is_unboxed t then
    let size = unsafe_to_int (unsafe_to_unboxed t) land 0x3ff in
    if size < Sys.int_size - 10 then
      let value = unsafe_to_int (unsafe_to_unboxed t) asr 10
      and msb = 1 lsl (size - 1) in
      Z.of_int ((value lxor msb) - msb)
    else Z.of_int (unsafe_to_int (unsafe_to_unboxed t) asr 10)
  else
    let { signed; _ } = unsafe_to_boxed t in
    signed

let equal = ( = )
let compare = compare
let hash = Hashtbl.hash

exception Bad_bound of string
exception Operands_size_conflict of string

type boolean = bool

let create_from_tuple (value, size) = create value size
let resize bv size = create (value_of bv) size
let update bv value = create value (size_of bv)
let diff bv1 bv2 = not (equal bv1 bv2)
let zero = create Z.zero 1
let one = create Z.one 1
let zeros size = create Z.zero size
let ones size = create Z.one size

let fill ?lo ?hi size =
  let lo = match lo with None -> 0 | Some l -> l in
  let hi = match hi with None -> size - 1 | Some h -> h in
  if lo < 0 || hi >= size || hi < lo then invalid_arg "Invalid bitvector size";
  create (Z.shift_left (Z.pred (Z.shift_left Z.one (hi - lo + 1))) lo) size

let max_ubv n = fill n

let max_sbv n =
  if n <= 0 then invalid_arg "Invalid bitvector size";
  create (Z.pred (Z.shift_left Z.one (n - 1))) n

let min_sbv n =
  if n <= 0 then invalid_arg "Invalid bitvector size";
  create (Z.shift_left Z.one (n - 1)) n

let is_zero bv = equal bv zero
let is_one bv = equal bv one

let is_zeros bv =
  if is_unboxed bv then unsafe_to_int (unsafe_to_unboxed bv) lsr 10 = 0
  else Z.equal (unsafe_to_boxed bv).unsigned Z.zero

let is_ones bv =
  if is_unboxed bv then unsafe_to_int (unsafe_to_unboxed bv) lsr 10 = 1
  else Z.equal (unsafe_to_boxed bv).unsigned Z.one

let is_fill bv = equal bv (fill (size_of bv))
let is_max_ubv bv = equal bv (max_ubv (size_of bv))
let is_max_sbv bv = equal bv (max_sbv (size_of bv))
let is_min_sbv bv = equal bv (min_sbv (size_of bv))

(* Utils *)

let pp ppf bv =
  Format.fprintf ppf "{%s; %i}" (Z.to_string (value_of bv)) (size_of bv)

let print bv =
  Printf.sprintf "{%s; %i}" (Z.to_string (value_of bv)) (size_of bv)

let binop_error bv1 bv2 msg =
  Printf.sprintf "%s %s %s" msg (print bv1) (print bv2)

let bvint_error bv i msg = Printf.sprintf "%s %s %i" msg (print bv) i

let unsigned_compare (f : Z.t -> Z.t -> bool) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2 then
    raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else f (value_of bv1) (value_of bv2)

let signed_compare (f : Z.t -> Z.t -> bool) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2 then
    raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else f (signed_of bv1) (signed_of bv2)

let unsigned_apply (f : Z.t -> Z.t -> Z.t) bv1 bv2 msg =
  let size = size_of bv1 in
  if size <> size_of bv2 then
    raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else create (f (value_of bv1) (value_of bv2)) size

let signed_apply (f : Z.t -> Z.t -> Z.t) bv1 bv2 msg =
  let size = size_of bv1 in
  if size <> size_of bv2 then
    raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else create (f (signed_of bv1) (signed_of bv2)) size

(* Comparison *)

let ule bv1 bv2 = unsigned_compare Z.leq bv1 bv2 "ule"
let uge bv1 bv2 = unsigned_compare Z.geq bv1 bv2 "uge"
let ult bv1 bv2 = unsigned_compare Z.lt bv1 bv2 "ult"
let ugt bv1 bv2 = unsigned_compare Z.gt bv1 bv2 "ugt"
let sle bv1 bv2 = signed_compare Z.leq bv1 bv2 "sle"
let sge bv1 bv2 = signed_compare Z.geq bv1 bv2 "sge"
let slt bv1 bv2 = signed_compare Z.lt bv1 bv2 "slt"
let sgt bv1 bv2 = signed_compare Z.gt bv1 bv2 "sgt"

(* Arithmetic *)

let succ bv = create (Z.succ (value_of bv)) (size_of bv)
let pred bv = create (Z.pred (value_of bv)) (size_of bv)
let add_int bv i = create (Z.add (Z.of_int i) (value_of bv)) (size_of bv)
let add bv1 bv2 = unsigned_apply Z.add bv1 bv2 "add"
let sub bv1 bv2 = unsigned_apply Z.sub bv1 bv2 "sub"
let mul bv1 bv2 = unsigned_apply Z.mul bv1 bv2 "mul"
let udiv bv1 bv2 = unsigned_apply Z.div bv1 bv2 "udiv"
let umod bv1 bv2 = unsigned_apply Z.rem bv1 bv2 "umod"
let urem bv1 bv2 = unsigned_apply Z.rem bv1 bv2 "urem"

let pow bv1 bv2 =
  unsigned_apply (fun z1 z2 -> Z.pow z1 (Z.to_int z2)) bv1 bv2 "pow"

let umax bv1 bv2 = if uge bv1 bv2 then bv1 else bv2
let umin bv1 bv2 = if ule bv1 bv2 then bv1 else bv2
let sdiv bv1 bv2 = signed_apply Z.div bv1 bv2 "sdiv"
let smod bv1 bv2 = signed_apply Z.rem bv1 bv2 "smod"

let srem bv1 bv2 =
  signed_apply
    (fun b1 b2 ->
      if Z.lt b1 Z.zero then Z.sub (Z.rem b1 b2) b2 else Z.rem b1 b2)
    bv1 bv2 "srem"

let neg bv = update bv (Z.neg (signed_of bv))
let smax bv1 bv2 = if sge bv1 bv2 then bv1 else bv2
let smin bv1 bv2 = if sle bv1 bv2 then bv1 else bv2
let is_neg bv = Z.lt (signed_of bv) Z.zero
let is_pos bv = Z.gt (signed_of bv) Z.zero

(* Logical *)

let logand bv1 bv2 = unsigned_apply Z.logand bv1 bv2 "logand"
let logor bv1 bv2 = unsigned_apply Z.logor bv1 bv2 "logor"
let logxor bv1 bv2 = unsigned_apply Z.logxor bv1 bv2 "logxor"
let lognot bv = update bv (Z.lognot (value_of bv))
let shift_left bv i = update bv (Z.shift_left (value_of bv) i)
let shift_right bv i = update bv (Z.shift_right (value_of bv) i)
let shift_right_signed bv i = update bv (Z.shift_right (signed_of bv) i)

let rotate_left bv i =
  let i = i mod size_of bv and value = value_of bv in
  update bv
    (Z.logor (Z.shift_left value i) (Z.shift_right value (size_of bv - i)))

let rotate_right bv i =
  let i = i mod size_of bv and value = value_of bv in
  update bv
    (Z.logor (Z.shift_right value i) (Z.shift_left value (size_of bv - i)))

let reduce bv i =
  if size_of bv < i then raise (Bad_bound (bvint_error bv i "reduce"))
  else resize bv i

let extend bv i =
  if size_of bv > i then raise (Bad_bound (bvint_error bv i "extend"))
  else resize bv i

let extend_signed bv i =
  if size_of bv > i then raise (Bad_bound (bvint_error bv i "extend_signed"))
  else create (signed_of bv) i

let extend_unsafe bv i = resize bv i
let bit_mask i = Z.shift_left Z.one i
let bit_mask_not i = Z.logxor Z.minus_one (bit_mask i)
let num_bits bv = Z.numbits (value_of bv)
let get_bit bv i = Z.testbit (value_of bv) i
let set_bit bv i = update bv (Z.logor (bit_mask i) (value_of bv))
let clear_bit bv i = update bv (Z.logand (bit_mask_not i) (value_of bv))
let flip_bit bv i = if get_bit bv i then clear_bit bv i else set_bit bv i

let append bv1 bv2 =
  create
    (Z.logor (Z.shift_left (value_of bv1) (size_of bv2)) (value_of bv2))
    (size_of bv1 + size_of bv2)

let concat = function
  | [] -> failwith "concat"
  | bv :: lst -> List.fold_left append bv lst

let extract bv { Basic_types.lo; Basic_types.hi } =
  if lo < 0 || hi >= size_of bv || hi < lo then
    let msg = Printf.sprintf "restrict %s [%i..%i]" (print bv) lo hi in
    raise (Bad_bound msg)
  else
    let size = hi - lo + 1 in
    create (Z.extract (value_of bv) lo size) size

(* Conversion *)

let to_hexstring bv : string =
  let size = ((size_of bv + 3) / 4) + 2 in
  let value = value_of bv in
  let init_fun = function
    | 0 -> '0'
    | 1 -> 'x'
    | n ->
        let offset = (size - n - 1) * 4 in
        let digit = unsafe_to_int (Z.extract value offset 4) in
        let shift = if digit < 10 then 0x30 else 0x57 in
        digit + shift |> char_of_int
  in
  String.init size init_fun

let to_bitstring bv : string =
  let size = size_of bv + 2 in
  let value = value_of bv in
  let init_fun = function
    | 0 -> '0'
    | 1 -> 'b'
    | n ->
        let offset = size - n - 1 in
        let digit = unsafe_to_int (Z.extract value offset 1) in
        digit + 0x30 |> char_of_int
  in
  String.init size init_fun

let to_asciistring bv : string =
  let n = (size_of bv + 7) / 8 in
  let v = Z.to_bits (value_of bv) in
  let s = Bytes.make n '\x00' in
  Bytes.blit_string v 0 s 0 (min n (String.length v));
  Bytes.unsafe_to_string s

let to_string bv =
  if size_of bv mod 4 == 0 then to_hexstring bv else to_bitstring bv

let of_bits str = create (Z.of_bits str) (8 * String.length str)

let of_string str =
  let len = String.length str in
  if len < 3 then failwith "Bitvector.of_string : too short string"
  else
    let size =
      match (str.[0], str.[1], str.[2]) with
      | '0', 'x', _ -> (len - 2) * 4
      | '0', 'b', _ -> len - 2
      | '+', '0', 'x' | '-', '0', 'x' -> (len - 3) * 4
      | '+', '0', 'b' | '-', '0', 'b' -> len - 3
      | _ -> failwith "Bitvector.of_string : should start with [+-]?0[xb]"
    in
    try create (Z.of_string str) size
    with Failure _ -> raise (Invalid_argument ("of_string : " ^ str))

let of_hexstring = of_string
let of_bool b = if b then one else zero

let to_bool x =
  if size_of x <> 1 then raise Z.Overflow else Z.equal (value_of x) Z.one

let of_char c = create (Z.of_int (Char.code c)) 8

let to_char x =
  if size_of x > 8 then raise Z.Overflow
  else Char.unsafe_chr (Z.to_int (value_of x))

let of_int32 i32 = create (Z.of_int32 i32) 32
let to_int32 bv = Z.to_int32 (signed_of bv)
let of_int64 i64 = create (Z.of_int64 i64) 64
let to_int64 bv = Z.to_int64 (signed_of bv)
let of_int ~size i = create (Z.of_int i) size
let to_int bv = Z.to_int (signed_of bv)
let to_uint bv = Z.to_int (value_of bv)
let pp_hex ppf bv = Format.fprintf ppf "{%s; %i}" (to_hexstring bv) (size_of bv)

(* Should this replace pp_hex? *)
let pp_hex_or_bin ppf bv = Format.fprintf ppf "%s" @@ to_string bv

module Random = struct
  let bits sz = sz |> create @@ Z.of_int @@ Random.bits ()

  let rec unroll sz bv =
    if sz > 30 then bits 30 |> append bv |> unroll @@ (sz - 30)
    else bits sz |> append bv

  let rand = function
    | sz when sz < 1 -> assert false
    | 1 when Random.bool () -> one
    | 1 -> zero
    | sz when sz <= 30 -> bits sz
    | sz -> bits 30 |> unroll @@ (sz - 30)
end

let rand = Random.rand

module type Common = sig
  type t
  type boolean = bool

  val create : Z.t -> int -> t
  val create_from_tuple : Z.t * int -> t

  (*
  val resize: t -> int -> t
  val update: t -> Bigint.t -> t
*)
  val value_of : t -> Z.t
  val signed_of : t -> Z.t
  val size_of : t -> int
  val compare : t -> t -> int
  val hash : t -> int
  val zero : t
  val one : t
  val zeros : int -> t
  val ones : int -> t
  val fill : ?lo:int -> ?hi:int -> int -> t
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_zeros : t -> bool
  val is_ones : t -> bool
  val is_fill : t -> bool
  val max_ubv : int -> t
  val max_sbv : int -> t
  val min_sbv : int -> t
  val is_max_ubv : t -> bool
  val is_max_sbv : t -> bool
  val is_min_sbv : t -> bool
  val equal : t -> t -> boolean
  val diff : t -> t -> boolean
  val ule : t -> t -> boolean
  val uge : t -> t -> boolean
  val ult : t -> t -> boolean
  val ugt : t -> t -> boolean
  val sle : t -> t -> boolean
  val sge : t -> t -> boolean
  val slt : t -> t -> boolean
  val sgt : t -> t -> boolean

  include Sigs.ARITHMETIC with type t := t

  val pow : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val add_int : t -> int -> t
  val umax : t -> t -> t
  val umin : t -> t -> t
  val smax : t -> t -> t
  val smin : t -> t -> t
  val is_neg : t -> bool
  val is_pos : t -> bool

  (* land, lor, lxor and lnot are keywords... *)
  include Sigs.BITWISE with type t := t

  val reduce : t -> int -> t
  val extend : t -> int -> t
  val extend_signed : t -> int -> t
  val extend_unsafe : t -> int -> t
  val num_bits : t -> int
  val get_bit : t -> int -> bool
  val set_bit : t -> int -> t
  val clear_bit : t -> int -> t
  val flip_bit : t -> int -> t
  val append : t -> t -> t
  val concat : t list -> t
  val extract : t -> int Basic_types.interval -> t
end

module Collection = Basic_types.Collection_make.Default (struct
  type nonrec t = t

  let compare = compare
end)
