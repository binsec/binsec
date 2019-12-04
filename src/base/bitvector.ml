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

exception Bad_bound of string
exception Operands_size_conflict of string


module Internal :
sig
  type internal = private
    { value: Bigint.t;
      size: int;
      ulimit: Bigint.t;
      slimit: Bigint.t }

  val create : Bigint.t -> int -> internal
end =
struct
  type internal =
    { value: Bigint.t;
      size: int;
      ulimit: Bigint.t;
      slimit: Bigint.t }

  let create value size =
    if size <= 0 then invalid_arg "Negative bitvector size";
    let ulimit = Bigint.shift_left_big_int Bigint.unit_big_int size in
    let slimit = Bigint.div_big_int ulimit (Bigint.big_int_of_int 2) in
    let value  = Bigint.mod_big_int value ulimit in
    { value; size; ulimit; slimit }

end

open Internal
type t = internal
type boolean = bool

let compare t1 t2 =
  let cmp = compare t1.size t2.size in
  if cmp = 0
  then Bigint.compare_big_int t1.value t2.value
  else cmp

let create value size = create value size
let create_from_tuple (value, size) = create value size

let resize bv size  = create bv.value size
let update bv value = create value bv.size

let value_of bv = bv.value

let signed_of bv =
  if Bigint.lt_big_int bv.value bv.slimit then bv.value
  else Bigint.sub_big_int bv.value bv.ulimit

let size_of bv = bv.size

let equal bv1 bv2 =
  size_of bv1 = size_of bv2 &&
  Bigint.eq_big_int (value_of bv1) (value_of bv2)

let diff bv1 bv2 = not (equal bv1 bv2)

let minus_unit_big_int = Bigint.minus_big_int Bigint.unit_big_int

let zero = create Bigint.zero_big_int 1
let one  = create Bigint.unit_big_int 1

let zeros size = create Bigint.zero_big_int size
let ones  size = create Bigint.unit_big_int size

let fill ?lo ?hi size =
  let lo = match lo with None -> 0 | Some l -> l in
  let hi = match hi with None -> size - 1 | Some h -> h in
  if lo < 0 || hi >= size || hi < lo then invalid_arg "Invalid bitvector size";
  create
    (Bigint.shift_left_big_int
       (Bigint.sub_big_int
          (Bigint.shift_left_big_int Bigint.unit_big_int (hi - lo + 1))
          Bigint.unit_big_int)
       lo)
    size

let max_ubv n = fill n

let max_sbv n = fill ~hi:(n - 2) n

let min_sbv n =
  if n <= 0 then invalid_arg "Invalid bitvector size";
  create (Bigint.shift_left_big_int Bigint.unit_big_int (n-1)) n

let is_zero bv = equal bv zero
let is_one  bv = equal bv one

let is_zeros bv = equal bv (zeros (size_of bv))
let is_ones  bv = equal bv (ones  (size_of bv))
let is_fill  bv = equal bv (fill  (size_of bv))

let is_max_ubv bv = equal bv (max_ubv (size_of bv))
let is_max_sbv bv = equal bv (max_sbv (size_of bv))
let is_min_sbv bv = equal bv (min_sbv (size_of bv))


(* Utils *)

let pp ppf bv =
  Format.fprintf ppf "{%s; %i}" (Bigint.string_of_big_int bv.value) bv.size

let print bv =
  Printf.sprintf "{%s; %i}" (Bigint.string_of_big_int bv.value) bv.size

let binop_error bv1 bv2 msg =
  Printf.sprintf "%s %s %s" msg (print bv1) (print bv2)

let bvint_error bv i msg =
  Printf.sprintf "%s %s %i" msg (print bv) i

let unsigned_compare (f: Bigint.t -> Bigint.t -> bool) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else f (value_of bv1) (value_of bv2)

let signed_compare (f: Bigint.t -> Bigint.t -> bool) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else f (signed_of bv1) (signed_of bv2)

let unsigned_apply (f: Bigint.t -> Bigint.t -> Bigint.t) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else update bv1 (f (value_of bv1) (value_of bv2))

let signed_apply (f: Bigint.t -> Bigint.t -> Bigint.t) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then raise (Operands_size_conflict (binop_error bv1 bv2 msg))
  else update bv1 (f (signed_of bv1) (signed_of bv2))


(* Comparison *)

let ule bv1 bv2 = unsigned_compare Bigint.le_big_int bv1 bv2 "ule"
let uge bv1 bv2 = unsigned_compare Bigint.ge_big_int bv1 bv2 "uge"
let ult bv1 bv2 = unsigned_compare Bigint.lt_big_int bv1 bv2 "ult"
let ugt bv1 bv2 = unsigned_compare Bigint.gt_big_int bv1 bv2 "ugt"

let sle bv1 bv2 = signed_compare Bigint.le_big_int bv1 bv2 "sle"
let sge bv1 bv2 = signed_compare Bigint.ge_big_int bv1 bv2 "sge"
let slt bv1 bv2 = signed_compare Bigint.lt_big_int bv1 bv2 "slt"
let sgt bv1 bv2 = signed_compare Bigint.gt_big_int bv1 bv2 "sgt"


(* Arithmetic *)

let succ bv = create (Bigint.succ_big_int (value_of bv)) (size_of bv)
let pred bv = create (Bigint.pred_big_int (value_of bv)) (size_of bv)

let add bv1 bv2 = unsigned_apply Bigint.add_big_int bv1 bv2 "add"
let sub bv1 bv2 = unsigned_apply Bigint.sub_big_int bv1 bv2 "sub"
let mul bv1 bv2 = unsigned_apply Bigint.mult_big_int bv1 bv2 "mul"

let udiv bv1 bv2 = unsigned_apply Bigint.div_big_int bv1 bv2 "udiv"
let umod bv1 bv2 = unsigned_apply Bigint.mod_big_int bv1 bv2 "umod"
let urem bv1 bv2 = unsigned_apply Bigint.mod_big_int bv1 bv2 "urem"

let pow bv1 bv2 = unsigned_apply Bigint.power_big_int_positive_big_int bv1 bv2 "pow"

let umax bv1 bv2 = if uge bv1 bv2 then bv1 else bv2
let umin bv1 bv2 = if ule bv1 bv2 then bv1 else bv2

let sdiv bv1 bv2 = signed_apply Bigint.div_big_int bv1 bv2 "sdiv"
let smod bv1 bv2 = signed_apply Bigint.mod_big_int bv1 bv2 "smod"
let srem bv1 bv2 = signed_apply
    (fun b1 b2 ->
       if Bigint.lt_big_int b1 Bigint.zero_big_int
       then Bigint.sub_big_int (Bigint.mod_big_int b1 b2) b2
       else Bigint.mod_big_int b1 b2)
    bv1 bv2 "srem"

let neg bv = update bv (Bigint.minus_big_int (signed_of bv))

let smax bv1 bv2 = if sge bv1 bv2 then bv1 else bv2
let smin bv1 bv2 = if sle bv1 bv2 then bv1 else bv2

let is_neg bv = Bigint.lt_big_int (signed_of bv) Bigint.zero_big_int
let is_pos bv = Bigint.gt_big_int (signed_of bv) Bigint.zero_big_int


(* Logical *)

let logand bv1 bv2 = unsigned_apply Bigint.and_big_int bv1 bv2 "logand"
let logor  bv1 bv2 = unsigned_apply Bigint.or_big_int  bv1 bv2 "logor"
let logxor bv1 bv2 = unsigned_apply Bigint.xor_big_int bv1 bv2 "logxor"
let lognot bv =
  update bv
    (Bigint.xor_big_int
       minus_unit_big_int
       (value_of bv))

let shift_left  bv i = update bv (Bigint.shift_left_big_int  (value_of bv) i)
let shift_right bv i = update bv (Bigint.shift_right_big_int (value_of bv) i)
let shift_right_signed bv i = update bv (Bigint.shift_right_towards_zero_big_int (signed_of bv) i)

let rotate_left bv i =
  update bv
    (Bigint.or_big_int
       (Bigint.shift_left_big_int  (value_of bv) i)
       (Bigint.shift_right_big_int (value_of bv) (size_of bv - i)))

let rotate_right bv i =
  update bv
    (Bigint.or_big_int
       (Bigint.shift_right_big_int (value_of bv) i)
       (Bigint.shift_left_big_int  (value_of bv) (size_of bv - i)))

let reduce bv i =
  if size_of bv < i
  then raise (Bad_bound (bvint_error bv i "reduce"))
  else resize bv i

let extend bv i =
  if size_of bv > i
  then raise (Bad_bound (bvint_error bv i "extend"))
  else resize bv i

let extend_signed bv i =
  if size_of bv > i
  then raise (Bad_bound (bvint_error bv i "extend_signed"))
  else create (signed_of bv) i

let extend_unsafe bv i = resize bv i

let bit_mask i = Bigint.shift_left_big_int Bigint.unit_big_int i
let bit_mask_not i = Bigint.xor_big_int minus_unit_big_int (bit_mask i)

let num_bits bv = Bigint.num_bits (value_of bv)

let get_bit bv i =
  Bigint.gt_big_int
    (Bigint.and_big_int (bit_mask i) (value_of bv))
    Bigint.zero_big_int

let set_bit bv i =
  update bv
    (Bigint.or_big_int
       (bit_mask i)
       (value_of bv))

let clear_bit bv i =
  update bv
    (Bigint.and_big_int
       (bit_mask_not i)
       (value_of bv))

let flip_bit bv i = if get_bit bv i then clear_bit bv i else set_bit bv i

let append bv1 bv2 =
  create
    (Bigint.add_big_int
       (Bigint.shift_left_big_int (value_of bv1) (size_of bv2))
       (value_of bv2))
    (size_of bv1 + size_of bv2)

let concat = function
  | [] -> failwith "concat"
  | bv :: lst -> List.fold_left append bv lst

let extract bv {Basic_types.lo; Basic_types.hi} =
  if (lo < 0) || (hi >= size_of bv) || (hi < lo)
  then
    let msg = Printf.sprintf "restrict %s [%i..%i]" (print bv) lo hi in
    raise (Bad_bound msg)
  else
    let size = hi - lo + 1 in
    create (Bigint.extract_big_int (value_of bv) lo size) size


(* Conversion *)

let to_hexstring bv : string =
  let size = (bv.size + 3) / 4 + 2 in
  let init_fun = function
    | 0 -> '0'
    | 1 -> 'x'
    | n ->
      let offset = (size - n - 1) * 4 in
      let digit = Bigint.extract_big_int bv.value offset 4
                  |> Bigint.int_of_big_int in
      let shift = if digit < 10 then 0x30 else 0x57 in
      digit + shift |> char_of_int
  in
  String.init size init_fun

let to_bitstring bv : string =
  let size = bv.size + 2 in
  let init_fun = function
    | 0 -> '0'
    | 1 -> 'b'
    | n ->
      let offset = size - n - 1 in
      let digit = Bigint.extract_big_int bv.value offset 1
                  |> Bigint.int_of_big_int in
      digit + 0x30 |> char_of_int
  in
  String.init size init_fun

let to_string bv =
  if bv.size mod 4 == 0
  then to_hexstring bv
  else to_bitstring bv
;;


let of_string str =
  let len = String.length str in
  if len < 3 then failwith "Bitvector.of_string : too short string" else
    let size =
      match str.[0], str.[1], str.[2] with
      | '0', 'x', _ -> (len - 2) * 4
      | '0', 'b', _ -> len - 2
      | '+', '0', 'x'
      | '-', '0', 'x' -> (len - 3) * 4
      | '+', '0', 'b'
      | '-', '0', 'b' -> len - 3
      | _ -> failwith "Bitvector.of_string : should start with [+-]?0[xb]"
    in
    try
      create (Bigint.big_int_of_string str) size
    with Failure _ -> raise (Invalid_argument ("of_string : " ^ str))

let of_hexstring = of_string

let of_bool b = if b then one else zero
let to_bool bv = not (is_zero bv)

let of_int32 i32 = create (Bigint.big_int_of_int32 i32) 32
let to_int32 bv  = Bigint.int32_of_big_int (signed_of bv)

let of_int64 i64 = create (Bigint.big_int_of_int64 i64) 64
let to_int64 bv  = Bigint.int64_of_big_int (signed_of bv)

let of_int ~size i = create (Bigint.big_int_of_int i) size
let to_int bv = Bigint.int_of_big_int (signed_of bv)

let pp_hex ppf bv =
  Format.fprintf ppf "{%s; %i}" (to_hexstring bv) bv.size
;;

(* Should this replace pp_hex? *)
let pp_hex_or_bin ppf bv =
  Format.fprintf ppf "%s" @@ to_string bv
;;


module Random = struct

  let bits sz = sz |> create @@ Bigint.big_int_of_int @@ Random.bits ()

  let rec unroll sz bv =
    if sz > 30 then bits 30 |> append bv |> unroll @@ sz - 30
    else bits sz |> append bv

  let rand = function
    | sz when sz < 1 -> assert false
    | 1 when Random.bool () -> one
    | 1 -> zero
    | sz when sz <= 30 -> bits sz
    | sz -> bits 30 |> unroll @@ sz - 30
end

let rand = Random.rand

module type Common =
sig
  type t
  type boolean = bool

  val create: Bigint.t -> int -> t
  val create_from_tuple: Bigint.t * int -> t
(*
  val resize: t -> int -> t
  val update: t -> Bigint.t -> t
*)
  val value_of  : t -> Bigint.t
  val signed_of : t -> Bigint.t
  val size_of : t -> int

  val compare : t -> t -> int

  val zero : t
  val one  : t

  val zeros : int -> t
  val ones  : int -> t
  val fill : ?lo:int -> ?hi:int -> int -> t

  val is_zero : t -> bool
  val is_one  : t -> bool

  val is_zeros : t -> bool
  val is_ones  : t -> bool
  val is_fill  : t -> bool

  val max_ubv : int -> t
  val max_sbv : int -> t
  val min_sbv : int -> t

  val is_max_ubv : t -> bool
  val is_max_sbv : t -> bool
  val is_min_sbv : t -> bool

  val equal : t -> t -> boolean
  val diff  : t -> t -> boolean

  val ule : t -> t -> boolean
  val uge : t -> t -> boolean
  val ult : t -> t -> boolean
  val ugt : t -> t -> boolean

  val sle : t -> t -> boolean
  val sge : t -> t -> boolean
  val slt : t -> t -> boolean
  val sgt : t -> t -> boolean

  include Sigs.Arithmetic with type t := t

  val succ : t -> t
  val pred : t -> t

  val umax : t -> t -> t
  val umin : t -> t -> t
  val smax : t -> t -> t
  val smin : t -> t -> t

  val is_neg : t -> bool
  val is_pos : t -> bool

  (* land, lor, lxor and lnot are keywords... *)
  include Sigs.Bitwise with type t := t

  val reduce : t -> int -> t
  val extend : t -> int -> t
  val extend_signed : t -> int -> t
  val extend_unsafe : t -> int -> t

  val num_bits  : t -> int
  val get_bit   : t -> int -> bool
  val set_bit   : t -> int -> t
  val clear_bit : t -> int -> t
  val flip_bit  : t -> int -> t

  val append  : t -> t -> t
  val concat  : t list -> t
  val extract : t -> int Basic_types.interval -> t
end


module Collection =
  Basic_types.Collection_make.Default(
     struct
       type t = internal
       let compare = compare
     end)
