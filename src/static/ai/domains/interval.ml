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

let trans_closure p a b c d =  p (p a b) (p c d)

let int_of_bv bv = Bigint.int_of_big_int (Bitvector.value_of bv)

module type S = sig
  type t = private { lo : Bitvector.t; hi : Bitvector.t; }

  val is_disjoint : t -> t -> bool
  val is_point : t -> bool
  val set_lo : Bitvector.t -> t -> t
  val set_hi : Bitvector.t -> t -> t
  val pp : Format.formatter -> t -> unit
  val create : Bitvector.t -> Bitvector.t -> t
  val of_point : Bitvector.t -> t
  val is_unit : t -> bool
  val equal : t -> t -> bool
  val bool_true : t
  val bool_false : t
  val bool_any : t

  val top : int -> t
  val topify : t -> t
  val maxify : t -> t
  val minify : t -> t

  val low : t -> t option
  val high : t -> t option

  val meet : t -> t -> t option
  val join : t -> t -> t

  val contains : t -> t -> bool
  val contains_zero : t -> bool
  val logand : t -> t -> t
  val logor  : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val shift_left  : t -> t -> t
  val shift_right : t -> t -> t
  val shift_right_signed : t -> t -> t

  val rotate_left  : t -> t -> t
  val rotate_right : t -> t -> t

  include Sigs.Arithmetic with type t := t

  val neg : t -> t

  val eq : t -> t -> t
  val diff  : t -> t -> t

  val ule : t -> t -> t
  val uge : t -> t -> t
  val ult : t -> t -> t
  val ugt : t -> t -> t

  val sle : t -> t -> t
  val sge : t -> t -> t
  val slt : t -> t -> t
  val sgt : t -> t -> t
  val extend_signed : t -> int -> t
  val extend : t -> int -> t
end


module Common = struct
  type t = {
    lo : Bitvector.t;
    hi : Bitvector.t;
  }

  let is_disjoint lt i1 i2 = lt i1.hi i2.lo || lt i2.hi i1.lo
  let is_point i = Bitvector.equal i.lo i.hi

  (* Only optimization is to print a single point whenever the interval is of
   * length one *)
  let pp ppf i =
    if is_point i then Format.fprintf ppf "%a" Bitvector.pp_hex i.lo
    else Format.fprintf ppf "[%a, %a]" Bitvector.pp_hex i.lo Bitvector.pp_hex i.hi

  let is_unit i = is_point i && Bitvector.is_one i.lo

  let equal i1 i2 = Bitvector.(equal i1.lo i2.lo && equal i1.hi i2.hi)

  let apply_on_bounds f i1 i2 =
    f i1.lo i2.lo,
    f i1.lo i2.hi,
    f i1.hi i2.lo,
    f i1.hi i2.hi

  let min_max_bounds min max f i1 i2 =
    let l_l, l_u, u_l, u_u = apply_on_bounds f i1 i2 in
    let l = trans_closure min l_l l_u u_l u_u in
    let u = trans_closure max l_l l_u u_l u_u in
    l, u

  let max_size = Bigint.big_int_of_int 100

  let pow _ _ = assert false
  let srem _ _ = assert false
  let urem _ _ = assert false
  let rotate_left _ _ = assert false
end


module Unsigned = struct
  open Bitvector
  include Common

  let create lo hi =
    assert (ule lo hi);
    {lo; hi}

  let of_point bv = create bv bv
  let bool_true = of_point Bitvector.one
  let bool_false = of_point Bitvector.zero


  let min_max_bounds = min_max_bounds Bitvector.umin Bitvector.umax
  let is_disjoint = is_disjoint Bitvector.ult

  let top size = create (Bitvector.zeros size) (Bitvector.max_ubv size)
  let topify i = top (size_of i.lo)
  let maxify i = { i with hi = Bitvector.max_ubv (size_of i.hi) }
  let minify i = { i with lo = Bitvector.zeros (size_of i.hi) }

  let set_lo v i = create v i.hi
  let set_hi v i = create i.lo v
  
  let low i =
    let len = size_of i.lo in
    let bv = Bitvector.succ (Bitvector.max_sbv len) in
    if ult i.hi bv then Some i
    else if ult i.lo bv then Some (set_hi (Bitvector.max_sbv len) i)
    else None

  let high i =
    let len = size_of i.lo in
    let bv = Bitvector.succ (Bitvector.max_sbv len) in
    if uge i.lo bv then Some i
    else if uge i.hi bv then Some (set_lo bv i)
    else None

  let cardinal i =
    Bigint.succ_big_int
      (Bigint.sub_big_int (Bitvector.value_of i.hi) (Bitvector.value_of i.lo))

  let map f i =
    if cardinal i > max_size then None
    else
      let rec loop e acc =
        if Bitvector.equal e i.hi then f i.hi :: acc
        else loop (Bitvector.succ e) (f e :: acc)
      in Some (loop i.lo [])

  let bool_any = create Bitvector.zero Bitvector.one

  let meet i1 i2 =
    assert (Bitvector.size_of i1.lo = Bitvector.size_of i2.lo);
    let lo = Bitvector.umax i1.lo i2.lo
    and hi = Bitvector.umin i1.hi i2.hi in
    if Bitvector.ule lo hi then Some (create lo hi) else None

  let join i1 i2 =
    assert (Bitvector.size_of i1.lo = Bitvector.size_of i2.lo);
    let lo = Bitvector.umin i1.lo i2.lo
    and hi = Bitvector.umax i1.hi i2.hi  in
    create lo hi

  let contains i1 i2 = ule i1.lo i2.lo && uge i1.hi i2.hi
  let contains_zero i = Bitvector.equal i.lo (zeros (size_of i.lo))
  let neg i =
    let lo = neg i.lo and hi = neg i.hi in
    if ule lo hi then create lo hi
    else top (size_of i.lo)

  let lognot i =
    let lo = lognot i.hi and hi = lognot i.lo in
    if ule lo hi then create lo hi
    else top (size_of i.lo)

  let overflow_add a b =
    let sz = Bitvector.size_of a in
    let a = Bitvector.extend a (sz + 1) in
    let b = Bitvector.extend b (sz + 1) in
    let res = Bitvector.add a b in
    let cf = Bitvector.extract res sz sz in
    not (Bitvector.is_zero cf)

  let add i1 i2 =
    if overflow_add i1.lo i2.lo || overflow_add i2.hi i1.hi
    then top (size_of i1.lo)
    else
      let lo = add i1.lo i2.lo and hi = add i1.hi i2.hi in
      create lo hi

  let overflow_sub = Bitvector.ult

  let sub i1 i2 =
    if overflow_sub i1.lo i2.hi || overflow_sub i1.hi i2.lo
    then top (size_of i1.lo)
    else
      let lo = sub i1.lo i2.hi and hi = sub i1.hi i2.lo in
      create lo hi

  let mul i1 i2 =
    let l_l = Bitvector.umul i1.lo i2.lo in
    let l_u = Bitvector.umul i1.lo i2.hi in
    let u_l = Bitvector.umul i1.hi i2.lo in
    let u_u = Bitvector.umul i1.hi i2.hi in
    let sz = Bitvector.size_of i1.lo in
    let sz2 =  2 * sz in
    let ex_l_l = umul (extend i1.lo sz2) (extend i2.lo sz2) in
    let ex_l_u = umul (extend i1.lo sz2) (extend i2.hi sz2) in
    let ex_u_l = umul (extend i1.hi sz2) (extend i2.lo sz2) in
    let ex_u_u = umul (extend i1.hi sz2) (extend i2.hi sz2) in
    if Bitvector.equal ex_l_l (Bitvector.extend l_l sz2) &&
       Bitvector.equal ex_l_u (Bitvector.extend l_u sz2) &&
       Bitvector.equal ex_u_l (Bitvector.extend u_l sz2) &&
       Bitvector.equal ex_u_u (Bitvector.extend u_u sz2)
    then
      let l = trans_closure Bitvector.umin l_u l_l u_l u_u in
      let u = trans_closure Bitvector.umax l_l l_u u_l u_u in
      create l u
    else top sz

  let umul = mul
  let smul = mul

  let udiv i1 i2 =
    if contains_zero i2 then raise Errors.Div_by_zero
    else
      let l, u = min_max_bounds Bitvector.udiv i1 i2 in
      (assert (Bitvector.ule l u); create l u)

  let sdiv i _ = top (size_of i.lo)

  let umod i1 i2 =
    if contains_zero i2 then failwith "Unsigned modU ..."
    else
      let l, u = min_max_bounds Bitvector.sdiv i1 i2 in
      (assert (Bitvector.ule l u); create l u)

  let smod _ = topify

  let logor i1 i2 =
    let lo = Bitvector.umax i1.lo i2.lo
    and hi = Bitvector.max_ubv (size_of i1.lo) in
    create lo hi

  let logand i1 i2 =
    let lo = zeros (size_of i1.lo)
    and hi = umin i1.hi i2.hi in
    create lo hi

  let logxor i1 i2 =
    let sz = size_of i1.lo in
    let hi = umin (Bitvector.add i1.hi i2.hi) (max_ubv sz) in
    let lo = zeros sz in
    create lo hi

  let shift_left i1 i2 =
    let lo = shift_left i1.lo (int_of_bv i2.lo)
    and hi = shift_left i1.hi (int_of_bv i2.hi) in
    create lo hi

  let shift_right i1 i2 =
    let lo = shift_right i1.lo (int_of_bv i2.lo)
    and hi = shift_right i1.hi (int_of_bv i2.hi) in
    create lo hi

  let shift_right_signed i1 i2 =
    (* ??? *)
    if is_point i2
    then
      let lo = shift_right_signed i1.lo (int_of_bv i2.lo)
      and hi = shift_right_signed i1.hi (int_of_bv i2.hi) in
      create lo hi
    else topify i2

  let rotate_left _ _ = assert false
  let rotate_right i1 i2 =
    if is_point i2 then
      let lo = rotate_right i1.lo (int_of_bv i2.lo)
      and hi = rotate_right i1.hi (int_of_bv i2.hi) in
      create lo hi
    else topify i2

  let eq i1 i2 =
    if is_point i1 && equal i1 i2 then bool_true
    else if is_disjoint i1 i2 then bool_false
    else bool_any

  let diff i1 i2 =
    if is_disjoint i1 i2 || equal i1 i2 then bool_true
    else bool_any

  let uge i1 i2 =
    if Bitvector.uge i1.lo i2.hi then bool_true
    else if Bitvector.ult i1.hi i2.lo then bool_false
    else bool_any

  let ult i1 i2 =
    if ult i1.hi i2.lo then bool_true
    else if ule i2.lo i1.lo && ule i2.hi i1.hi then bool_false
    else bool_any

  let sge _ _ = bool_any
  let sgt _ _ = bool_any
  let sle _ _ = bool_any
  let slt _ _ = bool_any
  let ugt _ _ = assert false
  let ule _ _ = assert false


  let gen_extend fext i n =
    let lo = fext i.lo n and hi = fext i.hi n in create lo hi

  let extend = gen_extend Bitvector.extend
  let extend_signed = gen_extend Bitvector.extend_signed

end


module Signed = struct
  open Bitvector
  include Common

  let min_max_bounds = min_max_bounds Bitvector.smin Bitvector.smax
  let is_disjoint = is_disjoint Bitvector.slt

  let create lo hi =
    if Bitvector.sle lo hi |> not then (
      Logger.debug "BOOM %a %a" Bitvector.pp_hex lo Bitvector.pp_hex hi;
      assert false
    );
    (*   assert (Bitvector.sle lo hi);  *)
    {lo; hi}

  let set_lo v i = create v i.hi
  let set_hi v i = create i.lo v

  let of_point bv = create bv bv
  let bool_true = of_point Bitvector.one
  let bool_false = of_point Bitvector.zero

  let smin size = Bitvector.succ (Bitvector.max_sbv size)

  let top size = create (smin size) (Bitvector.max_sbv size)
  let topify i = top (size_of i.lo)
  let maxify i = create i.lo (Bitvector.max_sbv (size_of i.hi))
  let minify i = create (smin (size_of i.hi)) i.hi
  let bool_any = Logger.debug "ANY SBOOL"; create Bitvector.one Bitvector.zero

  let low i =
    let bv = Bitvector.zeros (Bitvector.size_of i.lo) in
    if sge i.lo bv then Some i
    else if sge i.hi bv then Some (set_lo bv i)
    else None

  let high i =
    let len = size_of i.lo in
    let bv = Bitvector.zeros len in
    if slt i.hi bv then Some i
    else if slt i.lo bv then Some (set_hi (max_ubv len) i)
    else None


  let meet i1 i2 =
    let lo = Bitvector.smax i1.lo i2.lo
    and hi = Bitvector.smin i1.hi i2.hi in
    if Bitvector.sle lo hi
    then Some (create lo hi)
    else None

  let join i1 i2 =
    assert (Bitvector.size_of i1.lo = size_of i2.lo);
    let lo = Bitvector.smin i1.lo i2.lo
    and hi = Bitvector.smax i1.hi i2.hi in
    create lo hi

  let contains i1 i2 = sle i1.lo i2.lo && sge i1.hi i2.hi
  let contains_zero i = contains i (of_point (Bitvector.zeros (size_of i.lo)))

  let neg i =
    let lo = neg i.lo and hi = neg i.hi in
    if sle lo hi then create lo hi
    else
      let len = size_of i.lo in
      create (smin len) (max_sbv len)

  let lognot i = top (size_of i.lo)

  let overflow_add a b res =
    let sz = Bitvector.size_of a in
    let a = Bitvector.extend a sz in
    let b = Bitvector.extend b sz in
    let s_flag1 = Bitvector.extract a (sz - 1) (sz - 1) in
    let s_flag2 = Bitvector.extract b (sz - 1) (sz - 1) in
    let s_flag3 = Bitvector.extract res (sz - 1) (sz - 1) in
    (Bitvector.equal s_flag1 s_flag2) && (Bitvector.diff s_flag1 s_flag3)


  let add i1 i2 =
    let lo = add i1.lo i2.lo and hi = add i1.hi i2.hi in
    if overflow_add i1.lo i2.lo lo || overflow_add i2.hi i1.hi hi
    then top (size_of i1.lo)
    else create lo hi


  let overflow_sub a b res =
    let sz = Bitvector.size_of a in
    let a = Bitvector.extend a sz in
    let b = Bitvector.extend b sz in
    let s_flag1 = Bitvector.extract a (sz - 1) (sz - 1) in
    let s_flag2 = Bitvector.extract b (sz - 1) (sz - 1) in
    let s_flag3 = Bitvector.extract res (sz - 1) (sz - 1) in
    (Bitvector.diff s_flag1 s_flag2) && (Bitvector.diff s_flag1 s_flag3)


  let sub i1 i2 =
    let lo = sub i1.lo i2.hi and hi = sub i1.hi i2.lo in
    if overflow_sub i1.lo i2.hi lo || overflow_sub i1.hi i2.lo hi
    then top (size_of i1.lo)
    else create lo hi


  let mul i1 i2 =
    let l_l = Bitvector.smul i1.lo i2.lo in
    let l_u = Bitvector.smul i1.lo i2.hi in
    let u_l = Bitvector.smul i1.hi i2.lo in
    let u_u = Bitvector.smul i1.hi i2.hi in
    let sz = Bitvector.size_of i1.lo in
    let sz2 =  2 * sz in
    let ex_l_l = smul (extend_signed i1.lo sz2) (extend_signed i2.lo sz2) in
    let ex_l_u = smul (extend_signed i1.lo sz2) (extend_signed i2.hi sz2) in
    let ex_u_l = smul (extend_signed i1.hi sz2) (extend_signed i2.lo sz2) in
    let ex_u_u = smul (extend_signed i1.hi sz2) (extend_signed i2.hi sz2) in
    if Bitvector.equal ex_l_l (extend_signed l_l sz2) &&
       Bitvector.equal ex_l_u (extend_signed l_u sz2) &&
       Bitvector.equal ex_u_l (extend_signed u_l sz2) &&
       Bitvector.equal ex_u_u (extend_signed u_u sz2)
    then
      let l = trans_closure Bitvector.smin l_u l_l u_l u_u in
      let u = trans_closure Bitvector.smax l_l l_u u_l u_u in
      create l u
    else top sz

  let udiv i1 _ = top (size_of i1.lo)

  let rec sdiv i1 i2 =
    let v = zeros (size_of i2.lo) in
    if Bitvector.sgt i2.lo v
    then begin
      let lo, hi = min_max_bounds Bitvector.sdiv i1 i2 in
      assert (Bitvector.sle lo hi);
      create lo hi
    end
    else if Bitvector.slt i2.hi v
    then let sub1 = neg i1 and sub2 = neg i2 in sdiv sub1 sub2
    else raise Errors.Div_by_zero


  let umod _ = topify

  let rec smod i1 i2 =
    let z = zeros (size_of i2.lo) in
    if Bitvector.sgt i2.lo z
    then
      let lo, hi = min_max_bounds Bitvector.smod i1 i2 in
      (assert (Bitvector.sle lo hi); create lo hi)
    else if Bitvector.sle i2.hi z
    then
      let sub1 = neg i1 and sub2 = neg i2 in smod sub1 sub2
    else failwith "Signed divs ..."

  let logor _ = topify
  let logand _ = topify
  let logxor _ = topify
  let shift_left _ = topify

  let shift_right i1 i2 =
    if is_point i2 then Unsigned.shift_right i1 i2 else topify i1

  let shift_right_signed i1 i2 =
    if is_point i2
    then
      let aux bv = Bigint.int_of_big_int (value_of bv) in
      let lo = shift_right_signed i1.lo (aux i2.lo)
      and hi = shift_right_signed i1.hi (aux i2.hi) in
      create lo hi
    else topify i2

  let rotate_right i1 i2 =
    if is_point i2 then
      let lo = rotate_right i1.lo (int_of_bv i2.lo)
      and hi = rotate_right i1.hi (int_of_bv i2.hi) in
      create lo hi
    else topify i2

  let eq i1 i2 =
    if is_point i1 && equal i1 i2 then bool_true
    else if is_disjoint i1 i2 then bool_false
    else bool_any

  let diff i1 i2 =
    if is_disjoint i1 i2 || equal i1 i2 then bool_true
    else bool_any

  let uge _ _ = bool_any
  let ult _ _ = bool_any
  let ule _ _ = bool_any
  let ugt _ _ = bool_any
  let sle i1 i2 =
    if sge i2.lo i1.hi then bool_true
    else if slt i2.hi i1.lo then bool_false
    else bool_any

  let slt i1 i2 =
    if Bitvector.slt i1.hi i2.lo then bool_true
    else if Bitvector.sle i2.hi i1.lo then bool_false
    else bool_any

  let sge i1 i2 =
    if Bitvector.sge i1.lo i2.hi then bool_true
    else if Bitvector.slt i1.hi i2.lo then bool_false
    else bool_any

  let sgt i1 i2 =
    if Bitvector.sgt i1.lo i2.hi then bool_true
    else
    if Bitvector.sle i1.lo i2.lo && Bitvector.sle i1.hi i2.hi then bool_false
    else bool_any

  let extend _ = top

  let extend_signed i n =
    let lo = Bitvector.extend_signed i.lo n
    and hi = Bitvector.extend_signed i.hi n in
    create lo hi

  let umul = mul
  let smul = mul
end
