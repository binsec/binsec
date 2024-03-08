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

open Common

type t = { min : Z.t; max : Z.t; stride : int }
(** Represents the unsigned fixed-width integer interval
    between [min] and [max], with [stride] fixed bits. *)

let pp ppf { min; max; stride } =
  Format.fprintf ppf "{ min = %a; max = %a; stride = %d; rem = %a }" Z.pp_print
    min Z.pp_print max stride Z.pp_print
    (if stride = 0 then Z.zero else Z.extract min 0 stride)

let mem i ~size:_ t =
  Z.leq t.min i && Z.leq i t.max
  && Z.trailing_zeros (Z.logxor i t.min) >= t.stride

let included ~size:_ t t' =
  t == t'
  || Z.geq t.min t'.min && Z.leq t.max t'.max && t.stride >= t'.stride
     && Z.trailing_zeros (Z.logxor t.min t'.min) >= t'.stride

let project ~size t =
  if size = t.stride then Point t.min
  else if t.stride = 0 && Z.equal t.min Z.zero && Z.popcount t.max = size then
    Top
  else Seq { start = t.min; n = Z.succ (Z.sub t.max t.min) }

let top size = { min = Z.zero; max = Z.extract Z.minus_one 0 size; stride = 0 }
let signed_of ~size i = Z.signed_extract i 0 size

let round_down size pow rem =
  Z.logor (Z.shift_left (Z.extract Z.minus_one 0 (size - pow)) pow) rem

let constant ~size bits = { min = bits; max = bits; stride = size }
let zeros size = constant Z.zero ~size
let zero = zeros 1
let ones size = constant Z.one ~size
let one = ones 1

let uminus ~size t =
  if t.stride = size then constant ~size (Z.extract (Z.neg t.min) 0 size)
  else if Z.equal t.min Z.zero then top size
  else
    let min = Z.extract (Z.neg (Z.signed_extract t.max 0 size)) 0 size
    and max = Z.extract (Z.neg (Z.signed_extract t.min 0 size)) 0 size
    and stride = t.stride in
    { min; max; stride }

let add ~size t t' =
  let min = Z.add t.min t'.min
  and max = Z.add t.max t'.max
  and stride = min t.stride t'.stride in

  let n = Z.numbits min and n' = Z.numbits max in
  if n > size then
    let min = Z.extract min 0 size and max = Z.extract max 0 size in
    { min; max; stride }
  else if n' > size then
    let rem =
      if stride > 0 then Z.extract (Z.add t.min t'.min) 0 stride else Z.zero
    in
    let max = round_down size stride rem in
    { min = rem; max; stride }
  else { min; max; stride }

let sub ~size t t' =
  let stride = min t.stride t'.stride in
  let rem =
    if stride > 0 then Z.extract (Z.sub t.min t'.min) 0 stride else Z.zero
  in
  if stride = size then constant ~size rem
  else if Z.lt t.min t'.max then
    if Z.leq t'.min t.max then
      let max = round_down size stride rem in
      { min = rem; max; stride }
    else
      let min = Z.extract (Z.sub t.min t'.max) 0 size
      and max = Z.extract (Z.sub t.max t'.min) 0 size in
      { min; max; stride }
  else
    let min = Z.sub t.min t'.max and max = Z.sub t.max t'.min in
    { min; max; stride }

let mul ~size t t' =
  let min = Z.mul t.min t'.min
  and max = Z.mul t.max t'.max
  and stride =
    min size
      (min
         (min t.stride (Z.trailing_zeros t.min) + t'.stride)
         (t.stride + min t'.stride (Z.trailing_zeros t'.min)))
  in
  let rem =
    if stride > 0 then Z.extract (Z.mul t.min t'.min) 0 stride else Z.zero
  in
  if stride = size then constant ~size rem
  else
    let n = Z.numbits min and n' = Z.numbits max in
    if n > size || n' > size then
      let max = round_down size stride rem in
      { min = rem; max; stride }
    else { min; max; stride }

let smod ~size t t' =
  if t.stride = size && t'.stride = size && not (Z.equal t'.min Z.zero) then
    constant ~size
      (Z.extract
         (Z.rem (signed_of ~size t.min) (signed_of ~size t'.min))
         0 size)
  else top size
(* smarter? *)

let umod ~size t t' =
  if Z.equal t'.min Z.zero then top size
  else if t.stride = size && t'.stride = size then
    constant ~size (Z.rem t.min t'.min)
  else if Z.lt t.max t'.min then t
  else if t'.stride = size && Z.popcount t'.min = 1 then
    let n = Z.numbits t'.min - 1 in
    if n = 0 then zeros size
    else if n > t.stride then
      let rem = if t.stride > 0 then Z.extract t.min 0 t.stride else Z.zero in
      let max = round_down n t.stride rem in
      { min = rem; max; stride = t.stride }
    else constant ~size (Z.extract t.min 0 n)
  else { min = Z.zero; max = t'.max; stride = 0 }

let sdiv ~size t t' =
  if t.stride = size && t'.stride = size && not (Z.equal t'.min Z.zero) then
    constant ~size
      (Z.extract
         (Z.div (signed_of ~size t.min) (signed_of ~size t'.min))
         0 size)
  else top size
(* smarter? *)

let udiv ~size t t' =
  if Z.equal t'.min Z.zero then top size
  else if t.stride = size && t'.stride = size then
    constant ~size (Z.div t.min t'.min)
  else if Z.lt t.max t'.min then zeros size
  else
    let min = Z.div t.min t'.max and max = Z.div t.max t'.min in
    if Z.equal min max then constant ~size min
    else if Z.equal t'.min t'.max && Z.popcount t'.max = 1 then
      let n = Z.numbits t'.max - 1 in
      if n < t.stride then
        let stride = t.stride - n in
        { min; max; stride }
      else { min; max; stride = 0 }
    else { min; max; stride = 0 }

let append ~size1:_ t ~size2 t' =
  let min = Z.logor (Z.shift_left t.min size2) t'.min
  and max = Z.logor (Z.shift_left t.max size2) t'.max in
  if size2 = t'.stride then
    let stride = t.stride + t'.stride in
    { min; max; stride }
  else { min; max; stride = t'.stride }

let top1 = top 1

let complement1 t =
  if t.stride = 1 then if Z.equal t.min Z.zero then one else zero else top1

let equal ~size t t' =
  if size = t.stride && size = t'.stride && Z.equal t.min t'.min then one
  else if Z.lt t.max t'.min || Z.gt t.min t'.max then zero
  else
    let stride, rem, n =
      if t.stride >= t'.stride then (t.stride, t.min, t'.stride)
      else (t'.stride, t'.min, t.stride)
    in
    if Z.trailing_zeros (Z.logxor t.min t'.min) < n then zero
    else
      let min = Z.max t.min t'.min and max = Z.min t.max t'.max in
      if
        Z.trailing_zeros (Z.logxor min rem) < stride
        && Z.trailing_zeros (Z.logxor max rem) < stride
        && Z.equal
             (Z.shift_right (Z.sub max rem) stride)
             (Z.shift_right (Z.pred (Z.sub min rem)) stride)
      then zero
      else top1

let diff ~size t t' = complement1 (equal ~size t t')

let ule ~size:_ t t' =
  if Z.leq t.max t'.min then one else if Z.gt t.min t'.max then zero else top1

let uge ~size t t' = ule ~size t' t
let ult ~size t t' = complement1 (uge ~size t t')
let ugt ~size t t' = complement1 (ule ~size t t')

let ucmp f c ~size t t' =
  if t.stride = size && t'.stride = size then
    constant ~size:1
      (Z.of_int
         (Bool.to_int (c (signed_of ~size t.min) (signed_of ~size t'.min))))
  else if Z.numbits t.max < size && Z.numbits t'.max < size then f ~size t t'
  else top1

let sle = ucmp ule Z.leq
let sge = ucmp uge Z.geq
let slt = ucmp ult Z.lt
let sgt = ucmp ugt Z.gt

let logand ~size t t' =
  if t.stride = size && t'.stride = size then
    constant ~size (Z.logand t.min t'.min)
  else
    let n = min (Z.numbits t.max) (Z.numbits t'.max) in
    let rem = Z.logand t.min t'.min in
    if t.stride = t'.stride then
      let stride = t.stride in
      if stride >= size then constant ~size rem
      else
        let rem = if stride > 0 then Z.extract rem 0 stride else Z.zero in
        let max = round_down n stride rem in
        { min = rem; max; stride }
    else
      let delta, rem', n' =
        if t.stride > t'.stride then (t.stride - t'.stride, t.min, t'.stride)
        else (t'.stride - t.stride, t'.min, t.stride)
      in
      let ex = Z.extract rem' n' delta in
      let stride = n' + min (Z.trailing_zeros ex) delta in
      if stride >= n then constant ~size rem
      else
        let rem = if stride > 0 then Z.extract rem 0 stride else Z.zero in
        let max = round_down n stride rem in
        { min = rem; max; stride }

let logor ~size t t' =
  let n = max (Z.numbits t.max) (Z.numbits t'.max) in
  if t.stride = t'.stride then
    let stride = t.stride in
    let rem =
      if stride > 0 then Z.extract (Z.logor t.min t'.min) 0 stride else Z.zero
    in
    if stride >= n then constant ~size rem
    else
      let max = round_down n stride rem in
      { min = rem; max; stride }
  else
    let delta, rem, n' =
      if t.stride > t'.stride then (t.stride - t'.stride, t.min, t'.stride)
      else (t'.stride - t.stride, t'.min, t.stride)
    in
    let ex = Z.lognot (Z.extract rem n' delta) in
    let stride = n' + min (Z.trailing_zeros ex) delta in
    let rem =
      if stride > 0 then Z.extract (Z.logor t.min t'.min) 0 stride else Z.zero
    in
    if stride >= n then constant ~size rem
    else
      let max = round_down n stride rem in
      { min = rem; max; stride }

let logxor ~size t t' =
  let stride = min t.stride t'.stride in
  let rem =
    if stride > 0 then Z.extract (Z.logxor t.min t'.min) 0 stride else Z.zero
  in
  let n = max (Z.numbits t.max) (Z.numbits t'.max) in
  if stride >= n then constant ~size rem
  else
    let max = round_down n stride rem in
    { min = rem; max; stride }

let lognot ~size t =
  let stride = t.stride in
  let rem =
    if stride > 0 then Z.extract (Z.lognot t.min) 0 stride else Z.zero
  in
  if stride = size then constant ~size rem
  else
    let max = round_down size stride rem in
    { min = rem; max; stride }

let shift_left ~size t t' =
  if Z.geq t'.min (Z.of_int size) then zeros size
  else
    let delta = Z.to_int t'.min in
    if t'.stride = size then
      let delta = Z.to_int t'.min in
      if delta = 0 then t
      else
        let stride = t.stride + delta in
        let rem = Z.extract (Z.shift_left t.min delta) 0 (min stride size) in
        if stride >= size then constant ~size rem
        else
          let min = Z.shift_left t.min delta
          and max = Z.shift_left t.max delta in
          let n = Z.numbits min and n' = Z.numbits max in
          if n > size || n' > size then
            { min = rem; max = round_down size stride rem; stride }
          else { min; max; stride }
    else
      {
        min = Z.zero;
        max = round_down size delta Z.zero;
        (* smarter? *)
        stride = delta;
      }

let shift_right ~size t t' =
  if Z.geq t'.min (Z.of_int (Z.numbits t.max)) then zeros size
  else if t'.stride = size then
    let delta = Z.to_int t'.min in
    let max = Z.shift_right t.max delta in
    if t.stride = size then constant ~size max
    else if Z.equal max Z.zero then zeros size
    else
      let min = Z.shift_right t.min delta in
      if Z.equal min max then constant ~size min
      else
        let stride = t.stride - delta in
        if stride > 0 then { min; max; stride } else { min; max; stride = 0 }
  else
    let max = Z.shift_right t.max (Z.to_int t'.min)
    and min =
      if Z.geq t'.max (Z.of_int size) then Z.zero
      else Z.shift_right t.min (Z.to_int t'.max)
    in
    { min; max; stride = 0 }

let shift_right_signed ~size t t' =
  if t.stride = size && t'.stride = size then
    let delta = try Z.to_int t'.min with Z.Overflow -> size in
    if delta = 0 then t
    else
      constant ~size
        (Z.extract (Z.shift_right (signed_of ~size t.min) delta) 0 size)
  else if Z.numbits t.max < size then shift_right ~size t t'
  else top size
(* smarter? *)

let rotate_left ~size t t' =
  if t.stride = size && t'.stride = size then
    let delta = Z.to_int (Z.rem t'.min (Z.of_int size)) in
    if delta = 0 then t
    else
      constant ~size
        (Z.extract
           (Z.logor (Z.shift_left t.min delta)
              (Z.shift_right t.min (size - delta)))
           0 size)
  else top size

let rotate_right ~size t t' =
  if t.stride = size && t'.stride = size then
    let delta = Z.to_int (Z.rem t'.min (Z.of_int size)) in
    if delta = 0 then t
    else
      constant ~size
        (Z.extract
           (Z.logor
              (Z.shift_right t.min delta)
              (Z.shift_left t.min (size - delta)))
           0 size)
  else top size

let uext n ~size t =
  { t with stride = (if t.stride = size then size + n else t.stride) }

let sext n ~size t =
  let size' = size + n in
  if t.stride = size then
    constant ~size:size' (Z.extract (Z.signed_extract t.min 0 size) 0 size')
  else if Z.numbits t.max < size then uext n ~size t
  else top size'

let restrict ~lo ~hi ~size:_ t =
  let size = hi - lo + 1 in
  if hi < t.stride then constant ~size (Z.extract t.min lo size)
  else
    let n = Z.numbits t.max in
    if lo > n then zeros size
    else
      let stride = max 0 (t.stride - lo) in
      let rem = if stride = 0 then Z.zero else Z.extract t.min lo stride in
      let n' = min size (n - lo) in
      if stride >= n' then constant ~size rem
      else
        let max = round_down n' stride rem in
        { min = rem; max; stride }

let union ~size:_ t t' =
  let min = Z.min t.min t'.min
  and max = Z.max t.max t'.max
  and stride =
    min t.stride (min t'.stride (Z.trailing_zeros (Z.logxor t.min t'.min)))
  in
  { min; max; stride }

let create size min max stride rem =
  let min, max =
    if stride > 0 then
      ( (if Z.trailing_zeros (Z.logxor min rem) < stride then
           Z.logor
             (Z.shift_left
                (Z.succ (Z.shift_right (Z.pred (Z.sub min rem)) stride))
                stride)
             rem
         else min),
        if Z.trailing_zeros (Z.logxor max rem) < stride then
          Z.logor
            (Z.shift_left (Z.shift_right (Z.sub max rem) stride) stride)
            rem
        else max )
    else (min, max)
  in
  if Z.equal min max then constant ~size min
  else if Z.gt min max then raise Empty
  else { min; max; stride }

let inter ~size t t' =
  if Z.gt t.min t'.max || Z.lt t.max t'.min then raise Empty
  else
    let stride, rem, n =
      if t.stride >= t'.stride then (t.stride, t.min, t'.stride)
      else (t'.stride, t'.min, t.stride)
    in
    if Z.trailing_zeros (Z.logxor t.min t'.min) < n then raise Empty
    else
      create size (Z.max t.min t'.min) (Z.min t.max t'.max) stride
        (if stride = 0 then Z.zero else Z.extract rem 0 stride)

let overlap t t' =
  Z.leq t.min t.max && Z.geq t.max t'.min
  &&
  let stride, rem, n =
    if t.stride >= t'.stride then (t.stride, t.min, t'.stride)
    else (t'.stride, t'.min, t.stride)
  in
  let rem = if stride = 0 then Z.zero else Z.extract rem 0 stride in
  Z.trailing_zeros (Z.logxor t.min t'.min) >= n
  && (stride = 0
     ||
     let min = Z.max t.min t'.min and max = Z.min t.max t'.max in
     Z.leq
       (if Z.trailing_zeros (Z.logxor min rem) < stride then
          Z.logor
            (Z.shift_left
               (Z.succ (Z.shift_right (Z.pred (Z.sub min rem)) stride))
               stride)
            rem
        else min)
       (if Z.trailing_zeros (Z.logxor max rem) < stride then
          Z.logor
            (Z.shift_left (Z.shift_right (Z.sub max rem) stride) stride)
            rem
        else max))

let disjoint ~size:_ t t' = not (overlap t t')
let refine ~size t t' = if included ~size t t' then t else inter ~size t t'
let unary_nofeedback ~size:_ t _ = t
let binary_nofeedback ~size:_ t t' _ = (t, t')
let uminus_feedback = unary_nofeedback

let add_feedback ~size t t' r =
  (refine ~size t (sub ~size r t'), refine ~size t' (sub ~size r t))

let sub_feedback ~size t t' r =
  (refine ~size t (add ~size r t'), refine ~size t' (sub ~size t r))

let mul_feedback = binary_nofeedback
let smod_feedback = binary_nofeedback
let umod_feedback = binary_nofeedback
let udiv_feedback = binary_nofeedback
let sdiv_feedback = binary_nofeedback
let append_feedback ~size1:_ t ~size2:_ t' _ = (t, t')

let equal_feedback ~size t t' r =
  if r.stride = 1 then
    if Z.equal r.min Z.zero then (t, t')
    else (refine ~size t t', refine ~size t' t)
  else (t, t')

let diff_feedback ~size t t' r = equal_feedback ~size t t' (complement1 r)

let ule_feedback ~size t t' r =
  if r.stride = 1 then
    if Z.equal r.min Z.zero then
      let c = { min = t'.min; max = t.max; stride = 0 } in
      (refine ~size t c, refine ~size t' c)
    else
      let c = { min = t.min; max = t'.max; stride = 0 } in
      (refine ~size t c, refine ~size t' c)
  else (t, t')

let uge_feedback ~size t t' r =
  let t', t = ule_feedback ~size t' t r in
  (t, t')

let ult_feedback ~size t t' r = uge_feedback ~size t t' (complement1 r)
let ugt_feedback ~size t t' r = ule_feedback ~size t t' (complement1 r)
let sle_feedback = binary_nofeedback
let sge_feedback = binary_nofeedback
let slt_feedback = binary_nofeedback
let sgt_feedback = binary_nofeedback

(* result 0 0 -> ? | 1 0 -> 0 | 0 1 -> {} | 1 1 -> 1 *)
(* mask   0 0 -> 1 | 1 0 -> 0 | 0 1 -> ? | 1 1 -> 0 *)
let logand_feedback1 ~size t r =
  let stride =
    min r.stride
      (Z.trailing_zeros
         (Z.logand
            (Z.logor (Z.lognot t.min) (Z.shift_left Z.minus_one t.stride))
            (Z.lognot r.min)))
  in
  if Z.trailing_zeros (Z.logand (Z.lognot t.min) r.min) < min stride t.stride
  then raise Empty
  else
    let rem =
      if stride = 0 then Z.zero
      else
        Z.extract
          (Z.logand (Z.logor t.min (Z.shift_left Z.minus_one t.stride)) r.min)
          0 stride
    in
    if stride = size then constant ~size rem
    else { min = rem; max = round_down size stride rem; stride }

let logand_feedback ~size t t' r =
  ( refine ~size t (logand_feedback1 ~size t' r),
    refine ~size t' (logand_feedback1 ~size t r) )

(* result 0 0 -> 0 | 1 0 -> {} | 0 1 -> 1 | 1 1 -> ? *)
(* mask   0 0 -> 0 | 1 0 -> ? | 0 1 -> 0 | 1 1 -> 1 *)
let logor_feedback1 ~size t r =
  let stride =
    min r.stride
      (Z.trailing_zeros
         (Z.logand (Z.logor t.min (Z.shift_left Z.minus_one t.stride)) r.min))
  in
  let stride' = min stride t.stride in
  if Z.trailing_zeros (Z.logand t.min (Z.lognot r.min)) < stride' then
    raise Empty
  else
    let rem =
      if stride' = 0 then Z.zero else Z.extract (Z.logor t.min r.min) 0 stride'
    in
    if stride = size then constant ~size rem
    else { min = rem; max = round_down size stride rem; stride }

let logor_feedback ~size t t' r =
  ( refine ~size t (logor_feedback1 ~size t' r),
    refine ~size t' (logor_feedback1 ~size t r) )

let lognot_feedback ~size t r = refine ~size t (lognot ~size r)

let logxor_feedback ~size t t' r =
  (refine ~size t (logxor ~size t' r), refine ~size t' (logxor ~size t r))

let shift_left_feedback = binary_nofeedback
let shift_right_feedback = binary_nofeedback
let shift_right_signed_feedback = binary_nofeedback
let rotate_left_feedback = binary_nofeedback
let rotate_right_feedback = binary_nofeedback
let uext_feedback _ ~size t r = refine ~size t r
let sext_feedback _ ~size:_ t _ = t
let restrict_feedback ~lo:_ ~hi:_ ~size:_ t _ = t

let iter =
  let rec loop f i max step =
    if Z.leq i max then (
      f i;
      loop f (Z.add i step) max step)
  in
  fun f ~size:_ { min; max; stride; _ } ->
    loop f min max (Z.shift_left Z.one stride)

let fold =
  let rec loop f i a max step =
    if Z.leq i max then loop f (Z.add i step) (f i a) max step else a
  in
  fun f a ~size:_ { min; max; stride; _ } ->
    loop f min a max (Z.shift_left Z.one stride)

let for_all =
  let rec loop p i max step =
    Z.gt i max || (p i && loop p (Z.add i step) max step)
  in
  fun p ~size:_ { min; max; stride; _ } ->
    loop p min max (Z.shift_left Z.one stride)

let sum ~size:_ t =
  Z.shift_right
    (Z.mul
       (Z.succ (Z.shift_right (Z.sub t.max t.min) t.stride))
       (Z.add t.min t.max))
    1

let create ~size ~min ~max ~stride =
  if
    Z.gt min max
    || Z.numbits max > size
    || (stride = size && not (Z.equal min max))
    || stride <> 0
       && not (Z.equal (Z.extract min 0 stride) (Z.extract max 0 stride))
  then raise (Invalid_argument "create")
  else { min; max; stride }
