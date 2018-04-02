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

exception Elements_of_top

let level = 3

open Interval
module Value = struct

  type t = {
    region : Dba.region;
    signed : Interval.Signed.t;
    unsigned : Interval.Unsigned.t;
  }

  (* Invariant : all bitvector bounds of intervals must have the same size *)
  let create region signed unsigned =
    assert Bitvector.(size_of signed.Signed.lo = size_of unsigned.Unsigned.lo);
    { region; signed; unsigned; }

  let constant = create `Constant

  let of_point bv =
    let unsigned = Interval.Unsigned.of_point bv
    and signed = Interval.Signed.of_point bv in
    constant signed unsigned

  let relocate region t = { t with region }

  let same_range range t =
    let open Interval.Unsigned in
    let lo = range.lo and hi = range.hi in
    { t with signed = Interval.Signed.create lo hi;
             unsigned = Interval.Unsigned.create lo hi; }

  let bool_true = constant Interval.Signed.bool_true Interval.Unsigned.bool_true
  let bool_false = constant Interval.Signed.bool_false Interval.Unsigned.bool_false
  let bool_any = constant Interval.Signed.bool_any Interval.Unsigned.bool_any

  let _update p update_unsigned update_signed v1 v2 =
    if p v1.region v2.region then
      Some (update_signed v1.signed v2.signed,
            update_unsigned v1.unsigned v2.unsigned)
    else None

  let same_region v1 v2 = Region_bitvector.region_equal v1.region v2.region
  let is_constant_region v = Region_bitvector.region_equal v.region `Constant

  let apply2 f g v1 v2 =
    { v1 with signed = f v1.signed v2.signed;
              unsigned = g v1.unsigned v2.unsigned; }

  let _modify fsigned gunsigned v =
    { v with signed = fsigned v.signed; unsigned = gunsigned v.unsigned; }

  let set_unsigned unsigned v = { v with unsigned }
  let set_signed signed v = { v with signed }
  let set ~(signed:Interval.Signed.t) ~(unsigned:Interval.Unsigned.t) v =
    set_signed signed (set_unsigned unsigned v)
end

open Value

type t =
  | IVal of Value.t option
  | Top

let universe  = Top

let empty = IVal None

let is_empty value =
  match value with
  | IVal None -> true
  | _ -> false

let singleton value =
  match value with
  | `Value (region, bv) ->
    IVal (Some (Value.of_point bv |> Value.relocate region))
  | _ -> Top

let mk_ival (v: Value.t) = IVal (Some v)

let meet p1 p2 =
  match p1, p2 with
  | None, _  | _, None -> None
  | Some i1, Some i2 ->
    let l = Bitvector.umax i1.Unsigned.lo i2.Signed.lo in
    let u = Bitvector.umin i1.Unsigned.hi i2.Signed.hi in
    if Bitvector.ugt l u then None else Some (Unsigned.create l u)


let refine ival =
  match ival with
  | IVal Some v ->
    let unsigned1 = Unsigned.(low v.unsigned) in
    let unsigned2 = Unsigned.(high v.unsigned) in
    let signed1 = Signed.low v.signed in
    let signed2 = Signed.high v.signed in
    let ival1 = meet unsigned1 signed1 in
    let ival2 = meet unsigned2 signed2 in
    let res =
      match ival1, ival2 with
      | Some _, Some _ -> ival
      | None, Some i | Some i, None ->
        mk_ival (Value.same_range i v)
      | None, None -> IVal None
    in res
  | IVal None -> IVal None
  | Top -> Top


let pp ppf range =
  let open Format in
  match refine range with
  | Top -> fprintf ppf "[--.--]"
  | IVal None -> fprintf ppf "[_]"
  | IVal Some v ->
    fprintf ppf "@[";
    if not (is_constant_region v) then
      fprintf ppf "%@%a:" Dba_printer.Ascii.pp_region v.region;
    fprintf ppf "%a" Unsigned.pp v.unsigned;
    if not (Bitvector.equal v.signed.Signed.lo v.unsigned.Unsigned.lo &&
            Bitvector.equal v.signed.Signed.hi v.unsigned.Unsigned.hi)
    then fprintf ppf ":S %a" Signed.pp v.signed;
    fprintf ppf "@]"


let to_string range =
  pp Format.str_formatter range;
  Format.flush_str_formatter ()

(* let to_string value =
 *   let open Value in
 *   let value = refine value in
 *   match value with
 *     Top -> "⟙"
 *   | IVal None -> "⟘"
 *   | IVal Some v ->
 *     let refined = Bitvector.equal v.unsigned.lo v.signed.lo &&
 *                   Bitvector.equal v.unsigned.hi v.signed.hi in
 *     let r_string = match r with
 *       | `Constant -> ""
 *       | `Stack    -> "𝑺"
 *       | `Malloc((id, _), _) -> "𝑴 " ^ string_of_int id
 *     in
 *     let l2' =
 *       let (l_s, sz_l) =
 *         let l = l2 in
 *         Bitvector.value_of l, Bitvector.size_of l
 *       in
 *       let abs_l_s = Bigint.abs_big_int l_s in
 *       if (Bigint.lt_big_int l_s Bigint.zero_big_int)
 *       then ("-" ^ (Region_bitvector.st_of (Bitvector.create abs_l_s sz_l)))
 *       else (Region_bitvector.st_of (Bitvector.create l_s sz_l))
 *     in
 *     let u2' =
 *       let (u_s, sz_u) =
 *         let u = u2 in
 *         Bitvector.value_of u, Bitvector.size_of u
 *       in
 *       let abs_u_s = Bigint.abs_big_int u_s in
 *       if (Bigint.lt_big_int u_s Bigint.zero_big_int)
 *       then "-" ^ (Region_bitvector.st_of (Bitvector.create abs_u_s sz_u))
 *       else (Region_bitvector.st_of (Bitvector.create u_s sz_u))
 *     in
 *     let s1 = (Region_bitvector.st_of l1) in
 *     let s2 = (Region_bitvector.st_of u1) in
 *     if r_string = "" then (
 *       let l2' =
 *         let (l_s, _) =
 *           let l = l2 in
 *           Bitvector.value_of l, Bitvector.size_of l
 *         in
 *         Bigint.string_of_big_int l_s
 *       in
 *       let u2' =
 *         let (u_s, _sz_u) =
 *           let u = u2 in
 *           Bitvector.value_of u, Bitvector.size_of u
 *         in
 *         Bigint.string_of_big_int u_s
 *       in
 *       let l1 = Bitvector.value_of l1 in
 *       let u1 = Bitvector.value_of u1 in
 *       let s1 = (Bigint.string_of_big_int l1) in
 *       let s2 = (Bigint.string_of_big_int u1) in
 *       if refined then
 *         Format.asprintf "[%s, %s]" l2' u2'
 *       else
 *         Format.asprintf "⟮[%s, %s], [%s, %s]⟯" s1 s2 l2' u2'
 *     )
 *     else if refined then
 *       Format.asprintf "⟮%s, [%s, %s]⟯" r_string l2' u2'
 *     else
 *       Format.asprintf "⟮%s, [%s, %s], [%s, %s]⟯" r_string s1 s2 l2' u2' *)


let of_bounds (v1, v2) =
  match v1, v2  with
  | `Value (r1, bv1), `Value (r2, bv2) ->
    let size1 = Bitvector.size_of bv1 in
    let size2 = Bitvector.size_of bv2 in
    assert (size1 = size2);
    if Region_bitvector.region_equal r1 r2
    then
      if Bitvector.ule bv1 bv2 || Bitvector.sle bv1 bv2
      then
        let mk_interval ord create range =
          if ord bv1 bv2 then create bv1 bv2 else range
        in
        let unsigned_interval =
          mk_interval Bitvector.ule Unsigned.create (Unsigned.top size1)
        and signed_interval =
          mk_interval Bitvector.sle Signed.create (Signed.top size1) in
        IVal (Some (Value.create r1 signed_interval unsigned_interval))
      else IVal None
    else Top
  | _, _ -> Top


let apply2 unsigned_f signed_f val1 val2 =
  match val1, val2 with
  | IVal Some v1, IVal Some v2 when same_region v1 v2 ->
      let unsigned = unsigned_f v1.unsigned v2.unsigned in
      let signed = signed_f v1.signed v2.signed in
      mk_ival (Value.create v1.region signed unsigned)
  | IVal None, i -> i
  | i, IVal None -> i
  | Top, IVal _  -> Top
  | IVal _, Top  -> Top
  | Top, Top     -> Top
  | _, _ -> Top

let join = apply2 Unsigned.join Signed.join

let high_unsigned_threshold thresholds u =
  let rec aux i =
    if i < Array.length thresholds
    then
      let bv =
        Bitvector.create
          (Bigint.big_int_of_int thresholds.(i)) (Bitvector.size_of u) in
      if Bitvector.ult bv u then aux (i + 1) else bv
    else Bitvector.max_ubv (Bitvector.size_of u)
  in aux 0


let low_unsigned_threshold thresholds l =
  let rec aux i =
    if (i < Array.length thresholds)
    then
      let bv = Bitvector.create (Bigint.big_int_of_int thresholds.(i)) (Bitvector.size_of l) in
      if (Bitvector.ult l bv)
      then aux (i + 1)
      else bv
    else Bitvector.zeros (Bitvector.size_of l)
  in
  aux 0


let high_signed_threshold thresholds u =
  let rec aux i =
    if (i < Array.length thresholds)
    then
      let bv = Bitvector.create (Bigint.big_int_of_int thresholds.(i)) (Bitvector.size_of u) in
      if (Bitvector.slt bv u)
      then aux (i + 1)
      else bv
    else Bitvector.max_sbv (Bitvector.size_of u)
  in
  aux 0

let low_signed_threshold thresholds l =
  let rec aux i =
    if i < Array.length thresholds
    then
      let bv = Bitvector.create (Bigint.big_int_of_int thresholds.(i)) (Bitvector.size_of l) in
      if (Bitvector.slt l bv)
      then aux (i + 1)
      else bv
    else Signed.smin (Bitvector.size_of l)
  in aux 0


let widen val1 val2 thresholds =
  let thresholds1, thresholds2, thresholds3, thresholds4 = thresholds
  in
  let sub_widen_unsigned i1 i2 =
    let open Unsigned in
    let lo =

      if Bitvector.ule i1.lo i2.lo then i1.lo
      else low_unsigned_threshold thresholds2 i2.lo
    in
    let hi =
      if Bitvector.uge i1.hi i2.hi then i1.hi
      else high_unsigned_threshold thresholds1 i2.hi
    in Unsigned.create lo hi
  in
  let sub_widen_signed i1 i2 =
    let open Signed in
    let lo =
      if Bitvector.sle i1.lo i2.lo then i1.lo
      else low_signed_threshold thresholds4 i2.lo
    in
    let hi =
      if Bitvector.sge i1.hi i2.hi then i1.hi
      else high_signed_threshold thresholds3 i2.hi
    in Signed.create lo hi
  in apply2 sub_widen_unsigned sub_widen_signed val1 val2


let meet val1 val2 =
  match val1, val2 with
  | IVal Some v1, IVal Some v2 ->
    if Value.same_region v1 v2
    then
      let unsigned = Unsigned.meet v1.unsigned v2.unsigned  in
      let signed = Signed.meet v1.signed v2.signed in
      match unsigned, signed with
      | Some unsigned, Some signed ->
        mk_ival (Value.create v1.region signed unsigned)
      | _, _ -> IVal None
    else IVal None
  | IVal None, _ | _, IVal None -> IVal None
  | Top, i | i, Top -> i


let contains val1 val2 =
  let val1 = refine val1 in
  let val2 = refine val2 in
  match val1, val2 with
  | IVal Some v1, IVal Some v2 ->
    same_region v1 v2 && Signed.contains v1.signed v2.signed
    && Unsigned.contains v1.unsigned v2.unsigned
  | _, IVal None -> true
  | IVal None, _ -> false
  | Top, _       -> true
  | _, Top       -> false


let neg value =
  match value with
  | IVal (Some {region = `Constant; unsigned; signed;}) ->
    mk_ival (constant (Signed.neg signed) (Unsigned.neg unsigned) )
  | IVal _ | Top -> value


let lognot value =
  match value with
  | IVal (Some {region = `Constant; unsigned; signed;}) ->
    mk_ival (constant (Signed.lognot signed) (Unsigned.lognot unsigned) )
  | IVal _ | Top -> value


let add val1 val2 =
  let val1 = refine val1 in
  let val2 = refine val2 in
  match val1, val2 with
  | IVal (Some {region = r; unsigned = i1; signed = i2;}),
    IVal (Some {region = `Constant; unsigned = i1'; signed = i2';})
  | IVal (Some {region = `Constant; unsigned = i1; signed = i2}),
    IVal (Some {region = r; unsigned = i1'; signed = i2'})  ->
    mk_ival (Value.create r (Signed.add i2 i2') (Unsigned.add i1 i1'))
  | IVal (Some _), IVal (Some _) -> Top
  | IVal None, _ -> failwith "add : first range is empty"
  | _i, IVal None -> failwith "add : second range is empty"
  | Top, _i | _i, Top       -> Top


let sub val1 val2 =
  match val1, val2 with
  | IVal (Some { region = r1; unsigned = i1; signed = i2; }),
    IVal (Some { region = `Constant; unsigned = i1'; signed = i2'; })    ->
      let unsigned = Unsigned.sub i1 i1' in
      let signed = Signed.sub i2 i2' in
      mk_ival (Value.create r1 signed unsigned)
  | IVal Some v1, IVal Some v2 when same_region v1 v2 ->
    let unsigned = Unsigned.sub v1.unsigned v2.unsigned in
    let signed = Signed.sub v1.signed v2.signed in
    mk_ival (set_signed signed (set_unsigned unsigned v1))
  | IVal None, _i ->  failwith "sub : first range empty"
  | _i, IVal None -> failwith "sub : second range empty"
  | IVal Some _, IVal Some _
  | Top, _
  | _, Top       -> Top


let umul val1 val2 =
  let val1 = refine val1 in
  let val2 = refine val2 in
  match val1, val2 with
  | IVal (Some { region = `Constant; unsigned = i1; signed = i2 }),
    IVal (Some { region = `Constant; unsigned = i1'; signed = i2' }) ->
    let v = Value.constant (Signed.umul i2 i2') (Unsigned.umul i1 i1') in
    mk_ival v
  | IVal (Some { region = r; unsigned = i1; signed = i2 }),
    IVal (Some { region = `Constant; unsigned = i1'; signed = i2' })
  | IVal (Some { region = `Constant; unsigned = i1'; signed = i2' }),
    IVal (Some { region = r; unsigned = i1; signed = i2 }) ->
    if Unsigned.is_unit i1'
    then mk_ival (Value.create r (Signed.umul i2 i2') (Unsigned.umul i1 i1'))
    else Top
  | IVal None, _
  | _, IVal None -> IVal None
  | IVal (Some _), IVal (Some _)
  | Top, _
  | _, Top       -> Top


let smul = umul

let power _val1 _val2 = failwith "power"

let udiv val1 val2 =
  let val1 = refine val1 and val2 = refine val2 in
  match val1, val2 with
  | IVal (Some { region = `Constant; unsigned = i1; signed = i2 }),
    IVal (Some { region = `Constant; unsigned = i1';signed = i2'}) ->
    let unsigned = Unsigned.udiv i1 i1' in
    let signed = Signed.udiv i2 i2' in
    mk_ival (Value.constant signed unsigned)
  | IVal Some _, IVal Some _ -> Top
  | IVal None, _
  | _, IVal None -> failwith "div None"
  | Top, IVal (Some { region = `Constant; signed; _ }) ->
    if Signed.contains_zero signed then raise Errors.Div_by_zero
    else Top
  | Top, _ -> Top
  | _, Top -> raise Errors.Div_by_zero


let sdiv val1 val2 =
  let val1 = refine val1 in
  let val2 = refine val2 in
  match val1, val2 with
  | IVal Some { region = `Constant; unsigned = i1; signed = i2 },
    IVal Some { region = `Constant; unsigned = i1';signed = i2'} ->
    let unsigned = Unsigned.sdiv i1 i1' in
    let signed = Signed.sdiv i2 i2' in
    mk_ival (Value.constant signed unsigned)
  | IVal Some _, IVal Some _ -> Top
  | IVal None, _
  | _, IVal None -> failwith "div none2"
  | Top, IVal (Some { region = `Constant; signed; _ }) ->
    if Signed.contains_zero signed
    then raise Errors.Div_by_zero
    else Top
  | Top, _ -> Top
  | _, Top -> raise Errors.Div_by_zero


let mod_apply fsigned funsigned val1 val2 =
  match val1, val2 with
  | IVal Some v1, IVal Some v2 when is_constant_region v1 && is_constant_region v2 ->
    let signed = fsigned v1.signed v2.signed
    and unsigned = funsigned v1.unsigned v2.unsigned in
    mk_ival (Value.set ~signed ~unsigned v1)
  | IVal None, i | i, IVal None -> i
  | IVal Some _, IVal Some _ | Top, _ | _, Top -> Top

let umod = mod_apply Signed.umod Unsigned.umod
let smod = mod_apply Signed.smod Unsigned.smod


let apply_on_constants ?(top=Top) fsigned funsigned val1 val2 =
  match refine val1, refine val2 with
  | IVal Some v1, IVal Some v2  when same_region v1 v2 && is_constant_region v1 ->
    let unsigned = funsigned v1.unsigned v2.unsigned
    and signed = fsigned v1.signed v2.signed in
    mk_ival (set ~signed ~unsigned v1)
  | IVal None, _ | _, IVal None -> assert false
  | IVal Some _, IVal Some _
  | Top, IVal _
  | IVal _, Top
  | Top, Top  -> top

let top_apply = apply_on_constants ~top:Top

let logor = top_apply Signed.logor Unsigned.logor
let logxor = top_apply Signed.logxor Unsigned.logxor
let logand = top_apply Signed.logand Unsigned.logand

let max value =
  match value with
  | IVal Some v -> `Value (`Constant, v.unsigned.Unsigned.hi)
  (* FIXME : REALLY ?*)
  | IVal None | Top ->
    (* Seems dubious that a bv of size -1 should ever be created: check that *)
    `Value (`Constant, Bitvector.zeros (-1))


let lshift = top_apply Signed.shift_left Unsigned.shift_left
let rshiftU = top_apply Signed.shift_right Unsigned.shift_right
let rshiftS = top_apply Signed.shift_right_signed Unsigned.shift_right_signed

let rotate_left _val1 _val2 = failwith "rotate_left"

let rotate_right = top_apply Signed.rotate_right Unsigned.rotate_right

let eq val1 val2 =
  match refine val1, refine val2 with
  | IVal Some v1, IVal Some v2 ->
    if Value.same_region v1 v2
    then mk_ival (Value.apply2 Signed.eq Unsigned.eq v1 v2)
    else Top
  | IVal None, IVal None -> Value.bool_true |> mk_ival
  | IVal None, _
  | _, IVal None -> Value.bool_false |> mk_ival
  | Top, IVal _
  | IVal _, Top
  | Top, Top   -> mk_ival Value.bool_any


let diff val1 val2 =
  match refine val1, refine val2 with
  | IVal Some v1, IVal Some v2 when same_region v1 v2 ->
    mk_ival (Value.apply2 Signed.diff Unsigned.diff v1 v2)
  | IVal None, _ | _, IVal None -> mk_ival Value.bool_true
  | Top, IVal _
  | IVal _, Top
  | IVal Some _, IVal Some _
  | Top, Top -> mk_ival Value.bool_any

let comparison_apply fsigned funsigned val1 val2 =
  match refine val1, refine val2 with
    | IVal Some v1, IVal Some v2 when same_region v1 v2 ->
      mk_ival (Value.apply2 fsigned funsigned v1 v2)
    | IVal None, _
    | _, IVal None
    | Top, IVal _
    | IVal _, Top
    | IVal Some _, IVal Some _
    | Top, Top -> mk_ival Value.bool_any

let geqU = comparison_apply Signed.uge Unsigned.uge

let leqU val1 val2 = geqU val2 val1

let ltU = comparison_apply Signed.ult Unsigned.ult

let gtU val1 val2 = ltU val2 val1

let leqS = comparison_apply Signed.sle Unsigned.sle
let ltS = comparison_apply Signed.slt Unsigned.slt
let geqS = comparison_apply Signed.sge Unsigned.sge
let gtS =  comparison_apply Signed.sgt Unsigned.sgt

let extension value i =
  let value = refine value in
  match value with
  | Top       -> Top
  | IVal None -> IVal None
  | IVal (Some { region = `Constant; unsigned ; signed; }) ->
    let signed = Signed.extend signed i
    and unsigned = Unsigned.extend unsigned i in
    mk_ival (Value.constant signed unsigned)
  | IVal Some _ -> Top


let signed_extension value i =
  let value = refine value in
  match value with
  | Top       -> Top
  | IVal None -> IVal None
  | IVal (Some { region = `Constant; unsigned; signed; }) ->
    let signed = Signed.extend_signed signed i
    and unsigned = Unsigned.extend_signed unsigned i in
    mk_ival (Value.constant signed unsigned)
  | _ -> Top


let equal val1 val2 =
  match val1, val2 with
  | Top, Top
  | IVal None, IVal None -> true
  | IVal Some v1, IVal Some v2 ->
    Value.same_region v1 v2 && Signed.equal v1.signed v2.signed
    && Unsigned.equal v1.unsigned v2.unsigned
  | _, _ -> false


let is_true value _assumes _global_regions =
  let open Basic_types.Ternary in
  match value with
  | Top       -> Unknown
  | IVal None -> failwith "empty in is_true interval"
  | IVal (Some { region =`Constant; unsigned; signed  }) ->
    if Bitvector.(
        is_zeros unsigned.Unsigned.hi && is_zeros unsigned.Unsigned.lo
        || is_zeros signed.Signed.hi && is_zeros signed.Signed.lo)
    then False
    else
    if Bitvector.(is_one unsigned.Unsigned.hi && is_one unsigned.Unsigned.lo ||
                  is_one signed.Signed.hi && is_one signed.Signed.lo)
    then True
    else Unknown
  | _ -> assert false


let rec guard binop val1 val2 =
  Logger.debug
    "@[Guard entry: %a %a %a@]"
    Dba_printer.Ascii.pp_binary_op binop pp val1 pp val2;
  let val1 = refine val1
  and val2 = refine val2 in
  let v1, v2 =
    match binop with
    | Dba.Eq -> let i = meet val1 val2 in i, i
    | Dba.LtU ->
      begin
        match val1, val2 with
        | IVal Some v1, IVal Some v2 ->
          let a = v1.unsigned.Unsigned.lo
          and b = v1.unsigned.Unsigned.hi
          and c = v2.unsigned.Unsigned.lo
          and d = v2.unsigned.Unsigned.hi in
          if same_region v1 v2
          then
            let min = Bitvector.umin (Bitvector.pred d) b in
            let max = Bitvector.umax (Bitvector.succ a) c in
            let x =
              if Bitvector.ugt a min then IVal None
              else
                let unsigned = Unsigned.create a min in
                mk_ival (set_unsigned unsigned v1)
            and y =
              if Bitvector.ult d max then IVal None
              else mk_ival (set_unsigned (Unsigned.create max d) v2)
            in x, y
          else val1, val2
        | Top,
          IVal Some v when is_constant_region v ->
          let v1 =
            if Bitvector.is_zero v.unsigned.Unsigned.hi then IVal None
            else
              let lo =
                Bitvector.zeros (Bitvector.size_of v.unsigned.Unsigned.hi) in
              let hi = Bitvector.pred v.unsigned.Unsigned.lo in
              let unsigned = Unsigned.create lo hi in
              let signed = Signed.topify v.signed in
              mk_ival (set_signed signed (set_unsigned unsigned v))
          in v1, val2

        | IVal Some v, Top when is_constant_region v ->
          let v2 =
            if Bitvector.is_max_ubv v.unsigned.Unsigned.lo then val2
            else
              let lo = Bitvector.succ v.unsigned.Unsigned.lo in
              let hi =
                Bitvector.max_ubv (Bitvector.size_of v.unsigned.Unsigned.lo) in
              let unsigned = Unsigned.create lo hi in
              let signed = Signed.topify v.signed in
              mk_ival (set_signed signed (set_unsigned unsigned v))
          in val1, v2
        | _, _ -> val1, val2
      end

    | Dba.Diff ->
      let val11, val21 = guard Dba.LtU val1 val2 in
      let val22, val12 = guard Dba.LtU val2 val1 in
      join val11 val12, join val21 val22

    | Dba.GeqU ->
      begin
        match val1, val2 with
        | IVal Some v1, IVal Some v2 ->
          if same_region v1 v2
          then
            let open Unsigned in
            let min = Bitvector.umin v1.unsigned.hi v2.unsigned.hi in
            let max = Bitvector.umax v1.unsigned.lo v2.unsigned.lo in
            let x =
              if Bitvector.ugt max v1.unsigned.hi then IVal None
              else
                let unsigned = Unsigned.create max v1.unsigned.hi in
                mk_ival (set_unsigned unsigned v1)
            and y =
              if Bitvector.ult min v2.unsigned.lo then IVal None
              else
                let unsigned = Unsigned.create v2.unsigned.lo min in
                mk_ival (set_unsigned unsigned v2)
            in x, y
          else val1, val2
        | Top, IVal Some v when is_constant_region v ->
          (* IVal (Some { region =`Constant; unsigned = (l_i1, _); signed = (_,
           * u_i2)}) -> *)
          let unsigned = Unsigned.maxify v.unsigned
          and signed = Signed.topify v.signed in
          mk_ival (set_signed signed (set_unsigned unsigned v)), val2
        | IVal Some v, Top when is_constant_region v ->
          let unsigned = Unsigned.minify v.unsigned
          and signed = Signed.topify v.signed in
          val1, mk_ival (set_signed signed (set_unsigned unsigned v))
        | _, _ -> val1, val2
      end

    | Dba.LeqU -> guard Dba.GeqU val2 val1

    | Dba.GtU  -> guard Dba.LtU val2 val1

    | Dba.LeqS ->
      begin
        match val1, val2 with
        | IVal Some v1, IVal Some v2 ->
          if same_region v1 v2 then
            let min = Bitvector.smin v1.signed.Signed.hi v2.signed.Signed.hi
            and max =
              Bitvector.smax v1.signed.Signed.lo v2.signed.Signed.lo in
            let x =
              if Bitvector.sgt v1.signed.Signed.lo min then IVal None
              else
                let signed = Signed.create v1.signed.Signed.lo min in
                mk_ival (set_signed signed v1)
            and y =
              if Bitvector.slt v2.signed.Signed.hi max then IVal None
              else
                let signed = Signed.create max v2.signed.Signed.hi in
                mk_ival (set_signed signed v2)
            in x, y
          else val1, val2
        | Top, IVal Some v when is_constant_region v ->
          let signed = Signed.minify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          mk_ival (set_signed signed (set_unsigned unsigned v)), val2
        | IVal Some v, Top when is_constant_region v ->
          let signed = Signed.maxify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          val1, mk_ival (set_signed signed (set_unsigned unsigned v))
        | _, _ -> val1, val2
      end

    | Dba.LtS  ->
      begin
        match val1, val2 with
        | IVal Some v1, IVal Some v2 when same_region v1 v2 ->
          let open Signed in
          let min = Bitvector.smin (Bitvector.pred v2.signed.hi) v2.signed.hi
          and max = Bitvector.smax (Bitvector.succ v1.signed.lo) v2.signed.lo in
          let x =
            if Bitvector.sgt v1.signed.lo min then IVal None
            else
              let signed = Signed.create v1.signed.lo min in
              mk_ival (set_signed signed v1)
          and y =
            if Bitvector.slt v2.signed.hi max then IVal None
            else
              let signed = Signed.create max v2.signed.hi in
              mk_ival (set_signed signed v2)
          in x, y
        | Top, IVal Some v when is_constant_region v ->
          let signed = Signed.minify v.signed in
          let unsigned = Unsigned.topify v.unsigned in
          mk_ival (set_signed signed (set_unsigned unsigned v)), val2
        | IVal Some v, Top when is_constant_region v ->
          let signed = Signed.maxify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          val1, mk_ival (set_signed signed (set_unsigned unsigned v))
        | _, _ -> val1, val2
      end

    | Dba.GeqS ->
      begin
        match val1, val2 with
        | IVal Some v1,
          IVal Some v2 when same_region v1 v2 ->
          let min = Bitvector.smin v1.signed.Signed.hi v2.signed.Signed.hi
          and max = Bitvector.smax v2.signed.Signed.lo v2.signed.Signed.lo in
          let x =
            if Bitvector.sgt max v1.signed.Signed.hi then IVal None
            else
              let signed = Signed.create max v1.signed.Signed.hi in
              mk_ival (set_signed signed v1)
          and y =
              if Bitvector.slt min v2.signed.Signed.lo then IVal None
              else
                let signed = Signed.create v2.signed.Signed.lo min in
                mk_ival (set_signed signed v2)
          in x, y
        | Top, IVal Some v when is_constant_region v ->
          let signed = Signed.maxify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          mk_ival (set_signed signed (set_unsigned unsigned v)), val2
        | IVal Some v, Top when is_constant_region v ->
          let signed = Signed.minify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          val1, mk_ival (set_signed signed (set_unsigned unsigned v))
        | _, _ -> val1, val2
      end

    | Dba.GtS  ->
      begin
        match val1, val2 with
        | IVal Some v1,
          IVal Some v2 when same_region v1 v2 ->
          let open Signed in
          let min =
            Bitvector.smin (Bitvector.pred v1.signed.hi) v2.signed.hi
          and max =
            Bitvector.smax v2.signed.lo (Bitvector.succ v2.signed.lo) in
          let x =
            if Bitvector.sgt max v1.signed.hi then IVal None
            else
              let signed = Signed.create max v1.signed.hi in
              mk_ival (set_signed signed v1)
          and y =
            if Bitvector.slt min v2.signed.lo then IVal None
            else
              let signed = Signed.create v2.signed.lo min in
              mk_ival (set_signed signed v2)
          in x, y
        | Top, IVal Some v when is_constant_region v ->
          let signed = Signed.maxify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          mk_ival (set_signed signed (set_unsigned unsigned v)), val2
        | IVal Some v, Top when is_constant_region v ->
          let signed = Signed.minify v.signed
          and unsigned = Unsigned.topify v.unsigned in
          val1, mk_ival (set_signed signed (set_unsigned unsigned v))
        | _, _ -> val1, val2
      end
    | _ -> val1, val2
  in
  Logger.debug ~level "@[Post guard : %a %a %a@]"
    Dba_printer.Ascii.pp_binary_op binop pp v1 pp v2;
  v1, v2


let elements value =
  match refine value with
  | IVal None -> []
  | Top -> raise Elements_of_top
  | IVal Some v ->
    match Unsigned.map (fun bv -> `Value (v.region, bv)) v.unsigned with
    | Some l -> l
    | None -> raise Elements_of_top


let restrict value off1 off2 =
  Logger.debug "Restrict %a %d %d" pp value off1 off2;
  assert (off1 >= 0);
  assert (off2 >= off1);
  match refine value with
  | Top -> Top
  | IVal None -> IVal None
  | IVal Some v (*{ region =`Constant; unsigned = (l1, u1); signed = _})*) when
      is_constant_region v ->
    let open Unsigned in
    let ulo = Bitvector.extract v.unsigned.lo off1 off2 in
    let uhi = Bitvector.extract v.unsigned.hi off1 off2 in
    let unsigned = Unsigned.create ulo uhi in
    let len = off2 - off1 + 1 in
    let signed = Signed.top len in
    let v' = mk_ival (set_signed signed (set_unsigned unsigned v)) in
    if Unsigned.is_point v.unsigned
    || off1 = 0 && off2 = 7
       && Bitvector.ule
         v.unsigned.hi
         (Bitvector.extend
            (Bitvector.max_ubv 8) (Bitvector.size_of v.unsigned.hi))
    || off2 = Bitvector.size_of v.unsigned.lo - 1
    then v'
    else
      let len = off2 - off1 + 1 in
      let signed = Signed.top len
      and unsigned = Unsigned.top len in
      mk_ival (set_signed signed (set_unsigned unsigned v))
  | IVal (Some _) -> Top


let concat val1 val2 =
  let val1 = refine val1 in
  let val2 = refine val2 in
  match val1, val2 with
  | IVal Some v1, IVal Some v2 when same_region v1 v2 && is_constant_region v1
    ->
    let open Unsigned in
    let lo = Bitvector.append v1.unsigned.lo v2.unsigned.lo in
    let hi = Bitvector.append v1.unsigned.hi v2.unsigned.hi in
    let unsigned = Unsigned.create lo hi in
    let signed = Signed.top (Bitvector.size_of lo) in
    mk_ival (set_signed signed (set_unsigned unsigned v1))
  | IVal None, _ | _, IVal None -> IVal None
  | _, _ -> Top


let to_smt value (var: Smtlib2.smt_bv_expr) : Smtlib2.smt_bv_expr list =
  let open Smtlib2 in
   match value with
  | Top -> []
  | IVal None -> []
  | IVal Some v ->
    let u_low_bound  = SmtBvCst v.unsigned.Unsigned.lo in
    let u_high_bound = SmtBvCst v.unsigned.Unsigned.hi in
    let s_low_bound  = SmtBvCst v.signed.Signed.lo in
    let s_high_bound = SmtBvCst v.signed.Signed.hi in
    [SmtBvBinary (SmtBvUle, u_low_bound, var);
     SmtBvBinary (SmtBvUle, var, u_high_bound);
     SmtBvBinary (SmtBvSle, s_low_bound, var);
     SmtBvBinary (SmtBvSle, var, s_high_bound)]


let smt_refine ival smt_env var =
  match refine ival with
  | Top -> Top
  | IVal None -> IVal None
  | IVal Some v  ->
    let open Unsigned in
    let hi = Normalize_instructions.get_upper_bound smt_env var v.unsigned.hi
    and lo = Normalize_instructions.get_lower_bound smt_env var v.unsigned.lo
    in let unsigned = Unsigned.create lo hi in mk_ival (set_unsigned unsigned v)
