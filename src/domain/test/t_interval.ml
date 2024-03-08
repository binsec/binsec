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

open Binsec
open Domains

exception Skip

let for_all =
  let rec loop i n p =
    i >= n || ((try p i with Skip -> true) && loop (i + 1) n p)
  in
  loop 0

let for_all_interval size p =
  for_all (size + 1) (fun stride ->
      for_all (1 lsl stride) (fun rem ->
          for_all
            (1 lsl (size - stride))
            (fun i ->
              let min = (i lsl stride) lor rem in
              for_all
                ((1 lsl (size - stride)) - i)
                (fun j ->
                  let max = min + (j lsl stride) in
                  let stride = if min = max then size else stride in
                  p
                    (Interval.create ~size ~min:(Z.of_int min)
                       ~max:(Z.of_int max) ~stride)))))

let%test "fold" =
  for_all_interval 8 (fun it ->
      Interval.fold Z.add Z.zero ~size:8 it = Interval.sum ~size:8 it)

let%test "iter" =
  for_all_interval 8 (fun it ->
      let n = ref Z.zero in
      ignore (Interval.iter (fun i -> n := Z.add !n i) ~size:8 it);
      !n = Interval.fold Z.add Z.zero ~size:8 it)

let%test "for_all" =
  for_all_interval 8 (fun it ->
      let n = ref Z.zero in
      ignore
        (Interval.for_all
           (fun i ->
             n := Z.add !n i;
             true)
           ~size:8 it);
      !n = Interval.fold Z.add Z.zero ~size:8 it)

let app1 f i size = Bitvector.value_of (f (Bitvector.create i size))

let unary f c size size' =
  for_all_interval size (fun it ->
      let (it' : Interval.t) = f ~size it in
      Z.equal it'.min it'.max = (it'.stride = size')
      && (it.stride <> size || it'.stride = size')
      (* |> fun b ->
         * if not b then
         *   Format.eprintf "op %a -> %a@ " Interval.pp it Interval.pp it';
         * b *)
      && Interval.for_all
           (fun i -> Interval.mem (app1 c i size) ~size it')
           ~size it
         |> fun b ->
         if not b then
           Format.eprintf "op %a -> %a@ " Interval.pp it Interval.pp it';
         b)

(* let binary f c size =
 *   for_all_interval size (fun it ->
 *       Interval.for_all
 *         (fun i -> unary (f it) (c (Bitvector.create i size)) size)
 *         it) *)

let app2 f i i' size =
  Bitvector.value_of (f (Bitvector.create i size) (Bitvector.create i' size))

let binary f c size size' =
  for_all_interval size (fun it ->
      for_all_interval size (fun it' ->
          let (it'' : Interval.t) = f ~size it it' in
          Z.equal it''.min it''.max = (it''.stride = size')
          && (it.stride <> size || it'.stride <> size || it''.stride = size'
             || (ignore (app2 c it.min it'.min size);
                 false)
                |> fun b ->
                if not b then
                  Format.eprintf "%a op %a -> %a@ " Interval.pp it Interval.pp
                    it' Interval.pp it'';
                b)
          && Interval.for_all
               (fun i ->
                 Interval.for_all
                   (fun i' ->
                     Interval.mem (app2 c i i' size) ~size it'' |> fun b ->
                     if not b then
                       Format.eprintf "%a op %a -> %a@ " Interval.pp it
                         Interval.pp it' Interval.pp it'';
                     b)
                   ~size it')
               ~size it))

let ext f c size =
  for_all size (fun k ->
      let size' = size + k in
      unary (f k) (c size') size size')

let div f x y = try f x y with Division_by_zero -> raise_notrace Skip
let cmp f x y = Bitvector.of_bool (f x y)
let shift f x y = f x (Bitvector.to_uint y)
let%test "uminus" = unary Interval.uminus Bitvector.neg 4 4
let%test "add" = binary Interval.add Bitvector.add 4 4
let%test "sub" = binary Interval.sub Bitvector.sub 4 4
let%test "mul" = binary Interval.mul Bitvector.mul 4 4
let%test "smod" = binary Interval.smod (div Bitvector.smod) 4 4
let%test "umod" = binary Interval.umod (div Bitvector.umod) 4 4
let%test "udiv" = binary Interval.udiv (div Bitvector.udiv) 4 4
let%test "sdiv" = binary Interval.sdiv (div Bitvector.sdiv) 4 4

let%test "append" =
  binary
    (fun ~size x y -> Interval.append ~size1:size x ~size2:size y)
    Bitvector.append 4 8

let%test "equal" = binary Interval.equal (cmp Bitvector.equal) 4 1
let%test "diff" = binary Interval.diff (cmp Bitvector.diff) 4 1
let%test "ule" = binary Interval.ule (cmp Bitvector.ule) 4 1
let%test "uge" = binary Interval.uge (cmp Bitvector.uge) 4 1
let%test "ult" = binary Interval.ult (cmp Bitvector.ult) 4 1
let%test "ugt" = binary Interval.ugt (cmp Bitvector.ugt) 4 1
let%test "sle" = binary Interval.sle (cmp Bitvector.sle) 4 1
let%test "sge" = binary Interval.sge (cmp Bitvector.sge) 4 1
let%test "slt" = binary Interval.slt (cmp Bitvector.slt) 4 1
let%test "sgt" = binary Interval.sgt (cmp Bitvector.sgt) 4 1
let%test "logand" = binary Interval.logand Bitvector.logand 4 4
let%test "logor" = binary Interval.logor Bitvector.logor 4 4
let%test "logxor" = binary Interval.logxor Bitvector.logxor 4 4
let%test "lognot" = unary Interval.lognot Bitvector.lognot 4 4

let%test "shift_left" =
  binary Interval.shift_left (shift Bitvector.shift_left) 4 4

let%test "shift_right" =
  binary Interval.shift_right (shift Bitvector.shift_right) 4 4

let%test "shift_right_signed" =
  binary Interval.shift_right_signed (shift Bitvector.shift_right_signed) 4 4

let%test "rotate_left" =
  binary Interval.rotate_left (shift Bitvector.rotate_left) 4 4

let%test "rotate_right" =
  binary Interval.rotate_right (shift Bitvector.rotate_right) 4 4

let%test "uext" = ext Interval.uext (fun n x -> Bitvector.extend x n) 4
let%test "sext" = ext Interval.sext (fun n x -> Bitvector.extend_signed x n) 4

let%test "restrict" =
  for_all_interval 2 (fun it ->
      let hi = Z.to_int it.max and lo = Z.to_int it.min in
      unary
        (Interval.restrict ~hi ~lo)
        ((Fun.flip Bitvector.extract) { hi; lo })
        4
        (hi - lo + 1))

let%test "union" =
  for_all_interval 4 (fun it ->
      for_all_interval 4 (fun it' ->
          let it'' = Interval.union ~size:4 it it' in
          Interval.for_all (fun i -> Interval.mem i ~size:4 it'') ~size:4 it
          && Interval.for_all (fun i -> Interval.mem i ~size:4 it'') ~size:4 it'))

let%test "included" =
  for_all_interval 4 (fun it ->
      for_all_interval 4 (fun it' ->
          Interval.for_all ((Fun.flip (Interval.mem ~size:4)) it') ~size:4 it
          = Interval.included ~size:4 it it'))

let%test "disjoint" =
  for_all_interval 4 (fun it ->
      for_all_interval 4 (fun it' ->
          (not (Interval.disjoint ~size:4 it it'))
          || Interval.for_all
               (fun i ->
                 Interval.for_all (fun i' -> not (Z.equal i i')) ~size:4 it')
               ~size:4 it))

let%test "inter" =
  for_all_interval 4 (fun it ->
      for_all_interval 4 (fun it' ->
          let it'' =
            try Some (Interval.inter ~size:4 it it') with Empty -> None
          in
          Option.fold ~none:true
            ~some:(fun (it : Interval.t) ->
              Z.equal it.min it.max = (it.stride = 4))
            it''
          && Interval.for_all
               (fun i ->
                 Interval.for_all
                   (fun i' ->
                     if Z.equal i i' then
                       Option.fold ~none:false ~some:(Interval.mem ~size:4 i)
                         it''
                       (* |> fun b ->
                        * if not b then
                        *   Format.eprintf "%a /\\ %a -> %a@ " Interval.pp it
                        *     Interval.pp it'
                        *     (fun ppf it ->
                        *       match it with
                        *       | None -> Format.fprintf ppf "none"
                        *       | Some it -> Interval.pp ppf it)
                        *     it'';
                        * b *)
                     else true)
                   ~size:4 it')
               ~size:4 it))

let unary_feedback c b size =
  for_all_interval size (fun x ->
      for_all_interval size (fun r ->
          try
            let x' = b ~size x r in
            Interval.for_all
              (fun i ->
                let bv = Bitvector.create i size in
                (not (Interval.mem (Bitvector.value_of (c bv)) ~size r))
                || Interval.mem i ~size x')
              ~size x
            |> fun b ->
            if not b then
              Format.eprintf "o %a = %a -> %a @ " Interval.pp x Interval.pp r
                Interval.pp x';
            b
          with Empty ->
            Interval.for_all
              (fun i ->
                let bv = Bitvector.create i size in
                not (Interval.mem (Bitvector.value_of (c bv)) ~size r))
              ~size x
            |> fun b ->
            if not b then
              Format.eprintf "o %a = %a -> {}@ @ " Interval.pp x Interval.pp r;
            b))

let binary_feedback c b size size' =
  for_all_interval size (fun x ->
      for_all_interval size (fun y ->
          for_all_interval size' (fun r ->
              try
                let x', y' = b ~size x y r in
                (* if x' != x || y' != y then (
                 *   Format.eprintf "%a o %a = %a -> %a o %a@ @ " Interval.pp x
                 *     Interval.pp y Interval.pp r Interval.pp x' Interval.pp y';
                 *   false)
                 * else *)
                Interval.for_all
                  (fun i ->
                    Interval.for_all
                      (fun j ->
                        let bv = Bitvector.create i size
                        and bv' = Bitvector.create j size in
                        (not
                           (Interval.mem
                              (Bitvector.value_of (c bv bv'))
                              ~size:size' r))
                        || (Interval.mem i ~size x' && Interval.mem j ~size y'))
                      ~size y)
                  ~size x
                |> fun b ->
                if not b then
                  Format.eprintf "%a o %a = %a -> %a o %a@ @ " Interval.pp x
                    Interval.pp y Interval.pp r Interval.pp x' Interval.pp y';
                b
              with Empty ->
                Interval.for_all
                  (fun i ->
                    Interval.for_all
                      (fun j ->
                        let bv = Bitvector.create i size
                        and bv' = Bitvector.create j size in
                        not
                          (Interval.mem
                             (Bitvector.value_of (c bv bv'))
                             ~size:size' r))
                      ~size y)
                  ~size x
                |> fun b ->
                if not b then
                  Format.eprintf "%a o %a = %a -> {}@ @ " Interval.pp x
                    Interval.pp y Interval.pp r;
                b)))

let cmp_feedback c b size = binary_feedback c b size 1
let binary_feedback c b size = binary_feedback c b size size

let%test "equal_feedback" =
  cmp_feedback (cmp Bitvector.equal) Interval.equal_feedback 4

let%test "diff_feedback" =
  cmp_feedback (cmp Bitvector.diff) Interval.diff_feedback 4

let%test "ule_feedback" =
  cmp_feedback (cmp Bitvector.ule) Interval.ule_feedback 4

let%test "uge_feedback" =
  cmp_feedback (cmp Bitvector.uge) Interval.uge_feedback 4

let%test "ult_feedback" =
  cmp_feedback (cmp Bitvector.ult) Interval.ult_feedback 4

let%test "ugt_feedback" =
  cmp_feedback (cmp Bitvector.ugt) Interval.ugt_feedback 4

let%test "add_feedback" = binary_feedback Bitvector.add Interval.add_feedback 3
let%test "sub_feedback" = binary_feedback Bitvector.sub Interval.sub_feedback 3

let%test "lognot_feedback" =
  unary_feedback Bitvector.lognot Interval.lognot_feedback 3

let%test "logand_feedback" =
  binary_feedback Bitvector.logand Interval.logand_feedback 3

let%test "logor_feedback" =
  binary_feedback Bitvector.logor Interval.logor_feedback 3

let%test "logxor_feedback" =
  binary_feedback Bitvector.logxor Interval.logxor_feedback 3
