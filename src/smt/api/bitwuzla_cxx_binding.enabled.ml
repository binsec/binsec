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

module Bitvector = Binsec.Bitvector
module N = Bitwuzla_cxx
module Term = Bitwuzla_cxx.Term
module Sort = Bitwuzla_cxx.Sort
module Solver = Bitwuzla_cxx.Solver

let bv_sort =
  let module Tbl = Binsec.Basic_types.Int.Htbl in
  let tbl = Tbl.create 32 in
  fun sz ->
    try Tbl.find tbl sz
    with Not_found ->
      let sort = N.mk_bv_sort sz in
      Tbl.add tbl sz sort;
      sort

let bv_one = N.mk_bv_one (bv_sort 1)
let bv_zero = N.mk_bv_zero (bv_sort 1)

let of_z s v =
  if Z.fits_int v then N.mk_bv_value_int s (Z.to_int v)
  else N.mk_bv_value s (Z.format "%x" v) 16

module Bl = struct
  type t = Term.t

  let sort = N.mk_bool_sort ()
  let const name = N.mk_const ~symbol:name sort
  let top = N.mk_true ()
  let bot = N.mk_false ()
  let lognot x = N.mk_term1 Not x
  let logand x y = N.mk_term2 And x y
  let logor x y = N.mk_term2 Or x y
  let logxor x y = N.mk_term2 Xor x y
  let ite x y z = N.mk_term3 Ite x y z
  let equal x y = N.mk_term2 Equal x y
  let diff x y = N.mk_term2 Distinct x y
  let implies x y = N.mk_term2 Implies x y
  let to_bv x = N.mk_term3 Ite x bv_one bv_zero
end

module Bv = struct
  type t = Term.t

  let const size name = N.mk_const ~symbol:name (bv_sort size)
  let value sz bv = of_z (bv_sort sz) bv
  let equal x y = N.mk_term2 Equal x y
  let diff x y = N.mk_term2 Distinct x y
  let ule x y = N.mk_term2 Bv_ule x y
  let uge x y = N.mk_term2 Bv_uge x y
  let ult x y = N.mk_term2 Bv_ult x y
  let ugt x y = N.mk_term2 Bv_ugt x y
  let sle x y = N.mk_term2 Bv_sle x y
  let sge x y = N.mk_term2 Bv_sge x y
  let slt x y = N.mk_term2 Bv_slt x y
  let sgt x y = N.mk_term2 Bv_sgt x y
  let add x y = N.mk_term2 Bv_add x y
  let sub x y = N.mk_term2 Bv_sub x y
  let mul x y = N.mk_term2 Bv_mul x y
  let neg x = N.mk_term1 Bv_neg x
  let udiv x y = N.mk_term2 Bv_udiv x y
  let umod x y = N.mk_term2 Bv_urem x y
  let urem x y = N.mk_term2 Bv_urem x y
  let sdiv x y = N.mk_term2 Bv_sdiv x y
  let smod x y = N.mk_term2 Bv_smod x y
  let srem x y = N.mk_term2 Bv_srem x y
  let logand x y = N.mk_term2 Bv_and x y
  let logor x y = N.mk_term2 Bv_or x y
  let lognot x = N.mk_term1 Bv_not x
  let logxor x y = N.mk_term2 Bv_xor x y
  let lognand x y = N.mk_term2 Bv_nand x y
  let lognor x y = N.mk_term2 Bv_nor x y
  let logxnor x y = N.mk_term2 Bv_xnor x y
  let shift_left x y = N.mk_term2 Bv_shl x y
  let shift_right x y = N.mk_term2 Bv_shr x y
  let shift_right_signed x y = N.mk_term2 Bv_ashr x y
  let rotate_left x y = N.mk_term2 Bv_rol x y
  let rotate_right x y = N.mk_term2 Bv_ror x y
  let rotate_lefti x i = N.mk_term1_indexed1 Bv_roli x i
  let rotate_righti x i = N.mk_term1_indexed1 Bv_rori x i
  let append x y = N.mk_term2 Bv_concat x y
  let extract ~hi ~lo x = N.mk_term1_indexed2 Bv_extract x hi lo
  let uext n x = N.mk_term1_indexed1 Bv_zero_extend x n
  let sext n x = N.mk_term1_indexed1 Bv_sign_extend x n
  let ite x y z = N.mk_term3 Ite x y z
  let succ x = N.mk_term1 Bv_inc x
  let to_bl x = N.mk_term2 Equal x bv_one
end

module Ax = struct
  type t = Term.t
  type nonrec sort = Sort.t

  let sort ~idx elm = N.mk_array_sort (bv_sort idx) (bv_sort elm)
  let const sort name = N.mk_const ~symbol:name sort
  let store ax idx v = N.mk_term3 Store ax idx v
  let select ax idx = N.mk_term2 Select ax idx
  let ite x y z = N.mk_term3 Ite x y z
  let equal x y = N.mk_term2 Equal x y
  let diff x y = N.mk_term2 Distinct x y
end

let interrupt f () =
  match Unix.sigpending () with
  | [] | (exception Invalid_argument _) -> f ()
  | l -> List.mem Sys.sigint l || f ()

let mk_timeout t =
  if t = 0. then interrupt (Fun.const false)
  else
    let t = Unix.gettimeofday () +. t in
    interrupt (fun () -> Float.compare (Unix.gettimeofday ()) t >= 0)

module Session () : Common.S = struct
  module Bl = Bl
  module Bv = Bv
  module Ax = Ax

  let solver =
    let option = Bitwuzla_cxx.Options.default () in
    Bitwuzla_cxx.Options.set option Produce_models true;
    Bitwuzla_cxx.Options.set option Pp_skeleton_preproc false;
    Bitwuzla_cxx.Solver.create option

  let assert_formula bl = Solver.assert_formula solver bl
  let push () = Solver.push solver 1
  let pop () = Solver.pop solver 1

  let check_sat ?(timeout = 0.) () : Common.status =
    Solver.configure_terminator solver (Some (mk_timeout timeout));
    match Solver.check_sat solver with
    | Sat -> Sat
    | Unsat -> Unsat
    | Unknown -> Unknown

  let check_sat_assuming ?(timeout = 0.) e : Common.status =
    Solver.configure_terminator solver (Some (mk_timeout timeout));
    match Solver.check_sat ~assumptions:[| e |] solver with
    | Sat -> Sat
    | Unsat -> Unsat
    | Unknown -> Unknown

  let get_bv_value bv = Term.value Z (Solver.get_value solver bv)

  let rec fold_ax_values f ax v =
    match Term.kind ax with
    | Const_array -> v
    | Store ->
        fold_ax_values f (Term.get ax 0)
          (f (Term.value Z (Term.get ax 1)) (Term.value Z (Term.get ax 2)) v)
    | _ -> assert false

  let fold_ax_values f ax v = fold_ax_values f (Solver.get_value solver ax) v
  let close () = Solver.unsafe_delete solver
end

let factory = Some (module Session : Common.F)
