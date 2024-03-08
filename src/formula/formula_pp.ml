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

open Formula
open Format

let pp_status fmt res =
  match res with
  | SAT -> fprintf fmt "@{<green>SAT@}"
  | UNSAT -> fprintf fmt "@{<red>UNSAT@}"
  | TIMEOUT -> fprintf fmt "@{<brown>TIMEOUT@}"
  | UNKNOWN -> fprintf fmt "@{<purple>UNKNOWN@}"

let print_status res =
  pp_status Format.str_formatter res;
  Format.flush_str_formatter ()

let pp_bvunop ppf = function
  | BvNeg -> fprintf ppf "bvneg"
  | BvNot -> fprintf ppf "bvnot"
  | BvRepeat i -> fprintf ppf "(_ repeat %i)" i
  | BvZeroExtend i -> fprintf ppf "(_ zero_extend %i)" i
  | BvSignExtend i -> fprintf ppf "(_ sign_extend %i)" i
  | BvRotateLeft i -> fprintf ppf "(_ rotate_left %i)" i
  | BvRotateRight i -> fprintf ppf "(_ rotate_right %i)" i
  | BvExtract i -> fprintf ppf "(_ extract %i %i)" i.Interval.hi i.Interval.lo

let pp_bvbnop ppf = function
  (* linear arithmetic *)
  | BvAdd -> fprintf ppf "bvadd"
  | BvSub -> fprintf ppf "bvsub"
  (* non-linear arithmetic *)
  | BvMul -> fprintf ppf "bvmul"
  | BvUdiv -> fprintf ppf "bvudiv"
  | BvSdiv -> fprintf ppf "bvsdiv"
  | BvUrem -> fprintf ppf "bvurem"
  | BvSrem -> fprintf ppf "bvsrem"
  | BvSmod -> fprintf ppf "bvsmod"
  (* logical *)
  | BvOr -> fprintf ppf "bvor"
  | BvNor -> fprintf ppf "bvnor"
  | BvAnd -> fprintf ppf "bvand"
  | BvNand -> fprintf ppf "bvnand"
  | BvXor -> fprintf ppf "bvxor"
  | BvXnor -> fprintf ppf "bvxnor"
  | BvConcat -> fprintf ppf "concat"
  | BvShl -> fprintf ppf "bvshl"
  | BvLshr -> fprintf ppf "bvlshr"
  | BvAshr -> fprintf ppf "bvashr"
  (* comparison *)
  | BvCmp -> fprintf ppf "bvcomp"

let pp_bvcomp ppf = function
  | BvEqual -> fprintf ppf "="
  | BvDistinct -> failwith "distinct"
  | BvUle -> fprintf ppf "bvule"
  | BvUlt -> fprintf ppf "bvult"
  | BvUge -> fprintf ppf "bvuge"
  | BvUgt -> fprintf ppf "bvugt"
  | BvSle -> fprintf ppf "bvsle"
  | BvSlt -> fprintf ppf "bvslt"
  | BvSge -> fprintf ppf "bvsge"
  | BvSgt -> fprintf ppf "bvsgt"

let print_bv_unop = Print_utils.string_from_pp pp_bvunop
let print_bv_bnop = Print_utils.string_from_pp pp_bvbnop
let print_bv_comp = Print_utils.string_from_pp pp_bvcomp
let pp_bl_term ppf bl = Smtlib_pp.pp_term ppf (Formula_to_smtlib.bl_term bl)
let pp_bv_term ppf bv = Smtlib_pp.pp_term ppf (Formula_to_smtlib.bv_term bv)
let pp_ax_term ppf ax = Smtlib_pp.pp_term ppf (Formula_to_smtlib.ax_term ax)
let pp_term ppf tm = Smtlib_pp.pp_term ppf (Formula_to_smtlib.term tm)

let pp_entry ppf en =
  match en.entry_desc with
  | Assume bl ->
      Format.fprintf ppf "@[<v 1>(assert; assume\n%a)@]" pp_bl_term bl
  | _ -> Smtlib_pp.pp_command ppf (Formula_to_smtlib.entry en)

let pp_formula ppf fm =
  Format.pp_open_vbox ppf 0;
  (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_entry)
    ppf
    (Sequence.fold_backward (fun en acc -> en :: acc) fm.entries []);
  Format.pp_close_box ppf ()

let print_bv_term e = Print_utils.string_from_pp pp_bv_term e
let print_ax_term e = Print_utils.string_from_pp pp_ax_term e
let print_bl_term e = Print_utils.string_from_pp pp_bl_term e
let bl_to_string name = Printf.sprintf "(declare-fun %s () Bool" name

let bv_to_string name size =
  Printf.sprintf "(declare-fun %s () (_ BitVec %i))" name size

let ax_to_string name size1 size2 =
  Printf.sprintf "(declare-fun %s () (Array (_ BitVec %i) (_ BitVec %i)))" name
    size1 size2

let pp_varset ppf set =
  VarSet.iter
    (function
      | BlVar bl -> Format.fprintf ppf "%s;@ " (bl_to_string bl.bl_name)
      | BvVar bv ->
          Format.fprintf ppf "%s;@ " (bv_to_string bv.bv_name bv.bv_size)
      | AxVar ax ->
          Format.fprintf ppf "%s;@ "
            (ax_to_string ax.ax_name ax.idx_size ax.elt_size))
    set

let print_varset set = Format.asprintf "@[<v 0>%a@]" pp_varset set

let pp_header ppf () =
  let theory = if Formula_options.Flatten_memory.get () then "BV" else "ABV" in
  fprintf ppf "@[<v 0>(set-logic QF_%s)@ (set-info :smt-lib-version 2.0)@ @]"
    theory

let print_header () = Print_utils.string_from_pp pp_header ()

let pp_as_comment pp ppf v =
  let out_functions = pp_get_formatter_out_functions ppf () in
  let out_newline () = out_functions.out_string "\n;;" 0 3 in
  pp_set_formatter_out_functions ppf { out_functions with out_newline };
  kfprintf
    (fun ppf ->
      pp_print_flush ppf ();
      (* It seems like not flushing could lead to errors, for example by
         interpreting the newline in the wrong context.
         So to be more sure, we flush here.
         And yes, we lose all the open boxes.
      *)
      pp_set_formatter_out_functions ppf out_functions)
    ppf ";; %a" pp v
