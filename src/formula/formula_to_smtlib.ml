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
open Smtlib_utils

let mk_bv_string bv =
  Printf.sprintf "(_ bv%s %i)"
    (Z.to_string (Bitvector.value_of bv))
    (Bitvector.size_of bv)

let mk_bitvec_symbol sz = mk_symbol (Printf.sprintf "(_ BitVec %i)" sz)

let mk_array_symbol idx elt =
  mk_symbol (Printf.sprintf "(Array (_ BitVec %i) (_ BitVec %i))" idx elt)

let mk_select ax bv =
  mk_term_qual_identifier_terms
    (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol "select")))
    [ ax; bv ]

let mk_store ax bv1 bv2 =
  mk_term_qual_identifier_terms
    (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol "store")))
    [ ax; bv1; bv2 ]

let mk_concat =
  mk_term_qual_identifier_terms
    (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol "concat")))

let visit_bl_unop u = match u with BlNot -> mk_id_symbol (mk_symbol "not")

let visit_bl_bnop b =
  match b with
  | BlImply -> mk_id_symbol (mk_symbol "=>")
  | BlAnd -> mk_id_symbol (mk_symbol "and")
  | BlOr -> mk_id_symbol (mk_symbol "or")
  | BlXor -> mk_id_symbol (mk_symbol "xor")

let visit_bl_comp c =
  match c with
  | BlEqual -> mk_id_symbol (mk_symbol "=")
  | BlDistinct -> mk_id_symbol (mk_symbol "distinct")

let visit_bv_comp c =
  match c with
  | BvEqual -> mk_id_symbol (mk_symbol "=")
  | BvDistinct -> mk_id_symbol (mk_symbol "distinct")
  | BvUlt -> mk_id_symbol (mk_symbol "bvult")
  | BvUle -> mk_id_symbol (mk_symbol "bvule")
  | BvUgt -> mk_id_symbol (mk_symbol "bvugt")
  | BvUge -> mk_id_symbol (mk_symbol "bvuge")
  | BvSlt -> mk_id_symbol (mk_symbol "bvslt")
  | BvSle -> mk_id_symbol (mk_symbol "bvsle")
  | BvSgt -> mk_id_symbol (mk_symbol "bvsgt")
  | BvSge -> mk_id_symbol (mk_symbol "bvsge")

let visit_ax_comp c =
  match c with
  | AxEqual -> mk_id_symbol (mk_symbol "=")
  | AxDistinct -> mk_id_symbol (mk_symbol "distinct")

let visit_bv_unop u =
  match u with
  | BvNot -> mk_id_symbol (mk_symbol "bvnot")
  | BvNeg -> mk_id_symbol (mk_symbol "bvneg")
  | BvRepeat i -> mk_id_underscore (mk_symbol "repeat") [ mk_idx_num i ]
  | BvZeroExtend i ->
      mk_id_underscore (mk_symbol "zero_extend") [ mk_idx_num i ]
  | BvSignExtend i ->
      mk_id_underscore (mk_symbol "sign_extend") [ mk_idx_num i ]
  | BvRotateLeft i ->
      mk_id_underscore (mk_symbol "rotate_left") [ mk_idx_num i ]
  | BvRotateRight i ->
      mk_id_underscore (mk_symbol "rotate_right") [ mk_idx_num i ]
  | BvExtract i ->
      mk_id_underscore (mk_symbol "extract")
        Interval.[ mk_idx_num i.hi; mk_idx_num i.lo ]

let visit_bv_bnop b =
  match b with
  | BvConcat -> mk_id_symbol (mk_symbol "concat")
  | BvAnd -> mk_id_symbol (mk_symbol "bvand")
  | BvNand -> mk_id_symbol (mk_symbol "bvnand")
  | BvOr -> mk_id_symbol (mk_symbol "bvor")
  | BvNor -> mk_id_symbol (mk_symbol "bvnor")
  | BvXor -> mk_id_symbol (mk_symbol "bvxor")
  | BvXnor -> mk_id_symbol (mk_symbol "bvxnor")
  | BvCmp -> mk_id_symbol (mk_symbol "bvcomp")
  | BvAdd -> mk_id_symbol (mk_symbol "bvadd")
  | BvSub -> mk_id_symbol (mk_symbol "bvsub")
  | BvMul -> mk_id_symbol (mk_symbol "bvmul")
  | BvUdiv -> mk_id_symbol (mk_symbol "bvudiv")
  | BvSdiv -> mk_id_symbol (mk_symbol "bvsdiv")
  | BvUrem -> mk_id_symbol (mk_symbol "bvurem")
  | BvSrem -> mk_id_symbol (mk_symbol "bvsrem")
  | BvSmod -> mk_id_symbol (mk_symbol "bvsmod")
  | BvShl -> mk_id_symbol (mk_symbol "bvshl")
  | BvAshr -> mk_id_symbol (mk_symbol "bvashr")
  | BvLshr -> mk_id_symbol (mk_symbol "bvlshr")

let visit_sort = function
  | BlSort -> mk_sort_identifier (mk_symbol "Bool")
  | BvSort i -> mk_sort_identifier (mk_bitvec_symbol i)
  | AxSort (i, j) -> mk_sort_identifier (mk_array_symbol i j)

let get_elt, get_idx =
  let get_bv str bv =
    let v =
      bv_var (Printf.sprintf "%s_%x" str bv.bv_term_hash) bv.bv_term_size
    in
    (mk_bv_var v, [ mk_bv_def v [] bv ])
  in
  ( (fun str bv ->
      match Formula_utils.is_bv_cst bv with
      | Some _ -> (bv, [])
      | None -> (
          match Formula_utils.is_bv_var bv with
          | Some _ -> (bv, [])
          | None -> get_bv str bv)),
    fun str bv ->
      match bv.bv_term_desc with
      | BvCst _ | BvFun (_, []) -> (bv, [])
      | BvBnop ((BvAdd as b), bv1, bv2) | BvBnop ((BvSub as b), bv1, bv2) -> (
          match Formula_utils.is_bv_cst bv2 with
          | None -> get_bv str bv
          | Some _ ->
              let bv1, ls =
                match Formula_utils.is_bv_var bv1 with
                | None -> get_bv str bv1
                | Some _ -> (bv1, [])
              in
              (mk_bv_bnop b bv1 bv2, ls))
      | _ -> get_bv str bv )

let get_arr str ax =
  match Formula_utils.is_ax_var ax with
  | Some _ -> (ax, [])
  | None ->
      let v =
        ax_var
          (Printf.sprintf "%s_%x" str ax.ax_term_hash)
          ax.idx_term_size ax.elt_term_size
      in
      (mk_ax_var v, [ mk_ax_def v [] ax ])

let visit_list : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
 fun f env ls -> List.map (f env) ls

let rec visit_term_desc env = function
  | BlTerm bl -> visit_bl_term env bl
  | BvTerm bv -> visit_bv_term env bv
  | AxTerm ax -> visit_ax_term env ax

and visit_term env { term_desc; _ } = visit_term_desc env term_desc

and visit_bl_term_desc env = function
  | BlTrue -> mk_term_spec_constant (Smtlib.CstBool true)
  | BlFalse -> mk_term_spec_constant (Smtlib.CstBool false)
  | BlFun (v, ls) -> (
      let ls = visit_list visit_term env ls in
      match ls with
      | [] ->
          mk_term_qual_identifier
            (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol v.bl_name)))
      | _ ->
          mk_term_qual_identifier_terms
            (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol v.bl_name)))
            ls)
  | BlLet (bn, bl) ->
      let bl = visit_bl_term env bl in
      visit_bind env bn bl
  | BlUnop (u, bl) ->
      let bl = visit_bl_term env bl in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_bl_unop u))
        [ bl ]
  | BlBnop (b, bl1, bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_bl_bnop b))
        [ bl1; bl2 ]
  | BlComp (c, bl1, bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_bl_comp c))
        [ bl1; bl2 ]
  | BvComp (c, bv1, bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_bv_comp c))
        [ bv1; bv2 ]
  | AxComp (c, ax1, ax2) ->
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_ax_comp c))
        [ ax1; ax2 ]
  | BlIte (bl, bl1, bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol "ite")))
        [ bl; bl1; bl2 ]

and visit_bl_term env { bl_term_desc; _ } = visit_bl_term_desc env bl_term_desc

and visit_bv_term_desc env = function
  | BvCst bv -> mk_term_spec_constant (Smtlib.CstNumeral (mk_bv_string bv))
  | BvFun (v, ls) -> (
      let ls = visit_list visit_term env ls in
      match ls with
      | [] ->
          mk_term_qual_identifier
            (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol v.bv_name)))
      | _ ->
          mk_term_qual_identifier_terms
            (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol v.bv_name)))
            ls)
  | BvLet (bn, bv) ->
      let bv = visit_bv_term env bv in
      visit_bind env bn bv
  | BvUnop (u, bv) ->
      let bv = visit_bv_term env bv in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_bv_unop u))
        [ bv ]
  | BvBnop (b, bv1, bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (visit_bv_bnop b))
        [ bv1; bv2 ]
  | BvIte (bl, bv1, bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol "ite")))
        [ bl; bv1; bv2 ]
  | Select (1, ax, bv) ->
      let ax = visit_ax_term env ax in
      let bv = visit_bv_term env bv in
      mk_select ax bv
  | Select (n, ax, bv) ->
      let ax, ls_ax = get_arr "ax" ax in
      let bv, ls_bv = get_idx "tmp" bv in
      let ax = visit_ax_term env ax in
      visit_bind env (ls_ax @ ls_bv) (mk_concat (unfold_select env ax bv [] n))

and visit_bv_term env { bv_term_desc; _ } = visit_bv_term_desc env bv_term_desc

and visit_ax_term_desc env = function
  | AxFun (v, ls) -> (
      let ls = visit_list visit_term env ls in
      match ls with
      | [] ->
          mk_term_qual_identifier
            (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol v.ax_name)))
      | _ ->
          mk_term_qual_identifier_terms
            (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol v.ax_name)))
            ls)
  | AxLet (bn, ax) ->
      let ax = visit_ax_term env ax in
      visit_bind env bn ax
  | AxIte (bl, ax1, ax2) ->
      let bl = visit_bl_term env bl in
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_term_qual_identifier_terms
        (mk_qual_identifier_identifier (mk_id_symbol (mk_symbol "ite")))
        [ bl; ax1; ax2 ]
  | Store (1, ax, bv1, bv2) ->
      let ax = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_store ax bv1 bv2
  | Store (n, ax, bv1, bv2) ->
      let ax, ls_ax = get_arr "ax" ax in
      let bv1, ls_bv1 = get_idx "idx" bv1 in
      let bv2, ls_bv2 = get_elt "elt" bv2 in
      let elt_size = ax.elt_term_size in
      let ax = visit_ax_term env ax in
      visit_bind env
        (ls_ax @ ls_bv1 @ ls_bv2)
        (unfold_store env elt_size ax bv1 bv2 n)

and visit_ax_term env { ax_term_desc; _ } = visit_ax_term_desc env ax_term_desc

and unfold_select env ax bv l = function
  | 1 -> mk_select ax (visit_bv_term env bv) :: l
  | n ->
      unfold_select env ax (mk_bv_add_int bv 1)
        (mk_select ax (visit_bv_term env bv) :: l)
        (n - 1)

and unfold_store env elt_size ax bv1 bv2 = function
  | 1 -> mk_store ax (visit_bv_term env bv1) (visit_bv_term env bv2)
  | n ->
      let lsb = mk_bv_extract { Interval.lo = 0; hi = elt_size - 1 } bv2 in
      let msb =
        mk_bv_extract { Interval.lo = elt_size; hi = (n * elt_size) - 1 } bv2
      in
      unfold_store env elt_size
        (mk_store ax (visit_bv_term env bv1) (visit_bv_term env lsb))
        (mk_bv_add_int bv1 1) msb (n - 1)

and visit_bind env ls term =
  match ls with
  | [] -> term
  | _ ->
      let ls =
        List.map
          (fun (symbol, _sort, sorts, term) ->
            assert (sorts = []);
            mk_var_binding symbol term)
          (visit_list visit_def env ls)
      in
      mk_term_let_term ls term

and visit_def_desc env = function
  | BlDef (v, ls, bl) ->
      ( mk_symbol v.bl_name,
        mk_sort_identifier (mk_symbol "Bool"),
        visit_list visit_decl env ls,
        visit_bl_term env bl )
  | BvDef (v, ls, bv) ->
      ( mk_symbol v.bv_name,
        mk_sort_identifier (mk_bitvec_symbol v.bv_size),
        visit_list visit_decl env ls,
        visit_bv_term env bv )
  | AxDef (v, ls, ax) ->
      ( mk_symbol v.ax_name,
        mk_sort_identifier (mk_array_symbol v.idx_size v.elt_size),
        visit_list visit_decl env ls,
        visit_ax_term env ax )

and visit_def env { def_desc; _ } = visit_def_desc env def_desc

and visit_decl_desc _ = function
  | BlDecl (v, ls) ->
      ( mk_symbol v.bl_name,
        mk_sort_identifier (mk_symbol "Bool"),
        List.map visit_sort ls )
  | BvDecl (v, ls) ->
      ( mk_symbol v.bv_name,
        mk_sort_identifier (mk_bitvec_symbol v.bv_size),
        List.map visit_sort ls )
  | AxDecl (v, ls) ->
      ( mk_symbol v.ax_name,
        mk_sort_identifier (mk_array_symbol v.idx_size v.elt_size),
        List.map visit_sort ls )

and visit_decl env { decl_desc; _ } = visit_decl_desc env decl_desc

and visit_entry_desc env = function
  | Declare dc ->
      let symbol, sort, sorts = visit_decl env dc in
      mk_cmd_declare_fun symbol sorts sort
  | Define df ->
      let symbol, sort, sorts, term = visit_def env df in
      let sorted_vars =
        List.map
          (fun (symbol, sort, sorts) ->
            assert (sorts = []);
            mk_sorted_var symbol sort)
          sorts
      in
      mk_cmd_define_fun (mk_fun_def symbol sort sorted_vars term)
  | Assert bl | Assume bl ->
      let bl = visit_bl_term env bl in
      mk_command (Smtlib.CmdAssert bl)
  | Comment c -> mk_command (Smtlib.CmdComment c)

and visit_entry env { entry_desc; _ } = visit_entry_desc env entry_desc

and visit_formula env { entries; _ } =
  Sequence.map_forward (visit_entry env) entries

let bl_term bl_term = visit_bl_term () bl_term
let bv_term bv_term = visit_bv_term () bv_term
let ax_term ax_term = visit_ax_term () ax_term
let term term = visit_term () term
let entry entry = visit_entry () entry
let list_of_sequence seq = Sequence.fold_backward (fun x acc -> x :: acc) seq []

let formula ast =
  let open Smtlib in
  let script_commands =
    visit_formula () ast
    |> Sequence.push_back (mk_command (CmdSetLogic (mk_symbol "QF_ABV")))
    |> Sequence.push_front (mk_command CmdCheckSat)
    |> Sequence.push_front (mk_command CmdExit)
    |> list_of_sequence
  in
  let script_loc = Location.dummy_loc in
  { script_commands; script_loc }
