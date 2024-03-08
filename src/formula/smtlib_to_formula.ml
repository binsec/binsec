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

open Smtlib
open Format
open Formula
open Formula_options

let error msg loc =
  let msg = Format.asprintf "%a:%s" Location.pp_lines loc msg in
  failwith msg

let arity_error opname expected gotten =
  let msg =
    sprintf "Arity error on %s : expected %d, got %d" opname expected gotten
  in
  failwith msg

let unhandled_operator groupname opname =
  let msg = sprintf "Unhandle %s operator : %s" groupname opname in
  failwith msg

let bv_size_of_indexes = function
  | [ IdxNum s ] -> int_of_string s
  | [] | _ :: _ -> assert false

module Sort = struct
  type t =
    | Bool
    | BitVec of int
    | Array of int * int (* Handling only Array (_ BitVec n1) (_ BitVec n2) *)
    | Var of int

  let pp ppf = function
    | Bool -> Format.fprintf ppf "bool"
    | BitVec n -> Format.fprintf ppf "bitvec %d" n
    | Array (i, e) -> Format.fprintf ppf "array (%d,%d)" i e
    | Var d -> Format.fprintf ppf "var %d" d

  let bv n = BitVec n
  let array idx elt = Array (idx, elt)
  let boolean = Bool

  let fresh_var =
    let n = ref (-1) in
    fun () ->
      incr n;
      Var !n

  let of_string = function
    | "BitVec" -> bv 0
    | "Bool" -> boolean
    | "Array" -> array 0 0
    | sort -> failwith ("Unhandled sort symbol " ^ sort)

  let of_symbol symb =
    match symb.symbol_desc with
    | SimpleSymbol s -> of_string s
    | QuotedSymbol _ -> error "Unhandled quoted sort" symb.symbol_loc

  let of_id id =
    match id.id_desc with
    | IdSymbol symb -> of_symbol symb
    | IdUnderscore (symb, indexes) -> (
        match of_symbol symb with
        | Bool -> assert false
        | BitVec n ->
            assert (n = 0);
            assert (List.length indexes = 1);
            bv (bv_size_of_indexes indexes)
        | Array _ | Var _ -> assert false)

  let of_decl d =
    match d with
    | BlDecl _ -> Bool
    | BvDecl (bt, _) -> bv bt.bv_size
    | AxDecl (ax, _) -> array ax.idx_size ax.elt_size

  let bv_size = function
    | BitVec n -> n
    | Array _ | Var _ | Bool -> assert false

  let array_contents_bv_size = function
    | Array (_, m) -> m
    | BitVec _ | Var _ | Bool -> assert false

  let rec eval sort =
    match sort.sort_desc with
    | SortFun (id, sorts) -> (
        match of_id id with
        | Array (0, 0) -> (
            assert (List.length sorts = 2);
            match sorts with
            | s1 :: s2 :: _ -> Array (bv_size (eval s1), bv_size (eval s2))
            | _ -> assert false)
        | BitVec _ | Array _ | Var _ | Bool -> assert false)
    | SortIdentifier id -> of_id id

  let eq s1 s2 =
    match (s1, s2) with
    | Bool, Bool | BitVec _, BitVec _ | Array _, Array _ -> true
    | (BitVec _ | Array _ | Var _ | Bool), (BitVec _ | Array _ | Var _ | Bool)
      ->
        false

  let to_formula_sort s =
    match s with
    | Bool -> BlSort
    | BitVec i -> BvSort i
    | Array (idx, elt) -> AxSort (idx, elt)
    | Var _ -> assert false
end

type sorted_expr = { sort : Sort.t; term : Formula.term }

let sorted_expr sort term = { sort; term }

let bl_sorted_expr ble = sorted_expr Sort.boolean (mk_bl_term ble)
and bv_sorted_expr n bve = sorted_expr (Sort.bv n) (mk_bv_term bve)

and ax_sorted_expr idx elt axe =
  sorted_expr (Sort.array idx elt) (mk_ax_term axe)

let expr_of_name name sort =
  let expr =
    match sort with
    | Sort.Bool ->
        let bl = bl_var name in
        mk_bl_term (mk_bl_fun bl [])
    | Sort.BitVec n ->
        let bv = bv_var name n in
        mk_bv_term (mk_bv_fun bv [])
    | Sort.Array (i, e) ->
        let ax = ax_var name i e in
        mk_ax_term (mk_ax_fun ax [])
    | Sort.Var _ -> assert false
  in
  sorted_expr sort expr

let name_of_decl d =
  match d with
  | BlDecl (blv, _) -> blv.bl_name
  | BvDecl (bvv, _) -> bvv.bv_name
  | AxDecl (axv, _) -> axv.ax_name

let decl_of_name name decls sort =
  match sort with
  | Sort.Bool ->
      let bl_var = bl_var name in
      mk_bl_decl bl_var decls
  | Sort.BitVec n ->
      let bv_var = bv_var name n in
      mk_bv_decl bv_var decls
  | Sort.Array (i, e) ->
      let ax_var = ax_var name i e in
      mk_ax_decl ax_var decls
  | Sort.Var _ -> assert false

let decl_of_name name tys ty =
  let decls = List.map Sort.to_formula_sort tys in
  (* Not sure: was Ast.Var before *)
  decl_of_name name decls ty

let typerr texpected tgot =
  let msg = Format.sprintf "Expected type %s but got %s" texpected tgot in
  failwith msg

let unhide_blexpr term =
  match term.Formula.term_desc with
  | BlTerm ble -> ble
  | BvTerm _ -> typerr "bool" "bitvec"
  | AxTerm _ -> typerr "bool" "array"

let unhide_bvexpr term =
  match term.Formula.term_desc with
  | BlTerm _ -> typerr "bitvec" "bool"
  | BvTerm bve -> bve
  | AxTerm _ -> typerr "bitvec" "array"

let unhide_axexpr term =
  match term.Formula.term_desc with
  | BlTerm _ -> typerr "array" "bool"
  | BvTerm _ -> typerr "array" "bitvec"
  | AxTerm axe -> axe

let name_of_symb symb = Format.asprintf "%a" Smtlib_pp.pp_symbol symb

let decl_of_svar svar =
  match svar.sorted_var_desc with
  | SortedVar (symb, so) ->
      let name = name_of_symb symb in
      let ty = Sort.eval so in
      decl_of_name name [] ty

let name_of_qid qid =
  match qid.qual_identifier_desc with
  | QualIdentifierAs _ -> assert false
  | QualIdentifierIdentifier id -> (
      match id.id_desc with
      | IdSymbol symb | IdUnderscore (symb, _) -> name_of_symb symb)

module StringMap = Map.Make (String)

module FunTyp = struct
  type t = { user_defined : bool; formals : Sort.t list; return : Sort.t }

  let create user_defined formals return = { user_defined; formals; return }
  let xx_y ?(user_defined = false) x y = create user_defined [ x; x ] y
  let x_y ?(user_defined = false) x y = create user_defined [ x ] y
end

module BvTheory = struct
  open Sort

  let bv_bin sz1 sz2 sz3 = FunTyp.create false [ bv sz1; bv sz2 ] (bv sz3)
  let bin_same sz = bv_bin sz sz sz
  let bin_bool sz1 sz2 = FunTyp.create false [ bv sz1; bv sz2 ] boolean

  let un l =
    assert (List.length l = 1);
    FunTyp.create false l

  let un_bv n = un [ bv n ] (bv n)
  let vars = [ ("bv1", Sort.bv 1); ("bv0", Sort.bv 1) ]

  let functions =
    [
      ("bvand", bin_same 0);
      ("bvnand", bin_same 0);
      ("bvor", bin_same 0);
      ("bvxor", bin_same 0);
      ("bvneg", un_bv 0);
      ("bvnot", un_bv 0);
      ("bvadd", bin_same 0);
      ("bvsub", bin_same 0);
      ("bvmul", bin_same 0);
      ("bvudiv", bin_same 0);
      ("bvsdiv", bin_same 0);
      ("bvurem", bin_same 0);
      ("bvsrem", bin_same 0);
      ("bvashr", bin_same 0);
      ("bvlshr", bin_same 0);
      ("bvlshl", bin_same 0);
      ("bvshr", bin_same 0);
      ("bvshl", bin_same 0);
      ("bvcomp", bin_same 0);
      ("concat", bin_same 0);
      ("extract", un_bv 0);
      ("repeat", un_bv 0);
      ("sign_extend", un_bv 0);
      ("zero_extend", un_bv 0);
      ("rotate_left", un_bv 0);
      ("rotate_right", un_bv 0);
      (* Unsigned comparison operators *)
      ("bvuge", bin_bool 0 0);
      ("bvugt", bin_bool 0 0);
      ("bvule", bin_bool 0 0);
      ("bvult", bin_bool 0 0);
      (* Signed comparison operators *)
      ("bvslt", bin_bool 0 0);
      ("bvsle", bin_bool 0 0);
      ("bvsgt", bin_bool 0 0);
      ("bvsge", bin_bool 0 0);
    ]

  let bv_bnop_tbl =
    [
      ("concat", BvConcat);
      ("bvand", BvAnd);
      ("bvor", BvOr);
      ("bvxor", BvXor);
      ("bvmul", BvMul);
      ("bvadd", BvAdd);
      ("bvsub", BvSub);
      ("bvcomp", BvCmp);
      ("bvshl", BvShl);
      ("bvlshl", BvShl);
      ("bvlshr", BvLshr);
      ("bvashr", BvAshr);
      ("bvurem", BvUrem);
      ("bvudiv", BvUdiv);
      ("bvsrem", BvSrem);
      ("bvsdiv", BvSdiv);
    ]

  let bv_unop_tbl =
    [
      ("bvnot", BvNot);
      ("bvneg", BvNeg);
      ("zero_extend", BvZeroExtend 0);
      ("sign_extend", BvSignExtend 0);
      ("rotate_left", BvRotateLeft 0);
      ("rotate_right", BvRotateRight 0);
      ("repeat", BvRepeat 0);
      ("extract", BvExtract Interval.{ lo = 0; hi = 0 });
    ]

  type op =
    | SmtBvUnop of bv_unop
    | SmtBvBnop of bv_bnop
    | SmtBvIte
    | SmtBvSelect

  let mk_bv_bnop name = SmtBvBnop (List.assoc name bv_bnop_tbl)
  let mk_bv_unop name = SmtBvUnop (List.assoc name bv_unop_tbl)

  let fill_unop_indexes idxs = function
    | BvRepeat _ -> BvRepeat (List.hd idxs)
    | BvZeroExtend _ -> BvZeroExtend (List.hd idxs)
    | BvSignExtend _ ->
        let idx = List.hd idxs in
        BvSignExtend idx
    | BvRotateLeft _ -> BvRotateLeft (List.hd idxs)
    | BvRotateRight _ -> BvRotateRight (List.hd idxs)
    | BvExtract _ -> (
        match idxs with
        | [ i; j ] -> BvExtract Interval.{ lo = j; hi = i }
        | _ -> assert false)
    | u -> u

  let get_numeric_indexes indexes =
    let rec loop acc = function
      | [] -> List.rev acc
      | IdxNum s :: idxs -> loop (int_of_string s :: acc) idxs
      | IdxSymbol s :: _ ->
          let msg =
            Format.sprintf "Was expecting a numeric index but got symbol %s"
              (name_of_symb s)
          in
          failwith msg
    in
    loop [] indexes

  let un_or_bin_op name =
    try mk_bv_unop name
    with Not_found -> (
      try mk_bv_bnop name
      with Not_found ->
        let msg = Format.sprintf "Did not find %s in BV operators" name in
        failwith msg)

  let op_of_qid qid =
    match qid.qual_identifier_desc with
    | QualIdentifierAs _ -> assert false
    | QualIdentifierIdentifier id -> (
        match id.id_desc with
        | IdSymbol symb -> (
            match name_of_symb symb with
            | "ite" -> SmtBvIte
            | "select" -> SmtBvSelect
            | op -> un_or_bin_op op)
        | IdUnderscore (symb, indexes) ->
            let nidxs = get_numeric_indexes indexes in
            let name = name_of_symb symb in
            Logger.debug ~level:4 "Fill %s [%a]" name
              (fun ppf l -> List.iter (fun x -> fprintf ppf "%d;" x) l)
              nidxs;
            let bv_inop = List.assoc name bv_unop_tbl in
            SmtBvUnop (fill_unop_indexes nidxs bv_inop))
end

module ArrayTheory = struct
  let aty = Sort.array 0 0

  let functions =
    [
      ("select", FunTyp.create false [ aty; Sort.bv 0 ] (Sort.bv 0));
      ("store", FunTyp.create false [ aty; Sort.bv 0; Sort.bv 0 ] aty);
    ]
end

module CoreTheory = struct
  let boolbool_bool_fun = FunTyp.xx_y Sort.boolean Sort.boolean
  let vars = [ ("true", Sort.boolean); ("false", Sort.boolean) ]

  let functions =
    [
      ("not", FunTyp.x_y Sort.boolean Sort.boolean);
      ("and", boolbool_bool_fun);
      ("or", boolbool_bool_fun);
      ("=>", boolbool_bool_fun);
      ("xor", boolbool_bool_fun);
      ("=", FunTyp.xx_y (Sort.fresh_var ()) Sort.boolean);
      ("distinct", FunTyp.xx_y (Sort.fresh_var ()) Sort.boolean);
      ( "ite",
        let v = Sort.fresh_var () in
        FunTyp.create false [ Sort.boolean; v; v ] v );
    ]
end

module TypEnv = struct
  type t = { functions : FunTyp.t StringMap.t; variables : Sort.t StringMap.t }

  let empty = { functions = StringMap.empty; variables = StringMap.empty }
  let create functions variables = { functions; variables }
  let add_var env v s = { env with variables = StringMap.add v s env.variables }

  let find_var env v =
    try StringMap.find v env.variables with _ -> failwith "Unbound variable"

  let load_vars vars env =
    List.fold_left (fun env (v, ftyp) -> add_var env v ftyp) env vars

  let add_fun env v ftyp =
    { env with functions = StringMap.add v ftyp env.functions }

  let find_fun env v =
    try StringMap.find v env.functions with _ -> failwith "Unbound variable"

  let load_funs funs env =
    List.fold_left (fun env (v, ftyp) -> add_fun env v ftyp) env funs

  let init =
    load_funs BvTheory.functions empty
    |> load_vars BvTheory.vars
    |> load_funs ArrayTheory.functions
    |> load_funs CoreTheory.functions
    |> load_vars CoreTheory.vars
end

let eval_constant = function
  | CstNumeral _ | CstDecimal _ | CstString _ -> assert false
  | CstDecimalSize (bv_num, sz_num) ->
      let sz = int_of_string sz_num in
      let n = Z.of_string bv_num in
      let bv = Bitvector.create n sz in
      let term = mk_bv_term (mk_bv_cst bv) in
      sorted_expr (Sort.bv sz) term
  | CstHexadecimal s ->
      let sz = 4 * String.length s in
      let n = Z.of_string ("0x" ^ s) in
      let bv = Bitvector.create n sz in
      let term = mk_bv_term (mk_bv_cst bv) in
      sorted_expr (Sort.bv sz) term
  | CstBinary s ->
      let sz = String.length s in
      let n = Z.of_string ("0b" ^ s) in
      let bv = Bitvector.create n sz in
      let term = mk_bv_term (mk_bv_cst bv) in
      sorted_expr (Sort.bv sz) term
  | CstBool b ->
      let bl_term_desc = if b then BlTrue else BlFalse in
      let term = mk_bl_term (bl_term bl_term_desc) in
      sorted_expr Sort.boolean term

let _check_sort _ _ = ()
let check_length _ n l = assert (List.length l = n)
let param1 unhide = function [ e ] -> unhide e.term | [] | _ -> assert false

let param2 unhide = function
  | [ e1; e2 ] -> (unhide e1.term, unhide e2.term)
  | l ->
      let len = List.length l in
      arity_error "param2 unhide" 2 len

let unhide_3_branched name f g = function
  | [ e1; e2; e3 ] -> (f e1.term, g e2.term, g e3.term)
  | l -> arity_error name 3 (List.length l)

let unhide_ite_args g = unhide_3_branched "ite" unhide_blexpr g

type bool_op =
  | SmtEqual
  | SmtDistinct
  | SmtBlIte
  | SmtBlUnop of bl_unop
  | SmtBlBnop of bl_bnop
  | SmtBvCmp of bv_comp

let bool_tbl =
  [
    ("not", SmtBlUnop BlNot);
    ("=>", SmtBlBnop BlImply);
    ("and", SmtBlBnop BlAnd);
    ("or", SmtBlBnop BlOr);
    ("xor", SmtBlBnop BlXor);
    ("=", SmtEqual);
    ("distinct", SmtDistinct);
    ("ite", SmtBlIte);
    ("bvuge", SmtBvCmp BvUge);
    ("bvugt", SmtBvCmp BvUgt);
    ("bvule", SmtBvCmp BvUle);
    ("bvult", SmtBvCmp BvUlt);
    ("bvslt", SmtBvCmp BvSlt);
    ("bvsle", SmtBvCmp BvSle);
    ("bvsgt", SmtBvCmp BvSgt);
    ("bvsge", SmtBvCmp BvSge);
    ("bvcomp", SmtBvCmp BvEqual);
  ]

let type_of_branches = function
  | se1 :: se2 :: _ ->
      if Sort.eq se1.sort se2.sort then se1.sort
      else
        let msg =
          asprintf "Branches should have the same types %a <> %a" Sort.pp
            se1.sort Sort.pp se2.sort
        in
        failwith msg
  | l -> arity_error "branching term" 2 (List.length l)

let binary bl_bop exprs =
  let rec loop = function
    | [] -> assert false
    | [ e ] -> unhide_blexpr e.term
    | e :: es ->
        let e1 = unhide_blexpr e.term in
        mk_bl_bnop bl_bop e1 (loop es)
  in
  loop exprs

let genr_bool_apply name _ exprs =
  (*  List.iter2 check_sort ty.FunTyp.formals (sorts_of_sexprs exprs); *)
  let unhide1 = param1 unhide_blexpr in
  let unhide2 = param2 unhide_blexpr in
  let expr =
    match List.assoc name bool_tbl with
    | SmtEqual -> (
        match type_of_branches exprs with
        | Sort.Bool ->
            let e1, e2 = unhide2 exprs in
            mk_bl_equal e1 e2
        | Sort.BitVec _ ->
            let e1, e2 = param2 unhide_bvexpr exprs in
            mk_bv_equal e1 e2
        | Sort.Array _ ->
            let e1, e2 = param2 unhide_axexpr exprs in
            mk_ax_equal e1 e2
        | Sort.Var _ -> assert false)
    | SmtDistinct -> (
        match type_of_branches exprs with
        | Sort.Bool ->
            let e1, e2 = unhide2 exprs in
            mk_bl_distinct e1 e2
        | Sort.BitVec _ ->
            let e1, e2 = param2 unhide_bvexpr exprs in
            mk_bv_distinct e1 e2
        | Sort.Array _ ->
            let e1, e2 = param2 unhide_axexpr exprs in
            mk_ax_distinct e1 e2
        | Sort.Var _ -> assert false)
    | SmtBlUnop _ ->
        check_length name 1 exprs;
        mk_bl_not (unhide1 exprs)
    | SmtBlBnop bop ->
        let len = List.length exprs in
        if len >= 2 then binary bop exprs else arity_error name 2 len
    | SmtBlIte ->
        let e1, e2, e3 = unhide_ite_args unhide_blexpr exprs in
        mk_bl_ite e1 e2 e3
    | SmtBvCmp bvop ->
        let e1, e2 = param2 unhide_bvexpr exprs in
        mk_bv_comp bvop e1 e2
    | exception Not_found ->
        let msg = Format.sprintf "Bool apply failure : %s" name in
        failwith msg
  in
  bl_sorted_expr expr

let unhide2 e = param2 unhide_bvexpr e
let extract_bv_expr se = (unhide_bvexpr se.term, Sort.bv_size se.sort)

let get_one_param msg = function
  | [ e ] -> e
  | es -> arity_error msg 1 (List.length es)

let unop_bv_size arg_size = function
  | BvExtract { Interval.lo; Interval.hi } -> hi - lo + 1
  | BvZeroExtend n | BvSignExtend n -> n + arg_size
  | BvRepeat n -> n * arg_size (* not sure *)
  | _ -> arg_size

let add_int bv n =
  let sz = Bitvector.size_of bv in
  let bv_n = Bitvector.create (Z.of_int n) sz in
  Bitvector.add bv bv_n

let rec pp_bv ppf b =
  match b.bv_term_desc with
  | BvCst bv -> Bitvector.pp ppf bv
  | BvFun (b, _) -> Format.fprintf ppf "%s" b.bv_name
  | BvBnop (BvAdd, b1, b2) -> fprintf ppf "(%a + %a)" pp_bv b1 pp_bv b2
  | BvBnop (BvSub, b1, b2) -> fprintf ppf "(%a - %a)" pp_bv b1 pp_bv b2
  | _ -> Format.fprintf ppf "b??"

and pp_ax ppf a =
  match a.ax_term_desc with
  | AxFun (a, _) -> Format.fprintf ppf "%s" a.ax_name
  | Store (n, a, idx, _v) -> fprintf ppf "[%a %@ (%a, %d)] " pp_ax a pp_bv idx n
  | _ -> fprintf ppf "a??"

let is_int bv vi =
  match bv.bv_term_desc with
  | BvCst bv ->
      let sz = Bitvector.size_of bv in
      let v = Bitvector.create (Z.of_int vi) sz in
      Bitvector.equal bv v
  | _ -> false

(* Does indexed value at index [r] follows indexed value of size [sz] at index
   [l] ?
*)
let rec ( <@> ) r (l, sz) =
  let level = 4 in
  match (l.bv_term_desc, r.bv_term_desc) with
  | BvCst lb, BvCst rb -> Bitvector.equal (add_int lb sz) rb
  | BvBnop (BvAdd, v1, e1), BvBnop (BvAdd, v2, e2) ->
      Logger.debug ~level "Follows ADD (%a:%d) ? %a" pp_bv l sz pp_bv r;
      Formula.equal_bv_term v1 v2 && e2 <@> (e1, sz)
  | BvBnop (BvSub, v1, e1), BvBnop (BvSub, v2, e2) ->
      Logger.debug ~level "Follows SUB (%a:%d) %a" pp_bv v1 sz pp_bv v2;
      Formula.equal_bv_term v1 v2 && e1 <@> (e2, sz)
  | BvFun _, BvBnop (BvAdd, v1, e1) ->
      Logger.debug ~level "Follows ADD_GROUNDED (%a:%d) ? (%a + %a)" pp_bv l sz
        pp_bv v1 pp_bv e1;
      Formula.equal_bv_term v1 l && is_int e1 sz
  | _, _ -> false

let lower_stitchable_index ((idx1, _) as i1) ((idx2, _) as i2) =
  if idx1 <@> i2 then Some idx2 else if idx2 <@> i1 then Some idx1 else None

let genr_bv_apply qid exprs =
  let open BvTheory in
  match op_of_qid qid with
  | SmtBvUnop unop ->
      let msg = "foo" (* Format.asprintf "%a" Ast_pp.pp_bv_unop unop *) in
      let e, sz = extract_bv_expr (get_one_param msg exprs) in
      Formula.mk_bv_unop unop e |> bv_sorted_expr (unop_bv_size sz unop)
  | SmtBvBnop BvConcat ->
      let rec loop = function
        | [] -> assert false
        | [ e ] -> unhide_bvexpr e.term
        | e :: es ->
            let e1 = unhide_bvexpr e.term in
            mk_bv_concat e1 (loop es)
      in
      assert (List.length exprs >= 2);
      let bve = loop exprs
      and size =
        List.fold_left ( + ) 0 (List.map (fun e -> Sort.bv_size e.sort) exprs)
      in
      bv_sorted_expr size bve
  | SmtBvBnop BvCmp ->
      let e1, e2 = unhide2 exprs in
      Formula.mk_bv_cmp e1 e2 |> bv_sorted_expr 1
  | SmtBvBnop bnop ->
      let e1, e2 = unhide2 exprs and sz = Sort.bv_size (List.hd exprs).sort in
      Formula.mk_bv_bnop bnop e1 e2 |> bv_sorted_expr sz
  | SmtBvIte ->
      let e1, e2, e3 = unhide_ite_args unhide_bvexpr exprs
      (* bitvectors from else and then parts are assumed to have the same size *)
      and sz = Sort.bv_size (List.nth exprs 2).sort in
      Formula.mk_bv_ite e1 e2 e3 |> bv_sorted_expr sz
  | SmtBvSelect ->
      let asort, e1, e2 =
        match exprs with
        | [ e1; e2 ] -> (e1.sort, unhide_axexpr e1.term, unhide_bvexpr e2.term)
        | l -> arity_error "select" 2 (List.length l)
      in
      let sz = Sort.array_contents_bv_size asort in
      let bytesize = 1 in
      mk_select bytesize e1 e2 |> bv_sorted_expr sz

let get_idx_elt e =
  match e.sort with Sort.Array (idx, elt) -> (idx, elt) | _ -> assert false

(* Stitch preordered values [lo ... hi]
   This assumes that the values @ lo starts at a lower index than the value @
   hi.
*)
let stitch_array_values ~lo ~hi =
  match (lo.bv_term_desc, hi.bv_term_desc) with
  | BvCst fv, BvCst bv ->
      (* Logger.debug "Stitch_C: %a @ %a" Bitvector.pp fv Bitvector.pp bv; *)
      Some (mk_bv_cst (Bitvector.append bv fv))
  | ( BvUnop (BvExtract i1, ({ bv_term_desc = BvFun (v1, _); _ } as v)),
      BvUnop (BvExtract i2, { bv_term_desc = BvFun (v2, _); _ }) ) ->
      let open Interval in
      (* Logger.debug "Stitch_E: %s (%d, %d) %@ %s (%d, %d)"
       *   v1.bv_name i1.lo i1.hi
       *   v2.bv_name i2.lo i2.hi; *)
      if v1.bv_name = v2.bv_name && i2.lo = i1.hi + 1 then
        Some (mk_bv_extract { lo = i1.lo; hi = i2.hi } v)
      else None
  | _, _ -> None

(* This is where we try to generate store(n)
   This is a very partial solution, only "quasi-contiguous" series of array
   updates.
   The examples below should be handled:
   store (i + 2) (store i (store (i + 1) a v1) v2) v3 or
   store (i + 2) (store (i + 1) (store i a v1) v2) v3 or
   store i (store (i + 1) (store (i + 2) a v1) v2) v3

   The following example too (that's why the function is recursive):
   store (i + 1) (store i (store (i + 2) a v1) v2) v3 or
*)
let rec my_mk_store size ax index value =
  if Formula_options.No_stitching.get () then mk_store size ax index value
  else
    match ax.ax_term_desc with
    | Store (n, a, idx, v) -> (
        Logger.debug "Store in %a (%a, %d)" pp_ax ax pp_bv index size;
        match lower_stitchable_index (index, size) (idx, n) with
        | None -> mk_store size ax index value
        | Some lo_idx -> (
            let lo, hi = if lo_idx = index then (value, v) else (v, value) in
            match stitch_array_values ~lo ~hi with
            | Some v' ->
                Logger.debug "STMERGE %a %a to %d" pp_ax a pp_bv lo_idx
                  (size + n);
                my_mk_store (size + n) a lo_idx v'
            | None -> mk_store size ax index value))
    | _ -> mk_store size ax index value

let genr_arr_apply name exprs =
  let idx, elt, ax_expr =
    match name with
    | "ite" ->
        let e1, e2, e3 = unhide_ite_args unhide_axexpr exprs in
        let idx, elt = get_idx_elt (List.nth exprs 1) in
        (idx, elt, mk_ax_ite e1 e2 e3)
    | "store" ->
        let e1, e2, e3 =
          unhide_3_branched name unhide_axexpr unhide_bvexpr exprs
        in
        let idx, elt = get_idx_elt (List.hd exprs) in
        let size = 1 in
        let ax_term = my_mk_store size e1 e2 e3 in
        (idx, elt, ax_term)
    | opname -> unhandled_operator "array" opname
  in
  ax_sorted_expr idx elt ax_expr

let bl_def bl_name ?(decls = []) term =
  let bl_var = bl_var bl_name in
  mk_bl_def bl_var decls term

let bv_def bv_name bv_size ?(decls = []) term =
  let var = bv_var bv_name bv_size in
  mk_bv_def var decls term

let ax_def ax_name ~idx_size ~elt_size ?(decls = []) term =
  let var = ax_var ax_name idx_size elt_size in
  mk_ax_def var decls term

let def_of_binding (name, sexpr) =
  match sexpr.sort with
  | Sort.Bool -> bl_def name (unhide_blexpr sexpr.term)
  | Sort.BitVec n -> bv_def name n (unhide_bvexpr sexpr.term)
  | Sort.Array (idx_size, elt_size) ->
      ax_def name ~idx_size ~elt_size (unhide_axexpr sexpr.term)
  | Sort.Var _ -> assert false

let defs_of_bindings l = List.map def_of_binding l

let my_mk_ax_let defs axterm =
  if Formula_options.No_stitching.get () then mk_ax_let defs axterm
  else
    match (defs, axterm.ax_term_desc) with
    | ( [
          { def_desc = AxDef (a, [], ({ ax_term_desc = Store _; _ } as ax)); _ };
        ],
        Store (m, { ax_term_desc = AxFun (a', []); _ }, idx', v') ) ->
        if a.ax_name = a'.ax_name then (
          Logger.debug "let-stitching %s %@ %a (%d) " a.ax_name pp_bv idx' m;
          my_mk_store m ax idx' v')
        else mk_ax_let defs axterm
    | _, _ -> mk_ax_let defs axterm

let genr_let bindings se =
  let defs = defs_of_bindings bindings in
  let expr =
    match se.sort with
    | Sort.Bool -> mk_bl_let defs (unhide_blexpr se.term) |> mk_bl_term
    | Sort.BitVec _ -> mk_bv_let defs (unhide_bvexpr se.term) |> mk_bv_term
    | Sort.Array _ -> my_mk_ax_let defs (unhide_axexpr se.term) |> mk_ax_term
    | Sort.Var _ -> assert false
  in
  sorted_expr se.sort expr

let get_opt_bv_cst qid =
  match qid.qual_identifier_desc with
  | QualIdentifierAs _ -> None
  | QualIdentifierIdentifier id -> (
      match id.id_desc with
      | IdUnderscore (symb, [ IdxNum n ]) ->
          let name = name_of_symb symb in
          if name.[0] = 'b' && name.[1] = 'v' then
            let sv = String.sub name 2 (String.length name - 2) in
            let v = Z.of_string sv in
            let sz = int_of_string n in
            let bv = Bitvector.create v sz in
            let expr = mk_bv_cst bv |> mk_bv_term in
            Some (sorted_expr (Sort.bv sz) expr)
          else None
      | IdSymbol _ | IdUnderscore _ -> None)

let rec eval_term env term =
  Logger.debug ~level:3 "Term %a" Smtlib_pp.pp_term term;
  match term.Smtlib.term_desc with
  | TermSpecConstant c -> (env, eval_constant c)
  | TermQualIdentifier qid -> (
      match get_opt_bv_cst qid with
      | None ->
          let name = name_of_qid qid in
          let ty = TypEnv.find_var env name in
          (env, expr_of_name name ty)
      | Some rbv -> (env, rbv))
  | TermQualIdentifierTerms (qid, terms) -> eval_fun_apply env qid terms
  | TermLetTerm (vbindings, term) ->
      let env', bindings = add_vbindings env vbindings in
      let env'', se = eval_term env' term in
      (env'', genr_let bindings se)
  | TermLambdaTerm _ | TermExistsTerm _ | TermForallTerm _ | TermAnnotatedTerm _
    ->
      assert false

and eval_fun_apply env qid terms =
  let name = name_of_qid qid in
  assert (terms <> []);
  let ty = TypEnv.find_fun env name in
  let _, sexprs =
    List.fold_left
      (fun (env, es) term ->
        let env', e = eval_term env term in
        (env', e :: es))
      (env, []) terms
  in
  let exprs = List.rev sexprs in
  let sexpr =
    if not ty.FunTyp.user_defined then
      match ty.FunTyp.return with
      | Sort.Bool -> genr_bool_apply name ty exprs
      | Sort.BitVec _ -> genr_bv_apply qid exprs
      | Sort.Array _ -> genr_arr_apply name exprs
      | Sort.Var _ -> (
          match exprs with
          | _ :: { sort = Sort.Bool; _ } :: _ -> genr_bool_apply name ty exprs
          | _ :: { sort = Sort.BitVec _; _ } :: _ -> genr_bv_apply qid exprs
          | _ :: { sort = Sort.Array _; _ } :: _ -> genr_arr_apply name exprs
          | _ :: { sort = Sort.Var _; _ } :: _ | [] | _ :: _ -> assert false)
    else
      let smt_exprs = List.map (fun e -> e.term) exprs in
      match ty.FunTyp.return with
      | Sort.Bool ->
          let bl_var = bl_var name in
          bl_sorted_expr (mk_bl_fun bl_var smt_exprs)
      | Sort.BitVec n ->
          let bv_var = bv_var name n in
          bv_sorted_expr n (mk_bv_fun bv_var smt_exprs)
      | Sort.Array (idx, elt) ->
          let ax_var = ax_var name idx elt in
          ax_sorted_expr idx elt (mk_ax_fun ax_var smt_exprs)
      | Sort.Var _ -> assert false
  in
  (TypEnv.(create env.functions env.variables), sexpr)

and add_vbinding env vbinding =
  match vbinding.var_binding_desc with
  | VarBinding (symb, term) ->
      let name = name_of_symb symb in
      let _, smte = eval_term env term in
      (name, smte)

and add_vbindings env vbindings =
  (* Let binding are parallel, like let ... and ... definitions in OCaml.
     See p.27 of SMTLIB v2.5.
  *)
  let bindings = List.map (add_vbinding env) vbindings in
  let env' =
    List.fold_left
      (fun env (name, e) -> TypEnv.add_var env name e.sort)
      env bindings
  in
  (env', bindings)

let only_decls d = List.map (fun d -> d.Formula.decl_desc) d

let add_decls env decls =
  only_decls decls
  |> List.fold_left
       (fun env d -> TypEnv.add_var env (name_of_decl d) (Sort.of_decl d))
       env

let eval_fun env fdef =
  match fdef.fun_def_desc with
  | FunDef (symb, _, svars, sort, body) ->
      (* Smt update *)
      let decls = List.map decl_of_svar svars in
      let sort' = Sort.eval sort in
      let name = name_of_symb symb in
      let fenv = add_decls env decls in
      let mk_smt_expr unravel mk smt_expr = unravel smt_expr.term |> mk in
      let _, smt_expr = eval_term fenv body in
      (* FIXME : check type of body wrt type of declared return type *)
      (*let def = Ast.smt_fn_def {Ast.fn_name = name } decls smt_expr.expr in *)
      let def =
        match sort' with
        | Sort.Bool ->
            let mk = mk_bl_def (bl_var name) decls in
            mk_smt_expr unhide_blexpr mk smt_expr
        | Sort.BitVec n ->
            let mk = mk_bv_def (bv_var name n) decls in
            mk_smt_expr unhide_bvexpr mk smt_expr
        | Sort.Array (idx, elt) ->
            let mk = mk_ax_def (ax_var name idx elt) decls in
            mk_smt_expr unhide_axexpr mk smt_expr
        | Sort.Var _ -> assert false
      in
      (* Env update *)
      let env' =
        match svars with
        | [] ->
            (* A function of arity 0 is a variable *)
            TypEnv.add_var env name sort'
        | _ :: _ ->
            let funtyp =
              FunTyp.create true
                (List.map Sort.of_decl (only_decls decls))
                sort'
            in
            TypEnv.add_fun env name funtyp
      in
      (env', Some (Formula.Define def))

let mk_assert bl_t = Formula.Assert bl_t

let eval_command env cmd =
  let err msg = error msg cmd.command_loc in
  Logger.debug ~level:5 "Parsing %a" Smtlib_pp.pp_command cmd;
  match cmd.command_desc with
  | CmdAssert term ->
      let env, bl_smt_expr = eval_term env term in
      (env, Some (mk_assert (unhide_blexpr bl_smt_expr.term)))
  | CmdDeclareConst _ -> err "Unhandled declare const"
  | CmdDeclareFun (symb, _, sorts, sort) ->
      let ty = Sort.eval sort in
      let tys = List.map Sort.eval sorts in
      let name = name_of_symb symb in
      ( TypEnv.add_var env name ty,
        Some (Formula.Declare (decl_of_name name tys ty)) )
  | CmdDeclareSort _ -> err "Unhandled declaration of a new sort"
  | CmdDefineSort _ -> err "Unhandled definition of a new sort"
  | CmdDefineFunRec _ -> err "Unhandled recursive functions"
  | CmdDefineFun fdef -> eval_fun env fdef
  | CmdGetAssignment | CmdCheckSat | CmdCheckSatAssuming _ | CmdEcho _ | CmdExit
  | CmdGetAssertions | CmdGetInfo _ | CmdGetModel | CmdGetOption _ | CmdGetProof
  | CmdGetUnsatAssumptions | CmdGetUnsatCore | CmdGetValue _ | CmdMetaInfo _
  | CmdPop _ | CmdPush _ | CmdReset | CmdResetAssertions | CmdSetInfo _
  | CmdSetLogic _ | CmdComment _ | CmdSetOption _ ->
      (env, None)

let eval_commands env cmds =
  let ev (fml, env) cmd =
    let env, sentry_opt = eval_command env cmd in
    match sentry_opt with
    | None -> (fml, env)
    | Some entry_desc -> (Formula.push_front (Formula.entry entry_desc) fml, env)
  in
  let sentries, _env = List.fold_left ev (Formula.empty, env) cmds in
  sentries

let script script = eval_commands TypEnv.init script.script_commands
