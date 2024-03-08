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

type status = SAT | UNSAT | TIMEOUT | UNKNOWN

let status_to_exit_code = function
  | SAT -> 0
  | UNSAT -> 10
  | TIMEOUT -> 11
  | UNKNOWN -> 12

type bl_unop = BlNot
type bl_bnop = BlImply | BlAnd | BlOr | BlXor
type bl_comp = BlEqual | BlDistinct

type bv_unop =
  | BvNot
  | BvNeg
  | BvRepeat of int
  | BvZeroExtend of int
  | BvSignExtend of int
  | BvRotateLeft of int
  | BvRotateRight of int
  | BvExtract of int Interval.t

type bv_bnop =
  | BvConcat
  | BvAnd
  | BvNand
  | BvOr
  | BvNor
  | BvXor
  | BvXnor
  | BvCmp
  | BvAdd
  | BvSub
  | BvMul
  | BvUdiv
  | BvSdiv
  | BvUrem
  | BvSrem
  | BvSmod
  | BvShl
  | BvAshr
  | BvLshr

type bv_comp =
  | BvEqual
  | BvDistinct
  | BvUlt
  | BvUle
  | BvUgt
  | BvUge
  | BvSlt
  | BvSle
  | BvSgt
  | BvSge

type ax_comp = AxEqual | AxDistinct
type sort = BlSort | BvSort of int | AxSort of int * int
type bl_var = { bl_hash : int; bl_name : string }
type bv_var = { bv_hash : int; bv_name : string; bv_size : int }

type ax_var = {
  ax_hash : int;
  ax_name : string;
  idx_size : int;
  elt_size : int;
}

type var = BlVar of bl_var | BvVar of bv_var | AxVar of ax_var

type term_desc = BlTerm of bl_term | BvTerm of bv_term | AxTerm of ax_term
and term = { term_hash : int; term_desc : term_desc }

and bl_term_desc =
  | BlTrue
  | BlFalse
  | BlFun of bl_var * term list
  | BlLet of def list * bl_term
  | BlUnop of bl_unop * bl_term
  | BlBnop of bl_bnop * bl_term * bl_term
  | BlComp of bl_comp * bl_term * bl_term
  | BvComp of bv_comp * bv_term * bv_term
  | AxComp of ax_comp * ax_term * ax_term
  | BlIte of bl_term * bl_term * bl_term

and bl_term = { bl_term_hash : int; bl_term_desc : bl_term_desc }

and bv_term_desc =
  | BvCst of Bitvector.t
  | BvFun of bv_var * term list
  | BvLet of def list * bv_term
  | BvUnop of bv_unop * bv_term
  | BvBnop of bv_bnop * bv_term * bv_term
  | BvIte of bl_term * bv_term * bv_term
  | Select of int * ax_term * bv_term

and bv_term = {
  bv_term_hash : int;
  bv_term_desc : bv_term_desc;
  bv_term_size : int;
}

and ax_term_desc =
  | AxFun of ax_var * term list
  | AxLet of def list * ax_term
  | AxIte of bl_term * ax_term * ax_term
  | Store of int * ax_term * bv_term * bv_term

and ax_term = {
  ax_term_hash : int;
  ax_term_desc : ax_term_desc;
  idx_term_size : int;
  elt_term_size : int;
}

and def_desc =
  | BlDef of bl_var * decl list * bl_term
  | BvDef of bv_var * decl list * bv_term
  | AxDef of ax_var * decl list * ax_term

and def = { def_hash : int; def_desc : def_desc }

and decl_desc =
  | BlDecl of bl_var * sort list
  | BvDecl of bv_var * sort list
  | AxDecl of ax_var * sort list

and decl = { decl_hash : int; decl_desc : decl_desc }

type entry_desc =
  | Declare of decl
  | Define of def
  | Assert of bl_term
  | Assume of bl_term
  | Comment of string

type entry = { entry_hash : int; entry_desc : entry_desc }
type formula = { entries : entry Sequence.t }

(* Basic printing of formulas, usable here *)
module Printing = struct
  open Format

  let blunop_to_str bl_uop = match bl_uop with BlNot -> "blnot"

  let blbnop_to_str bl_bop =
    match bl_bop with
    | BlImply -> "imply"
    | BlAnd -> "bland"
    | BlOr -> "blor"
    | BlXor -> "blxor"

  let blcmp_to_str bl_cmp =
    match bl_cmp with BlEqual -> "bleq" | BlDistinct -> "bldiff"

  let bvcmp_to_str bv_cmp =
    match bv_cmp with
    | BvEqual -> "bveq"
    | BvDistinct -> "bvdiff"
    | BvUlt -> "bvult"
    | BvUle -> "bvule"
    | BvUgt -> "bvugt"
    | BvUge -> "bvuge"
    | BvSlt -> "bvslt"
    | BvSle -> "bvsle"
    | BvSgt -> "bvsgt"
    | BvSge -> "bvsge"

  let axcmp_to_str ax_cmp =
    match ax_cmp with AxEqual -> "axeq" | AxDistinct -> "axdiff"

  let bvunop_to_str bv_uop =
    match bv_uop with
    | BvNot -> "bvnot"
    | BvNeg -> "bvneg"
    | BvRepeat i -> sprintf "repeat %i" i
    | BvZeroExtend i -> sprintf "zext %i" i
    | BvSignExtend i -> sprintf "sext %i" i
    | BvRotateLeft i -> sprintf "lrotate %i" i
    | BvRotateRight i -> sprintf "rrotate %i" i
    | BvExtract int -> sprintf "extr {%i..%i}" int.Interval.lo int.Interval.hi

  let bvbnop_to_str bv_bop =
    match bv_bop with
    | BvConcat -> "bvconcat"
    | BvAnd -> "bvand"
    | BvNand -> "bvnand"
    | BvOr -> "bvor"
    | BvNor -> "bvnor"
    | BvXor -> "bvxor"
    | BvXnor -> "bvxnor"
    | BvCmp -> "bvcmp"
    | BvAdd -> "bvadd"
    | BvSub -> "bvsub"
    | BvMul -> "bvmul"
    | BvUdiv -> "bvudiv"
    | BvSdiv -> "bvsdiv"
    | BvUrem -> "bvurem"
    | BvSrem -> "bvsrem"
    | BvSmod -> "bvsmod"
    | BvShl -> "bvshl"
    | BvAshr -> "bvashr"
    | BvLshr -> "bvlshr"

  let p_sort fmt sort =
    match sort with
    | BlSort -> fprintf fmt "Bool"
    | BvSort i -> fprintf fmt "(BitVec %i)" i
    | AxSort (idx, elt) -> fprintf fmt "(Array (idx %i) (elt %i))" idx elt

  let rec p_blterm fmt bl =
    match bl.bl_term_desc with
    | BlTrue -> fprintf fmt "true"
    | BlFalse -> fprintf fmt "false"
    | BlFun ({ bl_name; _ }, []) -> fprintf fmt "%s" bl_name
    | BlFun ({ bl_name; _ }, list) ->
        fprintf fmt "@[<hv2>(%s@ %a)@]" bl_name
          (fun fmt ->
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") p_term fmt)
          list
    | BlLet (list, bl) ->
        fprintf fmt "@[<hv2>(bllet@ (@[<hv>%a@])@ %a)@]"
          (fun fmt ->
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
              (fun fmt -> fprintf fmt "(@[<hv>%a@])" p_define)
              fmt)
          list p_blterm bl
    | BlUnop (bl_uop, bl) ->
        fprintf fmt "@[<hv2>(%s@ %a)@]" (blunop_to_str bl_uop) p_blterm bl
    | BlBnop (bl_bop, bl1, bl2) ->
        fprintf fmt "@[<hv2>(%s@ %a@ %a)@]" (blbnop_to_str bl_bop) p_blterm bl1
          p_blterm bl2
    | BlComp (bl_cmp, bl1, bl2) ->
        fprintf fmt "@[<hv2>(%s@ %a@ %a)@]" (blcmp_to_str bl_cmp) p_blterm bl1
          p_blterm bl2
    | BvComp (bv_cmp, bv1, bv2) ->
        fprintf fmt "@[<hv2>(%s@ %a@ %a)@]" (bvcmp_to_str bv_cmp) p_bvterm bv1
          p_bvterm bv2
    | AxComp (ax_cmp, ax1, ax2) ->
        fprintf fmt "@[<hv2>(%s@ %a@ %a)@]" (axcmp_to_str ax_cmp) p_axterm ax1
          p_axterm ax2
    | BlIte (bl_cond, bl_then, bl_else) ->
        fprintf fmt "@[<hv2>(ite@ %a@ %a@ %a)@]" p_blterm bl_cond p_blterm
          bl_then p_blterm bl_else

  and p_bvterm fmt bv =
    match bv.bv_term_desc with
    | BvCst c ->
        fprintf fmt "(0x%x %i)"
          (Z.to_int (Bitvector.value_of c))
          (Bitvector.size_of c)
    | BvFun ({ bv_name; _ }, []) -> fprintf fmt "%s" bv_name
    | BvFun ({ bv_name; _ }, list) ->
        fprintf fmt "@[<hv2>(%s@ %a)@]" bv_name
          (fun fmt ->
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") p_term fmt)
          list
    | BvLet (list, bv) ->
        fprintf fmt "@[<hv2>(bvlet@ (@[<hv>%a@])@ %a)@]"
          (fun fmt ->
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
              (fun fmt -> fprintf fmt "(@[<hv>%a@])" p_define)
              fmt)
          list p_bvterm bv
    | BvUnop (bv_uop, bv) ->
        fprintf fmt "@[<hv2>(%s@ %a)@]" (bvunop_to_str bv_uop) p_bvterm bv
    | BvBnop (bv_bop, bv1, bv2) ->
        fprintf fmt "@[<hv2>(%s@ %a@ %a)@]" (bvbnop_to_str bv_bop) p_bvterm bv1
          p_bvterm bv2
    | BvIte (bl_cond, bv_then, bv_else) ->
        fprintf fmt "@[<hv2>(ite@ %a@ %a@ %a)@]" p_blterm bl_cond p_bvterm
          bv_then p_bvterm bv_else
    | Select (n, mem, idx) ->
        fprintf fmt "@[<hv2>(select %i@ %a@ %a)@]" n p_axterm mem p_bvterm idx

  and p_axterm fmt ax =
    match ax.ax_term_desc with
    | AxFun ({ ax_name; _ }, []) -> fprintf fmt "%s" ax_name
    | AxFun ({ ax_name; _ }, list) ->
        fprintf fmt "@[<hv2>(%s@ %a)@]" ax_name
          (fun fmt ->
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") p_term fmt)
          list
    | AxLet (list, ax) ->
        fprintf fmt "@[<hv2>(axlet@ (@[<hv>%a@])@ %a)@]"
          (fun fmt ->
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
              (fun fmt -> fprintf fmt "(@[<hv>%a@])" p_define)
              fmt)
          list p_axterm ax
    | AxIte (bl_cond, ax_then, ax_else) ->
        fprintf fmt "@[<hv2>(ite@ %a@ %a@ %a)@]" p_blterm bl_cond p_axterm
          ax_then p_axterm ax_else
    | Store (n, mem, idx, elt) ->
        fprintf fmt "@[<hv2>(store %i@ %a@ %a@ %a)@]" n p_axterm mem p_bvterm
          idx p_bvterm elt

  and p_term fmt t =
    match t.term_desc with
    | BlTerm bl -> p_blterm fmt bl
    | BvTerm bv -> p_bvterm fmt bv
    | AxTerm ax -> p_axterm fmt ax

  and p_declare fmt decl =
    match decl.decl_desc with
    | BlDecl ({ bl_name; _ }, []) -> fprintf fmt "%s@ %a" bl_name p_sort BlSort
    | BvDecl ({ bv_name; bv_size; _ }, []) ->
        fprintf fmt "%s@ %a" bv_name p_sort (BvSort bv_size)
    | AxDecl ({ ax_name; idx_size; elt_size; _ }, []) ->
        fprintf fmt "%s () (idx %i) (elt %i)" ax_name idx_size elt_size
    | BlDecl ({ bl_name; _ }, list) ->
        fprintf fmt "%s@ (@[<hv>%a@])@ %a" bl_name
          (fun fmt ->
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") p_sort fmt)
          list p_sort BlSort
    | BvDecl ({ bv_name; bv_size; _ }, list) ->
        fprintf fmt "%s@ (@[<hv>%a@])@ %a" bv_name
          (fun fmt ->
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") p_sort fmt)
          list p_sort (BvSort bv_size)
    | AxDecl ({ ax_name; idx_size; elt_size; _ }, list) ->
        fprintf fmt "%s@ (@[<hv>%a@])@ %a" ax_name
          (fun fmt ->
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") p_sort fmt)
          list p_sort
          (AxSort (idx_size, elt_size))

  and p_define fmt def =
    match def.def_desc with
    | BlDef ({ bl_name; _ }, [], bl) ->
        fprintf fmt "%s@ %a@ %a" bl_name p_sort BlSort p_blterm bl
    | BvDef ({ bv_name; bv_size; _ }, [], bv) ->
        fprintf fmt "%s@ %a@ %a" bv_name p_sort (BvSort bv_size) p_bvterm bv
    | AxDef ({ ax_name; idx_size; elt_size; _ }, [], ax) ->
        fprintf fmt "%s@ %a@ %a" ax_name p_sort
          (AxSort (idx_size, elt_size))
          p_axterm ax
    | BlDef ({ bl_name; _ }, list, bl) ->
        fprintf fmt "%s@ (@[<hv>%a@])@ %a@ %a" bl_name
          (fun fmt ->
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
              (fun fmt -> fprintf fmt "(@[<hv>%a@])" p_declare)
              fmt)
          list p_sort BlSort p_blterm bl
    | BvDef ({ bv_name; bv_size; _ }, list, bv) ->
        fprintf fmt "%s@ (@[<hv>%a@])@ %a@ %a" bv_name
          (fun fmt ->
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
              (fun fmt -> fprintf fmt "(@[<hv>%a@])" p_declare)
              fmt)
          list p_sort (BvSort bv_size) p_bvterm bv
    | AxDef ({ ax_name; idx_size; elt_size; _ }, list, ax) ->
        fprintf fmt "%s@ (@[<hv>%a@])@ %a@ %a" ax_name
          (fun fmt ->
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt "@ ")
              (fun fmt -> fprintf fmt "(@[<hv>%a@])" p_declare)
              fmt)
          list p_sort
          (AxSort (idx_size, elt_size))
          p_axterm ax

  let p_entry fmt e =
    fprintf fmt "@[<hv2>(";
    (match e.entry_desc with
    | Declare decl -> fprintf fmt "declare@ %a" p_declare decl
    | Define def -> fprintf fmt "define@ %a" p_define def
    | Assert _assert -> fprintf fmt "assert@ @[<hv>%a@]" p_blterm _assert
    | Assume assume -> fprintf fmt "assume@ @[<hv>%a@]" p_blterm assume
    | Comment com -> fprintf fmt "comment@ %s" com);
    fprintf fmt ")@]"

  let p_formula fmt fm =
    Sequence.iter_forward
      (fun e -> fprintf fmt "@[<v>%a@,@]" p_entry e)
      fm.entries
end

(* Some utilities *)

let equal_bl_term (bl1 : bl_term) (bl2 : bl_term) = bl1 = bl2
let equal_bv_term (bv1 : bv_term) (bv2 : bv_term) = bv1 = bv2
let equal_ax_term (ax1 : ax_term) (ax2 : ax_term) = ax1 = ax2

let is_bl_cst bl =
  match bl.bl_term_desc with
  | BlTrue -> Some true
  | BlFalse -> Some false
  | BlFun (_, _) -> None
  | BlLet (_, _) -> None
  | BlUnop (_, _) -> None
  | BlBnop (_, _, _) -> None
  | BlComp (_, _, _) -> None
  | BvComp (_, _, _) -> None
  | AxComp (_, _, _) -> None
  | BlIte (_, _, _) -> None

let is_bv_cst bv =
  match bv.bv_term_desc with
  | BvCst bv -> Some bv
  | BvFun (_, _) -> None
  | BvLet (_, _) -> None
  | BvUnop (_, _) -> None
  | BvBnop (_, _, _) -> None
  | BvIte (_, _, _) -> None
  | Select (_, _, _) -> None

(* Smart constructors *)

let bl_var bl_name =
  let bl_hash = Hashtbl.hash bl_name in
  { bl_hash; bl_name }

let bv_var bv_name bv_size =
  let bv_hash = Hashtbl.hash (bv_name, bv_size) in
  { bv_hash; bv_name; bv_size }

let ax_var ax_name idx_size elt_size =
  let ax_hash = Hashtbl.hash (ax_name, idx_size, elt_size) in
  { ax_hash; ax_name; idx_size; elt_size }

let bl_sort = BlSort
let bv_sort i = BvSort i
let ax_sort i j = AxSort (i, j)
let list_hash (f : 'a -> int) (l : 'a list) = Hashtbl.hash (List.map f l)

let term_desc_hash = function
  | BlTerm bl -> Hashtbl.hash bl.bl_term_hash
  | BvTerm bv -> Hashtbl.hash bv.bv_term_hash
  | AxTerm ax -> Hashtbl.hash ax.ax_term_hash

let term term_desc =
  let term_hash = term_desc_hash term_desc in
  { term_hash; term_desc }

let mk_bl_term bl = term (BlTerm bl)
let mk_bv_term bv = term (BvTerm bv)
let mk_ax_term ax = term (AxTerm ax)

let mk_bv_cst bv =
  let bv_term_hash = -Hashtbl.hash bv in
  let bv_term_desc = BvCst bv in
  let bv_term_size = Bitvector.size_of bv in
  { bv_term_hash; bv_term_desc; bv_term_size }

(* Boolean terms *)

let mk_bl_true =
  let bl_term_hash = -Hashtbl.hash BlTrue in
  { bl_term_hash; bl_term_desc = BlTrue }

let mk_bl_false =
  let bl_term_hash = -Hashtbl.hash BlFalse in
  { bl_term_hash; bl_term_desc = BlFalse }

let rec bl_term bl_term_desc =
  match bl_term_desc with
  | BlTrue -> mk_bl_true
  | BlFalse -> mk_bl_false
  | BlFun (v, ls) ->
      let bl_term_hash =
        Hashtbl.hash (v.bl_hash, list_hash (fun tm -> tm.term_hash) ls)
      in
      { bl_term_hash; bl_term_desc }
  | BlLet ([], bl) -> bl
  | BlLet (bn, bl) ->
      let bl_term_hash =
        Hashtbl.hash (list_hash (fun df -> df.def_hash) bn, bl.bl_term_hash)
      in
      { bl_term_hash; bl_term_desc }
  | BlUnop (u, bl) -> (
      match is_bl_cst bl with
      | None -> (
          match bl.bl_term_desc with
          | BlUnop (BlNot, bl) -> bl
          | BvComp (c, bv1, bv2) ->
              let c =
                match c with
                | BvEqual -> BvDistinct
                | BvDistinct -> BvEqual
                | BvUlt -> BvUge
                | BvUle -> BvUgt
                | BvUgt -> BvUle
                | BvUge -> BvUlt
                | BvSlt -> BvSge
                | BvSle -> BvSgt
                | BvSgt -> BvSle
                | BvSge -> BvSlt
              in
              bl_term (BvComp (c, bv1, bv2))
          | _ ->
              let bl_term_hash = Hashtbl.hash (u, bl.bl_term_hash) in
              { bl_term_hash; bl_term_desc })
      | Some bl -> (
          match u with BlNot -> if bl then mk_bl_false else mk_bl_true))
  | BlBnop (b, bl1, bl2) -> (
      match (is_bl_cst bl1, is_bl_cst bl2) with
      | None, None ->
          (* syntactic equality *)
          if equal_bl_term bl1 bl2 then
            match b with
            | BlImply -> mk_bl_true
            | BlAnd -> bl1
            | BlOr -> bl1
            | BlXor -> mk_bl_false
          else
            let bl_term_hash =
              Hashtbl.hash (b, bl1.bl_term_hash, bl2.bl_term_hash)
            in
            { bl_term_hash; bl_term_desc }
      | Some bl1, None -> (
          match b with
          | BlImply -> if not bl1 then mk_bl_true else bl2
          | BlAnd -> if bl1 then bl2 else mk_bl_false
          | BlOr -> if bl1 then mk_bl_true else bl2
          | BlXor -> if bl1 then mk_bl_not bl2 else bl2)
      | None, Some bl2 -> (
          match b with
          | BlImply -> if bl2 then mk_bl_true else bl1
          | BlAnd -> if bl2 then bl1 else mk_bl_false
          | BlOr -> if bl2 then mk_bl_true else bl1
          | BlXor -> if bl2 then mk_bl_not bl1 else bl1)
      | Some bl1, Some bl2 ->
          if
            match b with
            | BlImply -> (not bl1) || bl2
            | BlAnd -> bl1 && bl2
            | BlOr -> bl1 || bl2
            | BlXor -> bl1 <> bl2
          then mk_bl_true
          else mk_bl_false)
  | BlComp (c, bl1, bl2) -> (
      match (is_bl_cst bl1, is_bl_cst bl2) with
      | None, None -> (
          if (* syntactic equality *)
             equal_bl_term bl1 bl2 then
            match c with BlEqual -> mk_bl_true | BlDistinct -> mk_bl_false
          else
            (* Remove negations in both sides of (dis-)equality *)
            match (c, bl1.bl_term_desc, bl2.bl_term_desc) with
            | (BlEqual | BlDistinct), BlUnop (BlNot, bl1'), BlUnop (BlNot, bl2')
              ->
                bl_term (BlComp (c, bl1', bl2'))
            | _ ->
                let bl_term_hash =
                  Hashtbl.hash (c, bl1.bl_term_hash, bl2.bl_term_hash)
                in
                { bl_term_hash; bl_term_desc })
      | None, Some bl2 -> (
          match c with
          | BlEqual -> if bl2 then bl1 else mk_bl_not bl1
          | BlDistinct -> if bl2 then mk_bl_not bl1 else bl1)
      | Some bl1, None -> (
          match c with
          | BlEqual -> if bl1 then bl2 else mk_bl_not bl2
          | BlDistinct -> if bl1 then mk_bl_not bl2 else bl2)
      | Some bl1, Some bl2 ->
          if match c with BlEqual -> bl1 = bl2 | BlDistinct -> bl1 <> bl2 then
            mk_bl_true
          else mk_bl_false)
  | BvComp (c, bv1, bv2) -> (
      let bl_term_hash = Hashtbl.hash (c, bv1.bv_term_hash, bv2.bv_term_hash) in
      match (is_bv_cst bv1, is_bv_cst bv2) with
      | None, None -> (
          if (* syntactic equality *)
             equal_bv_term bv1 bv2 then
            match c with
            | BvEqual | BvUle | BvUge | BvSle | BvSge -> mk_bl_true
            | BvDistinct | BvUlt | BvUgt | BvSlt | BvSgt -> mk_bl_false
          else
            (* Remove negations (bvnot and bvneg) on both sides of (dis-)equality *)
            match (c, bv1.bv_term_desc, bv2.bv_term_desc) with
            | (BvEqual | BvDistinct), BvUnop (BvNot, bv1'), BvUnop (BvNot, bv2')
              ->
                bl_term (BvComp (c, bv1', bv2'))
            | (BvEqual | BvDistinct), BvUnop (BvNeg, bv1'), BvUnop (BvNeg, bv2')
              ->
                bl_term (BvComp (c, bv1', bv2'))
            | _ -> { bl_term_hash; bl_term_desc })
      | Some _, None ->
          let c =
            match c with
            | BvEqual | BvDistinct -> c
            | BvUlt -> BvUgt
            | BvUle -> BvUge
            | BvUgt -> BvUlt
            | BvUge -> BvUle
            | BvSlt -> BvSgt
            | BvSle -> BvSge
            | BvSgt -> BvSlt
            | BvSge -> BvSle
          in
          bl_term (BvComp (c, bv2, bv1))
      | None, Some bv -> (
          match c with
          | BvEqual -> (
              match bv1.bv_term_desc with
              | BvUnop (BvNot, bv') ->
                  mk_bv_equal bv' (mk_bv_cst (Bitvector.lognot bv))
              | BvBnop (BvCmp, bv1, bv2) ->
                  if Bitvector.is_one bv then mk_bv_equal bv1 bv2
                  else mk_bv_distinct bv1 bv2
              | BvBnop (BvAdd, bv1, bv2) -> (
                  match is_bv_cst bv2 with
                  | Some bv2 ->
                      mk_bv_equal bv1 (mk_bv_cst (Bitvector.sub bv bv2))
                  | None -> { bl_term_hash; bl_term_desc })
              | BvBnop (BvSub, bv1, bv2) -> (
                  match is_bv_cst bv1 with
                  | Some bv1 ->
                      mk_bv_equal bv2 (mk_bv_cst (Bitvector.sub bv1 bv))
                  | None -> (
                      match is_bv_cst bv2 with
                      | Some bv2 ->
                          mk_bv_equal bv1 (mk_bv_cst (Bitvector.add bv bv2))
                      | None ->
                          if
                            Bitvector.is_zeros bv
                            (* bv1 - bv2 = 0 <=> bv1 = bv2 *)
                          then mk_bv_equal bv1 bv2
                          else { bl_term_hash; bl_term_desc }))
              | BvBnop (BvAnd, bv1', bv2') ->
                  if Bitvector.is_fill bv then
                    mk_bl_and (mk_bv_equal bv1' bv2) (mk_bv_equal bv2' bv2)
                  else { bl_term_hash; bl_term_desc }
              | BvBnop (BvOr, bv1', bv2') ->
                  if Bitvector.is_zeros bv then
                    mk_bl_and (mk_bv_equal bv1' bv2) (mk_bv_equal bv2' bv2)
                  else { bl_term_hash; bl_term_desc }
              | BvBnop (BvConcat, bv1', bv2') ->
                  let sz1 = bv1'.bv_term_size in
                  let sz2 = bv2'.bv_term_size in
                  mk_bl_and
                    (mk_bv_equal bv1'
                       (mk_bv_cst
                          (Bitvector.extract bv
                             Interval.{ lo = sz2; hi = sz1 + sz2 - 1 })))
                    (mk_bv_equal bv2'
                       (mk_bv_cst
                          (Bitvector.extract bv
                             Interval.{ lo = 0; hi = sz2 - 1 })))
              | BvIte (bl, bv1, bv2) -> (
                  match (is_bv_cst bv1, is_bv_cst bv2) with
                  | None, None | None, Some _ | Some _, None ->
                      { bl_term_hash; bl_term_desc }
                  | Some bv1, Some bv2 ->
                      if Bitvector.(equal bv bv1 && not (equal bv bv2)) then bl
                      else if Bitvector.((not (equal bv bv1)) && equal bv bv2)
                      then mk_bl_not bl
                      else { bl_term_hash; bl_term_desc })
              | _ -> { bl_term_hash; bl_term_desc })
          | BvDistinct -> (
              match bv1.bv_term_desc with
              | BvUnop (BvNot, bv') ->
                  mk_bv_distinct bv' (mk_bv_cst (Bitvector.lognot bv))
              | BvBnop (BvCmp, bv1, bv2) ->
                  if Bitvector.is_zero bv then mk_bv_equal bv1 bv2
                  else mk_bv_distinct bv1 bv2
              | BvBnop (BvAdd, bv1, bv2) -> (
                  match is_bv_cst bv2 with
                  (* bv1 + cst2 != cst <=> bv1 != cst3,
                     where cst3 = cst - cst2 *)
                  | Some bv2 ->
                      mk_bv_distinct bv1 (mk_bv_cst (Bitvector.sub bv bv2))
                  | None -> { bl_term_hash; bl_term_desc })
              | BvBnop (BvSub, bv1, bv2) -> (
                  match is_bv_cst bv1 with
                  (* cst1 - bv2 != cst <=> bv2 != cst3,
                     where cst3 = cst1 - cst *)
                  | Some bv1 ->
                      mk_bv_distinct bv2 (mk_bv_cst (Bitvector.sub bv1 bv))
                  | None -> (
                      match is_bv_cst bv2 with
                      (* bv1 - cst2 != cst <=> bv1 != cst3,
                         where cst3 = cst + cst2 *)
                      | Some bv2 ->
                          mk_bv_distinct bv1 (mk_bv_cst (Bitvector.add bv bv2))
                      | None ->
                          if
                            Bitvector.is_zeros bv
                            (* bv1 - bv2 != 0 <=> bv1 != bv2 *)
                          then mk_bv_distinct bv1 bv2
                          else { bl_term_hash; bl_term_desc }))
              | BvIte (bl, bv1, bv2) -> (
                  match (is_bv_cst bv1, is_bv_cst bv2) with
                  | None, None | None, Some _ | Some _, None ->
                      { bl_term_hash; bl_term_desc }
                  | Some bv1, Some bv2 ->
                      if Bitvector.((not (equal bv bv1)) && equal bv bv2) then
                        bl
                      else if Bitvector.(equal bv bv1 && not (equal bv bv2))
                      then mk_bl_not bl
                      else { bl_term_hash; bl_term_desc })
              | _ -> { bl_term_hash; bl_term_desc })
          | _ -> { bl_term_hash; bl_term_desc })
      | Some bv1, Some bv2 ->
          let open Bitvector in
          if
            match c with
            | BvEqual -> equal bv1 bv2
            | BvDistinct -> diff bv1 bv2
            | BvUlt -> ult bv1 bv2
            | BvUle -> ule bv1 bv2
            | BvUgt -> ugt bv1 bv2
            | BvUge -> uge bv1 bv2
            | BvSlt -> slt bv1 bv2
            | BvSle -> sle bv1 bv2
            | BvSgt -> sgt bv1 bv2
            | BvSge -> sge bv1 bv2
          then mk_bl_true
          else mk_bl_false)
  | AxComp (c, ax1, ax2) ->
      (* syntactic equality *)
      if equal_ax_term ax1 ax2 then
        match c with AxEqual -> mk_bl_true | AxDistinct -> mk_bl_false
      else
        let bl_term_hash =
          Hashtbl.hash (c, ax1.ax_term_hash, ax2.ax_term_hash)
        in
        { bl_term_hash; bl_term_desc }
  | BlIte (bl, bl1, bl2) -> (
      match is_bl_cst bl with
      | Some bl -> if bl then bl1 else bl2
      | None ->
          if equal_bl_term bl1 bl2 then bl1
          else
            let bl_term_hash =
              Hashtbl.hash (bl.bl_term_hash, bl1.bl_term_hash, bl2.bl_term_hash)
            in
            { bl_term_hash; bl_term_desc })

and mk_bl_not bl = bl_term (BlUnop (BlNot, bl))
and mk_bv_equal bv1 bv2 = bl_term (BvComp (BvEqual, bv1, bv2))
and mk_bv_distinct bv1 bv2 = bl_term (BvComp (BvDistinct, bv1, bv2))
and mk_bl_and bv1 bv2 = bl_term (BlBnop (BlAnd, bv1, bv2))

let mk_bl_fun fn lst = bl_term (BlFun (fn, lst))
let mk_bl_let bn bl = bl_term (BlLet (bn, bl))
let mk_bl_unop u bl = bl_term (BlUnop (u, bl))
let mk_bl_bnop b bl1 bl2 = bl_term (BlBnop (b, bl1, bl2))
let mk_bl_comp c bl1 bl2 = bl_term (BlComp (c, bl1, bl2))
let mk_bv_comp c bv1 bv2 = bl_term (BvComp (c, bv1, bv2))
let mk_ax_comp c ax1 ax2 = bl_term (AxComp (c, ax1, ax2))
let mk_bl_ite bl bl1 bl2 = bl_term (BlIte (bl, bl1, bl2))

(* Bitvector terms *)

let bv_unop_size u bv =
  match u with
  | BvNot | BvNeg -> bv.bv_term_size
  | BvRepeat i ->
      assert (i >= 1);
      i * bv.bv_term_size
  | BvZeroExtend i | BvSignExtend i ->
      assert (i >= 0);
      i + bv.bv_term_size
  | BvRotateLeft i | BvRotateRight i ->
      assert (i >= 0);
      bv.bv_term_size
  | BvExtract i -> i.Interval.hi - i.Interval.lo + 1

let bv_bnop_size b bv1 bv2 =
  match b with
  | BvConcat -> bv1.bv_term_size + bv2.bv_term_size
  | BvCmp ->
      assert (bv1.bv_term_size = bv2.bv_term_size);
      1
  | BvAnd | BvNand | BvOr | BvNor | BvXor | BvXnor | BvAdd | BvSub | BvMul
  | BvUdiv | BvSdiv | BvUrem | BvSrem | BvSmod | BvShl | BvAshr | BvLshr ->
      assert (bv1.bv_term_size = bv2.bv_term_size);
      bv1.bv_term_size

let mk_bv_zero = mk_bv_cst Bitvector.zero
let mk_bv_one = mk_bv_cst Bitvector.one
let mk_bv_zeros n = mk_bv_cst (Bitvector.zeros n)
let mk_bv_ones n = mk_bv_cst (Bitvector.ones n)
let mk_bv_fill n = mk_bv_cst (Bitvector.fill n)

let rec bv_term bv_term_desc =
  match bv_term_desc with
  | BvCst bv -> mk_bv_cst bv
  | BvFun (v, ls) ->
      let bv_term_size = v.bv_size in
      let bv_term_hash =
        Hashtbl.hash (v.bv_hash, list_hash (fun tm -> tm.term_hash) ls)
      in
      { bv_term_hash; bv_term_desc; bv_term_size }
  | BvLet ([], bv) -> bv
  | BvLet (bn, bv) ->
      let bv_term_size = bv.bv_term_size in
      let bv_term_hash =
        Hashtbl.hash (list_hash (fun df -> df.def_hash) bn, bv.bv_term_hash)
      in
      { bv_term_hash; bv_term_desc; bv_term_size }
  | BvUnop (u, bv) -> (
      match is_bv_cst bv with
      | None -> (
          let bv_term_size = bv_unop_size u bv in
          let bv_term_hash = Hashtbl.hash (u, bv.bv_term_hash) in
          match u with
          | BvNot -> (
              match bv.bv_term_desc with
              | BvUnop (BvNot, bv) -> bv
              | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvNeg -> (
              match bv.bv_term_desc with
              | BvUnop (BvNeg, bv) -> bv
              | BvBnop (BvAdd, bv1, bv2) -> (
                  match (is_bv_cst bv1, is_bv_cst bv2) with
                  | Some bv1, None ->
                      mk_bv_sub (mk_bv_cst (Bitvector.neg bv1)) bv2
                  | None, Some bv2 ->
                      mk_bv_sub (mk_bv_cst (Bitvector.neg bv2)) bv1
                  | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
              | BvBnop (BvSub, bv1, bv2) -> mk_bv_sub bv2 bv1
              | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvRepeat i -> (
              if i = 1 then bv
              else
                match bv.bv_term_desc with
                | BvUnop (BvRepeat j, bv) ->
                    bv_term (BvUnop (BvRepeat (i * j), bv))
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvZeroExtend i -> (
              if i = 0 then bv
              else
                match bv.bv_term_desc with
                | BvUnop (BvZeroExtend j, bv) ->
                    bv_term (BvUnop (BvZeroExtend (i + j), bv))
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvSignExtend i -> (
              if i = 0 then bv
              else
                match bv.bv_term_desc with
                | BvUnop (BvSignExtend j, bv) ->
                    bv_term (BvUnop (BvSignExtend (i + j), bv))
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvRotateLeft i -> (
              if i = 0 then bv
              else
                match bv.bv_term_desc with
                | BvUnop (BvRotateLeft j, bv) ->
                    bv_term
                      (BvUnop (BvRotateLeft ((i + j) mod bv_term_size), bv))
                | BvUnop (BvRotateRight j, bv) ->
                    if i > j then bv_term (BvUnop (BvRotateLeft (i - j), bv))
                    else bv_term (BvUnop (BvRotateRight (j - i), bv))
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvRotateRight i -> (
              if i = 0 then bv
              else
                match bv.bv_term_desc with
                | BvUnop (BvRotateLeft j, bv) ->
                    if i > j then bv_term (BvUnop (BvRotateRight (i - j), bv))
                    else bv_term (BvUnop (BvRotateLeft (j - i), bv))
                | BvUnop (BvRotateRight j, bv) ->
                    bv_term
                      (BvUnop (BvRotateRight ((i + j) mod bv_term_size), bv))
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvExtract i as op -> (
              if i.Interval.lo = 0 && i.Interval.hi = bv.bv_term_size - 1 then
                bv
              else
                match bv.bv_term_desc with
                | BvUnop (BvZeroExtend _, bv) ->
                    if i.Interval.hi < bv.bv_term_size then
                      bv_term (BvUnop (BvExtract i, bv))
                    else if i.Interval.lo >= bv.bv_term_size then
                      mk_bv_zeros bv_term_size
                    else { bv_term_hash; bv_term_desc; bv_term_size }
                | BvUnop (BvSignExtend _, bv) ->
                    if i.Interval.hi < bv.bv_term_size then
                      bv_term (BvUnop (BvExtract i, bv))
                    else { bv_term_hash; bv_term_desc; bv_term_size }
                | BvUnop (BvExtract j, bv) ->
                    let lo = i.Interval.lo + j.Interval.lo in
                    let hi = i.Interval.hi + j.Interval.lo in
                    bv_term (BvUnop (BvExtract Interval.{ lo; hi }, bv))
                | BvBnop (BvConcat, bv1, bv2) ->
                    if i.Interval.hi < bv2.bv_term_size then
                      bv_term (BvUnop (BvExtract i, bv2))
                    else if i.Interval.lo >= bv2.bv_term_size then
                      let lo = i.Interval.lo - bv2.bv_term_size in
                      let hi = i.Interval.hi - bv2.bv_term_size in
                      bv_term (BvUnop (BvExtract Interval.{ lo; hi }, bv1))
                    else { bv_term_hash; bv_term_desc; bv_term_size }
                | BvBnop (((BvAnd | BvOr | BvXor) as op'), bv1, bv2) -> (
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some _, Some _ -> assert false
                    | None, None -> { bv_term_hash; bv_term_desc; bv_term_size }
                    | _, _ ->
                        mk_bv_bnop op' (mk_bv_unop op bv1) (mk_bv_unop op bv2))
                | Select (n, ax, bv) ->
                    let lo = i.Interval.lo / ax.elt_term_size in
                    let hi = i.Interval.hi / ax.elt_term_size in
                    if lo > 0 || hi < n - 1 then
                      let b = Bitvector.create (Z.of_int lo) bv.bv_term_size in
                      let bv =
                        bv_term
                          (Select (hi - lo + 1, ax, mk_bv_add bv (mk_bv_cst b)))
                      in
                      let hi = i.Interval.hi - (lo * ax.elt_term_size) in
                      let lo = i.Interval.lo - (lo * ax.elt_term_size) in
                      bv_term (BvUnop (BvExtract Interval.{ lo; hi }, bv))
                    else { bv_term_hash; bv_term_desc; bv_term_size }
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size }))
      | Some bv ->
          let open Bitvector in
          mk_bv_cst
            (match u with
            | BvNot -> lognot bv
            | BvNeg -> neg bv
            | BvRepeat i -> concat (List_utils.make i bv)
            | BvZeroExtend i -> extend bv (size_of bv + i)
            | BvSignExtend i -> extend_signed bv (size_of bv + i)
            | BvRotateLeft i -> rotate_left bv i
            | BvRotateRight i -> rotate_right bv i
            | BvExtract i -> extract bv i))
  | BvBnop (b, bv1, bv2) -> (
      let bv_term_size = bv_bnop_size b bv1 bv2 in
      let bv_term_hash = Hashtbl.hash (b, bv1.bv_term_hash, bv2.bv_term_hash) in
      match (is_bv_cst bv1, is_bv_cst bv2) with
      | None, None -> (
          match b with
          | BvAnd | BvOr ->
              if equal_bv_term bv1 bv2 then bv1
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvNand | BvNor ->
              if equal_bv_term bv1 bv2 then bv_term (BvUnop (BvNot, bv1))
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvXor ->
              if equal_bv_term bv1 bv2 then mk_bv_zeros bv_term_size
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvXnor ->
              if equal_bv_term bv1 bv2 then mk_bv_fill bv_term_size
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvCmp ->
              if equal_bv_term bv1 bv2 then mk_bv_one
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvAdd -> (
              match (bv1.bv_term_desc, bv2.bv_term_desc) with
              | _, BvUnop (BvNeg, bv2) -> mk_bv_sub bv1 bv2
              | BvUnop (BvNeg, bv1), _ -> mk_bv_sub bv2 bv1
              | BvBnop (BvAdd, bv11, bv12), BvBnop (BvAdd, bv21, bv22) -> (
                  (* (w+x) + (y+z) *)
                  match
                    ( is_bv_cst bv11,
                      is_bv_cst bv12,
                      is_bv_cst bv21,
                      is_bv_cst bv22 )
                  with
                  | Some bv11, None, Some bv21, None ->
                      mk_bv_add (mk_bv_add bv12 bv22)
                        (mk_bv_cst (Bitvector.add bv11 bv21))
                  | Some bv11, None, None, Some bv22 ->
                      mk_bv_add (mk_bv_add bv12 bv21)
                        (mk_bv_cst (Bitvector.add bv11 bv22))
                  | None, Some bv12, Some bv21, None ->
                      mk_bv_add (mk_bv_add bv11 bv22)
                        (mk_bv_cst (Bitvector.add bv12 bv21))
                  | None, Some bv12, None, Some bv22 ->
                      mk_bv_add (mk_bv_add bv11 bv21)
                        (mk_bv_cst (Bitvector.add bv12 bv22))
                  | Some _, None, None, None ->
                      mk_bv_add (mk_bv_add bv12 bv2) bv11
                  | None, Some _, None, None ->
                      mk_bv_add (mk_bv_add bv11 bv2) bv12
                  | None, None, Some _, None ->
                      mk_bv_add (mk_bv_add bv1 bv22) bv21
                  | None, None, None, Some _ ->
                      mk_bv_add (mk_bv_add bv1 bv21) bv22
                  | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
              | BvBnop (BvSub, bv11, bv12), BvBnop (BvSub, bv21, bv22) ->
                  (* (w-x) + (y-z) *)
                  mk_bv_sub (mk_bv_add bv11 bv21) (mk_bv_add bv12 bv22)
              | BvBnop (BvAdd, bv11, bv12), BvBnop (BvSub, bv21, bv22) -> (
                  (* (w+x) + (y-z) *)
                  match
                    ( is_bv_cst bv11,
                      is_bv_cst bv12,
                      is_bv_cst bv21,
                      is_bv_cst bv22 )
                  with
                  | Some bv11, None, Some bv21, None ->
                      mk_bv_add (mk_bv_sub bv12 bv22)
                        (mk_bv_cst (Bitvector.add bv11 bv21))
                  | Some bv11, None, None, Some bv22 ->
                      mk_bv_add (mk_bv_add bv12 bv21)
                        (mk_bv_cst (Bitvector.sub bv11 bv22))
                  | None, Some bv12, Some bv21, None ->
                      mk_bv_add (mk_bv_sub bv11 bv22)
                        (mk_bv_cst (Bitvector.add bv12 bv21))
                  | None, Some bv12, None, Some bv22 ->
                      mk_bv_add (mk_bv_add bv11 bv21)
                        (mk_bv_cst (Bitvector.sub bv12 bv22))
                  | Some _, None, None, None ->
                      mk_bv_add (mk_bv_add bv12 bv2) bv11
                  | None, Some _, None, None ->
                      mk_bv_add (mk_bv_add bv11 bv2) bv12
                  | None, None, Some _, None ->
                      mk_bv_add (mk_bv_sub bv1 bv22) bv21
                  | None, None, None, Some _ ->
                      mk_bv_sub (mk_bv_add bv1 bv21) bv22
                  | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
              | BvBnop (BvSub, _, _), BvBnop (BvAdd, _, _) ->
                  (* (w-x) + (y+z) *)
                  mk_bv_add bv2 bv1
              | _, _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvSub -> (
              if equal_bv_term bv1 bv2 then mk_bv_zeros bv_term_size
              else
                match (bv1.bv_term_desc, bv2.bv_term_desc) with
                | _, BvUnop (BvNeg, bv2) -> mk_bv_add bv1 bv2
                | _, BvBnop (BvSub, bv21, bv22) ->
                    (* x - (y-z) *)
                    mk_bv_add bv1 (mk_bv_sub bv22 bv21)
                | BvBnop (BvAdd, bv11, bv12), BvBnop (BvAdd, bv21, bv22) -> (
                    (* (w+x) - (y+z) *)
                    match
                      ( is_bv_cst bv11,
                        is_bv_cst bv12,
                        is_bv_cst bv21,
                        is_bv_cst bv22 )
                    with
                    | Some bv11, None, Some bv21, None ->
                        mk_bv_add (mk_bv_sub bv12 bv22)
                          (mk_bv_cst (Bitvector.sub bv11 bv21))
                    | Some bv11, None, None, Some bv22 ->
                        mk_bv_add (mk_bv_sub bv21 bv12)
                          (mk_bv_cst (Bitvector.sub bv11 bv22))
                    | None, Some bv12, Some bv21, None ->
                        mk_bv_add (mk_bv_sub bv11 bv22)
                          (mk_bv_cst (Bitvector.sub bv21 bv12))
                    | None, Some bv12, None, Some bv22 ->
                        mk_bv_add (mk_bv_sub bv11 bv21)
                          (mk_bv_cst (Bitvector.sub bv12 bv22))
                    | Some _, None, None, None ->
                        mk_bv_add (mk_bv_sub bv12 bv2) bv11
                    | None, Some _, None, None ->
                        mk_bv_add (mk_bv_sub bv11 bv2) bv12
                    | None, None, Some _, None ->
                        mk_bv_sub (mk_bv_sub bv1 bv22) bv21
                    | None, None, None, Some _ ->
                        mk_bv_sub (mk_bv_sub bv1 bv21) bv22
                    | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | BvBnop (BvSub, bv11, bv12), BvBnop (BvAdd, bv21, bv22) -> (
                    match
                      ( is_bv_cst bv11,
                        is_bv_cst bv12,
                        is_bv_cst bv21,
                        is_bv_cst bv22 )
                    with
                    | Some bv11, None, Some bv21, None ->
                        mk_bv_sub
                          (mk_bv_cst (Bitvector.sub bv11 bv21))
                          (mk_bv_add bv12 bv22)
                    | Some bv11, None, None, Some bv22 ->
                        mk_bv_sub
                          (mk_bv_cst (Bitvector.sub bv11 bv22))
                          (mk_bv_add bv12 bv21)
                    | None, Some bv12, Some bv21, None ->
                        mk_bv_sub (mk_bv_sub bv11 bv22)
                          (mk_bv_cst (Bitvector.add bv12 bv21))
                    | None, Some bv12, None, Some bv22 ->
                        mk_bv_sub (mk_bv_sub bv11 bv21)
                          (mk_bv_cst (Bitvector.add bv12 bv22))
                    | Some _, None, None, None ->
                        mk_bv_sub bv11 (mk_bv_add bv2 bv12)
                    | None, Some _, None, None ->
                        mk_bv_sub (mk_bv_sub bv11 bv2) bv12
                    | None, None, Some _, None ->
                        mk_bv_sub (mk_bv_sub bv1 bv22) bv21
                    | None, None, None, Some _ ->
                        mk_bv_sub (mk_bv_sub bv1 bv21) bv22
                    | _, _, _, _ -> { bv_term_hash; bv_term_desc; bv_term_size }
                    )
                | BvUnop (BvNeg, bv), BvBnop (BvAdd, bv1, bv2) -> (
                    (* -x - (y+z) *)
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some bv1, None ->
                        mk_bv_sub
                          (mk_bv_cst (Bitvector.neg bv1))
                          (mk_bv_add bv bv2)
                    | None, Some bv2 ->
                        mk_bv_sub
                          (mk_bv_cst (Bitvector.neg bv2))
                          (mk_bv_add bv bv1)
                    | _, _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | _, _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvConcat -> (
              match (bv1.bv_term_desc, bv2.bv_term_desc) with
              | BvBnop (BvConcat, bv11, bv12), _ ->
                  mk_bv_bnop BvConcat bv11 (mk_bv_bnop BvConcat bv12 bv2)
              | BvUnop (BvExtract i, b1), BvUnop (BvExtract j, b2) ->
                  if i.Interval.lo = j.Interval.hi + 1 then
                    if equal_bv_term b1 b2 then
                      bv_term
                        (BvUnop (BvExtract Interval.{ lo = j.lo; hi = i.hi }, b1))
                    else
                      match b1.bv_term_desc with
                      | BvUnop (BvZeroExtend _, bv) | BvUnop (BvSignExtend _, bv)
                        ->
                          if
                            j.Interval.hi < bv.bv_term_size
                            && equal_bv_term bv bv2
                          then
                            bv_term
                              (BvUnop
                                 ( BvExtract Interval.{ lo = j.lo; hi = i.hi },
                                   b1 ))
                          else { bv_term_hash; bv_term_desc; bv_term_size }
                      | _ -> { bv_term_hash; bv_term_desc; bv_term_size }
                  else { bv_term_hash; bv_term_desc; bv_term_size }
              | Select (n1, ax1, idx1), Select (n2, ax2, idx2)
                when equal_ax_term ax1 ax2 ->
                  (* simplify (concat (select a x+1) (select a x)) *)
                  let to_cst n =
                    let bv = Bitvector.of_int ~size:idx1.bv_term_size n in
                    mk_bv_cst bv
                  in
                  (* here the smart constructor will normalize the term so that
                   * a syntactic comparison is not so dumb *)
                  let add bv n = bv_term (BvBnop (BvAdd, bv, to_cst n)) in
                  if equal_bv_term idx1 (add idx2 n2) then
                    bv_term (Select (n1 + n2, ax1, idx2))
                  else { bv_term_hash; bv_term_desc; bv_term_size }
              | _, _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvMul | BvUdiv | BvSdiv | BvUrem | BvSrem | BvSmod | BvShl | BvAshr
          | BvLshr ->
              { bv_term_hash; bv_term_desc; bv_term_size })
      | None, Some bv -> (
          match b with
          | BvAnd -> (
              if Bitvector.is_zeros bv then bv2
              else if Bitvector.is_fill bv then bv1
              else
                match bv1.bv_term_desc with
                | BvBnop (BvConcat, bv1, bv2) ->
                    let b1 =
                      Bitvector.extract bv
                        Interval.
                          { lo = bv2.bv_term_size; hi = bv_term_size - 1 }
                    in
                    let b2 =
                      Bitvector.extract bv
                        Interval.{ lo = 0; hi = bv2.bv_term_size - 1 }
                    in
                    if Bitvector.(is_zeros b1 && is_fill b2) then
                      mk_bv_bnop BvConcat
                        (mk_bv_zeros (Bitvector.size_of b1))
                        bv2
                    else if Bitvector.(is_fill b1 && is_zeros b2) then
                      mk_bv_bnop BvConcat bv1
                        (mk_bv_zeros (Bitvector.size_of b2))
                    else { bv_term_hash; bv_term_desc; bv_term_size }
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvNand ->
              if Bitvector.is_zeros bv then mk_bv_fill bv_term_size
              else if Bitvector.is_fill bv then bv_term (BvUnop (BvNot, bv1))
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvOr -> (
              if Bitvector.is_fill bv then bv2
              else if Bitvector.is_zeros bv then bv1
              else
                match bv1.bv_term_desc with
                | BvBnop (BvConcat, bv1, bv2) ->
                    let b1 =
                      Bitvector.extract bv
                        Interval.
                          { lo = bv2.bv_term_size; hi = bv_term_size - 1 }
                    in
                    let b2 =
                      Bitvector.extract bv
                        Interval.{ lo = 0; hi = bv2.bv_term_size - 1 }
                    in
                    if Bitvector.(is_fill b1 && is_zeros b2) then
                      mk_bv_bnop BvConcat
                        (mk_bv_fill (Bitvector.size_of b1))
                        bv2
                    else if Bitvector.(is_zeros b1 && is_fill b2) then
                      mk_bv_bnop BvConcat bv1
                        (mk_bv_fill (Bitvector.size_of b2))
                    else { bv_term_hash; bv_term_desc; bv_term_size }
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvNor ->
              if Bitvector.is_fill bv then mk_bv_zeros bv_term_size
              else if Bitvector.is_zeros bv then bv_term (BvUnop (BvNot, bv1))
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvXor ->
              if Bitvector.is_zeros bv then bv1
              else if Bitvector.is_fill bv then bv_term (BvUnop (BvNot, bv1))
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvXnor ->
              if Bitvector.is_zeros bv then bv_term (BvUnop (BvNot, bv1))
              else if Bitvector.is_fill bv then bv1
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvAdd -> (
              if Bitvector.is_zeros bv then bv1
              else if Bitvector.is_neg bv && not Bitvector.(equal bv (neg bv))
              then mk_bv_sub bv1 (mk_bv_cst (Bitvector.neg bv))
              else
                match bv1.bv_term_desc with
                | BvUnop (BvNeg, bv1) -> mk_bv_sub bv2 bv1
                | BvBnop (BvAdd, bv1, bv2) -> (
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some bv1, None ->
                        mk_bv_add bv2 (mk_bv_cst (Bitvector.add bv bv1))
                    | None, Some bv2 ->
                        mk_bv_add bv1 (mk_bv_cst (Bitvector.add bv bv2))
                    | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | BvBnop (BvSub, bv1, bv2) -> (
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some bv1, None ->
                        mk_bv_sub (mk_bv_cst (Bitvector.add bv bv1)) bv2
                    | None, Some bv2 ->
                        mk_bv_add bv1 (mk_bv_cst (Bitvector.sub bv bv2))
                    | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvSub -> (
              if Bitvector.is_zeros bv then bv1
              else if Bitvector.is_neg bv && not Bitvector.(equal bv (neg bv))
              then mk_bv_add bv1 (mk_bv_cst (Bitvector.neg bv))
              else
                match bv1.bv_term_desc with
                | BvUnop (BvNeg, bv1) ->
                    mk_bv_sub (mk_bv_cst (Bitvector.neg bv)) bv1
                | BvBnop (BvAdd, bv1, bv2) -> (
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some bv1, None ->
                        mk_bv_add bv2 (mk_bv_cst (Bitvector.sub bv1 bv))
                    | None, Some bv2 ->
                        mk_bv_add bv1 (mk_bv_cst (Bitvector.sub bv2 bv))
                    | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | BvBnop (BvSub, bv1, bv2) -> (
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some bv1, None ->
                        mk_bv_sub (mk_bv_cst (Bitvector.sub bv1 bv)) bv2
                    | None, Some bv2 ->
                        mk_bv_sub bv1 (mk_bv_cst (Bitvector.add bv2 bv))
                    | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvMul ->
              if Bitvector.is_zeros bv then bv2
              else if Bitvector.is_ones bv then bv1
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvShl | BvAshr | BvLshr ->
              if Bitvector.is_zeros bv then bv1
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvUdiv | BvSdiv ->
              if Bitvector.is_ones bv then bv1
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvConcat | BvCmp | BvUrem | BvSrem | BvSmod ->
              { bv_term_hash; bv_term_desc; bv_term_size })
      | Some bv, None -> (
          match b with
          | BvAnd | BvNand | BvOr | BvNor | BvXor | BvXnor | BvAdd | BvMul
          | BvCmp ->
              bv_term (BvBnop (b, bv2, bv1))
          | BvSub -> (
              if Bitvector.is_zeros bv then mk_bv_neg bv2
              else
                match bv2.bv_term_desc with
                | BvUnop (BvNeg, bv2) -> mk_bv_add bv1 bv2
                | BvBnop (BvAdd, bv1, bv2) -> (
                    match (is_bv_cst bv1, is_bv_cst bv2) with
                    | Some bv1, None ->
                        mk_bv_sub (mk_bv_cst (Bitvector.sub bv bv1)) bv2
                    | None, Some bv2 ->
                        mk_bv_sub (mk_bv_cst (Bitvector.sub bv bv2)) bv1
                    | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
                | BvBnop (BvSub, bv1, bv2) ->
                    mk_bv_sub (mk_bv_add bv2 (mk_bv_cst bv)) bv1
                | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | BvUdiv | BvSdiv ->
              if Bitvector.is_zeros bv then mk_bv_zeros bv_term_size
              else { bv_term_hash; bv_term_desc; bv_term_size }
          | BvConcat -> (
              match bv2.bv_term_desc with
              | BvBnop (BvConcat, bv21, bv22) -> (
                  match is_bv_cst bv21 with
                  | None -> { bv_term_hash; bv_term_desc; bv_term_size }
                  | Some bv' ->
                      mk_bv_bnop BvConcat
                        (mk_bv_cst (Bitvector.append bv bv'))
                        bv22)
              | _ -> { bv_term_hash; bv_term_desc; bv_term_size })
          | (BvShl | BvAshr | BvLshr) when Bitvector.is_zeros bv -> bv1
          | BvUrem | BvSrem | BvSmod | BvShl | BvAshr | BvLshr ->
              { bv_term_hash; bv_term_desc; bv_term_size })
      | Some bv1, Some bv2 ->
          let open Bitvector in
          mk_bv_cst
            (match b with
            | BvConcat -> append bv1 bv2
            | BvAnd -> logand bv1 bv2
            | BvNand -> lognot (logand bv1 bv2)
            | BvOr -> logor bv1 bv2
            | BvNor -> lognot (logor bv1 bv2)
            | BvXor -> logxor bv1 bv2
            | BvXnor -> lognot (logxor bv1 bv2)
            | BvCmp -> if equal bv1 bv2 then one else zero
            | BvAdd -> add bv1 bv2
            | BvSub -> sub bv1 bv2
            | BvMul -> mul bv1 bv2
            | BvUdiv -> udiv bv1 bv2
            | BvSdiv -> sdiv bv1 bv2
            | BvUrem -> urem bv1 bv2
            | BvSrem -> srem bv1 bv2
            | BvSmod -> smod bv1 bv2
            | BvShl -> shift_left bv1 (Z.to_int (value_of bv2))
            | BvAshr -> shift_right_signed bv1 (Z.to_int (value_of bv2))
            | BvLshr -> shift_right bv1 (Z.to_int (value_of bv2))))
  | BvIte (bl, bv1, bv2) -> (
      assert (bv1.bv_term_size = bv2.bv_term_size);
      match is_bl_cst bl with
      | Some bl -> if bl then bv1 else bv2
      | None -> (
          if equal_bv_term bv1 bv2 then bv1
          else
            let bv_term_hash =
              Hashtbl.hash (bl, bv1.bv_term_hash, bv2.bv_term_hash)
            in
            let bv_term_size = bv1.bv_term_size in
            match (is_bv_cst bv1, is_bv_cst bv2) with
            | None, None | None, Some _ | Some _, None ->
                { bv_term_hash; bv_term_desc; bv_term_size }
            | Some b1, Some b2 ->
                if Bitvector.is_zero b1 && Bitvector.is_one b2 then
                  bv_term (BvIte (mk_bl_not bl, bv2, bv1))
                else if Bitvector.is_one b1 && Bitvector.is_zero b2 then
                  match bl.bl_term_desc with
                  | BvComp (BvEqual, bv1, bv2) ->
                      bv_term (BvBnop (BvCmp, bv1, bv2))
                  | BvComp (BvDistinct, bv1, bv2) ->
                      bv_term
                        (BvUnop (BvNot, bv_term (BvBnop (BvCmp, bv1, bv2))))
                  | _ -> { bv_term_hash; bv_term_desc; bv_term_size }
                else { bv_term_hash; bv_term_desc; bv_term_size }))
  | Select (n, ax, bv) ->
      assert (n > 0);
      assert (ax.idx_term_size = bv.bv_term_size);
      let bv_term_hash = Hashtbl.hash (n, ax.ax_term_hash, bv.bv_term_hash) in
      let bv_term_size = n * ax.elt_term_size in
      { bv_term_hash; bv_term_desc; bv_term_size }

and mk_bv_neg bv = bv_term (BvUnop (BvNeg, bv))
and mk_bv_add bv1 bv2 = bv_term (BvBnop (BvAdd, bv1, bv2))
and mk_bv_sub bv1 bv2 = bv_term (BvBnop (BvSub, bv1, bv2))
and mk_bv_unop u bv = bv_term (BvUnop (u, bv))
and mk_bv_bnop b bv1 bv2 = bv_term (BvBnop (b, bv1, bv2))

let mk_bv_fun fn lst = bv_term (BvFun (fn, lst))
let mk_bv_let bn bv = bv_term (BvLet (bn, bv))
let mk_bv_ite bl bv1 bv2 = bv_term (BvIte (bl, bv1, bv2))
let mk_select n ax bv = bv_term (Select (n, ax, bv))

(* Array terms *)

let ax_term ax_term_desc =
  match ax_term_desc with
  | AxFun (v, l) ->
      let ax_term_hash =
        Hashtbl.hash (v.ax_hash, list_hash (fun tm -> tm.term_hash) l)
      in
      let idx_term_size = v.idx_size in
      let elt_term_size = v.elt_size in
      { ax_term_hash; ax_term_desc; idx_term_size; elt_term_size }
  | AxLet ([], ax) -> ax
  | AxLet (bn, ax) ->
      let ax_term_hash =
        Hashtbl.hash (list_hash (fun df -> df.def_hash) bn, ax.ax_term_hash)
      in
      let idx_term_size = ax.idx_term_size in
      let elt_term_size = ax.elt_term_size in
      { ax_term_hash; ax_term_desc; idx_term_size; elt_term_size }
  | AxIte (bl, ax1, ax2) -> (
      assert (ax1.idx_term_size = ax2.idx_term_size);
      assert (ax1.elt_term_size = ax2.elt_term_size);
      match is_bl_cst bl with
      | Some bl -> if bl then ax1 else ax2
      | None ->
          if equal_ax_term ax1 ax2 then ax1
          else
            let ax_term_hash =
              Hashtbl.hash (bl, ax1.ax_term_hash, ax2.ax_term_hash)
            in
            let idx_term_size = ax1.idx_term_size in
            let elt_term_size = ax1.elt_term_size in
            { ax_term_hash; ax_term_desc; idx_term_size; elt_term_size })
  | Store (n, ax, bv1, bv2) ->
      assert (n > 0);
      assert (ax.idx_term_size = bv1.bv_term_size);
      assert (n * ax.elt_term_size = bv2.bv_term_size);
      let ax_term_hash =
        Hashtbl.hash (n, ax.ax_term_hash, bv1.bv_term_hash, bv2.bv_term_hash)
      in
      let idx_term_size = ax.idx_term_size in
      let elt_term_size = ax.elt_term_size in
      { ax_term_hash; ax_term_desc; idx_term_size; elt_term_size }

let mk_ax_fun fn lst = ax_term (AxFun (fn, lst))
let mk_ax_let bn ax = ax_term (AxLet (bn, ax))
let mk_ax_ite bl ax1 ax2 = ax_term (AxIte (bl, ax1, ax2))
let mk_store n ax bv1 bv2 = ax_term (Store (n, ax, bv1, bv2))

(* Definition, declaration and entries *)

let def_desc_hash = function
  | BlDef (v, ls, bl) ->
      Hashtbl.hash
        (v.bl_hash, list_hash (fun dc -> dc.decl_hash) ls, bl.bl_term_hash)
  | BvDef (v, ls, bv) ->
      Hashtbl.hash
        (v.bv_hash, list_hash (fun dc -> dc.decl_hash) ls, bv.bv_term_hash)
  | AxDef (v, ls, ax) ->
      Hashtbl.hash
        (v.ax_hash, list_hash (fun dc -> dc.decl_hash) ls, ax.ax_term_hash)

let def def_desc =
  let def_hash = def_desc_hash def_desc in
  { def_hash; def_desc }

let mk_bl_def v lst bl = def (BlDef (v, lst, bl))

let mk_bv_def v lst bv =
  if v.bv_size <> bv.bv_term_size then
    Formula_options.Logger.fatal
      "defining %s (a %d bit bv var) by %a (a %d bit bv value)" v.bv_name
      v.bv_size Printing.p_bvterm bv bv.bv_term_size;
  def (BvDef (v, lst, bv))

let mk_ax_def v lst ax =
  assert (v.idx_size = ax.idx_term_size);
  assert (v.elt_size = ax.elt_term_size);
  def (AxDef (v, lst, ax))

let decl_desc_hash = function
  | BlDecl (v, ls) -> Hashtbl.hash (v.bl_hash, ls)
  | BvDecl (v, ls) -> Hashtbl.hash (v.bv_hash, ls)
  | AxDecl (v, ls) -> Hashtbl.hash (v.ax_hash, ls)

let decl decl_desc =
  let decl_hash = decl_desc_hash decl_desc in
  { decl_hash; decl_desc }

let mk_bl_decl v lst = decl (BlDecl (v, lst))
let mk_bv_decl v lst = decl (BvDecl (v, lst))
let mk_ax_decl v lst = decl (AxDecl (v, lst))

let entry_desc_hash = function
  | Declare dc -> Hashtbl.hash dc.decl_hash
  | Define df -> Hashtbl.hash df.def_hash
  | Assert bl -> Hashtbl.hash bl.bl_term_hash
  | Assume bl -> Hashtbl.hash bl.bl_term_hash
  | Comment s -> Hashtbl.hash s

let entry entry_desc =
  let entry_hash = entry_desc_hash entry_desc in
  { entry_hash; entry_desc }

let mk_declare dc = entry (Declare dc)
let mk_define df = entry (Define df)
let mk_assert bl = entry (Assert bl)
let mk_assume bl = entry (Assume bl)
let mk_comment s = entry (Comment s)

(* Some helpers *)

let mk_bl_var bl = mk_bl_fun bl []
let mk_bv_var bv = mk_bv_fun bv []
let mk_ax_var ax = mk_ax_fun ax []
let mk_bl_not bl = mk_bl_unop BlNot bl
let mk_bv_not bv = mk_bv_unop BvNot bv
let mk_bv_neg bv = mk_bv_unop BvNeg bv
let mk_bv_repeat i bv = mk_bv_unop (BvRepeat i) bv
let mk_bv_zero_extend i bv = mk_bv_unop (BvZeroExtend i) bv
let mk_bv_sign_extend i bv = mk_bv_unop (BvSignExtend i) bv
let mk_bv_rotate_left i bv = mk_bv_unop (BvRotateLeft i) bv
let mk_bv_rotate_right i bv = mk_bv_unop (BvRotateRight i) bv

let mk_bv_extract i bv =
  let open Interval in
  if not (i.hi >= i.lo && bv.bv_term_size > i.hi && i.lo >= 0) then
    Formula_options.Logger.fatal
      "invalid extract of bits [%i;%i] for bv %a of size %i" i.lo i.hi
      Printing.p_bvterm bv bv.bv_term_size;
  mk_bv_unop (BvExtract i) bv

let mk_bl_imply bl1 bl2 = mk_bl_bnop BlImply bl1 bl2
let mk_bl_and bl1 bl2 = mk_bl_bnop BlAnd bl1 bl2
let mk_bl_or bl1 bl2 = mk_bl_bnop BlOr bl1 bl2
let mk_bl_xor bl1 bl2 = mk_bl_bnop BlXor bl1 bl2
let mk_bv_concat bv1 bv2 = mk_bv_bnop BvConcat bv1 bv2
let mk_bv_and bv1 bv2 = mk_bv_bnop BvAnd bv1 bv2
let mk_bv_nand bv1 bv2 = mk_bv_bnop BvNand bv1 bv2
let mk_bv_or bv1 bv2 = mk_bv_bnop BvOr bv1 bv2
let mk_bv_nor bv1 bv2 = mk_bv_bnop BvNor bv1 bv2
let mk_bv_xor bv1 bv2 = mk_bv_bnop BvXor bv1 bv2
let mk_bv_xnor bv1 bv2 = mk_bv_bnop BvXnor bv1 bv2
let mk_bv_cmp bv1 bv2 = mk_bv_bnop BvCmp bv1 bv2
let mk_bv_add bv1 bv2 = mk_bv_bnop BvAdd bv1 bv2
let mk_bv_sub bv1 bv2 = mk_bv_bnop BvSub bv1 bv2
let mk_bv_mul bv1 bv2 = mk_bv_bnop BvMul bv1 bv2
let mk_bv_udiv bv1 bv2 = mk_bv_bnop BvUdiv bv1 bv2
let mk_bv_sdiv bv1 bv2 = mk_bv_bnop BvSdiv bv1 bv2
let mk_bv_urem bv1 bv2 = mk_bv_bnop BvUrem bv1 bv2
let mk_bv_srem bv1 bv2 = mk_bv_bnop BvSrem bv1 bv2
let mk_bv_smod bv1 bv2 = mk_bv_bnop BvSmod bv1 bv2
let mk_bv_shl bv1 bv2 = mk_bv_bnop BvShl bv1 bv2
let mk_bv_ashr bv1 bv2 = mk_bv_bnop BvAshr bv1 bv2
let mk_bv_lshr bv1 bv2 = mk_bv_bnop BvLshr bv1 bv2
let mk_bl_equal bl1 bl2 = mk_bl_comp BlEqual bl1 bl2
let mk_bl_distinct bl1 bl2 = mk_bl_comp BlDistinct bl1 bl2
let mk_bv_equal bv1 bv2 = mk_bv_comp BvEqual bv1 bv2
let mk_bv_distinct bv1 bv2 = mk_bv_comp BvDistinct bv1 bv2
let mk_ax_equal ax1 ax2 = mk_ax_comp AxEqual ax1 ax2
let mk_ax_distinct ax1 ax2 = mk_ax_comp AxDistinct ax1 ax2
let mk_bv_ult bv1 bv2 = mk_bv_comp BvUlt bv1 bv2
let mk_bv_ule bv1 bv2 = mk_bv_comp BvUle bv1 bv2
let mk_bv_ugt bv1 bv2 = mk_bv_comp BvUgt bv1 bv2
let mk_bv_uge bv1 bv2 = mk_bv_comp BvUge bv1 bv2
let mk_bv_slt bv1 bv2 = mk_bv_comp BvSlt bv1 bv2
let mk_bv_sle bv1 bv2 = mk_bv_comp BvSle bv1 bv2
let mk_bv_sgt bv1 bv2 = mk_bv_comp BvSgt bv1 bv2
let mk_bv_sge bv1 bv2 = mk_bv_comp BvSge bv1 bv2

let mk_bv_add_int bv i =
  mk_bv_add bv (mk_bv_cst (Bitvector.create (Z.of_int i) bv.bv_term_size))

let mk_bv_sub_int bv i =
  mk_bv_sub bv (mk_bv_cst (Bitvector.create (Z.of_int i) bv.bv_term_size))

(* Sequence reification *)

let split_assert bl =
  let rec split_assert_aux bl acc seq =
    match bl.bl_term_desc with
    | BlBnop (BlAnd, bl1, bl2) -> split_assert_aux bl1 (bl2 :: acc) seq
    | _ -> (
        let seq = Sequence.push_front (mk_assert bl) seq in
        match acc with [] -> seq | bl :: acc -> split_assert_aux bl acc seq)
  in
  split_assert_aux bl [] Sequence.empty

let empty = { entries = Sequence.empty }
let length fm = Sequence.length fm.entries
let append fm1 fm2 = { entries = Sequence.append fm1.entries fm2.entries }

let push_front en fm =
  match en.entry_desc with
  | Assert bl ->
      let seq = split_assert bl in
      { entries = Sequence.append seq fm.entries }
  | _ -> { entries = Sequence.push_front en fm.entries }

let push_back en fm =
  match en.entry_desc with
  | Assert bl ->
      let seq = split_assert bl in
      { entries = Sequence.append fm.entries seq }
  | _ -> { entries = Sequence.push_back en fm.entries }

let push_front_declare dc fm = push_front (mk_declare dc) fm
let push_front_define df fm = push_front (mk_define df) fm
let push_front_assert bl fm = push_front (mk_assert bl) fm
let push_front_assume bl fm = push_front (mk_assume bl) fm
let push_front_comment s fm = push_front (mk_comment s) fm
let push_back_declare dc fm = push_back (mk_declare dc) fm
let push_back_define df fm = push_back (mk_define df) fm
let push_back_assert bl fm = push_back (mk_assert bl) fm
let push_back_assume bl fm = push_back (mk_assume bl) fm
let push_back_comment s fm = push_back (mk_comment s) fm
let peek_front fm = Sequence.peek_front fm.entries
let peek_back fm = Sequence.peek_back fm.entries

let pop_front fm =
  match Sequence.pop_front fm.entries with
  | Some entries -> Some { entries }
  | None -> None

let pop_back fm =
  match Sequence.pop_back fm.entries with
  | Some entries -> Some { entries }
  | None -> None

let map_forward f fm = { entries = Sequence.map_forward f fm.entries }
let map_backward f fm = { entries = Sequence.map_backward f fm.entries }
let iter_forward f fm = Sequence.iter_forward f fm.entries
let iter_backward f fm = Sequence.iter_backward f fm.entries
let fold_forward f fm acc = Sequence.fold_forward f fm.entries acc
let fold_backward f fm acc = Sequence.fold_backward f fm.entries acc

(* Modules *)

module VarSet = Set.Make (struct
  type t = var

  let compare = compare
end)

module BlVarSet = Set.Make (struct
  type t = bl_var

  let compare = compare
end)

module BvVarSet = Set.Make (struct
  type t = bv_var

  let compare = compare
end)

module AxVarSet = Set.Make (struct
  type t = ax_var

  let compare = compare
end)

module BlVarHashtbl = Hashtbl.Make (struct
  type t = bl_var

  let equal bl1 bl2 = bl1 = bl2
  let hash bl = bl.bl_hash
end)

module BvVarHashtbl = Hashtbl.Make (struct
  type t = bv_var

  let equal bv1 bv2 = bv1 = bv2
  let hash bv = bv.bv_hash
end)

module AxVarHashtbl = Hashtbl.Make (struct
  type t = ax_var

  let equal ax1 ax2 = ax1 = ax2
  let hash ax = ax.ax_hash
end)

module BlTermHashtbl = Hashtbl.Make (struct
  type t = bl_term

  let equal bl1 bl2 = bl1 = bl2
  let hash bl = bl.bl_term_hash
end)

module BvTermHashtbl = Hashtbl.Make (struct
  type t = bv_term

  let equal bv1 bv2 = bv1 = bv2
  let hash bv = bv.bv_term_hash
end)

module AxTermHashtbl = Hashtbl.Make (struct
  type t = ax_term

  let equal ax1 ax2 = ax1 = ax2
  let hash ax = ax.ax_term_hash
end)
