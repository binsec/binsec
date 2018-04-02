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

open Smtlib2

let cnt = ref 0 (* Counter to help having unique var name when inlining load32_at and store32_at *)

let pp_smt_result fmt res =
  let open Format in
  match res with
  | SAT -> fprintf fmt "@{<green>SAT@}"
  | UNSAT -> fprintf fmt "@{<red>UNSAT@}"
  | TIMEOUT -> fprintf fmt "@{<brown>TIMEOUT@}"
  | UNKNOWN -> fprintf fmt "@{<purple>UNKNOWN@}"

let smtres_to_string res =
  pp_smt_result Format.str_formatter res;
  Format.flush_str_formatter ()

let smtbvunary_to_string = function
  | SmtBvNeg -> "bvneg"
  | SmtBvNot -> "bvnot"
  | SmtBvExtract(i, j) -> Printf.sprintf "(_ extract %i %i)" j i
  | SmtBvZeroExtend(i) -> Printf.sprintf "(_ zero_extend %i)" i
  | SmtBvSignExtend(i) -> Printf.sprintf "(_ sign_extend %i)" i
  | SmtBvRotateL(i) -> Printf.sprintf "(_ rotate_left %i)" i
  | SmtBvRotateR(i) -> Printf.sprintf "(_ rotate_right %i)" i

let smtbvbinary_to_string = (function
    (* linear arithmetic *)
    | SmtBvAdd -> "bvadd"
    | SmtBvSub -> "bvsub"
    (* non-linear arithmetic *)
    | SmtBvMult -> "bvmul"
    | SmtBvUdiv -> "bvudiv"
    | SmtBvSdiv -> "bvsdiv"
    | SmtBvUrem -> "bvurem"
    | SmtBvSrem -> "bvsrem"
    | SmtBvSmod -> "bvsmod"
    (* logical *)
    | SmtBvOr -> "bvor"
    | SmtBvNor -> "bvnor"
    | SmtBvAnd -> "bvand"
    | SmtBvNand -> "bvnand"
    | SmtBvXor -> "bvxor"
    | SmtBvXnor -> "bvxnor"
    | SmtBvConcat -> "concat"
    | SmtBvShl -> "bvshl"
    | SmtBvLshr -> "bvlshr"
    | SmtBvAshr -> "bvashr"
    (* comparison *)
    | SmtBvComp -> "bvcomp"
    | SmtBvDiff -> failwith "no explicit translation for SmtBvDiff"
    | SmtBvUle -> "bvule"
    | SmtBvUlt -> "bvult"
    | SmtBvUge -> "bvuge"
    | SmtBvUgt -> "bvugt"
    | SmtBvSle -> "bvsle"
    | SmtBvSlt -> "bvslt"
    | SmtBvSge -> "bvsge"
    | SmtBvSgt -> "bvsgt"
  )

let rec smtbvexpr_to_string ?(inline=false) = function
  | SmtBvCst bv ->
    begin
      match Bitvector.size_of bv with
      | 1 -> Printf.sprintf "#b%d" (Bitvector.value_of bv |> Bigint.int_of_big_int)
      | x when x mod 4 = 0 -> 
        Bitvector.to_hexstring bv |> String_utils.lchop 1 |> (fun s -> "#"^s)
      | x -> Printf.sprintf "(_ bv%s %i)" (Bitvector.value_of bv |> Bigint.string_of_big_int) x
    end
  | SmtBvVar(name, _) -> name
  | SmtBvUnary(op, e) ->
    Printf.sprintf "(%s %s)" (smtbvunary_to_string op) (smtbvexpr_to_string ~inline e)
  | SmtBvBinary(SmtBvDiff, e1, e2) ->
    Printf.sprintf "(bvnot (bvcomp %s %s))" (smtbvexpr_to_string ~inline e1) (smtbvexpr_to_string ~inline e2)
  | SmtBvBinary(op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (smtbvbinary_to_string op) (smtbvexpr_to_string ~inline e1) (smtbvexpr_to_string ~inline e2)
  | SmtBvIte(e, e1, e2) ->
    Printf.sprintf "(ite %s %s %s)" (smtexpr_to_string ~inline e) (smtbvexpr_to_string ~inline e1) (smtbvexpr_to_string ~inline e2)
  | SmtABvSelect(arr, e) ->
    Printf.sprintf "(select %s %s)" (smtabvexpr_to_string ~inline arr) (smtbvexpr_to_string ~inline e)
  | SmtABvLoad32(arr, e) ->
    let arr_str = smtabvexpr_to_string ~inline arr in
    let addr_str = smtbvexpr_to_string ~inline e in
    if not inline then
      Printf.sprintf "(load32_at %s %s)" arr_str addr_str
    else
      (cnt := !cnt + 1;
       Printf.sprintf "(let ((tmp%d %s)) \n\
                       (concat (concat (concat\n\
                       (select %s (bvadd #x00000003 tmp%d))\n\
                       (select %s (bvadd #x00000002 tmp%d)))\n\
                       (select %s (bvadd #x00000001 tmp%d)))\n\
                       (select %s tmp%d)))"
         !cnt addr_str arr_str !cnt arr_str !cnt arr_str !cnt arr_str !cnt)
  | SmtBvLet(l, e) -> 
    let inter = List.fold_left (fun s (n_e, e) ->
        let s2 = Printf.sprintf " (%s %s)" (smtexpr_to_string ~inline n_e) (smtexpr_to_string ~inline e) in s^s2) "" l in
    Printf.sprintf "\n(let (%s) \n%s)" inter (smtbvexpr_to_string ~inline e)
  | SmtBvToken -> "token"


and smtabvexpr_to_string ?(inline=false) = (function
    | SmtABvArray(name, _indice_size, _value_size) -> name
    | SmtABvStore(arr, e1, e2) ->
      Printf.sprintf "(store %s %s %s)"
        (smtabvexpr_to_string ~inline arr) (smtbvexpr_to_string ~inline e1) (smtbvexpr_to_string ~inline e2)
    | SmtABvStore32(arr, e1, e2) ->
      let arr_str = (smtabvexpr_to_string ~inline arr) in
      let addr_str = (smtbvexpr_to_string ~inline e1) in
      let cnt_str = (smtbvexpr_to_string ~inline e2) in
      if not(inline) then
        Printf.sprintf "(store32_at %s %s %s)" arr_str addr_str cnt_str
      else
        (cnt := !cnt +1;
         Printf.sprintf "(let ((tmp_addr%d %s)(tmp_cnt%d %s))\n(store (store (store (store %s\n\t(bvadd #x00000003 tmp_addr%d) ((_ extract 31 24) tmp_cnt%d))\n\t(bvadd #x00000002 tmp_addr%d) ((_ extract 23 16) tmp_cnt%d))\n\t(bvadd #x00000001 tmp_addr%d) ((_ extract 15 8) tmp_cnt%d))\n\ttmp_addr%d ((_ extract 7 0) tmp_cnt%d)))" !cnt addr_str !cnt cnt_str arr_str !cnt !cnt !cnt !cnt !cnt !cnt !cnt !cnt)
    | SmtABvLet(l, e) ->
      let inter = List.fold_left (fun s (n_e, e) -> let s2 = Printf.sprintf "(%s %s)" (smtexpr_to_string ~inline n_e) (smtexpr_to_string ~inline e) in s^s2) "" l in
      Printf.sprintf "\n(let (%s) \n%s)" inter (smtabvexpr_to_string ~inline e)
  )

and smtexpr_to_string ?(inline=false) = (function
    | SmtBvExpr(e) -> smtbvexpr_to_string ~inline e
    | SmtABvArrayExpr(e) -> smtabvexpr_to_string ~inline e
    | SmtComp(e1, e2) -> Printf.sprintf "(= %s %s)" (smtexpr_to_string ~inline e1) (smtexpr_to_string ~inline e2)
    | SmtAnd(e1, e2) ->
      Printf.sprintf "(and %s %s)" (smtexpr_to_string ~inline e1) (smtexpr_to_string ~inline e2)
    | SmtOr(e1, e2) -> Printf.sprintf "(or %s %s)" (smtexpr_to_string ~inline e1) (smtexpr_to_string ~inline e2)
    | SmtNot(e) -> Printf.sprintf "(not %s)" (smtexpr_to_string ~inline e)
    | SmtLet(l, e) ->
      let inter = List.fold_left (fun s (n_e, e) -> let s2 = Printf.sprintf "(%s %s)" (smtexpr_to_string ~inline n_e) (smtexpr_to_string ~inline e) in s^s2) "" l in
      Printf.sprintf "(let (%s)\n%s)" inter (smtexpr_to_string ~inline e)
    | SmtIte(e, e1, e2) -> Printf.sprintf "(ite %s \n%s %s)" (smtexpr_to_string ~inline e) (smtexpr_to_string ~inline e1) (smtexpr_to_string ~inline e2)
    | SmtTrue -> "true"
    | SmtFalse -> "false"
    | SmtToken -> "Token"
    | SmtComment s -> ";---- "^s^"\n"
  )


let bv_to_string name size =
  Printf.sprintf "(declare-fun %s () (_ BitVec %i))" name size

let array_to_string name size1 size2 =
  Printf.sprintf "(declare-fun %s () (Array (_ BitVec %i) (_ BitVec %i)))" name size1 size2

let smtvarset_to_string set =
  SmtVarSet.fold
    (fun var s ->
       match var with
       | SmtBv(name, size) -> Printf.sprintf "%s%s\n" s (bv_to_string name size)
       | SmtABv(name, sizeI, sizeC) -> Printf.sprintf "%s%s\n" s (array_to_string name sizeI sizeC)
    )
    set
    ""

let smt_header () =
  let theory = if !Options.flatten_memory then "BV" else "ABV" in
  Printf.sprintf "(set-logic QF_%s) \n\
                  (set-info :smt-lib-version 2.0)\n" theory

let smt_functions () =
  if !Options.flatten_memory then "" else
    "\
(define-fun load32_at ( (memory (Array (_ BitVec 32) (_ BitVec 8))) (addr (_ BitVec 32))) (_ BitVec 32) \n\
\t(concat (concat (concat \n\
\t(select memory (bvadd #x00000003 addr)) \n\
\t(select memory (bvadd #x00000002 addr))) \n\
\t(select memory (bvadd #x00000001 addr))) \n\
\t(select memory addr)) \n\
) ; Return 32 bits bitvector \n\
\n\
(define-fun store32_at ( (memory (Array (_ BitVec 32) (_ BitVec 8))) (addr (_ BitVec 32)) (content (_ BitVec 32))) (Array (_ BitVec 32) (_ BitVec 8)) \n\
\t(store (store (store (store memory \n\
\t(bvadd #x00000003 addr) ((_ extract 31 24) content)) \n\
\t(bvadd #x00000002 addr) ((_ extract 23 16) content)) \n\
\t(bvadd #x00000001 addr) ((_ extract 15 8) content)) \n\
\taddr ((_ extract 7 0) content)) \n\
) ; Return a new array \n\
"
