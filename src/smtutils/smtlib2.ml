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

open Format
   
exception SmtBadSize of string

type smt_bv_unary =
  | SmtBvNeg
  | SmtBvNot
  | SmtBvExtract of int * int
  | SmtBvZeroExtend of int
  | SmtBvSignExtend of int
  | SmtBvRotateL of int
  | SmtBvRotateR of int


type smt_bv_binary =
  (* linear arithmetic *)
  | SmtBvAdd
  | SmtBvSub
  (* non-linear arithmetic *)
  | SmtBvMult
  | SmtBvUdiv
  | SmtBvSdiv
  | SmtBvUrem
  | SmtBvSrem
  | SmtBvSmod
  (* logical *)
  | SmtBvOr
  | SmtBvNor
  | SmtBvAnd
  | SmtBvNand
  | SmtBvXor
  | SmtBvXnor
  | SmtBvConcat
  | SmtBvShl
  | SmtBvLshr
  | SmtBvAshr
  (* comparison *)
  | SmtBvComp
  | SmtBvDiff
  (* the following operators return a boolean *)
  | SmtBvUle
  | SmtBvUlt
  | SmtBvUge
  | SmtBvUgt
  | SmtBvSle
  | SmtBvSlt
  | SmtBvSge
  | SmtBvSgt


type smt_bv_var = string * int (* name, size *)
type smt_abv_array = string * int * int (* name, size index, size content *)

type smt_var_decl =
  | SmtBv of smt_bv_var
  | SmtABv of smt_abv_array

type smt_bv_expr =
  | SmtBvCst of Bitvector.t
  | SmtBvVar of smt_bv_var
  | SmtBvUnary of smt_bv_unary * smt_bv_expr
  | SmtBvBinary of smt_bv_binary * smt_bv_expr * smt_bv_expr
  | SmtBvIte of smt_expr * smt_bv_expr * smt_bv_expr
  | SmtABvSelect of smt_abv_expr * smt_bv_expr
  | SmtBvLet of ((smt_expr * smt_expr) list) * smt_bv_expr
  | SmtABvLoad32 of smt_abv_expr * smt_bv_expr (* syntactic sugar for clearer formulas *)
  | SmtBvToken (* special type acting as placeholder for nested expresion replacement *)

and smt_abv_expr =
  | SmtABvArray of smt_abv_array
  | SmtABvStore of smt_abv_expr * smt_bv_expr * smt_bv_expr
  | SmtABvLet of ((smt_expr * smt_expr) list) * smt_abv_expr
  | SmtABvStore32 of smt_abv_expr * smt_bv_expr * smt_bv_expr (* syntactic sugar for clearer formulas *)

and smt_expr =
  | SmtBvExpr of smt_bv_expr
  | SmtABvArrayExpr of smt_abv_expr
  | SmtAnd of smt_expr * smt_expr
  | SmtOr of smt_expr * smt_expr
  | SmtComp of smt_expr * smt_expr
  | SmtNot of smt_expr
  | SmtLet of ((smt_expr * smt_expr) list) * smt_expr
  | SmtIte of smt_expr * smt_expr * smt_expr
  | SmtTrue
  | SmtFalse
  | SmtToken (* special type acting as placeholder for nested expresion replacement *)
  | SmtComment of string

type smt_result = | SAT | UNSAT | TIMEOUT | UNKNOWN

let smt_result_to_exit_code = function
  | SAT -> 0
  | UNSAT -> 10
  | TIMEOUT -> 11
  | UNKNOWN -> 12

(* Containers for manipulating SMT expressions *)
module SmtVarSet = Set.Make (
  struct
    type t = smt_var_decl
    let compare a b =
      match a, b with
      | SmtBv(n1, _), SmtBv(n2, _)
      | SmtABv(n1, _, _), SmtABv(n2, _, _) -> compare n1 n2
      | SmtBv(_), SmtABv(_) -> -1
      | SmtABv(_), SmtBv(_) -> 1
  end
  )

module SmtBvVarSet = Set.Make (
  struct
    type t = smt_bv_var
    let compare a b = compare (fst a) (fst b)
  end
  )
(* --------------------------------------------- *)


(* Old style function to get the size of an expression *)
let rec size_bvexpr = (function
    | SmtBvCst bv -> Bitvector.size_of bv
    | SmtBvVar (_, size) -> size
    | SmtBvUnary (op, e) ->
      (match op with
       | SmtBvExtract(i ,j) ->
         let s = size_bvexpr e in
         let new_s = j-i+1 in
         if new_s <= s then new_s
         else raise (SmtBadSize "SmtBvExtract")

       | SmtBvZeroExtend(i)
       | SmtBvSignExtend(i) ->
         let s = size_bvexpr e in
         s+i
       | _ -> size_bvexpr e)
    | SmtBvBinary(op, e1, e2) ->
      (match op with
       | SmtBvConcat ->
         let s1 = size_bvexpr e1 in
         let s2 = size_bvexpr e2 in
         s1 + s2
       (* en smtlib, ces opérations renvoie un booléen et non un bitvecteur *)
       | (SmtBvUle|SmtBvUlt|SmtBvUge|SmtBvUgt|SmtBvSle|SmtBvSlt|SmtBvSge|SmtBvSgt) ->
         let s1 = size_bvexpr e1 in
         let s2 = size_bvexpr e2 in
         if(s1 = s2) then 1
         else raise (SmtBadSize "SmtBvBinary - comparison")
       | _ ->
         let s1 = size_bvexpr e1 in
         let s2 = size_bvexpr e2 in
         if (s1 = s2) then s1
         else raise (SmtBadSize "SmtBvBinary"))
    | SmtBvIte(_x, e1, e2) ->
      (*let _ = size_Expr e in*) (* XXX doit forcément valoir 1 ?, le "_" permet de checker la size *)
      let s1 = size_bvexpr e1 in
      let s2 = size_bvexpr e2 in
      if (s1 = s2) then s1
      else raise (SmtBadSize "SmtBvIte")

    | SmtABvSelect(arr, e) ->
      let sizeI, sizeC = size_abvexpr arr in
      let s = size_bvexpr e in
      if (s = sizeI) then
        sizeC
      else raise (SmtBadSize "SmtABvSelect")

    | SmtABvLoad32(arr, e) ->
      let sizeI, _sizeC = size_abvexpr arr in
      let s = size_bvexpr e in
      if (s = sizeI) then
        s
      else raise (SmtBadSize "SmtABvLoad32")

    | SmtBvLet(l, e) ->
      let b = List.fold_left
          (fun b (e1, e2) ->
             match e1, e2 with
             | SmtBvExpr eX, SmtBvExpr eY ->
               let s1 = size_bvexpr eX in
               let s2 = size_bvexpr eY in
               b && (s1 = s2)
             | SmtABvArrayExpr eX, SmtABvArrayExpr eY ->
               let s1 = size_abvexpr eX in
               let s2 = size_abvexpr eY in
               b && (s1 = s2)
             | _, _ -> failwith "SmtSize: Ce cas de let n'est pas géré\n"
          ) true l in
      if b then size_bvexpr e
      else raise (SmtBadSize "SmtBvLet")
    | SmtBvToken -> raise (SmtBadSize "SmtBvToken")
  )

and size_abvexpr = function
  | SmtABvArray(_, sizeI, sizeC) -> sizeI, sizeC
  | SmtABvStore(arr, e1, e2) ->
    let s1 = size_bvexpr e1 in
    let s2 = size_bvexpr e2 in
    let sizeI, sizeC = size_abvexpr arr in
    if (sizeI = s1 && sizeC = s2) then
      s1, s2
    else raise (SmtBadSize "SmtABvStore")
  | SmtABvStore32(arr, e1, e2) ->
    let s1 = size_bvexpr e1 in
    let s2 = size_bvexpr e2 in
    let sizeI, _ = size_abvexpr arr in
    if sizeI = s1 && s2 = Machine.Word_size.get () then
      s1, s2
    else raise (SmtBadSize "SmtABvStore32")
  | SmtABvLet(l, e) ->
    let b = List.fold_left
        (fun b (e1, e2) ->
           match e1, e2 with
           | SmtBvExpr eX, SmtBvExpr eY ->
             let s1 = size_bvexpr eX in
             let s2 = size_bvexpr eY in
             b && (s1 = s2)
           | SmtABvArrayExpr eX, SmtABvArrayExpr eY ->
             let s1 = size_abvexpr eX in
             let s2 = size_abvexpr eY in
             b && (s1 = s2)
           | _, _ -> failwith "SmtSize: Ce cas de let n'est pas géré\n"
        ) true l in
    if b then size_abvexpr e
    else raise (SmtBadSize "SmtABvLet")



(* -------------- headers and utilitarian funs ------------ *)
let smtbv_add_one f = SmtBvBinary(SmtBvAdd,
                                  SmtBvCst(Bitvector.create Bigint.unit_big_int (size_bvexpr f)), f)

let smtbv_add_int f i = SmtBvBinary(SmtBvAdd, SmtBvCst(Bitvector.create
                                                         (Bigint.big_int_of_int i) (size_bvexpr f)), f)

let smtbv_sub_one f = SmtBvBinary(SmtBvSub, f, SmtBvCst(Bitvector.create Bigint.unit_big_int (size_bvexpr f)))

let smtbv_extract f i j = SmtBvUnary(SmtBvExtract(i, j), f)

let smtexpr_is_true (e:smt_expr): bool =
  match e with
  | SmtTrue  -> true
  | SmtBvExpr _ | SmtABvArrayExpr _
  | SmtAnd (_,_) | SmtOr (_,_)
  | SmtComp (_,_) | SmtNot _
  | SmtLet (_,_) | SmtIte (_,_,_)
  | SmtFalse | SmtToken | SmtComment _ -> false


let constant c = SmtBvCst c

let lognot e = SmtNot e
let logand e1 e2 = SmtAnd (e1, e2)
let logor  e1 e2 = SmtOr (e1, e2)
          

let bvone = constant Bitvector.one
let bvzero = constant Bitvector.zero

let one = SmtBvExpr bvone
let zero = SmtBvExpr bvzero


let pp_result ppf = function
  | SAT     -> fprintf ppf "SAT"
  | UNSAT   -> fprintf ppf "UNSAT"
  | UNKNOWN -> fprintf ppf "UNKNOWN"
  | TIMEOUT -> fprintf ppf "TIMEOUT"
(* --------------------------------------------------------- *)
(* --------------------------------------------------------- *)


let couple_eq eq (a, b) (c, d) = eq a b && eq c d

let rec expr_equal a b =
  match a, b with
  | SmtFalse, SmtFalse
  | SmtTrue, SmtTrue
  | SmtToken, SmtToken -> true
  | SmtBvExpr a1, SmtBvExpr b1 -> bvexpr_equal a1 b1
  | SmtABvArrayExpr a1, SmtABvArrayExpr b1 -> abvexpr_equal a1 b1
  | SmtAnd(a1, a2), SmtAnd(b1, b2) -> (expr_equal a1 b1) && (expr_equal a2 b2)
  | SmtComp(a1, a2), SmtComp(b1, b2) -> (expr_equal a1 b1) && (expr_equal a2 b2)
  | SmtOr(a1, a2), SmtOr(b1, b2) -> (expr_equal a1 b1) && (expr_equal a2 b2)
  | SmtNot a1, SmtNot a2 -> expr_equal a1 a2
  | SmtLet(l, e), SmtLet(l1, e1) ->
    expr_equal e e1
    && List.for_all2 (couple_eq expr_equal) l l1

  | SmtIte(a1, a2, a3), SmtIte(b1, b2, b3) ->
    expr_equal a1 b1 && expr_equal a2 b2 && expr_equal a3 b3
  | SmtComment(s1), SmtComment(s2) -> s1 = s2
  | _, _ -> false


and bvexpr_equal a b =
  match a,b with
  | SmtBvCst bv1, SmtBvCst bv2 ->  Bitvector.equal bv1 bv2

  | SmtBvVar(name, size), SmtBvVar(name1, size1) ->
    String.compare name name1 = 0 && size = size1

  | SmtBvUnary(op, e), SmtBvUnary(op1, e1) ->
    op = op1 && bvexpr_equal e e1

  | SmtBvBinary(op, a1, a2), SmtBvBinary(op1, b1, b2)  ->
    (op = op1) && (bvexpr_equal a1 b1) && (bvexpr_equal a2 b2)

  | SmtBvIte(a1, a2, a3), SmtBvIte(b1, b2, b3) ->
    (expr_equal a1 b1) && (bvexpr_equal a2 b2) && (bvexpr_equal a3 b3)

  | SmtABvSelect(arr, e), SmtABvSelect(arr1, e1) ->
    (abvexpr_equal arr arr1) && (bvexpr_equal e e1)

  | SmtABvLoad32(arr, e), SmtABvLoad32(arr1, e1) ->
    (abvexpr_equal arr arr1) && (bvexpr_equal e e1)

  | SmtBvLet(l, e), SmtBvLet(l1, e1) ->
    let l_iter = List.for_all2 (fun (a1, a2) (b1, b2) -> (expr_equal a1 b1) && (expr_equal a2 b2)) l l1 in
    (bvexpr_equal e e1) && l_iter
  | SmtBvToken, SmtBvToken -> true
  | _, _ -> false

and abvexpr_equal a b =
  match a, b with
  | SmtABvArray(name, sizeI, sizeC), SmtABvArray(name1, sizeI1, sizeC1) ->
    String.compare name name1 = 0 && sizeI = sizeI1  && sizeC = sizeC1
  | SmtABvStore(arr, a1, a2), SmtABvStore(arr1, b1, b2)  ->
    let c1 = compare arr arr1 in
    (c1 = 0) && (bvexpr_equal a1 b1) && (bvexpr_equal a2 b2)
  | SmtABvStore32(arr, a1, a2), SmtABvStore32(arr1, b1, b2)  ->
    let c1 = compare arr arr1 in
    (c1 = 0) && (bvexpr_equal a1 b1) && (bvexpr_equal a2 b2)
  | SmtABvLet(l, e), SmtABvLet(l1, e1) ->
    abvexpr_equal e e1 &&
    List.for_all2 (couple_eq expr_equal) l l1
  | _, _ -> false
