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

open Formula_type

open Smtlib2_visitor
open Smtlib2print
open Smtlib2

let smt_binary_op_to_bv_op (op:smt_bv_binary) =
  let apply f g a b = f a b |> g in
  let int_snd f a b = f a (Bigint.int_of_big_int (Bitvector.value_of b)) in
  match op with
  | SmtBvAdd    -> Bitvector.add
  | SmtBvSub    -> Bitvector.sub
  | SmtBvMult   -> Bitvector.umul
  | SmtBvUdiv   -> Bitvector.udiv
  | SmtBvSdiv   -> Bitvector.sdiv
  | SmtBvUrem   -> Bitvector.urem
  | SmtBvSrem   -> Bitvector.srem
  | SmtBvSmod   -> Bitvector.smod
  | SmtBvOr     -> Bitvector.logor
  | SmtBvNor    -> apply Bitvector.logor Bitvector.lognot
  | SmtBvAnd    -> Bitvector.logand
  | SmtBvNand   -> apply Bitvector.logand Bitvector.lognot
  | SmtBvXor    -> Bitvector.logxor
  | SmtBvXnor   -> apply Bitvector.logxor Bitvector.lognot
  | SmtBvConcat -> Bitvector.append
  | SmtBvShl    -> int_snd Bitvector.shift_left
  | SmtBvLshr   -> int_snd Bitvector.shift_right
  | SmtBvAshr   -> int_snd Bitvector.shift_right_signed
  | SmtBvComp   -> apply Bitvector.equal Bitvector.of_bool
  | SmtBvDiff   -> apply Bitvector.diff  Bitvector.of_bool
  | SmtBvUle    -> apply Bitvector.ule   Bitvector.of_bool
  | SmtBvUlt    -> apply Bitvector.ult   Bitvector.of_bool
  | SmtBvUge    -> apply Bitvector.uge   Bitvector.of_bool
  | SmtBvUgt    -> apply Bitvector.ugt   Bitvector.of_bool
  | SmtBvSle    -> apply Bitvector.sle   Bitvector.of_bool
  | SmtBvSlt    -> apply Bitvector.slt   Bitvector.of_bool
  | SmtBvSge    -> apply Bitvector.sge   Bitvector.of_bool
  | SmtBvSgt    -> apply Bitvector.sgt   Bitvector.of_bool

let smt_binary_op_to_fun (op:smt_bv_binary) =
  let aux_bv = fun f a b -> let bv = f a b in SmtBvCst bv in
  let aux_bool = fun f a b ->
    if Bigint.eq_big_int (Bitvector.value_of (f a b)) Bigint.unit_big_int
    then SmtBvCst Bitvector.one else  SmtBvCst Bitvector.zero in
  let f = match op with
    | SmtBvAdd | SmtBvSub | SmtBvMult | SmtBvUdiv | SmtBvSdiv | SmtBvUrem
    | SmtBvSrem | SmtBvSmod | SmtBvOr | SmtBvNor | SmtBvAnd  | SmtBvNand
    | SmtBvXor | SmtBvXnor | SmtBvConcat | SmtBvShl | SmtBvLshr | SmtBvAshr
    | SmtBvComp -> aux_bv
    | SmtBvDiff | SmtBvUle | SmtBvUlt | SmtBvUge | SmtBvUgt | SmtBvSle
    | SmtBvSlt | SmtBvSge | SmtBvSgt -> aux_bool
  in
  f (smt_binary_op_to_bv_op op)

(* ---------------------------------------------------- *)
(* ---------------- Constant Propagation -------------- *)
class simplify_visitor f max_depth =
  object(self) inherit smt_map_visitor as super

    val mutable toplevel = 0

    method! pre_smt_bv_expr bvexpr =
      match bvexpr with
      | SmtBvVar(name, _) ->
        if toplevel < max_depth then
          try
            let _,(_, var_e, _) = SymbVar.find name f.optim_map in
            if is_symbolic_expr var_e then bvexpr
            else (* Only substitute if not symbolic *)
              (toplevel <- succ toplevel;
               match var_e with
               | SmtBvExpr e -> e
               | SmtABvArrayExpr _ -> Logger.debug "ignore variable :%s" name; bvexpr
               | _ -> Logger.error "Impossible to have something else than bvexpr or abvexpr."; bvexpr)
          with Not_found -> bvexpr (* The var is an input *)
        else bvexpr
      | bv -> bv


    method! smt_bv_var var =
      toplevel <- pred toplevel;
      super#smt_bv_var var

    method! smt_bv_unary op e =
      match op,e with
      | SmtBvExtract(i, j), SmtBvCst bv ->
        assert (i <= j);
        SmtBvCst(Bitvector.extract bv i j)
      | SmtBvZeroExtend i, SmtBvCst bv ->
        (* FIXME *)
        SmtBvCst(Bitvector.extend_unsafe bv (Bitvector.size_of bv + i))
      | SmtBvSignExtend i, SmtBvCst bv ->
        SmtBvCst(Bitvector.extend_signed bv (Bitvector.size_of bv + i))
      | SmtBvExtract(i, j), e when (i = 0 && (j+1) = size_bvexpr e) -> e
      | SmtBvExtract(i, j), SmtBvUnary(SmtBvExtract(i', _), e') ->
        self#smt_bv_unary (SmtBvExtract(i'+i, i'+j)) e'
      | SmtBvExtract(i, j),
        SmtBvUnary((SmtBvZeroExtend _ | SmtBvSignExtend _), e) when j < size_bvexpr e ->
        if i = 0 then e
        else self#smt_bv_unary (SmtBvExtract(i, j)) e
      | SmtBvExtract(i, j),
        SmtBvBinary(
          (SmtBvOr | SmtBvNor | SmtBvAnd | SmtBvNand| SmtBvXor| SmtBvXnor) as op,
          SmtBvUnary((SmtBvZeroExtend _ | SmtBvSignExtend _), e1),
          SmtBvUnary((SmtBvZeroExtend _ | SmtBvSignExtend _), e2))
        when
          let s1 = size_bvexpr e1 in
          let s2 = size_bvexpr e2 in
          s1 = s2 && j < s1 && j < s2 ->
        self#smt_bv_unary (SmtBvExtract(i, j)) (SmtBvBinary(op, e1, e2))
      | SmtBvNot, SmtBvUnary(SmtBvNot, e) -> e
      | SmtBvNot, SmtBvBinary(SmtBvDiff, e1, e2) ->
        self#smt_bv_binary SmtBvComp e1 e2
      | _, _ -> super#smt_bv_unary op e

    method! smt_bv_binary op bv1 bv2 =
      match op,bv1,bv2 with
      | SmtBvOr, e1, e2 when e1 = e2 -> e1
      | SmtBvXor, e1, e2 when e1 = e2 ->
        SmtBvCst(Bitvector.zeros (size_bvexpr e1))
      | SmtBvAdd, SmtBvBinary(SmtBvAdd, e1, SmtBvCst cst1), SmtBvCst cst2 ->
        let res = (smt_binary_op_to_bv_op SmtBvAdd) cst1 cst2 in
        SmtBvBinary(SmtBvAdd, e1, SmtBvCst res)
      | SmtBvAdd, e1, SmtBvCst cst when Bitvector.value_of cst = Bigint.zero_big_int -> e1
      | SmtBvConcat,
        SmtBvUnary(SmtBvExtract(i1, j1), e1),
        SmtBvBinary(SmtBvConcat, SmtBvUnary(SmtBvExtract(i2, j2), e2), e3)
        when e1 = e2 && i1-j2 = 1 ->
        self#smt_bv_binary SmtBvConcat (SmtBvUnary(SmtBvExtract(i2, j1), e1)) e3
      | SmtBvConcat,
        SmtBvUnary(SmtBvExtract(i1, j1), e1),
        SmtBvUnary(SmtBvExtract(i2, j2), e2)
        when e1 = e2 && i1-j2 = 1 ->
        self#smt_bv_unary (SmtBvExtract(i2, j1)) e1
      | _, SmtBvCst bv1,SmtBvCst bv2 ->
        let f = smt_binary_op_to_fun op in
        f bv1 bv2
      | _,_,_ -> super#smt_bv_binary op bv1 bv2

    method! smt_bv_ite e bv1 bv2 =
      match e with
      | SmtTrue -> bv1
      | SmtBvExpr(SmtBvCst bv) when Bitvector.is_one bv -> bv1
      | SmtFalse -> bv2
      | SmtBvExpr(SmtBvCst bv) when Bitvector.is_zero bv -> bv2
      | _ -> super#smt_bv_ite e bv1 bv2


    method! smt_and e1 e2 =
      match e1, e2 with
      | SmtTrue, SmtTrue -> SmtTrue
      | SmtFalse, _ | _, SmtFalse -> SmtFalse
      | _ -> super#smt_and e1 e2

    method! smt_or e1 e2 =
      match e1, e2 with
      | SmtFalse, SmtFalse -> SmtFalse
      | SmtTrue, _ | _, SmtTrue -> SmtTrue
      | _ -> super#smt_or e1 e2

    method! smt_ite e e1 e2 =
      match e with
      | SmtTrue -> e1
      | SmtFalse -> e2
      | _ -> super#smt_ite e e1 e2

    method! smt_comp e1 e2 =
      match e1,e2 with
      | SmtBvExpr(SmtBvCst bv1), SmtBvExpr(SmtBvCst bv2) ->
        if bv1 = bv2 then SmtTrue else SmtFalse
      | SmtBvExpr(SmtBvBinary(SmtBvComp, e1, SmtBvCst bv1)), SmtBvExpr e2
        when Bitvector.size_of bv1 = 1 && e2 = bvone ->
        SmtComp(SmtBvExpr e1, SmtBvExpr(SmtBvCst bv1))
      | SmtBvExpr(SmtBvBinary(SmtBvComp, e1, e2)), SmtBvExpr(SmtBvCst bv) when
          Bitvector.size_of bv = 1 && bvexpr_equal e1 e2 &&
          Bigint.eq_big_int (Bitvector.value_of bv) Bigint.unit_big_int -> SmtTrue
      | _,_ -> super#smt_comp e1 e2

    method! smt_not e =
      match e with
      | SmtNot e -> e
      | SmtComp(e, SmtBvExpr(SmtBvCst bv)) ->
        if Bitvector.is_one bv then SmtComp(e, SmtBvExpr(SmtBvCst Bitvector.zero))
        else if Bitvector.is_zero bv then SmtComp(e, SmtBvExpr(SmtBvCst Bitvector.one))
        else SmtNot(SmtComp(e, SmtBvExpr(SmtBvCst bv)))
      | _ -> super#smt_not e

  end

let propagate_cst (f:formula) ?(recursive=1) (e:smt_expr): smt_expr =
  let visitor = new simplify_visitor f recursive in
  visitor#visit_smt_expr e

let propagate_cst_bv (f:formula) ?(recursive=1) (e:smt_bv_expr): smt_bv_expr =
  let res = propagate_cst f ~recursive (SmtBvExpr e) in
  match res with
  | SmtBvExpr e -> e
  | _ -> failwith "Expression not expected in propagation"

let propagate_cst_abv (f:formula) ?(recursive=1) (e:smt_abv_expr): smt_abv_expr =
  let res = propagate_cst f ~recursive (SmtABvArrayExpr e) in
  match res with
  | SmtABvArrayExpr e -> e
  | _ -> failwith "Expression not expected in propagation"
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)


(* ---------------------------------------------------- *)
(* ---------------------- Rebase ---------------------- *)
class rebase_visitor f =
  object inherit smt_map_visitor as super

    method! smt_bv_binary op bv1 bv2 =
      let bvexpr = super#smt_bv_binary op bv1 bv2 in
      let valid_op op = match op with SmtBvAdd | SmtBvSub -> true | _ -> false in
      let prefix name = Str.replace_first (Str.regexp "[0-9]*$") "" name in
      let aux name sz op1 bv1 op2 bv2 =
        let bv1 = (match op1 with SmtBvSub -> Bitvector.neg bv1 | _ -> bv1) in
        let bv2 = (match op2 with SmtBvSub -> Bitvector.neg bv2 | _ -> bv2) in
        let bv = Bitvector.add bv1 bv2 in
        if Bitvector.is_neg bv then
          let bv = Bitvector.neg bv in
          SmtBvBinary(SmtBvSub,SmtBvVar(name,sz), SmtBvCst bv)
        else
          SmtBvBinary(SmtBvAdd,SmtBvVar(name,sz), SmtBvCst bv)
      in
      match op,bv1,bv2 with
      | op1, SmtBvVar(nm,_), SmtBvCst bv1 ->
        (try
           let _,(_, var_e, _) = SymbVar.find nm f.optim_map in
           match var_e with
           | SmtBvExpr (SmtBvBinary(op2, SmtBvVar(nm2,sz2), SmtBvCst bv2))
             when (prefix nm) = (prefix nm2) ->
             if valid_op op1 && valid_op op2
             then aux nm2 sz2 op1 bv1 op2 bv2
             else bvexpr
           | _ -> bvexpr
         with Not_found -> bvexpr)
      | op1, SmtBvCst bv1, SmtBvBinary(op2, SmtBvVar(nm,sz), SmtBvCst bv2) ->
        if valid_op op1 && valid_op op2
        then aux nm sz op1 bv1 op2 bv2
        else bvexpr
      | _,_,_ -> bvexpr
  end;;

let rebase_bvexpr (f:formula) (e:smt_bv_expr): smt_bv_expr =
  let new_e =
    if f.aux_optim_rebase then
      match e with
      | SmtBvVar(name,_sz) ->
        (try
           let _,(_, var_e, _) = SymbVar.find name f.optim_map in
           match var_e with | SmtBvExpr ex -> ex | _ -> e
         with Not_found -> e)
      | _ -> e
    else e
  in
  let visitor = new rebase_visitor f in
  visitor#visit_smt_bv_expr new_e


let rebase_abvexpr (f:formula) (e:smt_abv_expr): smt_abv_expr =
  let visitor = new rebase_visitor f in
  visitor#visit_smt_abv_expr e

let rebase_expr (f:formula) (e:smt_expr): smt_expr =
  let visitor = new rebase_visitor f in
  visitor#visit_smt_expr e
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)

type k_state = Rebase | Found | Disjoint
let k_history = ref []

let get_row_stats () = 
  let mini, maxi, sum = List.fold_left
      (fun (mini, maxi, sum) (v,_) -> min mini v, max maxi v, sum+v)
      (max_int,0,0) !k_history in
  let found, rebase, disjoint =
    List.fold_left
      (fun (f, r, d) (_, s) ->
         match s with
         | Found -> (f+1, r, d)
         | Rebase -> (f, r+1, d)
         | Disjoint -> (f, r, d+1))
      (0, 0, 0)
      !k_history
  in
  let nb = found + rebase + disjoint in
  mini, maxi, (if nb=0 then 0 else sum/nb), found, rebase, disjoint


(* ---------------------------------------------------- *)
(* ------------------ Read-Over-Write ----------------- *)
class row_visitor f ktimes =

  let rec aux e addr name k sz =
    try
      let _,(_, mem, _) = SymbVar.find name f.optim_map in
      match mem with
      | SmtABvArrayExpr(SmtABvStore32(SmtABvArray(name2,asz,csz),addr2, content))
      | SmtABvArrayExpr(SmtABvStore(SmtABvArray(name2,asz,csz), addr2, content)) ->
        if bvexpr_equal addr addr2 then begin
          k_history := (ktimes-k, Found) :: !k_history;
          Logger.debug ~level:3
            "Match for mem:%s with %s" name2 (smtbvexpr_to_string content);
          let size = Bigint.int_of_big_int sz in
          match mem with
          | SmtABvArrayExpr(SmtABvStore32(_,_,_)) when size = 1 ->
            (* It was a select and we have a store32 *)
            SmtBvUnary(SmtBvExtract(0,7), content)
          | SmtABvArrayExpr(SmtABvStore(_,_,_)) when size = 4 ->
            Logger.debug ~level:3 "Cannot ROW a select32 from a store";
            e
          | _ -> content (* Return the content 'as is' because match the
                            size of the select *)
        end
        else (* different *)
        if k = 0 then begin
          Logger.debug ~level:3 "Rebase k=0";
          k_history := (0,Rebase)::!k_history;
          match e with
          | SmtABvLoad32(_) -> SmtABvLoad32(SmtABvArray(name,asz,csz), addr)
          | SmtABvSelect(_) -> SmtABvSelect(SmtABvArray(name,asz,csz), addr)
          | _ -> assert false
        end
        else begin
          Logger.debug ~level:3
            "Addr1:%s Addr2:%s"
            (smtbvexpr_to_string addr) (smtbvexpr_to_string addr2);
          let add_sz value sz = Bigint.add_big_int value sz in
          let is_not_overlapping v1 v2 v1s v2s =
            (Bigint.lt_big_int v1 v2
             && Bigint.le_big_int (add_sz v1 sz) v2
             && (Bigint.lt_big_int v2 v2s || Bigint.le_big_int v2s v1))
            || (Bigint.lt_big_int v2 v1
                && Bigint.le_big_int (add_sz v2 sz) v1
                && (Bigint.lt_big_int v1 v1s || Bigint.le_big_int v1s v2))
          in
          match addr, addr2 with
          | SmtBvCst bv1,SmtBvCst bv2 ->
            let v1 = Bitvector.value_of bv1 in
            let v2 = Bitvector.value_of bv2 in
            if is_not_overlapping v1 v2 (add_sz v1 sz) (add_sz v2 sz) then
              aux e addr name2 (k-1) sz
            else begin
              Logger.debug ~level:3 "overlap %d !" k;
              k_history := (ktimes-k, Disjoint)::!k_history;
              e
            end
          | SmtBvBinary(op1,SmtBvVar(n1,ns1),SmtBvCst bv1),
            SmtBvBinary(op2,SmtBvVar(n2,ns2),SmtBvCst bv2)
            when op1=op2 && n1=n2 && ns1=ns2 ->
            let v1 = Bitvector.value_of bv1 in
            let v2 = Bitvector.value_of bv2 in
            if is_not_overlapping v1 v2 (add_sz v1 sz) (add_sz v2 sz) then
              aux e addr name2 (k-1) sz
            else begin
              Logger.debug ~level:3 "overlap %d !" k;
              k_history := (ktimes-k, Disjoint)::!k_history;
              e
            end
          | _ ->
            begin
              Logger.debug ~level:3 "Rebase not cst %d" k;
              k_history := (ktimes-k, Disjoint)::!k_history;
              match e with
              | SmtABvLoad32 _ ->
                SmtABvLoad32(SmtABvArray(name,asz,csz), addr)
              | SmtABvSelect _ ->
                SmtABvSelect(SmtABvArray(name,asz,csz), addr)
              | _ -> assert false
            end
        end
      | _ -> e
    with
    | Not_found -> e
  in

  let try_substitute exp =
    match exp with
    | SmtBvVar(name2,_) ->
      (try match SymbVar.find name2 f.optim_map with _,(_,SmtBvExpr e,_) -> e | _ -> exp
       with Not_found -> exp)
    | _ -> exp
  in

  object inherit smt_map_visitor as super

    method! smt_abv_load32 abv bv =
      let bvexpr = super#smt_abv_load32 abv bv in
      match abv,bv with
      | SmtABvArray(name,_,_), addr ->
        let new_addr = try_substitute addr in
        let res = aux bvexpr new_addr name ktimes (Bigint.big_int_of_int 4) in
        (match res with
         | SmtABvLoad32(mem, resaddr)
           when (bvexpr_equal new_addr resaddr) ->
           SmtABvLoad32(mem,addr)
         | _ -> res)
      | _,_ -> bvexpr

    method! smt_abv_select abv bv =
      let bvexpr = super#smt_abv_select abv bv in
      match abv,bv with
      | SmtABvArray(name,_,_), addr ->
        let new_addr = try_substitute addr in
        let res = aux bvexpr new_addr name ktimes (Bigint.big_int_of_int 1) in
        (match res with
         | SmtABvSelect(mem, resaddr)
           when (bvexpr_equal new_addr resaddr) ->
           SmtABvSelect(mem, addr)
         | _ -> res)
      | _,_ -> bvexpr

  end;;

let read_over_write_bv (f:formula) (e:smt_bv_expr): smt_bv_expr =
  let visitor = new row_visitor f f.optim_row_k in
  let new_exp = visitor#visit_smt_bv_expr e in
  if not (bvexpr_equal new_exp e)
  then Logger.debug ~level:3 "ROW:%s -> %s@."
      (smtbvexpr_to_string e) (smtbvexpr_to_string new_exp);
  new_exp

let read_over_write (f:formula) (e:smt_expr): smt_expr =
  let visitor = new row_visitor f f.optim_row_k in
  let new_exp = visitor#visit_smt_expr e in
  if not(expr_equal new_exp e)
  then Logger.debug ~level:3 "ROW:%s -> %s@."
      (smtexpr_to_string e) (smtexpr_to_string new_exp);
  new_exp

let read_over_write_abv (f:formula) (e:smt_abv_expr): smt_abv_expr =
  let visitor = new row_visitor f f.optim_row_k in
  let new_exp = visitor#visit_smt_abv_expr e in
  if not (abvexpr_equal new_exp e)
  then Logger.debug ~level:3 "ROW:%s -> %s@."
      (smtabvexpr_to_string e) (smtabvexpr_to_string new_exp);
  new_exp
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)


(* ---------------------------------------------------- *)
(* ---------------Read-Over-Write-Plus----------------- *)
let rec is_same_base (e1:smt_bv_expr) (e2:smt_bv_expr): bool =
  match e1, e2 with
  | SmtBvCst _, SmtBvCst _ -> true
  | SmtBvVar(v1), SmtBvVar(v2) -> v1 = v2
  | SmtBvUnary(op1, e1),SmtBvUnary(op2, e2) when op1=op2 -> is_same_base e1 e2
  | SmtBvBinary(bop1, SmtBvCst(_), SmtBvVar(v1)), SmtBvBinary(bop2, SmtBvCst(_), SmtBvVar(v2))
  | SmtBvBinary(bop1, SmtBvVar(v1), SmtBvCst(_)), SmtBvBinary(bop2, SmtBvVar(v2), SmtBvCst(_)) when bop1=bop2 -> v1 = v2
  | SmtBvBinary(_, SmtBvVar(v1), _), SmtBvVar(v2)
  | SmtBvVar(v1), SmtBvBinary(_, SmtBvVar(v2), _) -> v1 = v2
  | _ -> false

let try_substitute_same_base (f:formula) (e1:smt_bv_expr) (e2:smt_bv_expr): smt_bv_expr * bool = (* Heuristics of "edi phenomenom" to retry is same_base with value of content *)
  let aux name =
    (try
       match SymbVar.find name f.optim_map with
       | _,(_,SmtBvExpr e,_) ->
         let res = is_same_base e1 e in
         Logger.debug ~level:1 "[Try substitute %s by %s(current:%s) => %b]"
           (smtbvexpr_to_string e2) (smtbvexpr_to_string e)
           (smtbvexpr_to_string e1) res
         ;
         (* Return the value substituted only if we managed to find a common base *)
         if res then e, res else e2, res
       | _ -> e2, false
     with Not_found -> e2, false)
  in
  match e2 with
  | SmtBvVar(name,_sz) -> aux name
  | SmtBvBinary(bop, SmtBvVar(name, _sz), new_e2) ->
    let new_e, res = aux name in
    if res then
      propagate_cst_bv f (SmtBvBinary(bop, new_e, new_e2)), res
    else
      e2, false
  | _ -> e2, false

let rec get_addr_offset (addr:smt_bv_expr): int64 =
  match addr with
  | SmtBvCst bv -> Bigint.int64_of_big_int (Bitvector.value_of bv)
  | SmtBvVar(_name,_) -> 0L
  | SmtBvBinary(bop, _e1, SmtBvCst bv) ->
    let f = smt_binary_op_to_bv_op bop in
    f (Bitvector.zeros (Bitvector.size_of bv)) bv |> Bitvector.value_of |> Bigint.int64_of_big_int
  | SmtBvBinary(_bop, _e1, e2) -> get_addr_offset e2
  | SmtABvLoad32(_, SmtBvCst bv) -> Bitvector.value_of bv |> Bigint.int64_of_big_int
  | _ -> failwith ("Unknown addr offset for hybrid memory: "^(smtbvexpr_to_string addr))

let update_mapping (map:smt_bv_expr Basic_types.Addr64.Map.t) (addr:smt_bv_expr) (cnt:smt_bv_expr): smt_bv_expr Basic_types.Addr64.Map.t =
  let size = size_bvexpr cnt in
  let conc_addr = get_addr_offset addr in
  let rec aux map addr sz f_cnt l h =
    let st_addr = Int64.add addr (Int64.of_int (sz)) in
    match sz with
    | 0 -> Basic_types.Addr64.Map.add st_addr (smtbv_extract f_cnt l h) map
    | _ -> aux (Basic_types.Addr64.Map.add st_addr (smtbv_extract f_cnt l h) map) addr (sz-1) f_cnt (l-8) (h-8)
  in
  match size/8 with
  | 1 -> Basic_types.Addr64.Map.add conc_addr cnt map
  | 4 -> aux map conc_addr 3 cnt 24 31
  | _ -> failwith (Printf.sprintf "Unknown size for data stored %d: [%s]:%s" size (smtbvexpr_to_string addr) (smtbvexpr_to_string cnt))

let create_new_chunk (name:string) (addr:smt_bv_expr) (cnt:smt_bv_expr): hybrid_mem_chunk =
  let new_map = update_mapping Basic_types.Addr64.Map.empty addr cnt in
  {base=addr; name=name; mapping=new_map}

let update_hybrid_memory (f:formula) (name:string) (e:smt_abv_expr): hybrid_mem_t * smt_abv_expr =
  Logger.debug ~level:1 "Store memory %s: %s "
    name (smtabvexpr_to_string e);
  match e with
  | SmtABvStore32(SmtABvArray(name,_,_),f_addr, f_cnt)
  | SmtABvStore(SmtABvArray(name,_,_), f_addr, f_cnt) ->
    begin match f.hybrid_memory with
      | memory::tl ->
        let is_same = is_same_base memory.base f_addr in
        let new_f_addr, is_same = if is_same then f_addr, is_same else try_substitute_same_base f memory.base f_addr in
        let new_e = match e
          with SmtABvStore32(x,_,c) -> SmtABvStore32(x,new_f_addr,c)
             | SmtABvStore(x,_,c) -> SmtABvStore(x,new_f_addr,c)| _ -> e in
        if is_same then begin
          Logger.debug ~level:1 "[update %s]" memory.name;
          let updated = update_mapping memory.mapping new_f_addr f_cnt in
          {memory with mapping=updated}::tl, new_e
        end
        else begin
          Logger.debug ~level:1 "[fork]";
          (create_new_chunk name new_f_addr f_cnt)::f.hybrid_memory, new_e
        end
      | _ ->
        Logger.debug ~level:1 "[create]";
        [create_new_chunk name f_addr f_cnt], e
    end
  | _ ->
    Logger.warning "Unexpected memory for hybrid optimization";
    f.hybrid_memory, e



class row_hybrid_visitor f =

  let rec recurse_exists map addr i =
    match i with
    | 0 -> true
    | _ -> Basic_types.Addr64.Map.mem (Int64.add addr (Int64.of_int (i-1))) map
           && recurse_exists map addr (i-1)
  in
  let rec recurse_disjoint map addr i =
    match i with
    | 0 -> true
    | _ -> not(Basic_types.Addr64.Map.mem (Int64.add addr (Int64.of_int i)) map)
           && recurse_disjoint map addr (i-1)
  in
  let rec recurse_symb_value map addr i size =
    let index = size - i in
    let f_expr = Basic_types.Addr64.Map.find (Int64.add addr (Int64.of_int index)) map in
    (* let _ = Logger.result 1 (smtbvexpr_to_string f_expr) in *)
    match index with
    | 0 -> f_expr
    | _ ->
      let f_inner = recurse_symb_value map addr (i+1) size in
      begin match f_expr, f_inner with
        | SmtBvUnary(SmtBvExtract(i1, j1), SmtBvCst bv1),
          SmtBvUnary(SmtBvExtract(i2, j2), SmtBvCst bv2) ->
          let bv =
            Bitvector.append
              (Bitvector.extract bv1 i1 j1 ) (Bitvector.extract bv2 i2 j2)
          in SmtBvCst bv
        | SmtBvUnary(SmtBvExtract(i1, j1), SmtBvCst bv1), SmtBvCst bv2 ->
          let bv = Bitvector.append (Bitvector.extract bv1 i1 j1) bv2 in
          SmtBvCst bv
        | SmtBvUnary (SmtBvExtract(_i1, j1), e1), SmtBvUnary(SmtBvExtract(i2, _j2), e2)
          when bvexpr_equal e1 e2 ->
          let size_e = size_bvexpr e1 in
          (* let  _ = Logger.result 1 (Printf.sprintf "Match sz:%d %d:%d %d:%d" size_e i1 j1 i2 j2) in *)
          if size_e = (j1-i2+1) then
            e1 (* pop the extract since we are extracting the whole value *)
          else
            (* let  _ = Logger.result 1 (Printf.sprintf "Next extraction %d:%d" i2 j1) in *)
            SmtBvUnary(SmtBvExtract(i2, j1), e1) (* Return the new extraction *)
        | _ -> SmtBvBinary(SmtBvConcat, f_expr, f_inner) (* Otherwise concat blindly *)
      end
  in

  let rec recurse_memory_chunks h_mem mem_name f_addr size =
    match h_mem with
    | [] -> false, "memory", f_addr
    | chunk::tl ->
      Logger.debug ~level:1 "Read[%d] mem:%s at:%s "
        size mem_name (smtbvexpr_to_string f_addr);
      let is_same = is_same_base chunk.base f_addr in
      let new_f_addr, is_same =
        if is_same
        then f_addr, is_same
        else try_substitute_same_base f chunk.base f_addr in
      if is_same then
        let conc_addr = get_addr_offset new_f_addr in
        if recurse_exists chunk.mapping conc_addr size then begin
          k_history := (Basic_types.Addr64.Map.cardinal chunk.mapping, Found)::!k_history;
          Logger.debug ~level:1 "[exist]";
          true, chunk.name, recurse_symb_value chunk.mapping conc_addr
            0 (size-1)
        end
        else begin
          Logger.debug ~level:1 "[not exists]";
          if recurse_disjoint chunk.mapping conc_addr size then begin
            k_history := (Basic_types.Addr64.Map.cardinal chunk.mapping, Disjoint)::!k_history;
            if tl <> [] then Logger.debug ~level:1 "Recurse !";
            recurse_memory_chunks tl chunk.name new_f_addr size (* Useful to
                                                                   recurse? *)
          end
          else begin
            k_history := (Basic_types.Addr64.Map.cardinal chunk.mapping, Rebase )::!k_history;
            false, chunk.name, new_f_addr (* If not disjoint cannot say much
                                             so rebase to the current memory
                                             chunk *)
          end
        end
      else begin
        Logger.debug ~level:1 "[disjoint]";
        k_history := (Basic_types.Addr64.Map.cardinal chunk.mapping, Disjoint)::!k_history;
        false, mem_name, f_addr
      end
  in

  object inherit smt_map_visitor as super

    method! smt_abv_load32 abv bv =
      match abv,bv with
      | SmtABvArray(name,a,b), f_addr ->
        let replace, arr_name, new_f = recurse_memory_chunks f.hybrid_memory name f_addr 4 in
        if replace then new_f
        else SmtABvLoad32(SmtABvArray(arr_name,a,b), f_addr)
      | _,_ -> super#smt_abv_load32 abv bv

    method! smt_abv_select abv bv =
      match abv,bv with
      | SmtABvArray(name,a,b), f_addr ->
        let replace, arr_name, new_f = recurse_memory_chunks f.hybrid_memory name f_addr 1 in
        if replace then new_f
        else SmtABvSelect(SmtABvArray(arr_name,a,b), f_addr)
      | _,_ -> super#smt_abv_select abv bv


  end;;


let read_over_write_hybrid_abv (f:formula) (e:smt_abv_expr): smt_abv_expr =
  let visitor = new row_hybrid_visitor f in
  let new_exp = visitor#visit_smt_abv_expr e in
  new_exp


let read_over_write_hybrid_bv (f:formula) (e:smt_bv_expr): smt_bv_expr =
  let visitor = new row_hybrid_visitor f in
  let new_exp = visitor#visit_smt_bv_expr e in
  new_exp


let read_over_write_hybrid (f:formula) (e:smt_expr): smt_expr =
  let visitor = new row_hybrid_visitor f in
  let new_exp = visitor#visit_smt_expr e in
  new_exp

(* ----------------------------------------------- *)
(* ------------ Memory flattening ---------------- *)

class memory_flattener_visitor =
  object(self) inherit smt_map_visitor as super

    val memory_map = Hashtbl.create 50

    method private add_vars i =
      let new_symb = SmtBvVar(Printf.sprintf "mem_%Lx" i,8) in
      Hashtbl.add memory_map i new_symb

    method private get_i base i =
      Hashtbl.find memory_map (Int64.add base i)

    method get_new_symbols () =
      Hashtbl.fold (fun _ symb acc ->
          match symb with
          | SmtBvVar(var) -> SmtVarSet.add (SmtBv(var)) acc
          | _ -> acc
        ) memory_map SmtVarSet.empty

    method! smt_abv_load32 abv bv =
      match abv,bv with
      | SmtABvArray _, SmtBvCst addr ->
        let base = Bitvector.value_of addr |> Bigint.int64_of_big_int in
        List.iter (fun i ->
            let addr64 = Int64.add base i in
            if not(Hashtbl.mem memory_map addr64) then
              self#add_vars addr64
          ) [0L;1L;2L;3L];
        SmtBvBinary
          (SmtBvConcat,
           (SmtBvBinary
              (SmtBvConcat,
               (SmtBvBinary
                  (SmtBvConcat,
                   self#get_i base 0L,
                   self#get_i base 1L)),
               self#get_i base 2L)),
           self#get_i base 3L)
      | _,_ -> super#smt_abv_load32 abv bv

    method! smt_abv_select abv bv =
      match abv,bv with
      | SmtABvArray _, SmtBvCst addr ->
        let addr64 = Bitvector.value_of addr |> Bigint.int64_of_big_int in
        if not(Hashtbl.mem memory_map addr64) then
          self#add_vars addr64;
        Hashtbl.find memory_map addr64
      | _,_ -> super#smt_abv_select abv bv

  end
