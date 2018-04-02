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

open Basic_types
open Smtlib2
open Smtlib2_visitor
open Formula_type

type logical_state_t = {
  equal: smt_bv_expr option;
  constraints: smt_expr list;
}

(** Not yet used type to provide the faulty Constraint
    or the faulty inequality in case of unsatisfiability *)
type rationale_type =
  | Cst of Smtlib2.smt_expr
  | Ineq of string * Smtlib2.smt_bv_expr * Smtlib2.smt_bv_expr

class qed_checker _ =
  object(self) inherit smt_iter_visitor as super
    (* The goal of this class is to find within a formula
       any invalid clauses constraints in the formula to
       know quickly if it is trivially UNSAT prior to any
       solving. 
    *)

    val mutable status = UNKNOWN
    val mutable rationale = None
    val mutable logical_state = String.Map.empty

    method get_status () = status
    method get_rationale () = rationale

    method check_equality_constraint (name:string) (e:smt_bv_expr): unit =
      if String.Map.mem name logical_state then
        let value = String.Map.find name logical_state in
        match value.equal with
        | Some x -> if x <> e then (status <- UNSAT; rationale <- Some(Ineq(name, e, x))) else ()
        | None -> logical_state <- String.Map.add name {value with equal=Some e} logical_state
      else
        let value = {equal=Some e; constraints=[]} in
        logical_state <- String.Map.add name value logical_state

    method visit_constraint (e:smt_expr): unit =
      match e with
      (* Check simple inequalities *)
      | SmtFalse -> status <- UNSAT; rationale <- Some(Cst(e))
      | SmtComp(SmtBvExpr(SmtBvBinary(SmtBvComp,SmtBvCst bv1, SmtBvCst bv2)), SmtBvExpr(e1))
        when e1 = Smtlib2.bvone && bv1 != bv2 -> status <- UNSAT; rationale <- Some(Cst(e))
      (* Extract equality constraints on variables *)
      | SmtComp(SmtBvExpr(SmtBvBinary(SmtBvComp,SmtBvVar(n, _), SmtBvCst bv2)), SmtBvExpr(e1))
        when e1 = bvone -> (* eg: (= (bvcomp esp 0xXXXXX) #b1) *)
        self#check_equality_constraint n (SmtBvCst bv2)
      | SmtComp(SmtBvExpr(SmtBvVar(n, _)), SmtBvExpr(e1)) when e1 = bvone -> (* eg: (= ZF1 #b1) *)
        self#check_equality_constraint n e1
      | SmtComp(SmtBvExpr(SmtBvBinary(SmtBvComp,SmtBvBinary(SmtBvAdd, SmtBvVar(n, _), SmtBvCst bv1), SmtBvCst bv2)), SmtBvExpr(e1))
        when e1 = bvone -> (* eg: (= (bvcomp (bvadd esp 0xxxx) #0xyyyy) #b1) *)
        self#check_equality_constraint n (constant (Bitvector.sub bv2 bv1))
      | SmtAnd(e1, e2) ->
        self#visit_constraint e1; self#visit_constraint e2
      | _ -> super#visit_smt_expr e

    method visit_define_fun (_name:string) (_e:smt_bv_expr): unit =
      () (* TODO: Complete it and extract constraints from definition. And try to invalidate
            a definition by replacing every variable by their constraint + cst propagation *)

    method visit_formula_entry (entry:formula_entry): unit =
      match entry with
      | Comment _  -> ()
      | VarDefinition (SmtBvExpr(SmtBvVar(name, _)), SmtBvExpr e, csts) -> (* For now ignore memory definitions *)
        List.iter (fun i -> self#visit_constraint i) csts;
        self#visit_define_fun name e
      | Constraint (c) -> self#visit_constraint(c)
      | _ -> ()

  end;;


let create_qed_checker (f:formula) =
  new qed_checker f

let qed_check_entry qed (entry:formula_entry): unit =
  let check_rationale () =
    match qed#get_status (), qed#get_rationale () with
    | UNSAT, Some(Cst e) -> Logger.warning "Qed: Formula UNSAT by constraint:%s" (Smtlib2print.smtexpr_to_string e) 
    | UNSAT, Some(Ineq(name,e1,e2)) -> Logger.warning "Qed: Formula UNSAT by value:%s  %s <> %s"
                                         name (Smtlib2print.smtbvexpr_to_string e1) (Smtlib2print.smtbvexpr_to_string e2)
    | UNSAT, None -> Logger.error "Qed: Rationale must be set for UNSAT formula"
    | _ -> ()
  in
  match qed#get_status () with
  | UNKNOWN -> qed#visit_formula_entry entry; check_rationale ()
  | UNSAT | SAT | TIMEOUT -> ()

let qed_get_status qed = 
  match qed#get_status () with
  | UNKNOWN -> None
  | x -> Some x
