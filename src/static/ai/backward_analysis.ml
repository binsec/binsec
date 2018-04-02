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
open Dba
open Normalize_instructions

let update instr cur_addr pre_addr _ varIndexes vars lets inputs env =
  match instr with
  | IkAssign (lhs, expr, _) ->
    let new_let, _, inputs, varIndexes =
      assign_to_f lhs expr varIndexes vars inputs in
    new_let :: lets, env, inputs, varIndexes
  | IkIf (cond, codeaddr, _) ->
    let next_addr =
      match codeaddr with
      | JInner off -> Dba_types.Caddress.reid cur_addr off
      | JOuter addr -> addr
    in
    let c, inputs =
      if Dba_types.Caddress.equal next_addr pre_addr then
        cond_to_f cond inputs varIndexes
      else
        cond_to_f (Dba.CondNot cond) inputs varIndexes
    in
    lets, c :: env, inputs, varIndexes
  | IkSJump (_, Some (Call _))
  | IkDJump (_, Some (Call _)) (* -> raise Not_found  *)
  | IkSJump (_, _)
  | IkDJump (_, _)
  | IkMalloc (_, _, _)
  | IkFree (_, _)
  | IkStop _
  | IkAssert (_, _)
  | IkAssume (_, _)
  | IkNondetAssume (_, _, _)
  | IkNondet (_, _, _)
  | IkUndef (_, _)
  | IkPrint (_, _) -> lets, env, inputs, varIndexes



let values_to_assertions addrStack states varIndexes inputs f =
  let m, _, _, _ = Dba_types.AddressStack.Map.find addrStack states in
  f m varIndexes inputs



let get_targeted_var instr _varIndexes =
  match instr with
  | IkAssign (LhsStore (_, _, ExprVar(name, 32, _)), _, _) ->
    name ^ "0"
  | IkDJump (ExprVar (name, 32, _), _) ->
    name ^ "0"
  | IkDJump (ExprLoad (sz, en,  expr), _) ->
    let inputs = Smtlib2.SmtVarSet.empty in
    let varIndexes = Basic_types.String.Map.empty in
    let smt_v, _ = load_to_smt expr sz en inputs varIndexes in
    Smtlib2print.smtbvexpr_to_string smt_v

  | IkAssign (_, _, _)
  | IkDJump (_, _)
  | IkSJump (_, _)
  | IkIf (_, _, _)
  | IkStop _
  | IkAssert (_, _)
  | IkAssume (_, _)
  | IkNondetAssume (_, _, _)
  | IkNondet (_, _, _)
  | IkUndef (_, _)
  | IkMalloc (_, _, _)
  | IkFree (_, _)
  | IkPrint (_, _) ->
    Logger.error "Instruction %a"
      Dba_printer.Ascii.pp_instruction instr;
    failwith "invalid targeted instruction in backward analysis!"


(* FIXME: What is the goal of this function ? : delay ?
   Checkout : draft
*)
let check_widening_point addr w_pts =
  match Dba_types.Caddress.Map.find addr w_pts with
  | -1 -> Logger.error "Exception raised %@ %a"
            Dba_printer.Ascii.pp_code_address addr
  | _ -> ()
  | exception Not_found -> ()


let backward_refine_elements addrStack states instrs f w_pts =
  let rec collect addrStack varIndexes vars inputs lets env =
    let backward_collect addrStack pre_addr (lets, env, inputs) =
      let addr, _, _ = addrStack in
      let instr, _ = Dba_types.Caddress.Map.find addr instrs in
      let lets, env, inputs, varIndexes =
        update instr addr pre_addr lets varIndexes vars lets inputs env in
      collect addrStack varIndexes vars inputs lets env
    in
    let default =
      let env', inputs =
        values_to_assertions addrStack states varIndexes inputs f in
      let env = env' @ env in
      lets, env, inputs, varIndexes
    in
    let (_, _, _, preds) = Dba_types.AddressStack.Map.find addrStack states in
    if Dba_types.AddressStack.Set.cardinal preds = 1 then
      let (addr, _, _) = addrStack in
      let a =  Dba_types.AddressStack.Set.choose preds in
      let addr_pre, _, _ =  a in
      try
          check_widening_point addr_pre w_pts;
          backward_collect a addr (lets, env, inputs)
      with Not_found -> default
    else default
  in
  let addr, _, _ = addrStack in
  let instr, _ = Dba_types.Caddress.Map.find addr instrs in
  let inputs = SmtVarSet.empty in
  let vars =  Basic_types.String.Map.empty in
  let varIndexes = Basic_types.String.Map.empty in
  let lets, env, inputs, varIndexes =
    collect addrStack varIndexes vars inputs [] [] in
  let var = get_targeted_var instr varIndexes in
  let elements = apply_smt_elements_recovery lets env inputs var varIndexes in
  elements, (lets, env, inputs, var, varIndexes)
