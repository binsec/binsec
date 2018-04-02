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


open Dba_types
open Simplification_dba_utils

exception Killed_tmp
exception Used_tmp of int

let get_redundant_assign (lhs_temp, addr) block =
  let rec aux addr acc =
    if Caddress.Map.mem addr acc then None, acc
    else
      let (ik, opc) = try Caddress.Map.find addr block with Not_found -> raise Killed_tmp in
      match ik with
      | Dba.IkAssign (lhs, e, id_next) ->
        if (must_lhs_expr_equal lhs_temp e)
        then ((Some (lhs, addr, id_next)), acc)
        else if (lhs_mustkilled_by_lhs lhs lhs_temp)
        then raise Killed_tmp
        else aux (Caddress.reid addr id_next) (Caddress.Map.add addr (ik, opc) acc)
      | Dba.IkIf (_, Dba.JInner id1, id2) ->
        let acc = Caddress.Map.add addr (ik, opc) acc in
        let instr1, acc = aux (Caddress.reid addr id1) acc in
        let instr2, acc = aux (Caddress.reid addr id2) acc in
        begin
          match instr1, instr2 with
          | None, None -> None, acc
          | Some i, None -> Some i, acc
          | None, Some i -> Some i, acc
          | Some (_, a1, _), Some (_, a2, _) ->
            if a1.Dba.id = a2.Dba.id then instr1, acc else raise Killed_tmp
        end
      | Dba.IkNondetAssume (lhslist, _, id) ->
        let s =
          List.fold_left
            (fun res lhs ->
               if lhs_mustkilled_by_lhs lhs lhs_temp then false
               else res
            ) true lhslist
        in
        if s then aux (Caddress.reid addr id) (Caddress.Map.add addr (ik, opc) acc)
        else raise Killed_tmp
      | Dba.IkNondet (lhs, _, id)
      | Dba.IkUndef (lhs, id)
      | Dba.IkMalloc (lhs, _, id) ->
        if lhs_mustkilled_by_lhs lhs_temp lhs then raise Killed_tmp
        else aux (Caddress.reid addr id) (Caddress.Map.add addr (ik, opc) acc)
      | Dba.IkFree (_, id)
      | Dba.IkPrint (_, id)
      | Dba.IkSJump (Dba.JInner id, _)
      | Dba.IkIf (_, Dba.JOuter _, id)
      | Dba.IkAssert (_, id)
      | Dba.IkAssume (_, id) ->
        aux (Caddress.reid addr id) (Caddress.Map.add addr (ik, opc) acc)
      | Dba.IkSJump (Dba.JOuter _, _)
      | Dba.IkDJump (_, _)
      | Dba.IkStop _ -> None, acc
  in
  aux addr Caddress.Map.empty


let is_not_mayused_in_block temp_lhs insts =
  Caddress.Map.fold (fun _ (ik, _) res ->
      let s = match ik with
        | Dba.IkAssign (lhs, e, _) ->
          not ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
               (lhs_mayused_in_expr temp_lhs e) ||
               (lhs_mayused_in_lhs temp_lhs lhs))
        | Dba.IkSJump (Dba.JInner _, _)
        | Dba.IkSJump (Dba.JOuter _, _)
        | Dba.IkStop (_) -> true
        | Dba.IkDJump (expr, _) -> not (lhs_mayused_in_expr temp_lhs expr)
        | Dba.IkIf (c, _, _)
        | Dba.IkAssert (c, _)
        | Dba.IkAssume (c, _) -> not (lhs_mayused_in_bcond temp_lhs c)
        | Dba.IkNondetAssume (lhslist, c, _) ->
          List.fold_left (fun res lhs ->
              if ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
                  (lhs_mayused_in_bcond temp_lhs c) ||
                  (lhs_mayused_in_lhs temp_lhs lhs))
              then false
              else res
            ) true lhslist
        | Dba.IkNondet (lhs, _, _) -> not (lhs_mustkilled_by_lhs lhs temp_lhs)
        | Dba.IkUndef (lhs, _) -> not (lhs_mustkilled_by_lhs lhs temp_lhs)
        | Dba.IkMalloc (lhs, expr, _) ->
          (not (lhs_mustkilled_by_lhs lhs temp_lhs)) &&
          (not (lhs_mayused_in_expr temp_lhs expr))
        | Dba.IkFree (expr, _off) -> not (lhs_mayused_in_expr temp_lhs expr)
        | Dba.IkPrint (printarglist, _off) ->
          List.fold_left (fun res elem ->
              match elem with
              | Dba.Exp e -> if lhs_mayused_in_expr temp_lhs e then false else res
              | _ -> res)
            true printarglist
      in (s && res)
    ) insts true


let nb_used_in_block temp_lhs insts =
  try
    Caddress.Map.fold (fun _addr (ik, _) nb ->
        if (nb > 0) then raise (Used_tmp (nb));
        match ik with
        | Dba.IkAssign (lhs, e, _id) ->
          if ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
              (lhs_mayused_in_expr temp_lhs e) ||
              (lhs_mayused_in_lhs temp_lhs lhs))
          then nb + 1
          else nb
        | Dba.IkSJump ( _, _)
        | Dba.IkStop (_) -> nb
        | Dba.IkDJump (expr, _) -> if (lhs_mayused_in_expr temp_lhs expr) then nb + 1 else nb
        | Dba.IkIf (c, _, _)
        | Dba.IkAssert (c, _)
        | Dba.IkAssume (c, _) -> if (lhs_mayused_in_bcond temp_lhs c) then nb + 1 else nb
        | Dba.IkNondetAssume (lhslist, c, _) ->
          nb + List.fold_left (fun res lhs ->
              if ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
                  (lhs_mayused_in_bcond temp_lhs c) ||
                  (lhs_mayused_in_lhs temp_lhs lhs))
              then res + 1
              else res
            ) 0 lhslist
        | Dba.IkNondet (lhs, _, _) ->
          if (lhs_mustkilled_by_lhs lhs temp_lhs) then nb + 1 else nb
        | Dba.IkUndef (lhs, _) ->
          if (lhs_mustkilled_by_lhs lhs temp_lhs) then nb + 1 else nb
        | Dba.IkMalloc (lhs, expr, _) ->
          if ((lhs_mustkilled_by_lhs lhs temp_lhs)) ||
             ((lhs_mayused_in_expr temp_lhs expr)) then nb + 1 else nb
        | Dba.IkFree (expr, _off) ->
          if (lhs_mayused_in_expr temp_lhs expr) then nb + 1 else nb
        | Dba.IkPrint (printarglist, _off) ->
          nb + List.fold_left (fun res elem ->
              match elem with
              | Dba.Exp e ->
                if (lhs_mayused_in_expr temp_lhs e) then (res + 1) else res
              | _ -> res)
            0 printarglist
      ) insts 0
  with
    Used_tmp nb -> nb



let rec replace_lhs_in_expr tmp_lhs red_lhs expr =
  match expr with
  | Dba.ExprVar (name1, size1, _vartaglist) ->
    begin
      match tmp_lhs with
      | Dba.LhsVar (name2, _, _) ->
        if name1 = name2 then Dba_types.Expr.of_lvalue red_lhs else expr
      | Dba.LhsVarRestrict (name2, _, i, j) ->
        if name1 = name2 && i = 0 && j = size1 - 1
        then Dba_types.Expr.of_lvalue red_lhs else expr
      | Dba.LhsStore (_size, _endian, expr) -> expr
    end
  | Dba.ExprLoad (size1, endian1, e1) ->
    begin
      match tmp_lhs with
        Dba.LhsVar (_, _, _)
      | Dba.LhsVarRestrict (_, _, _, _) -> expr
      | Dba.LhsStore (_, _, _) ->
        if lhs_mustkilled_by_lhs tmp_lhs (Dba_types.LValue.of_expr expr)
        then Dba_types.Expr.of_lvalue red_lhs
        else
          let e1 = replace_lhs_in_expr tmp_lhs red_lhs e1 in
          Dba.ExprLoad (size1, endian1, e1)
    end
  | Dba.ExprCst _ -> expr
  | Dba.ExprUnary (uop, e) ->
    Dba.ExprUnary (uop, replace_lhs_in_expr tmp_lhs red_lhs e)
  | Dba.ExprBinary (bop, e1, e2) ->
    Dba.ExprBinary (bop, replace_lhs_in_expr tmp_lhs red_lhs e1,
                    replace_lhs_in_expr tmp_lhs red_lhs e2)
  | Dba.ExprRestrict (e, i, j) ->
    Dba.ExprRestrict (replace_lhs_in_expr tmp_lhs red_lhs e, i, j)
  | Dba.ExprExtU (e, size) ->
    Dba.ExprExtU (replace_lhs_in_expr tmp_lhs red_lhs e, size)
  | Dba.ExprExtS (e, size) ->
    Dba.ExprExtS (replace_lhs_in_expr tmp_lhs red_lhs e, size)
  | Dba.ExprIte (cond, e1, e2) ->
    Dba.ExprIte (replace_lhs_in_cond tmp_lhs red_lhs cond,
                 replace_lhs_in_expr tmp_lhs red_lhs e1,
                 replace_lhs_in_expr tmp_lhs red_lhs e2)
  | Dba.ExprAlternative (exprlist, tag) ->
    let exprlist = List.fold_left (fun acc expr ->
        (replace_lhs_in_expr tmp_lhs red_lhs expr) :: acc) [] exprlist
    in
    Dba.ExprAlternative (exprlist, tag)


and replace_lhs_in_cond tmp_lhs red_lhs cond =
  match cond with
    Dba.CondReif e -> Dba.CondReif (replace_lhs_in_expr tmp_lhs red_lhs e)
  | Dba.CondNot c -> Dba.CondNot (replace_lhs_in_cond tmp_lhs red_lhs c)
  | Dba.CondAnd (c1, c2) ->
    Dba.CondAnd ((replace_lhs_in_cond tmp_lhs red_lhs c1),
                 (replace_lhs_in_cond tmp_lhs red_lhs c2))
  | Dba.CondOr (c1, c2)->
    Dba.CondOr ((replace_lhs_in_cond tmp_lhs red_lhs c1),
                (replace_lhs_in_cond tmp_lhs red_lhs c2))
  | Dba.True -> Dba.True
  | Dba.False -> Dba.False



let replace_lhs_in_lhs tmp_lhs red_lhs lhs =
  if lhs_mustkilled_by_lhs lhs tmp_lhs then red_lhs
  else
    match lhs with
    | Dba.LhsVar (_, _, _)
    | Dba.LhsVarRestrict (_, _, _, _) -> lhs
    | Dba.LhsStore (size, endian, expr) ->
      Dba.LhsStore (size, endian, (replace_lhs_in_expr tmp_lhs red_lhs expr))


let replace lhs_red lhs_temp insts block =
  let f addr (ik, opcode) block =
    let instr =
      match ik with
      | Dba.IkAssign (lhs, e, id) ->
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        Dba.IkAssign (lhs, e, id)
      | Dba.IkDJump (e, tag) ->
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        Dba.IkDJump (e, tag)
      | Dba.IkIf (cond, id1, id2) ->
        let cond = replace_lhs_in_cond lhs_temp lhs_red cond in
        Dba.IkIf (cond, id1, id2)
      | Dba.IkAssert (cond, id) ->
        let cond = replace_lhs_in_cond lhs_temp lhs_red cond in
        Dba.IkAssert (cond, id)
      | Dba.IkAssume (cond, id) ->
        let cond = replace_lhs_in_cond lhs_temp lhs_red cond in
        Dba.IkAssume (cond, id)
      | Dba.IkNondetAssume (lhslist, cond, id) ->
        let lhslist = List.fold_left (fun res lhs ->
            (replace_lhs_in_lhs lhs_temp lhs_red lhs) :: res
          ) [] lhslist
        in
        let cond = replace_lhs_in_cond lhs_temp lhs_red cond in
        Dba.IkNondetAssume (lhslist, cond, id)
      | Dba.IkNondet (lhs, region, id) ->
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        Dba.IkNondet (lhs, region, id)
      | Dba.IkUndef (lhs, id) ->
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        Dba.IkUndef (lhs, id)
      | Dba.IkMalloc (lhs, e, id) ->
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        Dba.IkMalloc (lhs, e, id)
      | Dba.IkFree (e, id) ->
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        Dba.IkFree (e, id)
      | Dba.IkPrint (printarglist, id) ->
        let printarglist = List.fold_left (fun res elem ->
            match elem with
            | Dba.Exp e -> (Dba.Exp (replace_lhs_in_expr lhs_temp lhs_red e)) :: res
            | i -> i :: res) [] printarglist
        in
        Dba.IkPrint (printarglist, id)
      | _ -> ik
    in
    Caddress.Map.add addr (instr, opcode) block
  in
  Caddress.Map.fold f insts block


let try_replace addr (lhs_temp, e, id_next, opc) block =
  try
    let inst, insts =
      get_redundant_assign (lhs_temp, Dba_types.Caddress.reid addr id_next) block in
    match inst with
      None ->
      let nb = nb_used_in_block lhs_temp insts in
      if nb = 0 then Caddress.Map.add addr (Dba.IkSJump (Dba.JInner id_next, None), opc) block
      else block
    | Some (lhs_red, addr_red, nid_red) ->
      if is_not_mayused_in_block lhs_red insts
      then
        let bloc = Caddress.Map.add addr_red (Dba.IkSJump (Dba.JInner nid_red, None), opc) block in
        let block =
          Caddress.Map.add
            (Dba_types.Caddress.reid addr 0)
            (Dba.IkAssign (lhs_red, e, id_next), opc) bloc in
        replace lhs_red lhs_temp insts block
      else block
  with
    Killed_tmp -> block


let remove_redundant_assign block =
  let f addr (ik, opc) block =
    (* if id = 0 then  *)
    match ik with
    | Dba.IkAssign (lhs, e, id_next) ->
      if Dba_types.LValue.is_temporary lhs
      then try_replace addr (lhs, e, id_next, opc) block
      else block
    | _ -> block
    (* else block *)
  in
  Caddress.Map.fold f block block



module Env = struct

  include Basic_types.String.Map

  (* Test if env1 contains env2 *)
  let contains env1 env2 =
    let mem vname cst =
      match find vname env1 with
      | v ->  Region_bitvector.equal (`Value v) (`Value cst)
      | exception Not_found -> false in
    for_all mem env2

  let add vname cst env =
    match find vname env with
    | v ->
      if Region_bitvector.equal (`Value v) (`Value cst) then env
      else remove vname env
    | exception Not_found -> add vname cst env
end

module Constant_propagation = struct

  let rec eval_expr env = function
    | Dba.ExprVar(vname, _, _) as e ->
      begin
        match Basic_types.String.Map.find vname env with
        | region, bv -> Expr.constant ~region bv
        | exception Not_found -> e
      end
    | Dba.ExprLoad (sz, en, e) ->
      Expr.load (Basic_types.ByteSize.create sz) en (eval_expr env e)
    | Dba.ExprCst _  as e -> e
    | Dba.ExprUnary (uop, e) ->
      Expr.unary uop (eval_expr env e)
    | Dba.ExprBinary (bop, e1, e2) ->
      Expr.binary bop (eval_expr env e1) (eval_expr env e2)
    | Dba.ExprRestrict (e, o1, o2) ->
      Expr.restrict (eval_expr env e) o1 o2
    | Dba.ExprExtU (e, sz) ->
      Expr.uext (eval_expr env e) (Basic_types.BitSize.create sz)
    | Dba.ExprExtS (e, sz) ->
      Expr.sext (eval_expr env e) (Basic_types.BitSize.create sz)
    | Dba.ExprIte (c, e1, e2) ->
      Expr.ite (eval_cond env c) (eval_expr env e1) (eval_expr env e2)
    | Dba.ExprAlternative _ ->
      failwith "simplification_block: not yet implemented alternative case"


  and eval_cond env = function
    | Dba.CondReif e -> Condition.creified (eval_expr env e)
    | Dba.CondNot c -> Condition.cnot (eval_cond env c)
    | Dba.CondAnd (c1, c2) ->
      Condition.cand (eval_cond env c1) (eval_cond env c2)
    | Dba.CondOr (c1, c2) ->
      Condition.cor (eval_cond env c1) (eval_cond env c2)
    | Dba.True
    | Dba.False as cond -> cond


  let eval_instruction penv = function
    | Dba.IkAssign (lv, e, id) ->
      Instruction.assign lv (eval_expr penv e) id
    | Dba.IkDJump (e, tag) ->
      Instruction.dynamic_jump ~tag (eval_expr penv e)
    | Dba.IkIf (c, jt, id) ->
      Instruction.ite (eval_cond penv c) jt id
    | Dba.IkAssert (c, id) -> Instruction.iassert (eval_cond penv c) id
    | Dba.IkAssume (c, id) -> Instruction.assume (eval_cond penv c) id
    | Dba.IkNondetAssume (lvs, c, id) ->
      Instruction.non_deterministic_assume lvs (eval_cond penv c) id
    | Dba.IkMalloc (lv, e, id) ->
      Instruction.malloc lv (eval_expr penv e) id
    | Dba.IkFree (e, id) -> Instruction.free (eval_expr penv e) id
    | Dba.IkPrint _
    | Dba.IkUndef _
    | Dba.IkNondet _
    | Dba.IkStop _
    | Dba.IkSJump _ as instr -> instr


  let gather_propagations ?(env=Env.empty) block =
    (* All elements are initialized at None *)
    let envs = Block.to_list block |> List.map (fun _ -> None) |> Array.of_list in
    let should_propagate env id  =
      match envs.(id) with
      | None -> true (* this index was never visited *)
      | Some e -> not (Env.contains env e)
    in
    let mark_env env idx = envs.(idx) <- Some env in
    let remove lval env =
      match LValue.name_of lval with
      | Some vname -> Basic_types.String.Map.remove vname env
      | None -> env
    in
    let rec loop env idx =
      if should_propagate env idx then begin
        mark_env env idx;
        match Block.get block idx with
        | Dba.IkAssign (lv, Dba.ExprCst (r, v), idx') ->
          begin
            match LValue.name_of lv with
            | Some vname -> loop (Env.add vname (r,v) env) idx'
            | None -> loop env idx'
          end
        | Dba.IkIf (_, Dba.JInner idx1, idx2) ->
          loop (loop env idx1) idx2
        | Dba.IkNondet (lv, _, id)
        | Dba.IkMalloc (lv, _, id) ->
          loop (remove lv env) id
        | Dba.IkNondetAssume (lvals, _, id) ->
          (* Let's be over-cautious in this case and treat all variables as
             non-constants *)
          let env' = List.fold_left (fun e v -> remove v e) env lvals in
          loop env' id
        | Dba.IkAssert (_, id)
        | Dba.IkAssume (_, id)
        | Dba.IkUndef (_, id)
        | Dba.IkAssign (_, _, id)
        | Dba.IkSJump (Dba.JInner id, _)
        | Dba.IkIf (_, Dba.JOuter _, id)
        | Dba.IkFree (_, id)
        | Dba.IkPrint (_, id) -> loop env id
        | Dba.IkSJump (Dba.JOuter _, _)
        | Dba.IkDJump _
        | Dba.IkStop _ -> env
      end
      else env
    in
    ignore (loop env (Block.start block));
    envs


  let do_propagations block propagation_envs =
    Block.mapi
      (fun i instruction ->
         match propagation_envs.(i) with
         | None -> instruction
         | Some env ->
           if Basic_types.String.Map.is_empty env
           then instruction
           else eval_instruction env instruction)
      block

  let eval block =
    Logger.debug ~level:3 "@[<v 0>Prepropagation@ %a@]" Block.pp block;
    let b =
      gather_propagations block |> do_propagations block in
    Logger.debug ~level:3 "@[<v 0>Post-propagation@ %a@]" Block.pp b;
    b

end





let block_simplifications inst_map =
  Logger.debug ~level:3 "Block simplifications ...";

  (*  let l_blocks = blocks_of_program inst_map in *)
  (*  let f acc block = *)
  (*   let block = constant_propagation block in *)
  (*   let block = remove_redundant_assign block in *)
  (*   merge_blocks block acc *)
  (* in *)
  (* List.fold_left f Caddress.Map.empty l_blocks *)

  remove_redundant_assign inst_map
(* inst_map *)
