(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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


open Simplification_dba_utils
open Simplification_options

exception Killed_tmp
exception Used_tmp of int

let get_redundant_assign (lhs_temp, addr) block =
  let open Dba_types in
  let rec aux addr acc =
    if Caddress.Map.mem addr acc then None, acc
    else
      let (ik, opc) = try Caddress.Map.find addr block with Not_found -> raise Killed_tmp in
      match ik with
      | Dba.Instr.Assign (lhs, e, id_next) ->
        if must_lhs_expr_equal lhs_temp e
        then Some (lhs, addr, id_next), acc
        else if lhs_mustkilled_by_lhs lhs lhs_temp
        then raise Killed_tmp
        else aux (Caddress.reid addr id_next) (Caddress.Map.add addr (ik, opc) acc)
      | Dba.Instr.If (_, Dba.JInner id1, id2) ->
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
      | Dba.Instr.NondetAssume (lhslist, _, id) ->
        let s =
          List.fold_left
            (fun res lhs ->
               if lhs_mustkilled_by_lhs lhs lhs_temp then false
               else res
            ) true lhslist
        in
        if s then aux (Caddress.reid addr id) (Caddress.Map.add addr (ik, opc) acc)
        else raise Killed_tmp
      | Dba.Instr.Nondet (lhs, _, id)
      | Dba.Instr.Undef (lhs, id)
      | Dba.Instr.Malloc (lhs, _, id) ->
        if lhs_mustkilled_by_lhs lhs_temp lhs then raise Killed_tmp
        else aux (Caddress.reid addr id) (Caddress.Map.add addr (ik, opc) acc)
      | Dba.Instr.Free (_, id)
      | Dba.Instr.Print (_, id)
      | Dba.Instr.SJump (Dba.JInner id, _)
      | Dba.Instr.If (_, Dba.JOuter _, id)
      | Dba.Instr.Assert (_, id)
      | Dba.Instr.Assume (_, id) ->
        aux (Caddress.reid addr id) (Caddress.Map.add addr (ik, opc) acc)
      | Dba.Instr.SJump (Dba.JOuter _, _)
      | Dba.Instr.DJump (_, _)
      | Dba.Instr.Stop _ -> None, acc
  in
  aux addr Caddress.Map.empty


let is_not_mayused_in_block temp_lhs insts =
  Dba_types.Caddress.Map.fold (fun _ (ik, _) res ->
      let s = match ik with
        | Dba.Instr.Assign (lhs, e, _) ->
          not ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
               (lhs_mayused_in_expr temp_lhs e) ||
               (lhs_mayused_in_lhs temp_lhs lhs))
        | Dba.Instr.SJump (Dba.JInner _, _)
        | Dba.Instr.SJump (Dba.JOuter _, _)
        | Dba.Instr.Stop (_) -> true
        | Dba.Instr.DJump (expr, _) -> not (lhs_mayused_in_expr temp_lhs expr)
        | Dba.Instr.If (c, _, _)
        | Dba.Instr.Assert (c, _)
        | Dba.Instr.Assume (c, _) -> not (lhs_mayused_in_expr temp_lhs c)
        | Dba.Instr.NondetAssume (lhslist, c, _) ->
          List.fold_left (fun res lhs ->
              if ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
                  (lhs_mayused_in_expr temp_lhs c) ||
                  (lhs_mayused_in_lhs temp_lhs lhs))
              then false
              else res
            ) true lhslist
        | Dba.Instr.Nondet (lhs, _, _) -> not (lhs_mustkilled_by_lhs lhs temp_lhs)
        | Dba.Instr.Undef (lhs, _) -> not (lhs_mustkilled_by_lhs lhs temp_lhs)
        | Dba.Instr.Malloc (lhs, expr, _) ->
          (not (lhs_mustkilled_by_lhs lhs temp_lhs)) &&
          (not (lhs_mayused_in_expr temp_lhs expr))
        | Dba.Instr.Free (expr, _off) -> not (lhs_mayused_in_expr temp_lhs expr)
        | Dba.Instr.Print (printarglist, _off) ->
          List.fold_left (fun res elem ->
              match elem with
              | Dba.Exp e -> if lhs_mayused_in_expr temp_lhs e then false else res
              | _ -> res)
            true printarglist
      in (s && res)
    ) insts true


let nb_used_in_block temp_lhs insts =
  try
    Dba_types.Caddress.Map.fold (fun _addr (ik, _) nb ->
        if (nb > 0) then raise (Used_tmp (nb));
        match ik with
        | Dba.Instr.Assign (lhs, e, _id) ->
          if ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
              (lhs_mayused_in_expr temp_lhs e) ||
              (lhs_mayused_in_lhs temp_lhs lhs))
          then nb + 1
          else nb
        | Dba.Instr.SJump ( _, _)
        | Dba.Instr.Stop (_) -> nb
        | Dba.Instr.DJump (expr, _) -> if (lhs_mayused_in_expr temp_lhs expr) then nb + 1 else nb
        | Dba.Instr.If (c, _, _)
        | Dba.Instr.Assert (c, _)
        | Dba.Instr.Assume (c, _) -> if (lhs_mayused_in_expr temp_lhs c) then nb + 1 else nb
        | Dba.Instr.NondetAssume (lhslist, c, _) ->
          nb + List.fold_left (fun res lhs ->
              if ((lhs_mustkilled_by_lhs lhs temp_lhs) ||
                  (lhs_mayused_in_expr temp_lhs c) ||
                  (lhs_mayused_in_lhs temp_lhs lhs))
              then res + 1
              else res
            ) 0 lhslist
        | Dba.Instr.Nondet (lhs, _, _) ->
          if (lhs_mustkilled_by_lhs lhs temp_lhs) then nb + 1 else nb
        | Dba.Instr.Undef (lhs, _) ->
          if (lhs_mustkilled_by_lhs lhs temp_lhs) then nb + 1 else nb
        | Dba.Instr.Malloc (lhs, expr, _) ->
          if ((lhs_mustkilled_by_lhs lhs temp_lhs)) ||
             ((lhs_mayused_in_expr temp_lhs expr)) then nb + 1 else nb
        | Dba.Instr.Free (expr, _off) ->
          if (lhs_mayused_in_expr temp_lhs expr) then nb + 1 else nb
        | Dba.Instr.Print (printarglist, _off) ->
          nb + List.fold_left (fun res elem ->
              match elem with
              | Dba.Exp e ->
                if (lhs_mayused_in_expr temp_lhs e) then (res + 1) else res
              | _ -> res)
            0 printarglist
      ) insts 0
  with
  | Used_tmp nb -> nb



let rec replace_lhs_in_expr tmp_lhs red_lhs expr =
  let open Dba.Expr in
  match expr with
  | Dba.(Expr.Var { name = name1; size = size1; _}) ->
    begin
      match tmp_lhs with
      | Dba.(LValue.Var { name = name2; _}) ->
        if name1 = name2 then Dba_types.Expr.of_lvalue red_lhs else expr
      | Dba.LValue.Restrict ({Dba.name = name2; _}, {Interval.lo=i; Interval.hi=j}) ->
        if name1 = name2 && i = 0 && j = size1 - 1
        then Dba_types.Expr.of_lvalue red_lhs else expr
      | Dba.LValue.Store (_size, _endian, expr) -> expr
    end
  | Dba.Expr.Load (size1, endian1, e1) ->
    begin
      match tmp_lhs with
      | Dba.LValue.Var _
      | Dba.LValue.Restrict _ -> expr
      | Dba.LValue.Store _ ->
        if lhs_mustkilled_by_lhs tmp_lhs (Dba.LValue.of_expr expr)
        then Dba_types.Expr.of_lvalue red_lhs
        else
          let e = replace_lhs_in_expr tmp_lhs red_lhs e1 in
          let sz1 = Size.Byte.create size1 in
          Dba.Expr.load sz1 endian1 e
    end
  | Dba.Expr.Cst _ -> expr
  | Dba.Expr.Unary (uop, e) ->
    unary uop (replace_lhs_in_expr tmp_lhs red_lhs e)
  | Dba.Expr.Binary (bop, e1, e2) ->
    let f = replace_lhs_in_expr tmp_lhs red_lhs in
    binary bop (f e1) (f e2)
  | Dba.Expr.Ite (cond, e1, e2) ->
    ite
      (replace_lhs_in_expr tmp_lhs red_lhs cond)
      (replace_lhs_in_expr tmp_lhs red_lhs e1)
      (replace_lhs_in_expr tmp_lhs red_lhs e2)


let replace_lhs_in_lhs tmp_lhs red_lhs lhs =
  if lhs_mustkilled_by_lhs lhs tmp_lhs then red_lhs
  else
    match lhs with
    | Dba.LValue.Var _
    | Dba.LValue.Restrict _ -> lhs
    | Dba.LValue.Store (size, endian, expr) ->
      let size = Size.Byte.create size in
      Dba.LValue.store size endian (replace_lhs_in_expr tmp_lhs red_lhs expr)


let replace lhs_red lhs_temp insts block =
  let open Dba.Instr in
  let f addr (ik, opcode) block =
    let instr =
      match ik with
      | Dba.Instr.Assign (lhs, e, id) ->
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        assign lhs e id
      | Dba.Instr.DJump (e, tag) ->
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        dynamic_jump e ?tag
      | Dba.Instr.If (cond, id1, id2) ->
        let cond = replace_lhs_in_expr lhs_temp lhs_red cond in
        ite cond id1 id2
      | Dba.Instr.Assert (cond, id) ->
        let cond = replace_lhs_in_expr lhs_temp lhs_red cond in
        _assert cond id
      | Dba.Instr.Assume (cond, id) ->
        let cond = replace_lhs_in_expr lhs_temp lhs_red cond in
        assume cond id
      | Dba.Instr.NondetAssume (lhslist, cond, id) ->
        let lhslist = List.fold_left (fun res lhs ->
            (replace_lhs_in_lhs lhs_temp lhs_red lhs) :: res
          ) [] lhslist
        in
        let cond = replace_lhs_in_expr lhs_temp lhs_red cond in
        non_deterministic_assume lhslist cond id
      | Dba.Instr.Nondet (lhs, region, id) ->
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        non_deterministic lhs ~region id
      | Dba.Instr.Undef (lhs, id) ->
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        undefined lhs id
      | Dba.Instr.Malloc (lhs, e, id) ->
        let lhs = replace_lhs_in_lhs lhs_temp lhs_red lhs in
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        malloc lhs e id
      | Dba.Instr.Free (e, id) ->
        let e = replace_lhs_in_expr lhs_temp lhs_red e in
        free e id
      | Dba.Instr.Print (printarglist, id) ->
        let printarglist =
          List.fold_left
            (fun res elem ->
               match elem with
               | Dba.Exp e -> (Dba.Exp (replace_lhs_in_expr lhs_temp lhs_red e)) :: res
               | i -> i :: res) [] printarglist
        in print printarglist id
      | _ -> ik
    in
    Dba_types.Caddress.Map.add addr (instr, opcode) block
  in
  Dba_types.Caddress.Map.fold f insts block


let try_replace addr (lhs_temp, e, id_next, opc) block =
  try
    let inst, insts =
      get_redundant_assign (lhs_temp, Dba_types.Caddress.reid addr id_next) block in
    match inst with
    | None ->
      let nb = nb_used_in_block lhs_temp insts in
      if nb = 0 then
        let open Dba in
        let terminator = Instr.static_jump (Jump_target.inner id_next) in
        Dba_types.Caddress.Map.add addr (terminator, opc) block
      else block
    | Some (lhs_red, addr_red, nid_red) ->
      if is_not_mayused_in_block lhs_red insts
      then
        let open Dba in
        let instr = Instr.static_jump (Jump_target.inner nid_red) in
        let bloc = Dba_types.Caddress.Map.add addr_red (instr, opc) block in
        let block =
          Dba_types.Caddress.Map.add
            (Dba_types.Caddress.reid addr 0)
            (Instr.assign lhs_red e id_next, opc) bloc in
        replace lhs_red lhs_temp insts block
      else block
  with
  | Killed_tmp -> block


let remove_redundant_assign block =
  let f addr (ik, opc) block =
    (* if id = 0 then  *)
    match ik with
    | Dba.Instr.Assign (lhs, e, id_next) ->
      if Dba_types.LValue.is_temporary lhs
      then try_replace addr (lhs, e, id_next, opc) block
      else block
    | _ -> block
    (* else block *)
  in Dba_types.Caddress.Map.fold f block block



module Env = struct

  include Basic_types.String.Map

  let eq = (=) (* Maybe this is not the right equality for region * Bv.t type *)

  (* Test if env1 contains env2 *)
  let contains env1 env2 =
    let mem vname cst =
      match find vname env1 with
      | v ->  eq v cst
      | exception Not_found -> false in
    for_all mem env2

  let add vname cst env =
    match find vname env with
    | v ->
      if eq v cst then env
      else remove vname env
    | exception Not_found -> add vname cst env
end

module Constant_propagation = struct
  open Dba
  let rec eval_expr env = function
    | Dba.Expr.Var v as e ->
      begin
        match Basic_types.String.Map.find v.name env with
        | region, bv -> Expr.constant ~region bv
        | exception Not_found -> e
      end
    | Dba.Expr.Load (sz, en, e) ->
      let sz = Size.Byte.create sz in
      Expr.load sz en (eval_expr env e)
    | Dba.Expr.Cst _  as e -> e
    | Dba.Expr.Unary (uop, e) ->
      Expr.unary uop (eval_expr env e)
    | Dba.Expr.Binary (bop, e1, e2) ->
      Expr.binary bop (eval_expr env e1) (eval_expr env e2)
    | Dba.Expr.Ite (c, e1, e2) ->
      Expr.ite (eval_expr env c) (eval_expr env e1) (eval_expr env e2)

  let eval_instruction penv i =
    match i with
    | Dba.Instr.Assign (lv, e, id) ->
      Instr.assign lv (eval_expr penv e) id
    | Dba.Instr.DJump (e, tag) ->
      Instr.dynamic_jump ?tag (eval_expr penv e)
    | Dba.Instr.If (c, jt, id) ->
      Instr.ite (eval_expr penv c) jt id
    | Dba.Instr.Assert (c, id) -> Instr._assert (eval_expr penv c) id
    | Dba.Instr.Assume (c, id) -> Instr.assume (eval_expr penv c) id
    | Dba.Instr.NondetAssume (lvs, c, id) ->
      Instr.non_deterministic_assume lvs (eval_expr penv c) id
    | Dba.Instr.Malloc (lv, e, id) ->
      Instr.malloc lv (eval_expr penv e) id
    | Dba.Instr.Free (e, id) -> Instr.free (eval_expr penv e) id
    | Dba.Instr.Print _
    | Dba.Instr.Undef _
    | Dba.Instr.Nondet _
    | Dba.Instr.Stop _
    | Dba.Instr.SJump _ as instr -> instr


  let gather_propagations ?(env=Env.empty) block =
    (* All elements are initialized at None *)
    let envs =
      Dhunk.to_list block
      |> List.map (fun _ -> None) |> Array.of_list in
    let should_propagate env id  =
      match envs.(id) with
      | None -> true (* this index was never visited *)
      | Some e -> not (Env.contains env e)
    in
    let mark_env env idx = envs.(idx) <- Some env in
    let remove lval env =
      match Dba_types.LValue.name_of lval with
      | Some vname -> Basic_types.String.Map.remove vname env
      | None -> env
    in
    let rec loop env idx =
      if should_propagate env idx then begin
        mark_env env idx;
        match Dhunk.inst block idx with
        | None -> env
        | Some i ->
          begin match i with
            | Dba.Instr.Assign (Dba.LValue.Var {Dba.name; _},
                                Dba.Expr.Cst (r, v), idx') ->
              loop (Env.add name (r,v) env) idx'
            | Dba.Instr.If (_, Dba.JInner idx1, idx2) ->
              loop (loop env idx1) idx2
            | Dba.Instr.Nondet (lv, _, id)
            | Dba.Instr.Malloc (lv, _, id) ->
              loop (remove lv env) id
            | Dba.Instr.NondetAssume (lvals, _, id) ->
              (* Let's be over-cautious in this case and treat all variables as
                 non-constants *)
              let env' = List.fold_left (fun e v -> remove v e) env lvals in
              loop env' id
            | Dba.Instr.Assert (_, id)
            | Dba.Instr.Assume (_, id)
            | Dba.Instr.Undef (_, id)
            | Dba.Instr.Assign (_, _, id)
            | Dba.Instr.SJump (Dba.JInner id, _)
            | Dba.Instr.If (_, Dba.JOuter _, id)
            | Dba.Instr.Free (_, id)
            | Dba.Instr.Print (_, id) -> loop env id
            | Dba.Instr.SJump (Dba.JOuter _, _)
            | Dba.Instr.DJump _
            | Dba.Instr.Stop _ -> env
          end
      end
      else env
    in
    ignore (loop env (Dhunk.(start block |> Node.id)));
    envs


  let do_propagations block propagation_envs =
    Dhunk.mapi
      ~f:(fun i instruction ->
          match propagation_envs.(i) with
          | None -> instruction
          | Some env ->
            if Basic_types.String.Map.is_empty env
            then instruction
            else eval_instruction env instruction)
      block

  let eval block =
    Logger.debug ~level:5 "@[<v 0>Prepropagation@ %a@]" Dhunk.pp block;
    let b =
      gather_propagations block |> do_propagations block in
    Logger.debug ~level:5 "@[<v 0>Post-propagation@ %a@]" Dhunk.pp b;
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
