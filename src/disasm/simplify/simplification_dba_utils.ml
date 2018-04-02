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

open Dba

let statistics instMap =
  let open Dba_types in
  let f _ (ik, _) (accusize, accugoto, temp, flag) =
    match ik with
    | Dba.IkSJump (JOuter _, _) ->
      accusize + 1, accugoto + 1, temp, flag
    | Dba.IkAssign (lhs, _exp, _) when LValue.is_temporary lhs ->
      accusize + 1, accugoto + 1, temp + 1, flag
    | Dba.IkAssign (lhs, _exp, _) when LValue.is_flag lhs ->
      accusize + 1, accugoto + 1, temp, flag + 1
    | Dba.IkUndef (lhs, _) when LValue.is_temporary lhs ->
      accusize + 1, accugoto + 1, temp + 1, flag
    | Dba.IkUndef (lhs, _) when LValue.is_flag lhs ->
      accusize + 1, accugoto + 1, temp, flag + 1
    | _ -> accusize + 1, accugoto, temp, flag
  in Caddress.Map.fold f instMap (0, 0, 0, 0)


let are_expr_same_cst e1 e2 : bool =
  (* resultat approche mais safe : oui => oui *)
  match e1, e2 with
  | Dba.ExprCst (r1, bi1), Dba.ExprCst(r2, bi2) ->
    let bi1 = Bitvector.value_of bi1 in
    let bi2 = Bitvector.value_of bi2 in
    Dba_types.Region.compare r1 r2 = 0 && Bigint.eq_big_int bi1 bi2
  | _ -> false


let must_lhs_expr_equal lhs expr : bool =
  (* result : safe sur oui *)
  (* on n'evalue pas expr des arrays, car utilisation sur locs differentes *)
  (* donc valuations differentes *)
  match lhs, expr with
  | Dba.LhsVar (name1, _, _), Dba.ExprVar(name2, _, _) ->
    String.compare name1 name2 = 0
  | Dba.LhsVarRestrict (name1, _, i1, j1),
    Dba.ExprRestrict (Dba.ExprVar (name2, _, _), i2, j2) ->
    (=) name1 name2 && i1 = i2 && j1 = j2
  | Dba.LhsStore (_, endian1, e1), Dba.ExprLoad (_, endian2, e2) ->
    (* on se limite au cas ou tableaux a acces constants *)
    endian1 = endian2 && are_expr_same_cst e1 e2
  | _ -> false


let lhs_mustkilled_by_lhs lhs1 lhs2 : bool =
  (*  est-ce que si j'affecte lhs2, je tue a coup sûr lhs1 *)
  match lhs1 with
  | Dba.LhsVar (name1, _size1, _) ->
    begin
      match lhs2 with
      | Dba.LhsVar (name2, _size2, _) -> name1 = name2
      | Dba.LhsVarRestrict (name2, size2, i2, j2) ->
        name1 = name2 && i2 = 0 && j2 = size2 - 1
      | Dba.LhsStore (_, _, _) -> false
    end
  | Dba.LhsVarRestrict (name1, _size1, i1, j1) ->
    begin
      match lhs2 with
      | Dba.LhsVar (name2, _size2, _) -> (name1 = name2)
      | Dba.LhsVarRestrict (name2, _size2, i2, j2) ->
        ((name1 = name2) && not(j1 < i2 || j2 < i1))
      | Dba.LhsStore (_size2, _endian2, _expr2) -> false
    end
  | Dba.LhsStore (_size1, _endian1, expr1) ->
    begin
      match lhs2 with
      | Dba.LhsVar (_name2,_size2, _) -> false
      | Dba.LhsVarRestrict (_name2, _size2, _i2, _j2) -> false
      | Dba.LhsStore (_size2, _endian2, expr2) -> (are_expr_same_cst expr1 expr2)
    end


(* reponse false est safe, on se concentre sur qqes cas particuliers utiles *)
let rec lhs_mayused_in_expr lhs expr =
  match lhs, expr with
  | _, Dba.ExprCst (_,_)
  | Dba.LhsStore (_, _, _), Dba.ExprVar(_, _, _) -> false
  | _, Dba.ExprUnary (_, e)
  | _, Dba.ExprExtU  (e, _)
  | _, Dba.ExprExtS  (e, _)
  | Dba.LhsVarRestrict _, Dba.ExprLoad (_, _, e)
  | Dba.LhsVar _, Dba.ExprLoad(_, _, e) -> lhs_mayused_in_expr lhs e
  | Dba.LhsVarRestrict (name1, _, _, _), Dba.ExprVar (name2, _, _)
  | Dba.LhsVar (name1, _, _), Dba.ExprRestrict (Dba.ExprVar (name2, _, _), _, _)
  | Dba.LhsVar (name1, _, _), Dba.ExprVar(name2, _, _) ->
    (=) name1 name2
  | Dba.LhsVarRestrict (name1, _, i1, j1),
    Dba.ExprRestrict (Dba.ExprVar (name2, _, _), i2, j2) ->
    (* If restricted areas overlap then may be used *)
    (=) name1 name2 && j1 >= i2 && j2 >= i1
  | Dba.LhsStore (size1, _, Dba.ExprCst (r1, bi1)),
    Dba.ExprLoad (size2, _, Dba.ExprCst (r2, bi2)) ->
    let bi1 = Bitvector.value_of bi1 in
    let bi2 = Bitvector.value_of bi2 in
    (* indep de endian *)
    let (i1, j1) = (bi1, (Bigint.add_int_big_int size1 bi1))
    and (i2, j2) = (bi2, (Bigint.add_int_big_int size2 bi2)) in
    Dba_types.Region.compare r1 r2 = 0
    && not (Bigint.lt_big_int j1 i2 || Bigint.lt_big_int j2 i1)
  | _, Dba.ExprRestrict (e, _, _)
  | Dba.LhsStore _, Dba.ExprLoad(_, _, e) -> lhs_mayused_in_expr lhs e
  | _, Dba.ExprBinary (_bop, e1, e2) ->
    lhs_mayused_in_expr lhs e1 || lhs_mayused_in_expr lhs e2
  | _, Dba.ExprIte (cond, e1, e2) ->
    lhs_mayused_in_bcond lhs cond
    || lhs_mayused_in_expr lhs e1
    || lhs_mayused_in_expr lhs e2
  | _, Dba.ExprAlternative _ -> failwith "dba.ml: lhs_mayused_inexpr "


and lhs_mayused_in_bcond lhs cond =
  match cond with
    Dba.CondReif e -> lhs_mayused_in_expr lhs e
  | Dba.CondNot  c -> lhs_mayused_in_bcond lhs c
  | Dba.CondAnd (c1, c2)
  | Dba.CondOr  (c1, c2) ->
    lhs_mayused_in_bcond lhs c1 || lhs_mayused_in_bcond lhs c2
  | Dba.True | Dba.False -> false


and lhs_mayused_in_lhs lv = function
  | Dba.LhsVar _
  | Dba.LhsVarRestrict _ -> false
  | Dba.LhsStore (_, _, e) -> lhs_mayused_in_expr lv e

let rec is_not_mayused prog addr look_ahead_limit var flags_env :
  (bool Basic_types.String.Map.t Dba_types.Caddress.Map.t) * bool =
  if look_ahead_limit = 0
  then flags_env, false  (* sliding window ends without mustkill proof *)
  else
    try
      let nik, _ = Dba_types.Caddress.Map.find addr prog in
      is_not_mayused_in_instr nik prog addr look_ahead_limit var flags_env
    with Not_found -> flags_env, false


and is_not_mayused_in_instr ik prog addr niter var flags_env :
  (bool Basic_types.String.Map.t Dba_types.Caddress.Map.t) * bool =
  let open Dba_types in
  let retarget addr = function
    | JInner id -> Caddress.reid addr id
    | JOuter a -> a
  in
  let loop id =
    let prog = Caddress.Map.remove addr prog in
    let a = Caddress.reid addr id in
    is_not_mayused prog a (niter - 1) var flags_env
  in
  match ik with
  | Dba.IkAssign (lhs, e, id) ->
    if lhs_mayused_in_expr var e then flags_env, false
    else if lhs_mayused_in_lhs var lhs then flags_env, false
    else if lhs_mustkilled_by_lhs var lhs then flags_env, true
    else
      let prog = Caddress.Map.remove addr prog in
      let addr' = Caddress.reid addr id in
      is_not_mayused prog addr' (niter - 1) var flags_env
  | Dba.IkSJump (JOuter addr', Some (Dba.Call _)) ->
    if !Disasm_options.simpl_fun || !Disasm_options.simpl_sequence
    then
      if !Disasm_options.simpl_inline_calls then
        let prog = Caddress.Map.remove addr prog in
        is_not_mayused_in_call prog addr' var flags_env
      else
        flags_env, false
    else
      let prog = Caddress.Map.remove addr prog in
      is_not_mayused prog addr' (niter - 1) var flags_env
  | Dba.IkSJump (JOuter addr', Some Dba.Return) ->
    if !Disasm_options.simpl_fun || !Disasm_options.simpl_sequence
    then
      if !Disasm_options.simpl_inline_calls then
        let prog = Caddress.Map.remove addr prog in
        is_not_mayused_in_call prog addr' var flags_env
      else flags_env, false
    else
      let prog = Caddress.Map.remove addr prog in
      is_not_mayused prog addr' (niter - 1) var flags_env
  | Dba.IkSJump (t, _tag) ->
    let prog = Caddress.Map.remove addr prog in
    is_not_mayused prog (retarget addr t) (niter - 1) var flags_env
  | Dba.IkDJump (_e, _tag) -> flags_env, false
  | Dba.IkIf (cond, t, id2) ->
    if !Disasm_options.simpl_sequence
    then flags_env, false
    else if lhs_mayused_in_bcond var cond
    then flags_env, false
    else
      let flags_env, b =
        let prog = Caddress.Map.remove addr prog in
        let a = retarget addr t in
        is_not_mayused prog a (niter - 1) var flags_env in
      if b then
        let a = Caddress.reid addr id2 in
        is_not_mayused prog a (niter - 1) var flags_env
      else flags_env, false
  | Dba.IkPrint (args, id) ->
    let f bool elem =
      match elem with
      | Exp e -> (lhs_mayused_in_expr var e) || bool
      | Str _s -> bool
    in
    let b = List.fold_left f false args in
    let res = loop id in
    if b then flags_env, false else res
  | Dba.IkNondetAssume (lhs, cond, id) ->
    if lhs_mayused_in_bcond var cond then flags_env, false
    else
      let f1 bool lhs = lhs_mayused_in_lhs var lhs || bool in
      let f2 bool lhs = lhs_mustkilled_by_lhs var lhs && bool in
      let b1 = List.fold_left f1 false lhs in
      if b1 then flags_env, false
      else
        let b2 = List.fold_left f2 true lhs in
        if b2 then flags_env, true
        else loop id
  | Dba.IkAssume (cond, id)
  | Dba.IkAssert (cond, id) ->
    if lhs_mayused_in_bcond var cond then flags_env, false
    else loop id
  | Dba.IkMalloc (lhs, e, id) ->
    if (lhs_mayused_in_expr var e) then flags_env, false
    else if (lhs_mustkilled_by_lhs var lhs) then flags_env, true
    else loop id
  | Dba.IkFree (e, id) ->
    if lhs_mayused_in_expr var e then flags_env, false
    else loop id
  | Dba.IkUndef (lhs, id)
  | Dba.IkNondet (lhs, _, id) ->
    if (lhs_mayused_in_lhs var lhs) then flags_env, false
    else if (lhs_mustkilled_by_lhs var lhs) then flags_env, true
    else loop id
  | Dba.IkStop _ -> flags_env, true  (* if not used then useless *)


and is_not_mayused_in_call prog addr lhs flags_env :
  ((bool Basic_types.String.Map.t) Dba_types.Caddress.Map.t) * bool =
  match lhs with
  | Dba.LhsVar (name, _, _) ->
    begin
      if (!Disasm_options.simpl_no_summaries) then
        (is_not_mayused prog addr 100 lhs flags_env)
      else
        try flags_env, (Basic_types.String.Map.find name (Dba_types.Caddress.Map.find addr flags_env))
        with Not_found ->
          let flags_env, b = is_not_mayused prog addr 100 lhs flags_env in
          let sub_flags_env =
            try Dba_types.Caddress.Map.find addr flags_env with Not_found -> Basic_types.String.Map.empty in
          let flags = Basic_types.String.Map.add name b sub_flags_env in
          let flags_env = Dba_types.Caddress.Map.add addr flags flags_env in
          flags_env, b
    end
  | Dba.LhsVarRestrict (_, _, _, _)
  | Dba.LhsStore (_, _, _) ->
    flags_env, false (* lhs is a flag = cannot be a store *)


let display_results map ppf time =
  let finalsize, _finalgoto, ftemps, fflags = statistics map in
  Options.finalsize := !Options.finalsize + finalsize;
  Options.ftemps := !Options.ftemps + ftemps;
  Options.fflags := !Options.fflags + fflags;
  let mk_ratio init final = (float (100 * (init - final))) /. (float init) in
  let pp_ratio init ppf final =
    let r = mk_ratio init final in
    Format.fprintf ppf "%.2f (%d/%d)" r final init
  in
  Format.fprintf ppf "\
        @[<v 0>Simplification statistics (%.2fs):@ \
        Disassembled instructions : %d@ \
        Generated dba instructions : %d@ \
        Simplification : %a@ \
        Temporaries simplification : %a@ \
        Flags simplification : %a@ \
        @] "
        time
        (X86toDba.native_instructions_decoded ())
        !Options.finalsize
        (pp_ratio !Options.initsize) !Options.finalsize
        (pp_ratio !Options.itemps) !Options.ftemps
        (pp_ratio !Options.iflags) !Options.fflags
