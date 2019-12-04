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

open Dba

type stats = int * int * int * int ;;

let statistics instMap =
  let f _ (ik, _) (accusize, accugoto, temp, flag) =
    match ik with
    | Dba.Instr.SJump (JOuter _, _) ->
      accusize + 1, accugoto + 1, temp, flag
    | Dba.Instr.Assign (lhs, _exp, _) when Dba_types.LValue.is_temporary lhs ->
      accusize + 1, accugoto + 1, temp + 1, flag
    | Dba.Instr.Assign (lhs, _exp, _) when Dba_types.LValue.is_flag lhs ->
      accusize + 1, accugoto + 1, temp, flag + 1
    | Dba.Instr.Undef (lhs, _) when Dba_types.LValue.is_temporary lhs ->
      accusize + 1, accugoto + 1, temp + 1, flag
    | Dba.Instr.Undef (lhs, _) when Dba_types.LValue.is_flag lhs ->
      accusize + 1, accugoto + 1, temp, flag + 1
    | _ -> accusize + 1, accugoto, temp, flag
  in Dba_types.Caddress.Map.fold f instMap (0, 0, 0, 0)


let are_expr_same_cst e1 e2 : bool =
  (* Approximated result but safe *)
  match e1, e2 with
  | Dba.Expr.Cst (r1, bi1), Dba.Expr.Cst(r2, bi2) ->
    let bi1 = Bitvector.value_of bi1 in
    let bi2 = Bitvector.value_of bi2 in
    Dba_types.Region.compare r1 r2 = 0 && Bigint.eq_big_int bi1 bi2
  | _ -> false


let must_lhs_expr_equal lhs expr : bool =
  (* result : safe sur oui *)
  (* on n'evalue pas expr des arrays, car utilisation sur locs differentes *)
  (* donc valuations differentes *)
  match lhs, expr with
  | Dba.LValue.Var v1, Dba.Expr.Var v2 -> String.compare v1.name v2.name = 0
  | Dba.LValue.Restrict (v1, {Interval.lo=i1; Interval.hi=j1}),
    Dba.Expr.Unary(Unary_op.Restrict {Interval.lo = i2; Interval.hi = j2;},
                   Dba.Expr.Var v2) ->
    (=) v1.name v2.name && i1 = i2 && j1 = j2
  | Dba.LValue.Store (_, endian1, e1), Dba.Expr.Load (_, endian2, e2) ->
    (* on se limite au cas ou tableaux a acces constants *)
    endian1 = endian2 && are_expr_same_cst e1 e2
  | _ -> false


let lhs_mustkilled_by_lhs lhs1 lhs2 : bool =
  (*  est-ce que si j'affecte lhs2, je tue a coup sûr lhs1 *)
  match lhs1 with
  | Dba.LValue.Var { name = name1; _} ->
    begin
      match lhs2 with
      | Dba.LValue.Var { name = name2; _}-> name1 = name2
      | Dba.LValue.Restrict (v, {Interval.lo=i2; Interval.hi=j2}) ->
        v.name = name1 && i2 = 0 && j2 = v.size - 1
      | Dba.LValue.Store (_, _, _) -> false
    end
  | Dba.LValue.Restrict ({name = name1; _}, {Interval.lo=i1; Interval.hi=j1}) ->
    begin
      match lhs2 with
       | Dba.LValue.Var { name = name2; _}-> name1 = name2
       | Dba.LValue.Restrict ({ name = name2; _}, {Interval.lo=i2; Interval.hi=j2}) ->
          name1 = name2 && not(j1 < i2 || j2 < i1)
      | Dba.LValue.Store (_size2, _endian2, _expr2) -> false
    end
  | Dba.LValue.Store (_size1, _endian1, expr1) ->
    begin
      match lhs2 with
      | Dba.LValue.Var _
      | Dba.LValue.Restrict _ -> false
      | Dba.LValue.Store (_size2, _endian2, expr2) ->
        (are_expr_same_cst expr1 expr2)
    end


(* reponse false est safe, on se concentre sur qqes cas particuliers utiles *)
let rec lhs_mayused_in_expr lhs expr =
  match lhs, expr with
  | _, Dba.Expr.Cst _
  | Dba.LValue.Store _, Dba.Expr.Var _ -> false
  | Dba.LValue.Restrict ({name = name1; _}, {Interval.lo=i1; Interval.hi=j1}),
    Dba.Expr.Unary (Dba.Unary_op.Restrict {Interval.lo = i2; Interval.hi = j2;},
                    Dba.Expr.Var v) ->
    (* If restricted areas overlap then may be used *)
    name1 = v.name && j1 >= i2 && j2 >= i1
  | _, Dba.Expr.Unary (_, e)
  | Dba.LValue.Restrict _, Dba.Expr.Load (_, _, e)
  | Dba.LValue.Var _, Dba.Expr.Load(_, _, e) -> lhs_mayused_in_expr lhs e
  | (Dba.LValue.Restrict ({name; _}, _) | Dba.LValue.Var { name;_}),
    Dba.Expr.Var v ->
    name = v.name
  | Dba.LValue.Store (size1, _, Dba.Expr.Cst (r1, bi1)),
    Dba.Expr.Load (size2, _, Dba.Expr.Cst (r2, bi2)) ->
    let bi1 = Bitvector.value_of bi1 in
    let bi2 = Bitvector.value_of bi2 in
    (* indep de endian *)
    let i1, j1 = bi1, Bigint.add_int_big_int size1 bi1
    and i2, j2 = bi2, Bigint.add_int_big_int size2 bi2 in
    Dba_types.Region.compare r1 r2 = 0
    && not (Bigint.lt_big_int j1 i2 || Bigint.lt_big_int j2 i1)
  | Dba.LValue.Store _, Dba.Expr.Load(_, _, e) -> lhs_mayused_in_expr lhs e
  | _, Dba.Expr.Binary (_bop, e1, e2) ->
    lhs_mayused_in_expr lhs e1 || lhs_mayused_in_expr lhs e2
  | _, Dba.Expr.Ite (cond, e1, e2) ->
    lhs_mayused_in_expr lhs cond
    || lhs_mayused_in_expr lhs e1
    || lhs_mayused_in_expr lhs e2

and lhs_mayused_in_lhs lv = function
  | Dba.LValue.Var _
  | Dba.LValue.Restrict _ -> false
  | Dba.LValue.Store (_, _, e) -> lhs_mayused_in_expr lv e

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
  | Dba.Instr.Assign (lhs, e, id) ->
    if lhs_mayused_in_expr var e then flags_env, false
    else if lhs_mayused_in_lhs var lhs then flags_env, false
    else if lhs_mustkilled_by_lhs var lhs then flags_env, true
    else
      let prog = Caddress.Map.remove addr prog in
      let addr' = Caddress.reid addr id in
      is_not_mayused prog addr' (niter - 1) var flags_env
  | Dba.Instr.SJump (JOuter addr', Some (Dba.Call _ | Dba.Return)) ->
     begin
       let open Simplification_options in
       match Simplification.get () with
       | Function s  | Sequence s ->
          if s = NoInline then flags_env, false
          else
            let prog = Caddress.Map.remove addr prog in
            is_not_mayused_in_call prog addr' var flags_env
       | _ ->
          let prog = Caddress.Map.remove addr prog in
          is_not_mayused prog addr' (niter - 1) var flags_env
     end
  | Dba.Instr.SJump (t, _tag) ->
    let prog = Caddress.Map.remove addr prog in
    is_not_mayused prog (retarget addr t) (niter - 1) var flags_env
  | Dba.Instr.DJump (_e, _tag) -> flags_env, false
  | Dba.Instr.If (cond, t, id2) ->
     begin
       let open Simplification_options in
       match Simplification.get () with
       | Sequence _ -> flags_env, false
       | _ ->
          if lhs_mayused_in_expr var cond
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
     end
  | Dba.Instr.Print (args, id) ->
    let f bool elem =
      match elem with
      | Exp e -> (lhs_mayused_in_expr var e) || bool
      | Str _s -> bool
    in
    let b = List.fold_left f false args in
    let res = loop id in
    if b then flags_env, false else res
  | Dba.Instr.NondetAssume (lhs, cond, id) ->
    if lhs_mayused_in_expr var cond then flags_env, false
    else
      let f1 bool lhs = lhs_mayused_in_lhs var lhs || bool in
      let f2 bool lhs = lhs_mustkilled_by_lhs var lhs && bool in
      let b1 = List.fold_left f1 false lhs in
      if b1 then flags_env, false
      else
        let b2 = List.fold_left f2 true lhs in
        if b2 then flags_env, true
        else loop id
  | Dba.Instr.Assume (cond, id)
  | Dba.Instr.Assert (cond, id) ->
    if lhs_mayused_in_expr var cond then flags_env, false
    else loop id
  | Dba.Instr.Malloc (lhs, e, id) ->
    if (lhs_mayused_in_expr var e) then flags_env, false
    else if (lhs_mustkilled_by_lhs var lhs) then flags_env, true
    else loop id
  | Dba.Instr.Free (e, id) ->
    if lhs_mayused_in_expr var e then flags_env, false
    else loop id
  | Dba.Instr.Undef (lhs, id)
  | Dba.Instr.Nondet (lhs, _, id) ->
    if (lhs_mayused_in_lhs var lhs) then flags_env, false
    else if (lhs_mustkilled_by_lhs var lhs) then flags_env, true
    else loop id
  | Dba.Instr.Stop _ -> flags_env, true  (* if not used then useless *)


and is_not_mayused_in_call prog addr lhs flags_env :
  ((bool Basic_types.String.Map.t) Dba_types.Caddress.Map.t) * bool =
  match lhs with
  | Dba.LValue.Var {name; _} ->
    begin
      let open Simplification_options in
      let magic_value = 100 in
      match Simplification.get () with
      | Sequence NoSummaries | Function NoSummaries ->
         is_not_mayused prog addr magic_value lhs flags_env

      | _ ->
         match  Basic_types.String.Map.find name
                  (Dba_types.Caddress.Map.find addr flags_env)
         with
         | v -> flags_env, v
         | exception Not_found ->
          let flags_env, b =
            is_not_mayused prog addr magic_value lhs flags_env in
          let sub_flags_env =
            try Dba_types.Caddress.Map.find addr flags_env
            with Not_found -> Basic_types.String.Map.empty in
          let flags = Basic_types.String.Map.add name b sub_flags_env in
          let flags_env = Dba_types.Caddress.Map.add addr flags flags_env in
          flags_env, b
    end
  | Dba.LValue.Restrict _
  | Dba.LValue.Store _ -> flags_env, false (* lhs is a flag = cannot be a store *)


let display_results (initsize, _, itemps, iflags) map ppf time =
  let finalsize, _finalgoto, ftemps, fflags = statistics map in
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
    finalsize
    (pp_ratio initsize) finalsize
    (pp_ratio itemps) ftemps
    (pp_ratio iflags) fflags
    ;;
