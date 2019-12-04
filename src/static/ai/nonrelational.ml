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

open Dba_utils
open! Basic_types
open High_level_predicate
open Format
open Static_types (* Env *)
open Ai_options

exception RecursiveCall of Dba.address

module Malloc_status = Dba_types.Region.Map

module MemInterval = struct
  type t = {
    base : Bigint.t;
    size : Size.Byte.t;
  }

  let create base n =
    assert (n >= 0);
    { base; size = Size.Byte.create n; }

  let size_of m = Size.Byte.to_int m.size
  let base_of m = m.base
  let upper_bound m =
    Bigint.(pred_big_int (add_int_big_int (Size.Byte.to_int m.size) m.base))

  let compare m1 m2 =
    if Bigint.lt_big_int (upper_bound m1) m2.base then -1
    else if Bigint.lt_big_int (upper_bound m2) m1.base then 1
    else 0 (*compare m1 m2*)

  let _overlap m1 m2 =
    not (Bigint.lt_big_int (upper_bound m1) m2.base &&
         Bigint.lt_big_int (upper_bound m2) m1.base)

  let same_base m1 m2 = m1.base = m2.base
  let equal m1 m2 = same_base m1 m2  && m1.size = m2.size

  let empty = create Bigint.zero_big_int 1

  (* Beware of this function if you have integers bigger than max_int *)
  let pp ppf t =
    Format.fprintf ppf "@[0x%x+%a@]"
      (Bigint.int_of_big_int t.base)
      Size.Byte.pp_hex t.size
end

module MemMap = Map.Make(MemInterval)

exception Emptyset
exception Unknown

type update_type = Strong | Weak

module Make(Val: Ai_sigs.Domain) =
struct
  module Eq = Union_find.Make (Val)
  type env  = (MemInterval.t * Machine.endianness * Val.t) MemMap.t Env.t
  type t =  env option
  type equalities = Eq.t
  type thresholds = int array * int array * int array * int array
  type elementsRecord = Region_bitvector.t list Dba_types.AddressStack.Map.t
  type naturalPredicatesRecord = (Dba.Expr.t * Dba.Expr.t) Dba_types.Caddress.Map.t
  let default_address =
    Dba_types.Caddress.block_start @@ Virtual_address.create 0, [], 0
  (* X86 :: Default size to 32 *)
  let current_address = ref default_address

  let top = Some Env.empty, High_level_predicate.empty, Eq.create ()

  let bottom = None, High_level_predicate.bottom, Eq.bottom

  let is_empty s =
    match s with
    | None -> true
    | Some _ -> false

  let s_init : (env option) ref = ref (Some Env.empty)

  let pp ppf t =
    match t with
    | None -> fprintf ppf "{}"
    | Some env ->
      let pp key subenv =
        match key with
        | Static_types.Var (name, _) ->
          fprintf ppf "@[<v 0>";
          MemMap.iter
            (fun _ (_, _, value) ->
               fprintf ppf "%s = %a@ " name Val.pp value) subenv;
          fprintf ppf "@]"
        | Static_types.Array region ->
          fprintf ppf "@[<v 0>Region %a@ {@[<hov 0>"
            Dba_printer.Ascii.pp_region region;
          MemMap.iter
            (fun mloc (_, _, value) ->
               fprintf ppf "%a -> %a;@ " MemInterval.pp mloc Val.pp value)
            subenv;
          fprintf ppf "@]}@]@ ";
      in
      fprintf ppf "@[<v 1>{";
      Env.iter pp env;
      fprintf ppf "}@]"

  let pp_equalities ppf equalities = Eq.pp ppf equalities

  let to_string (s, equalities) =
    fprintf str_formatter "@[<v 0>";
    if not (is_empty s) then
      Format.fprintf Format.str_formatter "Env:@ %a@ " pp s;
    fprintf str_formatter "Equalities@ %a@ " pp_equalities equalities;
    fprintf str_formatter "@]";
    Format.flush_str_formatter ()

  let regs_in_expr_to_string expr ppf (s, _, _) =
    let contains_expr_var expr var =
      let s1 = asprintf "%a" Dba_printer.Ascii.pp_bl_term expr in
      let re = Str.regexp_string var in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false
    in
    match s with
    | None -> fprintf ppf "{}"
    | Some s ->
      fprintf ppf "@[<hov 0>";
      Env.iter (
        fun key sub_m ->
          match key with
          | Static_types.Var (vname, _) ->
            if contains_expr_var expr vname
            then
              MemMap.iter (fun _ (_, _, v) ->
                  fprintf ppf ".%s: %s; "
                    vname (Val.to_string v))
                sub_m
          | Static_types.Array _ -> ()
      ) s;
      fprintf ppf "@]"



  let read key subkey s =
    try let _, _, v = (MemMap.find subkey (Env.find key s)) in v
    with Not_found -> Val.universe


  let leq s1 s2 =
    match (s1, s2) with
    | _, None -> false
    | None, _ -> true
    | Some s1, Some s2 ->
      (* for each constraint in s1 there
         should be a stricter constraint in s2 *)
      let has_constraint key subkey (_, _, v2) =
        let v1 = read key subkey s1 in Val.contains v2 v1
      in Env.for_all (fun k v -> MemMap.for_all (has_constraint k) v) s2


  let rec collect loc id size sub_m =
    if id > size then raise Not_found
    else
      try MemMap.find (MemInterval.create loc id) sub_m
      with Not_found -> collect loc (id + 1) size sub_m


  let join (s1, flgs1, equalities1) (s2, flgs2, equalities2) =
    match (s1, s2) with
    | (None, s) -> (s, flgs2, equalities2)
    | (s, None) -> (s, flgs1, equalities1)
    | (Some s1, Some s2) ->
      let rec join_values sub_s2 _ (mloc1, en1, v1) acc =
        let size1 = MemInterval.size_of mloc1 in
        let base1 = MemInterval.base_of mloc1 in
        try
          let mloc2, en2, v2 = collect base1 1 size1 sub_s2 in
          let size2 = MemInterval.size_of mloc2 in
          let base2 = MemInterval.base_of mloc2 in
          if MemInterval.equal mloc1 mloc2 && (en1 = en2)
          then MemMap.add mloc1 (mloc1, en1, Val.join v1 v2) acc
          else
          if MemInterval.same_base mloc1 mloc2 && en1 = en2 && en1 = Machine.LittleEndian
          then
            if size1 < size2
            then
              let v2' = Val.restrict v2 0 ((size1 * 8) - 1) in
              MemMap.add mloc1 (mloc1, en1, Val.join v1 v2') acc
            else (
              let v1' = Val.restrict v1 0 ((size2 * 8) - 1) in
              let v = Val.join v1' v2 in
              let loc = MemInterval.create base1 size2  in
              let acc = MemMap.add loc (loc, en1, v) acc in
              let v1'' = Val.restrict v1 (size2 * 8) (size1 * 8 - 1) in
              let sz = Bigint.big_int_of_int size2 in
              let base1 = Bigint.add_big_int base1 sz in
              let sz = size1 - size2 in
              let loc = MemInterval.create base1 sz in
              join_values sub_s2 loc (loc, en1, v1'') acc
            )
          else
          if en1 = en2 && en1 = Machine.LittleEndian
          then
            if Bigint.lt_big_int base1 base2
            then
              let sz = Bigint.sub_big_int base2 base1 in
              let sz = Bigint.int_of_big_int sz in
              let sz1 = size1 - sz in
              let loc = MemInterval.create base2 sz1 in
              let v1' = Val.restrict v1 (sz * 8) (size1 * 8 - 1) in
              join_values sub_s2 loc (loc, en1, v1') acc
            else
              let sz = Bigint.sub_big_int base1 base2 in
              let sz = Bigint.int_of_big_int sz in
              let sz2 = size2 - sz in
              let v2' = Val.restrict v2 (sz * 8) (size2 * 8 - 1) in
              if size1 = sz2 then
                let loc = MemInterval.create base1 size1 in
                MemMap.add loc (loc, en1, Val.join v1 v2') acc
              else if size1 < sz2
              then
                let v2' = Val.restrict v2' 0 ((size1 * 8) - 1) in
                MemMap.add mloc1 (mloc1, en1, Val.join v1 v2') acc
              else (
                let v1' = Val.restrict v1 0 ((sz2 * 8) - 1) in
                let v = Val.join v1' v2 in
                let loc = MemInterval.create base1 sz  in
                let acc = MemMap.add loc (loc, en1, v) acc in
                let v1'' = Val.restrict v1 (sz2 * 8) (size1 * 8 - 1) in
                let sz = Bigint.big_int_of_int sz2 in
                let base1 = Bigint.add_big_int base1 sz in
                let sz = size1 - sz2 in
                let loc = MemInterval.create base1 sz in
                join_values sub_s2 loc (loc, en1, v1'') acc
              )
          else failwith "unrelState.ml: fail3"
        with Not_found -> acc
      in
      let sub_join array_var sub_s1 =
        try let sub_s2 = Env.find array_var s2 in
          MemMap.fold (join_values sub_s2) sub_s1 MemMap.empty
        with Not_found -> MemMap.empty
      in
      let s = Some (Env.mapi sub_join s1) in
      let flgs = High_level_predicate.join flgs1 flgs2 in
      let t0 = Unix.gettimeofday () in
      let equalities = Eq.join equalities1 equalities2 in
      Ai_options.time_equalities := Unix.gettimeofday () -. t0 +. !Ai_options.time_equalities;
      (s, flgs, equalities)



  let widen (s1, flgs1, equalities1) (s2, flgs2, equalities2) thresholds =
    match (s1, s2) with
    | (None, s) -> (s, flgs2, equalities2)
    | (s, None) -> (s, flgs1, equalities1)
    | (Some s1, Some s2) ->
      let rec widen_values sub_s2 _ (base1,en1,v1) acc =
        let size1 = MemInterval.size_of base1 in
        let base1 = MemInterval.base_of base1 in
        try
          let (base2, en2, v2) = collect base1 1 size1 sub_s2 in
          let size2 = MemInterval.size_of base2 in
          let base2 = MemInterval.base_of base2 in
          if (Bigint.eq_big_int base1 base2) && (size1 = size2) && (en1 = en2)
          then
            let loc = MemInterval.create base1 size1 in
            MemMap.add loc (loc, en1, Val.widen v1 v2 thresholds) acc
          else
          if (Bigint.eq_big_int base1 base2) && (en1 = en2) && (en1 = Machine.LittleEndian)
          then
            if size1 < size2
            then
              let v2' = Val.restrict v2 0 ((size1 * 8) - 1) in
              let loc = MemInterval.create base1 size1 in
              MemMap.add loc (loc, en1, Val.widen v1 v2' thresholds) acc
            else (
              let v1' = Val.restrict v1 0 ((size2 * 8) - 1) in
              let v = Val.widen v1' v2 thresholds in
              let loc = MemInterval.create base1 size2  in
              let acc = MemMap.add loc (loc, en1, v) acc in
              let v1'' = Val.restrict v1 (size2 * 8) (size1 * 8 - 1) in
              let sz = Bigint.big_int_of_int size2 in
              let base1 = Bigint.add_big_int base1 sz in
              let sz = size1 - size2 in
              let loc = MemInterval.create base1 sz in
              widen_values sub_s2 loc (loc, en1, v1'') acc
            )
          else
          if (en1 = en2) && (en1 = Machine.LittleEndian)
          then
            if (Bigint.lt_big_int base1 base2)
            then
              let sz = Bigint.sub_big_int base2 base1 in
              let sz = Bigint.int_of_big_int sz in
              let sz1 = size1 - sz in
              let loc = MemInterval.create base2 sz1 in
              let v1' = Val.restrict v1 (sz * 8) (size1 * 8 - 1) in
              widen_values sub_s2 loc (loc, en1, v1') acc
            else
              let sz = Bigint.sub_big_int base1 base2 in
              let sz = Bigint.int_of_big_int sz in
              let sz2 = size2 - sz in
              let v2' = Val.restrict v2 (sz * 8) (size2 * 8 - 1) in
              if size1 = sz2 then
                let loc = MemInterval.create base1 size1 in
                MemMap.add loc (loc, en1, Val.widen v1 v2' thresholds) acc
              else if size1 < sz2
              then
                let v2' = Val.restrict v2' 0 ((size1 * 8) - 1) in
                let loc = MemInterval.create base1 size1 in
                MemMap.add loc (loc, en1, Val.widen v1 v2' thresholds) acc
              else (
                let v1' = Val.restrict v1 0 ((sz2 * 8) - 1) in
                let v = Val.widen v1' v2 thresholds in
                let loc = MemInterval.create base1 sz  in
                let acc = MemMap.add loc (loc, en1, v) acc in
                let v1'' = Val.restrict v1 (sz2 * 8) (size1 * 8 - 1) in
                let sz = Bigint.big_int_of_int sz2 in
                let base1 = Bigint.add_big_int base1 sz in
                let sz = size1 - sz2 in
                let loc = MemInterval.create base1 sz in
                widen_values sub_s2 loc (loc, en1, v1'') acc
              )
          else failwith "unrelState.ml: fail3"
        with Not_found -> acc
      in
      let sub_widen array_var sub_s1 =
        try let sub_s2 = Env.find array_var s2 in
          MemMap.fold (widen_values sub_s2) sub_s1 MemMap.empty
        with Not_found -> MemMap.empty
      in
      let s = Some (Env.mapi sub_widen s1) in
      let flgs = High_level_predicate.join flgs1 flgs2 in
      let l0 = Unix.gettimeofday () in
      let equalities = Eq.widen equalities1 equalities2 thresholds in
      Ai_options.time_equalities := Unix.gettimeofday () -. l0 +. !Ai_options.time_equalities;
      (s, flgs, equalities)


  let meet s1 s2 =
    match s1, s2 with
    | (None, _) | (_, None) -> None
    | (Some s1, Some s2) ->
      let res = ref Env.empty in
      let meet_info key subkey (i, en, v1) =
        let v2 = read key subkey s2 in
        let v = Val.meet v1 v2 in
        res := match key with
          | Static_types.Var _ ->
            let loc = subkey in
            let sub_s = MemMap.add loc (loc, en, v) MemMap.empty in
            (Env.add key sub_s !res)
          | Static_types.Array r ->
            let sub_s =
              try Env.find (Static_types.Array r) !res
              with Not_found -> MemMap.empty in
            let sub_s = MemMap.add subkey (i, en, v) sub_s in
            (Env.add (Static_types.Array r) sub_s !res)
      in
      Env.iter (fun key v -> MemMap.iter (meet_info key) v) s1;
      Some !res

  let add_addr_macro (s : t) elem =
    let bv     = Region_bitvector.bitvector_of elem in
    let v      = Val.singleton (`Value (`Constant, bv)) in
    let loc    = MemInterval.create Bigint.zero_big_int 1 in
    let en     = Machine.LittleEndian in
    let sub_s  = MemMap.add loc (loc, en, v) MemMap.empty in
    let v_addr = Var ("\\addr", Kernel_options.Machine.word_size ()) in
    let s      =
      match s with
      | None -> Some (Env.add v_addr sub_s Env.empty)
      | Some s -> Some (Env.add v_addr sub_s s)
    in s


  let rec is_mul expr =
    match expr with
    | Dba.Expr.Cst _
    | Dba.Expr.Var _ -> ()
    | Dba.Expr.Load (_, _, expr)
    | Dba.Expr.Unary (_, expr) -> is_mul expr
    | Dba.Expr.Binary (Dba.Binary_op.Mult, _expr1, _expr2) -> raise Errors.Enumerate_Top
    | Dba.Expr.Ite (_, expr1, expr2)
    | Dba.Expr.Binary (_, expr1, expr2) -> is_mul expr1; is_mul expr2


  let rec eval_expr expr s assumes globals elements equalities =
    let eval_without_equalities () =
      match expr with
      | Dba.Expr.Var { Dba.name = st; Dba.size; _} ->
          let v =
            let loc = (Bigint.zero_big_int, 1, Machine.LittleEndian) in
            try load (Static_types.Var (st, size)) loc s assumes globals elements equalities
            with Not_found | Errors.Empty_env ->
            try load (Static_types.Var (st, size)) loc !s_init assumes globals elements equalities
            with Not_found -> Val.universe
          in v, assumes

      | Dba.Expr.Load (size, en, e) -> (
          try
            let v_exp, assumes = eval_expr e s assumes globals elements equalities in
            let indexes =
              try let _ = is_mul e in
                Val.elements v_exp
              with Errors.Enumerate_Top ->
                if List.length elements = 0
                then raise Errors.Enumerate_Top
                else elements
            in
            List.fold_right (fun elem acc ->
                let region = Region_bitvector.region_of elem in
                let i      = Region_bitvector.value_of elem in
                let s      = add_addr_macro s elem in
                let arr    = Array region in
                let ret    = load arr (i, size, en) s assumes globals elements equalities in
                Val.join ret acc
              ) indexes Val.empty, assumes
          with Val.Elements_of_top -> Val.universe, assumes
        )
      | Dba.Expr.Cst (r, v) -> Val.singleton (`Value (r, v)), assumes
      | Dba.Expr.Unary (uop, expr) ->
        let e, assumes = eval_expr expr s assumes globals elements equalities in
        let f =
          match uop with
          | Dba.Unary_op.UMinus -> Val.neg
          | Dba.Unary_op.Not -> Val.lognot
          | Dba.Unary_op.Uext n -> fun e -> Val.extension e n
          | Dba.Unary_op.Sext n -> fun e -> Val.signed_extension e n
          | Dba.Unary_op.Restrict {Interval.lo; Interval.hi} ->
            fun e -> Val.restrict e lo hi
        in f e, assumes
      | Dba.Expr.Binary (bop, expr1, expr2) ->
        let op1, assumes = eval_expr expr1 s assumes globals elements equalities in
        let op2, assumes = eval_expr expr2 s assumes globals elements equalities in
        let build_bop =
          match bop with
          | Dba.Binary_op.Plus        -> Val.add
          | Dba.Binary_op.Minus       -> Val.sub
          | Dba.Binary_op.Mult        -> Val.mul
          | Dba.Binary_op.DivU        -> Val.udiv
          | Dba.Binary_op.DivS        -> Val.sdiv
          | Dba.Binary_op.ModU        -> Val.umod
          | Dba.Binary_op.ModS        -> Val.smod
          | Dba.Binary_op.Or          -> Val.logor
          | Dba.Binary_op.And         -> Val.logand
          | Dba.Binary_op.Xor         -> Val.logxor
          | Dba.Binary_op.Concat      -> Val.concat
          | Dba.Binary_op.LShift      -> Val.lshift
          | Dba.Binary_op.RShiftU     -> Val.rshiftU
          | Dba.Binary_op.RShiftS     -> Val.rshiftS
          | Dba.Binary_op.LeftRotate  -> Val.rotate_left
          | Dba.Binary_op.RightRotate -> Val.rotate_right
          | Dba.Binary_op.Eq          -> Val.eq
          | Dba.Binary_op.Diff        -> Val.diff
          | Dba.Binary_op.LeqU        -> Val.leqU
          | Dba.Binary_op.LtU         -> Val.ltU
          | Dba.Binary_op.GeqU        -> Val.geqU
          | Dba.Binary_op.GtU         -> Val.gtU
          | Dba.Binary_op.LeqS        -> Val.leqS
          | Dba.Binary_op.LtS         -> Val.ltS
          | Dba.Binary_op.GeqS        -> Val.geqS
          | Dba.Binary_op.GtS         -> Val.gtS
        in build_bop op1 op2, assumes

      | Dba.Expr.Ite (cond, expr1, expr2) ->
        let cond, assumes = eval_cond cond s assumes globals elements equalities in
        begin match cond with
          | Ternary.True ->
            eval_expr expr1 s assumes globals elements equalities
          | Ternary.False ->
            eval_expr expr2 s assumes globals elements equalities
          | Ternary.Unknown ->
            let op1 =
              eval_expr expr1 s assumes globals elements equalities |> fst
            and op2 =
              eval_expr expr2 s assumes globals elements equalities |> fst
            in Val.join op1 op2, assumes
        end
    in
    (* Redefinition for stat purposes *)
    let eval_without_equalities () =
      Display.save_evaluation_counts ();
      let _time, v = Utils.time (eval_without_equalities) in
      (* add_time_without_equalities time; *)
      Display.restore_evaluation_counts ();
      v
    in
    Display.increase_evaluation_count ();
    match Dba.LValue.of_expr expr with
    | lhs_e ->
      Display.increase_lhs_evaluation_count ();
      let t0 = Unix.gettimeofday () in
      let equal_lhs_e, equal_v_e = Eq.find equalities lhs_e in
      Ai_options.time_equalities := Unix.gettimeofday () -. t0
                                    +. !Ai_options.time_equalities;
      begin
        match equal_lhs_e, equal_v_e with
        |  _, None | None, _ -> eval_without_equalities ()
        | Some lhs_eq, Some v ->
          Display.increase_lhseq_evaluation_count ();
          if not (Dba.LValue.equal lhs_e lhs_eq) then
            Display.equality_use !current_address lhs_eq lhs_e;

          let v_without_equalities, _ = eval_without_equalities () in

          if Val.contains v_without_equalities v &&
             not (Val.contains v v_without_equalities)
          then incr Ai_options.nb_equalities_refinement;
          v, assumes
      end
    | exception Failure _ -> eval_without_equalities ()



  and eval_cond expr s assumes globals elements equalities =
    try
      let op,assumes = eval_expr expr s assumes globals elements equalities in
      Val.is_true op assumes globals, assumes
    with Smt_bitvectors.Assume_condition smb ->
      let assumes = smb :: assumes in
      eval_cond expr s assumes globals elements equalities


  and get_elem i r m =
    let en = Machine.LittleEndian in
    try MemMap.find (MemInterval.create i 1) (Env.find (Static_types.Array r) m)
    with Not_found ->
    match !s_init with
    | None -> raise Errors.Empty_env
    | Some s ->
      try MemMap.find (MemInterval.create i 1) (Env.find (Static_types.Array r) s)
      with Not_found ->
        if Region_bitvector.region_equal r `Constant
        then begin
          let value =
            try Val.singleton (Region_bitvector.get_byte_region_at i)
            with _ -> Val.universe
          in
          Display.add_call i;
          MemInterval.create i 1, en, value
        end
        else MemInterval.create i 1, en, Val.universe


  and retrieve_value_little i r m =
    let size = MemInterval.size_of i in
    let i = MemInterval.base_of i in
    let a, en, value = get_elem i r m in
    let sz = MemInterval.size_of a in
    let a = MemInterval.base_of a in
    match en with
    | Machine.LittleEndian ->
      if Bigint.eq_big_int a i && sz = size
      then value
      else if Bigint.eq_big_int a i
      then
        if size < sz
        then Val.restrict value 0 ((size * 8) - 1)
        else
          let sz' = Bigint.big_int_of_int sz in
          let i   = Bigint.add_big_int i sz' in
          let v   = retrieve_value_little (MemInterval.create i (size - sz)) r m in
          Val.concat v value
      else
        let sz_minus1   = Bigint.big_int_of_int (sz - 1)   in
        let size_minus1 = Bigint.big_int_of_int (size - 1) in
        let upper_a = (Bigint.add_big_int a sz_minus1) in
        let upper_i = (Bigint.add_big_int i size_minus1) in
        if Bigint.lt_big_int a i then
          if Bigint.ge_big_int upper_a upper_i
          then
            let delta = Bigint.sub_big_int i a in
            let off1 = (Bigint.int_of_big_int delta) * 8 in
            let off2 = off1 + size * 8 - 1 in
            Val.restrict value off1 off2
          else
            let delta = Bigint.sub_big_int i a in
            let off1 = (Bigint.int_of_big_int delta) * 8 in
            let off2 = (sz * 8) - 1 in
            let v1 = Val.restrict value off1 off2 in
            let i  = Bigint.succ_big_int upper_a in
            let size = Bigint.sub_big_int upper_i upper_a in
            let size = Bigint.int_of_big_int size in
            let v2 = retrieve_value_little (MemInterval.create i size) r m in
            Val.concat v2 v1
        else failwith "unrelSate.ml: load_little_e"
    | Machine.BigEndian ->
      let rec invert value off1 off2 acc sz =
        if sz < 1
        then acc
        else let v = Val.restrict value off1 off2 in
          let acc = Val.concat acc v in
          invert value (off1 + 8) (off2 + 8) acc (sz - 1)
      in
      if (Bigint.eq_big_int a i) && (sz=size)
      then
        let v = Val.restrict value 0 7 in
        invert value 8 15 v (sz - 1)
      else if (Bigint.eq_big_int a i)
      then
        if size < sz
        then
          let off1 = (sz - size) * 8 in
          let off2 = off1 + 7 in
          let v = Val.restrict value off1 off2 in
          invert value off1 off2 v (size - 1)
        else
          let v = Val.restrict value 0 7 in
          let v1 = invert value 8 15 v (sz - 1) in
          let sz' = Bigint.big_int_of_int sz in
          let i   = Bigint.add_big_int i sz' in
          let v2 =  retrieve_value_little (MemInterval.create i (size - sz)) r m in
          Val.concat v2 v1
      else failwith "unrelSate.ml: impossible case in load_big_e"


  and retrieve_value_big i r m =
    let size = MemInterval.size_of i in
    let i = MemInterval.base_of i in
    let en = Machine.LittleEndian in
    let a, _en, value =
      try MemMap.find (MemInterval.create i 1) (Env.find (Static_types.Array r) m)
      with Not_found ->
      match !s_init with
      | None -> raise Errors.Empty_env
      | Some s ->
        try MemMap.find (MemInterval.create i 1) (Env.find (Static_types.Array r) s)
        with Not_found ->
          let v =
            try Val.singleton (Region_bitvector.get_byte_region_at i)
            with Errors.Invalid_address _ -> Val.universe
          in MemInterval.create i 1, en, v
    in
    let sz = MemInterval.size_of a in
    let a = MemInterval.base_of a in
    if Bigint.eq_big_int a i && sz = size
    then value
    else if Bigint.eq_big_int a i
    then
      if size < sz
      then Val.restrict value 0 (size - 1)
      else
        let i = Bigint.add_big_int i (Bigint.big_int_of_int sz) in
        let v = retrieve_value_big (MemInterval.create i (size - sz)) r m in
        Val.concat v value
    else
      let sz_minus1   = Bigint.big_int_of_int (sz - 1)   in
      let size_minus1 = Bigint.big_int_of_int (size - 1) in
      let upper_a = (Bigint.add_big_int a sz_minus1) in
      let upper_i = (Bigint.add_big_int i size_minus1) in
      if Bigint.lt_big_int a i then
        if Bigint.ge_big_int upper_a upper_i
        then
          let delta = Bigint.sub_big_int upper_a upper_i in
          let off1 = Bigint.int_of_big_int delta in
          let off2 = sz - 1 in
          Val.restrict value off1 off2
        else
          let off1 = 0 in
          let delta = Bigint.sub_big_int upper_a i in
          let off2 = Bigint.int_of_big_int delta in
          let v1 = Val.restrict value off1 off2 in
          let i = Bigint.succ_big_int upper_a in
          let size = off2 - off1 + 1 in
          let v2 = retrieve_value_big (MemInterval.create i size) r m in
          Val.concat v2 v1
      else failwith "unrelSate.ml: impossible case in load_big"


  (* Checking read of memory permissions here *)
  and load x (i, size, en) m assumes globals elements equalities =
    match m with
    | None -> raise Errors.Empty_env
    | Some m ->
      match x with
      | Static_types.Var _ ->
        let loc = MemInterval.create Bigint.zero_big_int 1 in
        let _, _, av = MemMap.find loc (Env.find x m) in av
      | Static_types.Array r ->
        let c =
          try Dba_types.Rights.find_read_right r !Concrete_eval.permis
          with Not_found -> Dba.Expr.one in
        let b, _assumes = eval_cond c (Some m) assumes globals elements equalities in
        match b with
        | Ternary.True ->
          begin
            match en with
            | Machine.LittleEndian ->
              retrieve_value_little (MemInterval.create i size) r m
            | Machine.BigEndian ->
              retrieve_value_big (MemInterval.create i size) r m
          end
        | Ternary.False -> raise Errors.Read_permission_denied
        | Ternary.Unknown -> failwith "read permission unknown"


  and update base en old_value new_value sub_m =
    let size = MemInterval.size_of base in
    let base = MemInterval.base_of base in
    let loc = MemInterval.create base size in
    let v = Val.join old_value new_value in
    MemMap.add loc (loc, en, v) sub_m


  and clear_at_beginning old_info new_info sub_m _sw =
    Logger.debug "Clear beginning";
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (_new_loc, new_size, _new_en, _new_value) = new_info in
    let off1 = new_size * 8 in
    let off2 = old_size * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let sz = Bigint.big_int_of_int new_size in
    let base = Bigint.add_big_int old_loc sz in
    let loc = MemInterval.create base (old_size - new_size) in
    let sub_m = MemMap.add loc (loc, old_en, v) sub_m in
    sub_m


  and update_at_beginning old_info new_info sub_m sw =
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    if sw = Strong
    then
      clear_at_beginning old_info new_info sub_m sw
    else
      let off1 = 0 in
      let off2 = 8 * new_size - 1 in
      assert (off1 <= off2);
      let v = Val.restrict old_value off1 off2 in
      let loc = MemInterval.create new_loc new_size in
      let sub_m = update loc new_en v new_value sub_m in
      let off1 = new_size * 8 in
      let off2 = old_size * 8 - 1 in
      assert (off1 <= off2);
      let v = Val.restrict old_value off1 off2 in
      let sz = Bigint.big_int_of_int new_size in
      let base = Bigint.add_big_int old_loc sz in
      let loc = MemInterval.create base (old_size - new_size) in
      let sub_m = MemMap.add loc (loc, old_en, v) sub_m in
      sub_m


  and clear_at_beginning_and_beyond addrStack old_info new_info sub_m sw =
    Logger.debug "Clear beginning & beyond called";
    let old_loc, old_size, _, _ = old_info in
    let (_new_loc, new_size, new_en, new_value) = new_info in
    let off1 = old_size * 8 in
    let off2 = new_size * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict new_value off1 off2 in
    let sz = Bigint.big_int_of_int old_size in
    let base = Bigint.add_big_int old_loc sz in
    split_regions addrStack sub_m v (base, new_size - old_size) new_en sw


  and update_at_beginning_and_beyond addrStack old_info new_info sub_m sw =
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    let off1 = 0 in
    let off2 = 8 * old_size - 1 in
    assert (off1 <= off2);
    let v = Val.restrict new_value off1 off2 in
    let loc = MemInterval.create new_loc old_size in
    let sub_m = update loc new_en v old_value sub_m in
    let off1 = old_size * 8 in
    let off2 = new_size * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict new_value off1 off2 in
    let sz = Bigint.big_int_of_int old_size in
    let base = Bigint.add_big_int old_loc sz in
    let sub_m = MemMap.add (MemInterval.create old_loc old_size)
        (MemInterval.create old_loc old_size, old_en, old_value) sub_m in
    split_regions addrStack sub_m v (base, new_size - old_size) new_en sw


  and clear_inside_and_beyond addrStack old_info new_info sub_m sw =
    Logger.debug "Clear inside & beyond called";
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    let delta = Bigint.sub_big_int new_loc old_loc in
    let delta = Bigint.int_of_big_int delta in
    let off1 = 0 in
    let off2 = delta * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let loc = MemInterval.create old_loc delta in
    let sub_m = MemMap.add loc (loc, old_en, v) sub_m in
    let off1 = delta * 8 in
    let off2 = old_size * 8 - 1 in
    let off2' = off2 - off1 in
    let off1 = off2' + 1 in
    let off2 = new_size * 8 - 1 in
    if (off1 > off2) then
      sub_m
    else
      let v = Val.restrict new_value off1 off2 in
      let sz = Bigint.big_int_of_int ((off2' + 1) / 8) in
      let base = Bigint.add_big_int new_loc sz in
      split_regions addrStack sub_m v (base, (off2 - off1) / 8 + 1) new_en sw


  and update_inside_and_beyond addrStack old_info new_info sub_m sw =
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    let delta = Bigint.sub_big_int new_loc old_loc in
    let delta = Bigint.int_of_big_int delta in
    let off1 = 0 in
    let off2 = delta * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let loc = MemInterval.create old_loc delta in
    let sub_m = MemMap.add loc (loc, old_en, v) sub_m in
    let off1 = delta * 8 in
    let off2 = old_size * 8 - 1 in
    assert (off1 <= off2);
    let v1 = Val.restrict old_value off1 off2 in
    let off1' = 0 in
    let off2' = off2 - off1 in
    assert (off1' <= off2');
    let v2 = Val.restrict new_value off1' off2' in
    let v = Val.join v1 v2 in
    let loc = MemInterval.create new_loc (off2' / 8 + 1) in
    let sub_m = MemMap.add loc (loc, new_en, v) sub_m in
    let off1 = off2' + 1 in
    let off2 = new_size * 8 - 1 in
    if (off1 > off2) then
      sub_m
    else
      let v = Val.restrict new_value off1 off2 in
      let sz = Bigint.big_int_of_int off2' in
      let base = Bigint.add_big_int new_loc sz in
      let sub_m = MemMap.add (MemInterval.create old_loc old_size)
          ((MemInterval.create old_loc old_size), old_en, old_value) sub_m in
      split_regions addrStack sub_m v (base, ((off2 - off1) / 8 + 1)) new_en sw


  and clear_inside addrStack old_info new_info sub_m sw =
    Logger.debug "Clear inside called";
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, _new_value) = new_info in
    let delta = Bigint.sub_big_int new_loc old_loc in
    let delta = Bigint.int_of_big_int delta in
    let off1 = 0 in
    let off2 = delta * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let loc = MemInterval.create old_loc delta in
    let sub_m = MemMap.add loc (loc, old_en, v) sub_m in
    let off2 = (new_size + delta) * 8 - 1 in
    let sz = Bigint.big_int_of_int off2 in
    let off1 = off2 + 1 in
    let off2 = old_size * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let base = Bigint.add_big_int new_loc sz in
    let sub_m = MemMap.add (MemInterval.create old_loc old_size)
        ((MemInterval.create old_loc old_size), old_en, old_value) sub_m in
    let sub_m = split_regions addrStack sub_m v (base, ((off2 - off1) / 8 + 1)) new_en sw in
    sub_m


  and update_inside addrStack old_info new_info sub_m sw =
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    let delta = Bigint.sub_big_int new_loc old_loc in
    let delta = Bigint.int_of_big_int delta in
    let off1 = 0 in
    let off2 = delta * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let loc = MemInterval.create old_loc delta in
    let sub_m = MemMap.add loc (loc, old_en, v) sub_m in
    let off1 = delta * 8 in
    let off2 = (new_size + delta) * 8 - 1 in
    assert (off1 <= off2);
    let v1 = Val.restrict old_value off1 off2 in
    let off1' = 0 in
    let off2' = off2 - off1 in
    assert (off1' <= off2');
    let v2 = Val.restrict new_value off1' off2' in
    let v = Val.join v1 v2 in
    let loc = MemInterval.create new_loc (off2' / 8 + 1) in
    let sub_m = MemMap.add loc (loc, new_en, v) sub_m in
    let sz = Bigint.big_int_of_int off2 in
    let off1 = off2 + 1 in
    let off2 = old_size * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict old_value off1 off2 in
    let base = Bigint.add_big_int new_loc sz in
    let sub_m = MemMap.add (MemInterval.create old_loc old_size)
        ((MemInterval.create old_loc old_size), old_en, old_value) sub_m in
    let sub_m = split_regions addrStack sub_m v
        (base, succ (off2 - off1) / (Constants.bytesize:>int)) new_en sw in
    sub_m


  and clear_before_and_beyond addrStack old_info new_info sub_m sw =
    Logger.debug "Clear before & beyond";
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    let delta = Bigint.sub_big_int old_loc new_loc in
    let delta = Bigint.int_of_big_int delta in
    let off1 = delta * 8 in
    let off2 = new_size * 8  - 1 in
    Logger.debug "OFF1:%d OFF2:%d" off1 off2;
    assert (off1 <= off2);
    let v = Val.restrict new_value off1 off2 in
    let sub_m = MemMap.add (MemInterval.create old_loc old_size)
        ((MemInterval.create old_loc old_size), old_en, old_value) sub_m in
    let sub_m = split_regions addrStack sub_m v (old_loc, new_size - delta) new_en sw in
    sub_m


  and update_before_and_beyond addrStack old_info new_info sub_m sw =
    let (old_loc, old_size, old_en, old_value) = old_info in
    let (new_loc, new_size, new_en, new_value) = new_info in
    let delta = Bigint.sub_big_int old_loc new_loc in
    let delta = Bigint.int_of_big_int delta in
    let sub_m =
      if (sw = Strong) then
        let off1 = 0 in
        let off2 = delta * 8 - 1 in
        assert (off1 <= off2);
        let v = Val.restrict new_value off1 off2 in
        let loc = MemInterval.create new_loc delta in
        MemMap.add loc (loc, new_en, v) sub_m
      else sub_m
    in
    let off1 = delta * 8 in
    let off2 = new_size * 8 - 1 in
    assert (off1 <= off2);
    let v = Val.restrict new_value off1 off2 in
    let sub_m = MemMap.add (MemInterval.create old_loc old_size)
        (MemInterval.create old_loc old_size, old_en, old_value) sub_m in
    let sub_m = split_regions addrStack sub_m v (old_loc, new_size - delta) new_en sw in
    sub_m


  and split_regions addrStack sub_m new_value (address, nbytes) new_en sw =
    let new_loc = address
    and new_size = nbytes in
    let (_, _, loop) = addrStack in
    try
      let old_loc, old_en, old_value = collect new_loc 1 new_size sub_m in
      let old_size = MemInterval.size_of old_loc in
      let old_loc = MemInterval.base_of old_loc in
      if loop > 1 then
        Logger.warning "Potential buffer overflow at %a"
          Dba_types.AddressStack.pp addrStack;
      let loc = MemInterval.create old_loc old_size in
      let sub_m = MemMap.remove loc sub_m in
      let old_info = (old_loc, old_size, old_en, old_value) in
      let new_info = (new_loc, new_size, new_en, new_value) in
      match old_en, new_en with
      | Machine.LittleEndian, Machine.LittleEndian -> (
          match sw with
          | Strong ->
            if (Bigint.eq_big_int old_loc new_loc) && (new_size = old_size)
            then sub_m
            else if Bigint.eq_big_int old_loc new_loc
            then
              let f =
                if new_size < old_size
                then clear_at_beginning
                else clear_at_beginning_and_beyond addrStack
              in f old_info new_info sub_m sw

            else
              let old_size_1 = Bigint.big_int_of_int (old_size - 1) in
              let new_size_1 = Bigint.big_int_of_int (new_size - 1) in
              let upper_old = Bigint.add_big_int old_loc old_size_1 in
              let upper_new = Bigint.add_big_int new_loc new_size_1 in
              if Bigint.lt_big_int old_loc new_loc then
                if Bigint.ge_big_int upper_new upper_old
                then clear_inside_and_beyond addrStack old_info new_info sub_m sw
                else clear_inside addrStack old_info new_info sub_m sw
              else clear_before_and_beyond addrStack old_info new_info sub_m sw
          | Weak ->
            if (Bigint.eq_big_int old_loc new_loc) && (new_size = old_size)
            then update (MemInterval.create new_loc new_size) new_en old_value new_value sub_m
            else (
              if (Bigint.eq_big_int old_loc new_loc)
              then
                if new_size < old_size
                then update_at_beginning old_info new_info sub_m sw
                else update_at_beginning_and_beyond addrStack old_info new_info sub_m sw
              else
                let old_size_1 = Bigint.big_int_of_int (old_size - 1) in
                let new_size_1 = Bigint.big_int_of_int (new_size - 1) in
                let upper_old = Bigint.add_big_int old_loc old_size_1 in
                let upper_new = Bigint.add_big_int new_loc new_size_1 in
                if Bigint.lt_big_int old_loc new_loc then
                  if Bigint.ge_big_int upper_new upper_old
                  then update_inside_and_beyond addrStack old_info new_info sub_m sw
                  else update_inside addrStack old_info new_info sub_m sw
                else update_before_and_beyond addrStack old_info new_info sub_m sw
            )
        )
      | Machine.BigEndian, Machine.LittleEndian
      | Machine.LittleEndian, Machine.BigEndian
      | Machine.BigEndian, Machine.BigEndian -> failwith "split_regions:big_endian"
    with Not_found ->
      if sw = Strong
      then
        let loc = MemInterval.create new_loc new_size in
        Logger.debug "SPLITREG STORE: %a" MemInterval.pp loc;
        MemMap.add loc (loc, new_en, new_value) sub_m
      else sub_m


  (* Checking write to memory permissions here *)
  and store addrStack x (i, nbytes) en value m assumes globals sw elements equalities =
    let m = match m with
      | None -> Env.empty
      | Some m -> m
    in
    match x with
    | Static_types.Var _ ->
      let loc = MemInterval.create Bigint.zero_big_int 1 in
      let sub_m = MemMap.singleton loc (loc, en, value) in
      Some (Env.add x sub_m m)
    | Static_types.Array r ->
      let c =
        try Dba_types.Rights.find_write_right r !Concrete_eval.permis
        with Not_found -> Dba.Expr.one
      in
      let b, _assumes = eval_cond c (Some m) assumes globals elements equalities in
      match b with
      | Ternary.True ->
        let sub_m =
          try Env.find (Static_types.Array r) m with Not_found -> MemMap.empty
        in
        let sub_m = split_regions addrStack sub_m value (i, nbytes) en sw in
        let sub_m =
          if sw = Strong then begin
            let loc = MemInterval.create i nbytes in
            Logger.debug "LOCSTORE: %a" MemInterval.pp loc;
            MemMap.add loc (loc, en, value) sub_m
          end
          else sub_m
        in Some (Env.add (Static_types.Array r) sub_m m)
      | Ternary.False -> raise Errors.Write_permission_denied
      |	Ternary.Unknown -> failwith "write permission unknown"


  and check_region_size region i sz =
    if sz < 1 then
      raise (Errors.Bad_bound ("store, case1: store size = " ^ (string_of_int sz)))
    else
      match region with
      | `Constant | `Stack -> ()
      | `Malloc ((id, _), malloc_size) ->
        let open Bigint in
        if gt_big_int (add_big_int i (big_int_of_int (sz - 1))) malloc_size
        then
          let message = Format.asprintf "store, case2: store at 𝑴 %d[@%s, size = %d bytes] but size(𝑴 %d) = %s bytes!"
              id (Bigint.string_of_big_int i) sz id (Bigint.string_of_big_int malloc_size) in
          raise (Errors.Bad_bound message)
        else ()


  let store endianness addrStack sz v e env assumes globals recordMap elements
      equalities =
    let v_index, assumes = eval_expr e env assumes globals elements equalities
    in
    Logger.debug "VAL: %a" Val.pp v_index;
    let en = endianness in
    let indexes =
      try Val.elements v_index
      with
      | Val.Elements_of_top ->
        if Ai_options.FailSoftMode.get () then
          try Dba_types.AddressStack.Map.find addrStack recordMap
          with Not_found -> []
        else
        if List.length elements = 0
        then raise Errors.Enumerate_Top
        else elements
    in
    let apply update elem env =
      let region = Region_bitvector.region_of elem in
      let i = Region_bitvector.value_of elem in
      Logger.debug "RBVVAL: %s" (Bigint.string_of_big_int i);
      check_region_size region i sz;
      let r = Static_types.Array region in
      store addrStack r (i, sz) en v env assumes globals update elements equalities
    in
    let env =
      match indexes with
      | [] -> env
      | [elem] -> apply Strong elem env
      | elems -> List.fold_right (apply Weak) elems env
    in
    env, assumes, Dba_types.AddressStack.Map.add addrStack indexes recordMap

  let store_little_end = store Machine.LittleEndian
  let store_big_end = store Machine.BigEndian

  let assign addrStack lhs e s assumes globals recordMap elements equalities =
    current_address := addrStack;
    let v, assumes = eval_expr e s assumes globals elements equalities in
    match lhs with
    | Dba.(LValue.Var { name = st; size; _}) ->
      let v_string = Val.to_string v in
      Display.display (Display.Assign (st, e, v_string));
      let loc = MemInterval.create Bigint.zero_big_int 1 in
      let sub_s = MemMap.add loc (loc, Machine.LittleEndian, v) MemMap.empty in
      let s = match s with None -> Env.empty | Some s -> s in
      let s = Some (Env.add (Static_types.Var (st, size)) sub_s s) in
      s, assumes, recordMap, v
    | Dba.LValue.Restrict ({ Dba.name = st; Dba.size; _}, {Interval.lo=of1; Interval.hi=of2}) ->
      let s =
        match s with
          None -> Env.empty
        | Some s -> s
      in
      let loc = MemInterval.create Bigint.zero_big_int 1 in
      let en, x =
        try let _, en, av = (MemMap.find loc (Env.find (Var (st, size)) s))
          in en, av
        with Not_found -> Machine.LittleEndian,Val.universe
      in
      let temp1 =
        if (of1 = 0) then v
        else (Val.concat v (Val.restrict x 0 (of1 - 1)))
      in
      let temp2 = Val.restrict x (of2 + 1) (size - 1) in
      let loc = MemInterval.create Bigint.zero_big_int 1 in
      let v = Val.concat temp2 temp1 in
      let sub_s = MemMap.add loc (loc, en, v) MemMap.empty in
      let s = Some (Env.add (Static_types.Var (st, size)) sub_s s) in
      s, assumes, recordMap, v
    | Dba.LValue.Store (sz, endianness, e) ->
      let op, assumes, recordMap =
        store endianness addrStack sz v e s assumes globals recordMap elements equalities in
      op, assumes, recordMap, v


  let rec guard addr cond s assumes glbs rcd elements equalities =
    match s with
    | None -> None, assumes, rcd, equalities
    | Some m ->
      match cond with
      | Dba.Expr.Cst (_, v) ->
        let s' = if Bitvector.is_zero v then None else s in
        s', assumes, rcd, equalities
      | Dba.Expr.Binary (bop, exp1, exp2) -> (
          match bop with
          | Dba.Binary_op.Eq | Dba.Binary_op.Diff | Dba.Binary_op.LeqU | Dba.Binary_op.LtU
          | Dba.Binary_op.GeqU | Dba.Binary_op.GtU | Dba.Binary_op.LeqS | Dba.Binary_op.LtS
          | Dba.Binary_op.GeqS | Dba.Binary_op.GtS ->
            let v_1, v_2 =
              let op1, assumes = eval_expr exp1 s assumes glbs elements equalities in
              let op2, _assumes = eval_expr exp2 s assumes glbs elements equalities in
              Val.guard bop op1 op2 in
            let loc = MemInterval.empty in
            (match exp1, exp2 with
             | Dba.Expr.Var { Dba.name = v1; Dba.size; _}, Dba.Expr.Cst _ ->
               let en = Machine.LittleEndian in
               let sub_m = MemMap.add loc (loc, en, v_1) MemMap.empty in
               let s = Some (Env.add (Static_types.Var (v1, size)) sub_m m) in
               let equalities = Eq.refine exp1 v_1 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Unary (Dba.Unary_op.Restrict
                                 {Interval.lo = o1; Interval.hi = o2;},
                               Dba.Expr.Var { Dba.name = v1; Dba.size; _}), Dba.Expr.Cst _ ->
               let en, x =
                 try
                   let _, en, av = MemMap.find loc (Env.find (Var (v1, size)) m)
                   in en, av
                 with Not_found -> Machine.LittleEndian, Val.universe
               in
               let temp1 =
                 if o1 = 0 then v_1
                 else Val.concat v_1 (Val.restrict x 0 (o1 - 1)) in
               let temp2 = Val.restrict x (o2 + 1) (size - 1) in
               let v = Val.concat temp2 temp1 in
               let sub_m = MemMap.add loc (loc, en, v) MemMap.empty in
               let s = Some (Env.add (Static_types.Var (v1, size)) sub_m m) in
               let equalities = Eq.refine exp1 v_1 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Load (sz, Machine.LittleEndian, e), Dba.Expr.Cst _ ->
               let s, assumes, rcd = store_little_end addr sz v_1 e s assumes glbs rcd elements equalities in
               let equalities = Eq.refine exp1 v_1 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Cst _, Dba.Expr.Load (sz, Machine.LittleEndian, e) ->
               let s, assumes, rcd =
                 store_little_end addr sz v_2 e s assumes glbs rcd  elements equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Cst _, Dba.Expr.Var { Dba.name = v2; Dba.size; _} ->
               let en = Machine.LittleEndian in
               let sub_m = MemMap.add loc (loc, en, v_2) MemMap.empty in
               let s = Some (Env.add (Static_types.Var (v2, size)) sub_m m) in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Var { Dba.name = v1; _},
               Dba.Expr.Var {Dba.name = v2; Dba.size; _} ->
               let en = Machine.LittleEndian in
               let sub_m = MemMap.add loc (loc, en, v_1) MemMap.empty in
               let m = Env.add (Static_types.Var (v1, size)) sub_m m in
               let sub_m = MemMap.add loc (loc, en, v_2) MemMap.empty in
               let s = Some (Env.add (Static_types.Var (v2, size)) sub_m m) in
               let equalities = Eq.refine exp1 v_1 equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Var {Dba.name = v1; Dba.size = size2; _}, Dba.Expr.Load (size, Machine.BigEndian, e) ->
               let en = Machine.LittleEndian in
               let sub_m = MemMap.add loc (loc, en, v_1) MemMap.empty in
               let m = Some (Env.add (Static_types.Var (v1, size2)) sub_m m) in
               let s, assumes, rcd = store_big_end addr size v_2 e m assumes glbs rcd elements equalities in
               let equalities = Eq.refine exp1 v_1 equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Load (size, Machine.BigEndian, e), Dba.Expr.Var {Dba.name = v2; Dba.size = size2; _} ->
               let en = Machine.LittleEndian in (* FIXME ? *)
               let sub_m = MemMap.singleton loc (loc, en, v_2) in
               let m = Some (Env.add (Static_types.Var (v2, size2)) sub_m m) in
               let s, assumes, rcd = store_big_end addr size v_1 e m assumes glbs rcd elements equalities in
               let equalities = Eq.refine exp1 v_1 equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             |  Dba.Expr.Var {Dba.name = v1; Dba.size = size1; _}, Dba.Expr.Load (sz, Machine.LittleEndian, e) ->
               let en = Machine.LittleEndian in
               let sub_m = MemMap.add loc (loc, en, v_1) MemMap.empty in
               let m = Some (Env.add (Static_types.Var (v1, size1)) sub_m m) in
               let s, assumes, rcd = store_little_end addr sz v_2 e m assumes glbs rcd elements equalities in
               let equalities = Eq.refine exp1 v_1 equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Load (sz, Machine.LittleEndian, e),  Dba.Expr.Var {Dba.name = v2; Dba.size = size2; _} ->
               let en = Machine.LittleEndian in
               let sub_m = MemMap.add loc (loc, en, v_2) MemMap.empty in
               let m = Some (Env.add (Static_types.Var (v2, size2)) sub_m m) in
               let s, assumes, rcd = store_little_end addr sz v_1 e m assumes glbs rcd elements equalities in
               let equalities = Eq.refine exp1 v_1 equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | Dba.Expr.Load (size1, endianness1, e1),
               Dba.Expr.Load (size2, endianness2, e2) ->
               let m, assumes, rcd = store endianness1
                   addr size1 v_1 e1 s assumes glbs rcd elements equalities in
               let s, assumes, rcd = store endianness2
                   addr size2 v_2 e2 m assumes glbs rcd elements equalities in
               let equalities = Eq.refine exp1 v_1 equalities in
               let equalities = Eq.refine exp2 v_2 equalities in
               s, assumes, rcd, equalities
             | _, _ -> s, assumes, rcd, equalities
            )
          | _ -> s, assumes, rcd, equalities
        )
      | Dba.Expr.Unary (uop, expr) ->
        ( match uop with
          | Dba.Unary_op.Not ->
            (match expr with
             | Dba.Expr.Binary (bop, e1, e2) ->
               let k c =
                 guard addr c s assumes glbs rcd elements equalities in
               begin
                 match bop with
                 | Dba.Binary_op.Eq   -> Dba.Expr.diff e1 e2 |> k
                 | Dba.Binary_op.Diff -> Dba.Expr.equal e1 e2   |> k
                 | Dba.Binary_op.LeqU -> Dba.Expr.ugt e1 e2  |> k
                 | Dba.Binary_op.LtU  -> Dba.Expr.uge e1 e2  |> k
                 | Dba.Binary_op.GeqU -> Dba.Expr.ult e1 e2  |> k
                 | Dba.Binary_op.GtU  -> Dba.Expr.ule e1 e2  |> k
                 | Dba.Binary_op.LeqS -> Dba.Expr.sgt e1 e2  |> k
                 | Dba.Binary_op.LtS  -> Dba.Expr.sge e1 e2  |> k
                 | Dba.Binary_op.GeqS -> Dba.Expr.slt e1 e2  |> k
                 | Dba.Binary_op.GtS  -> Dba.Expr.sle e1 e2  |> k
                 | Dba.Binary_op.Or   -> Dba.Expr.(logand (lognot e1) (lognot e2)) |> k
                 | Dba.Binary_op.And  -> Dba.Expr.(logor (lognot e1) (lognot e2)) |> k
                 | _ -> s, assumes, rcd, equalities
               end
             | _ -> s, assumes, rcd, equalities)
          | _ -> s, assumes, rcd, equalities)
      | _ -> s, assumes, rcd, equalities


  let string_of_args args m assumes globals elements equalities =
    let b = Buffer.create 1024 in
    let rec aux assumes args =
      match args with
      | [] -> ()
      | Dba.Str s :: tl ->
        Buffer.add_string b (Scanf.unescaped s);
        aux assumes tl
      | Dba.Exp e :: tl ->
        let op, assumes = eval_expr e m assumes globals elements equalities in
        let temp = Val.to_string op in
        Buffer.add_string b temp;
        aux assumes tl
    in aux assumes args;
    Buffer.contents b


  let check_exec_permission addr s assumes globals elements equalities =
    let m = match s with None -> Env.empty | Some m -> m in
    let bv = Bitvector.create (Virtual_address.to_bigint addr.Dba.base) 32 in
    let v = Val.singleton (`Value (`Constant, bv)) in
    let loc = MemInterval.create Bigint.zero_big_int 1 in
    let sub_m =
      MemMap.add loc (loc, Machine.LittleEndian, v) MemMap.empty in
    let s = Some (Env.add (Var ("\\addr", Kernel_options.Machine.word_size ())) sub_m m) in
    let c =
      try Dba_types.Rights.find_exec_right `Constant !Concrete_eval.permis
      with Not_found -> Dba.Expr.one in

    let b, _assumes = (eval_cond c s assumes globals elements equalities) in
    match b with
    | Ternary.True -> addr
    | Ternary.False -> raise Errors.Exec_permission_denied
    | Ternary.Unknown -> failwith "exec permission unknown"


  let free expr s assumes globals elements equalities =
    let v, _assumes = eval_expr expr s assumes globals elements equalities in
    let l =
      try Val.elements v
      with Val.Elements_of_top -> raise Errors.Enumerate_Top
    in
    List.iter (fun elem ->
        match elem with
        | `Value (`Malloc _ as v, bv) ->
          let st =
            try Malloc_status.find v !Simulate.mallocs
            with Not_found -> failwith "Unbound free region"
          in
          let open Dba in
          begin match st with
            | Freeable when Bitvector.is_zero bv ->
              Simulate.mallocs :=
                Malloc_status.add v Freed !Simulate.mallocs
            | Freed -> raise Errors.Freed_variable_access
            | Freeable -> raise Errors.Invalid_free_address
          end
        | _ -> raise Errors.Invalid_free_region
      ) l;
    s


  let rec is_visited (a_ret, a_callee) callstack nb =
    match callstack with
    | [] -> false
    | (_, callee) :: tl ->
      if Dba_types.Caddress.equal callee a_callee
      then nb = 0 || is_visited (a_ret, a_callee) tl (nb - 1)
      else is_visited (a_ret, a_callee) tl nb


  let resolve_jump addr expr s flgs equalities recordMap assumes globals djumps_map tag elements =
    let _, cstack, loop = addr in
    let v, _assumes = eval_expr expr s assumes globals elements equalities in
    let l =
      if Ai_options.FailSoftMode.get () then
        try Val.elements v
        with Val.Elements_of_top ->
        try Dba_types.AddressStack.Map.find addr recordMap
        with Not_found -> []
      else
        try Val.elements v
        with Val.Elements_of_top ->
          if List.length elements = 0
          then raise Errors.Enumerate_Top
          else elements
    in
    let locations, djumps_map =
      List.fold_right (fun elem (acc1, acc2) ->
          if Region_bitvector.region_of elem = `Constant then
            if Region_bitvector.size_of elem =
                 Kernel_options.Machine.word_size () then
              begin
                let a =
                  Dba_types.Caddress.block_start @@
                    Virtual_address.of_bitvector @@
                  Region_bitvector.bitvector_of elem in

                let addrStack =
                  let open Dba in
                  match tag, cstack with
                  | Some Call addr_ret, cstack ->
                    Display.increase_function_count ();
                    let calling_callee_addr = addr_ret, a in
                    if is_visited calling_callee_addr cstack 20
                    then raise (RecursiveCall a)
                    else (
                      Display.display (Display.Call(a, addr_ret));
                      let cstack = calling_callee_addr :: cstack in
                      (a, cstack, loop)
                    )
                  | Some Return, (old_ret, old_start) :: cstack ->
                    let cstack =
                      if Dba_types.Caddress.equal old_ret a then cstack
                      else (old_ret, old_start) :: cstack in
                    a, cstack, loop
                  | Some Return, [] -> a, cstack, loop
                  | None, cstack    -> a, cstack, loop
                in
                let t1 = (addrStack, s, flgs, equalities) :: acc1 in
                let t2 =
                  let s =
                    try Dba_types.AddressStack.Map.find addr acc2
                    with Not_found -> Dba_types.Caddress.Set.empty
                  in
                  Dba_types.AddressStack.Map.add addr (Dba_types.Caddress.Set.add a s) acc2 in
                t1, t2
              end
            else
              raise Errors.Bad_address_size
          else
            raise (Errors.Bad_region "Dynamic jump")
        ) l ([], djumps_map) in
    locations, (Dba_types.AddressStack.Map.add addr l recordMap), djumps_map


  let resolve_if cond s flgs (m1, eq1) (m2, eq2) addr_suiv1 addr_suiv2 assumes globals elements equalities =
    match eval_cond cond s assumes globals elements equalities with
    | Ternary.True, _  -> [addr_suiv1, m1, flgs, eq1]
    | Ternary.False, _ -> [addr_suiv2, m2, flgs, eq2]
    | Ternary.Unknown, _ ->
      [ addr_suiv1, m1, flgs, eq1;
        addr_suiv2, m2, flgs, eq2; ]


  let resolve_assume addr cond s flgs equalities addr_suiv assumes glbs rcd elements =
    match eval_cond cond s assumes glbs elements equalities with
    | Ternary.Unknown, _ -> [addr_suiv, s, flgs, equalities]
    | _, assumes ->
      let s, _assumes, _rcd, equalities =
        guard addr cond s assumes glbs rcd elements equalities in
      [addr_suiv, s, flgs, equalities]


  let resolve_assert cond s flgs equalities addr_suiv addrStack instr assumes glbs rcd elements =
    let addr, _cstack, _loop = addrStack in
    let condi, assumes = (eval_cond cond s assumes glbs elements equalities) in
    let continue = Ternary.to_bool condi in
    if continue then
      let s, _assumes, _rcd, equalities = guard addrStack cond s assumes glbs rcd elements equalities in
      [addr_suiv, s, flgs, equalities]
    else Errors.assert_failure addr instr


  let resolve_nondet_assume addr lhslist cond s flgs equalities addr_suiv assumes glbs rcd elements =
    let rec update_memory_nondet lhslist s assumes glbs rcd =
      match lhslist with
      | [] -> s, assumes, rcd
      | (Dba.LValue.Var {Dba.name = st; Dba.size; _}) :: tl ->
        let l = MemInterval.create Bigint.zero_big_int 1 in
        let en = Machine.LittleEndian in
        let sub_s = MemMap.add l (l, en, Val.universe) MemMap.empty in
        let s = match s with None -> Env.empty | Some s -> s in
        let m' = Some (Env.add (Static_types.Var (st, size)) sub_s s) in
        update_memory_nondet tl m' assumes glbs rcd
      | Dba.LValue.Restrict _ :: _ ->
        failwith "UnrelState.ml: restrict case not handled2"
      | Dba.LValue.Store (sz, endianness, e) :: tl ->
        let m', assumes, rcd = store endianness addr sz Val.universe e s assumes
            glbs rcd elements equalities in
        update_memory_nondet tl m' assumes glbs rcd
    in
    let rec iterate cond iter assumes glbs rcd equalities =
      if iter mod 100000 = 0
      then Logger.debug "NONDET iterartion num %d" iter;
      let m', assumes, rcd = update_memory_nondet lhslist s assumes glbs rcd in
      let condi, assumes = eval_cond cond m' assumes glbs elements equalities in
      match condi with
      | Ternary.True -> m', assumes, rcd, equalities
      | Ternary.False ->
        iterate cond (iter + 1) assumes glbs rcd equalities
      | Ternary.Unknown ->
        guard addr cond m' assumes glbs rcd elements equalities
        (* TODO : apply a guard here *)
    in
    let op, assumes, rcd, equalities = iterate cond 0 assumes glbs rcd equalities in
    [addr_suiv, op, flgs, equalities], assumes, rcd


  let resolve_nondet addr lhs region s flgs equalities addr_suiv assumes glbs rcd elements =
    let _region = region in
    let m = match lhs with
        Dba.LValue.Var { Dba.name = st; Dba.size; _} ->
        let loc = MemInterval.create Bigint.zero_big_int 1 in
        let en = Machine.LittleEndian in
        let sub_s = MemMap.add loc (loc, en, Val.universe) MemMap.empty in
        let s = match s with None -> Env.empty | Some s -> s in
        Some (Env.add (Static_types.Var (st, size)) sub_s s)
      | Dba.LValue.Restrict _ ->
        failwith "unrelState.ml: restrict case not handled"
      | Dba.LValue.Store (size, Machine.BigEndian, expr) ->
        let op, _assumes, _rcd = store_big_end addr size Val.universe expr s assumes glbs rcd elements equalities in
        op
      | Dba.LValue.Store (size, Machine.LittleEndian, expr) ->
        let op, _assumes, _rcd = store_little_end addr size Val.universe expr s assumes glbs rcd elements equalities in
        op
    in
    [addr_suiv, m, flgs, equalities]


  let resolve_undef addr lhs s flgs equalities addr_suiv assumes glbs rcd elements =
    (* TODO : Kset can contain an undef value or
       it is trqnsformed to Top in this case *)
    let m = match lhs with
        Dba.LValue.Var { Dba.name = st; Dba.size; _} ->
        let loc = MemInterval.create Bigint.zero_big_int 1 in
        let v =Val.singleton
                 (`Undef (computesize_dbalhs lhs)) in
        let en = Machine.LittleEndian in
        let sub_s = MemMap.add loc (loc, en, v) MemMap.empty in
        let s = match s with None -> Env.empty | Some s -> s in
        Some (Env.add (Static_types.Var (st, size)) sub_s s)
      | Dba.LValue.Restrict _ ->
        failwith "unrelState.ml: restrict case not handled3"
      | Dba.LValue.Store (size, Machine.BigEndian, expr) ->
        let v = Val.singleton
                  (`Undef (computesize_dbalhs lhs)) in
        let op, _assumes, _rcd =
          store_big_end addr size v expr s assumes glbs rcd elements equalities in
        op
      | Dba.LValue.Store (size, Machine.LittleEndian, expr) ->
        let v = Val.singleton
                  (`Undef (computesize_dbalhs lhs)) in
        let op, _assumes, _rcd =
          store_little_end addr size v expr s assumes glbs rcd elements equalities in
        op
    in
    [addr_suiv, m, flgs, equalities]


  let resolve_print args s flgs equalities addr_suiv assumes glbs elements =
    Logger.debug "%s" (string_of_args args s assumes glbs elements equalities);
    [addr_suiv, s, flgs, equalities]


  let remove_memory_overlaps equalities lhs1 m assumes glbs elements =
    let open Dba in
    match lhs1 with
    | LValue.Store (sz1, _en1, e1) ->
      let equals = Eq.copy_equalities equalities in
      let lhs_list = Eq.get_elements equalities in
      let zero = Region_bitvector.zeros 32 in
      let sz1 = Region_bitvector.create_constant (Bigint.big_int_of_int sz1) 32 in
      let v_sz1 = Val.of_bounds (zero, sz1) in
      let v1, _ = eval_expr e1 m assumes glbs elements equals in
      let v1 = Val.add v1 v_sz1 in
      let remove_memory_overlap acc lhs2 =
        match lhs2 with
        | LValue.Store (sz2, _en2, e2) ->
          let sz2 = Region_bitvector.create_constant (Bigint.big_int_of_int sz2) 32 in
          let v_sz2 = Val.of_bounds (zero, sz2) in
          let v2, _ = eval_expr e2 m assumes glbs elements equals in
          let v2 = Val.add v2 v_sz2 in
          if Val.is_empty (Val.meet v1 v2) then acc
          else Eq.remove acc lhs2
        | _ -> acc
      in
      List.fold_left remove_memory_overlap equalities lhs_list
    | _ -> equalities


  let update_equalities lhs e equalities v m assumes glbs elements =
    match Dba.LValue.of_expr e with
    | lhs_expr ->
      if Eq.is_same_class equalities lhs lhs_expr
      then equalities
      else (
        let equalities = Eq.remove_syntax_overlaps equalities lhs in
        let equalities = remove_memory_overlaps equalities lhs m assumes glbs elements in
        Display.display (Display.RemoveEqualities (lhs, Eq.to_string equalities));
        Eq.union equalities lhs lhs_expr v
      )
    | exception Failure _ ->
      let equalities = Eq.remove_syntax_overlaps equalities lhs in
      Display.display (Display.RemoveEqualities (lhs, Eq.to_string equalities));
      Eq.union equalities lhs lhs v


  let post abs_vals addrStack instr cache assumes glbs djumps_map unrolled_loops elements =
    let addr, cstack, loop = addrStack in
    let rcd, rcd_conds = cache in
    let m, flags, equalities = abs_vals in
    let equalities = Eq.copy_equalities equalities in
    let open Dba in
    match instr with
    | Dba.Instr.Stop (Some KO) ->
      Errors.assert_failure addr instr

    | Dba.Instr.Stop (Some (Undefined s)) ->
      raise (Errors.Stop_Unsupported s)

    | Dba.Instr.Stop (Some (Unsupported s)) ->
      raise (Errors.Stop_Unsupported s)

    | Dba.Instr.Stop _tag ->
      [], cache, assumes, djumps_map

    | Dba.Instr.Assign (lhs, expr, id_suiv) ->
      let m', assumes', rcd, v = assign addrStack lhs expr m assumes glbs rcd elements equalities in
      let flags = update_flags lhs expr flags in
      let t0 = Unix.gettimeofday () in
      let equalities = update_equalities lhs expr equalities v m assumes glbs elements in
      Ai_options.time_equalities :=
        Unix.gettimeofday () -. t0 +. !Ai_options.time_equalities;
      Ai_options.nb_equalities_names :=
        max (Eq.get_nb_names equalities) !Ai_options.nb_equalities_names;
      Ai_options.nb_equalities_classes :=
        max (Eq.get_nb_classes equalities) !Ai_options.nb_equalities_classes;
      let cache = rcd, rcd_conds in
      [ (Dba_types.Caddress.reid addr id_suiv, cstack, loop), m', flags, equalities],
      cache, assumes', djumps_map

    | Dba.Instr.Malloc (lhs, expr, id_suiv) ->
      let v, assumes =  eval_expr expr m assumes glbs elements equalities in
      incr Dba_types.malloc_id;
      let size =
        let sz = Val.elements v in
        match sz with
        | (`Value (`Constant, size)) :: [] -> Bitvector.value_of size
        | _ -> failwith "unrelstate.ml: malloc size"
      in
      let region = `Malloc ((!Dba_types.malloc_id, addr), size) in
      let bv = Bitvector.zeros (Kernel_options.Machine.word_size ()) in
      Simulate.mallocs := Malloc_status.add region Freeable !Simulate.mallocs;
      let v = Dba.Expr.constant ~region bv in
      let op,assumes,rcd,_ = assign addrStack lhs v m assumes glbs rcd elements equalities in
      let cache = (rcd, rcd_conds) in
      [ (Dba_types.Caddress.reid addr id_suiv, cstack, loop), op, flags, equalities], cache, assumes, djumps_map

    | Dba.Instr.Free (expr, id_suiv) ->
      let addr = Dba_types.Caddress.reid addr id_suiv in
      let a = check_exec_permission addr m assumes glbs elements equalities in
      let m = free expr m assumes glbs elements equalities in
      let addrStack = (a, cstack, loop) in
      [addrStack, m, flags, equalities], cache, assumes, djumps_map

    | Dba.Instr.SJump (JInner id_suiv, _call_return_tag) ->
      let a = Dba_types.Caddress.reid addr id_suiv in
      let addrStack = a, cstack, loop in
      [addrStack, m, flags, equalities], cache, assumes, djumps_map

    | Dba.Instr.SJump (JOuter addr_suiv, Some Dba.Call addr_ret) ->
      let calling_callee_addr = addr_ret, addr_suiv in
      if is_visited calling_callee_addr cstack 20
      then
        raise (RecursiveCall addr_suiv)
      else (
        Display.display (Display.Call(addr_suiv, addr_ret));
        let cstack = calling_callee_addr :: cstack in
        let addrStack = addr_suiv, cstack, loop in
        [addrStack, m, flags, equalities], cache, assumes, djumps_map
      )

    | Dba.Instr.SJump (JOuter addr_suiv, Some Return) ->
      Display.increase_function_count ();
      let cstack =
        match cstack with
        | (old_ret, _) :: cstack' ->
          if Dba_types.Caddress.equal old_ret addr_suiv then cstack'
          else cstack
        | [] -> []
      in
      let addrStack = addr_suiv, cstack, loop in
      [addrStack, m, flags, equalities], cache, assumes, djumps_map

    | Dba.Instr.SJump (JOuter addr_suiv, _call_return_tag) ->
      let addrStack = addr_suiv, cstack, loop in
      [addrStack, m, flags, equalities], cache, assumes, djumps_map
    (* *********************************************************** *)

    | Dba.Instr.DJump (expr, call_return_tag) ->
      let a, rcd, djumps_map =
        resolve_jump addrStack expr m flags equalities rcd assumes glbs djumps_map call_return_tag elements in
      let cache = rcd, rcd_conds in
      a, cache, assumes, djumps_map
    (* *********************************************************** *)

    | Dba.Instr.If (condition, JOuter addr_suiv1, id_suiv2) ->
      (* let loop, loop_a = loop in  *)
      let cond, rcd_conds = retrieve_comparison ~condition flags addr rcd_conds in
      let eq1 = Eq.copy_equalities equalities in
      let eq2 = Eq.copy_equalities equalities in
      let m1, assumes, rcd, eq1 = guard addrStack cond m assumes glbs rcd elements eq1 in
      let n_cond = Dba.Expr.lognot cond in
      let m2, assumes, rcd, eq2 = guard addrStack n_cond m assumes glbs rcd elements eq2 in
      let a1 = addr_suiv1 in
      let a2 = Dba_types.Caddress.reid addr id_suiv2 in
      let loops =
        try Dba_types.Caddress.Map.find addr unrolled_loops
        with Not_found -> Dba_types.Caddress.Set.empty
      in
      let loop1, loop2 = (* 0, 0 *)
        if Dba_types.Caddress.Set.cardinal loops = 1
        then
          if Dba_types.Caddress.Set.mem a1 loops
          then min (loop + 1) 50, 0
          else 0, min (loop + 1) 50
        else loop, loop
      in
      let addr_suiv1 = (a1, cstack, loop1) in
      let addr = (a2, cstack, loop2) in
      let l = resolve_if cond m flags (m1, eq1) (m2, eq2) addr_suiv1 addr assumes glbs elements equalities in
      let cache = (rcd, rcd_conds) in
      l, cache, assumes, djumps_map

    | Dba.Instr.If (condition, JInner id_suiv1, id_suiv2) ->
      (* let loop, loop_a = loop in  *)

      let cond, rcd_conds = retrieve_comparison ~condition flags addr rcd_conds in
      let eq1 = Eq.copy_equalities equalities in
      let eq2 = Eq.copy_equalities equalities in
      let m1, assumes, rcd, eq1 =
        guard addrStack cond m assumes glbs rcd elements eq1 in
      let ncond = Dba.Expr.lognot cond in
      let m2, assumes, rcd, eq2 =
        guard addrStack ncond m assumes glbs rcd elements eq2 in
      let loops =
        try Dba_types.Caddress.Map.find addr unrolled_loops
        with Not_found ->  Dba_types.Caddress.Set.empty
      in
      let a1 = Dba_types.Caddress.reid addr id_suiv1 in
      let a2 = Dba_types.Caddress.reid addr id_suiv2 in
      let loop1, loop2 =  (* 0, 0 *)
        if Dba_types.Caddress.Set.cardinal loops = 1
        then
          if Dba_types.Caddress.Set.mem a1 loops
          then min (loop + 1) 50, 0
          else 0, min (loop + 1) 50
        else loop, loop
      in
      let a1 = (a1, cstack, loop1) in
      let a2 = (a2, cstack, loop2) in
      let l = resolve_if cond m flags (m1, eq1) (m2, eq2) a1 a2 assumes glbs elements equalities in
      let cache = (rcd, rcd_conds) in
      l, cache, assumes, djumps_map
    | Dba.Instr.Assert (condition, id_suiv) ->
      let cond, rcd_conds = retrieve_comparison ~condition flags addr rcd_conds in
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let l = resolve_assert cond m flags equalities a addrStack instr assumes glbs rcd elements in
      let cache = (rcd, rcd_conds) in
      l, cache, assumes, djumps_map
    | Dba.Instr.Assume (condition, id_suiv) ->
      let cond, rcd_conds = retrieve_comparison ~condition flags addr rcd_conds in
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let l = resolve_assume addrStack cond m flags equalities a assumes glbs rcd elements in
      let cache = (rcd, rcd_conds) in
      l, cache, assumes, djumps_map
    | Dba.Instr.NondetAssume (lhslst, condition, id_suiv) ->
      let cnd, rcd_conds = retrieve_comparison ~condition flags addr rcd_conds in
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let op, assumes, rcd =
        resolve_nondet_assume addrStack lhslst cnd m flags equalities a assumes glbs rcd elements in
      let cache = (rcd, rcd_conds) in
      op, cache, assumes, djumps_map
    | Dba.Instr.Nondet (lhs, region, id_suiv) ->
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let op = resolve_nondet addrStack lhs region m flags equalities a assumes glbs rcd elements in
      op, cache, assumes, djumps_map
    | Dba.Instr.Undef (lhs, id_suiv) ->
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let l = resolve_undef addrStack lhs m flags equalities a assumes glbs rcd elements in
      l, cache, assumes, djumps_map
    | Dba.Instr.Print (args, id_suiv) ->
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let l = resolve_print args m flags equalities a assumes glbs elements in
      l, cache, assumes, djumps_map


  let get_initial_state inits =
    let addr = Dba_types.Caddress.block_start @@ Virtual_address.create 0 in
    let cstack = [] in
    let loop = 0 in
    let addrStack = (addr, cstack, loop) in
    let rcd = Dba_types.AddressStack.Map.empty in
    let equalities = Eq.create () in
    let conds = [] in
    let glbs = Dba_types.Caddress.Set.empty in
    let djmps = Dba_types.AddressStack.Map.empty in
    let flags = High_level_predicate.empty in
    let rcd_conds = Dba_types.Caddress.Map.empty in
    let unrolled_loops = Dba_types.Caddress.Map.empty in
    let cache = rcd, rcd_conds in
    let init_state m instr =
      let abs_vals = m, flags, equalities in
      post abs_vals addrStack instr cache conds glbs djmps unrolled_loops []
    in
    let f m instr =
      match init_state m instr with
      | (_, res, _, _) :: _, _, _, _ -> res
      | [], _, _, _ -> assert false
    in
    let m_top = Some Env.empty in
    List.fold_left f m_top inits


  let _projection _s _p = failwith "Not implemented"


  let env_to_smt_list m varIndexes inputs =
    let env, inputs =
      match m with
      | None -> [], inputs
      | Some m ->
        Env.fold (
          fun key sub_m (acc, inputs) ->
            match key with
            | Static_types.Var (n, size) ->
              MemMap.fold (fun _bv (_, _, v) (acc, inputs) ->
                  let id =
                    try String.Map.find n varIndexes
                    with Not_found -> 0
                  in
                  let name = n^(string_of_int id) in
                  let var = Formula.bv_var name size in
                  let var1 = Formula.BvVar var in
                  let var2 = Formula.mk_bv_var var in
                  if Formula.VarSet.mem var1 inputs then
                    (Val.to_smt v var2) @ acc, inputs
                  else
                    let inputs = Formula.VarSet.add var1 inputs in
                    (Val.to_smt v var2) @ acc, inputs) sub_m (acc, inputs)
            | Static_types.Array region ->
              MemMap.fold (fun bv (_, en, v) (acc, inputs) ->
                  let i = MemInterval.base_of bv in
                  let size = MemInterval.size_of bv in
                  let expr = Dba.Expr.constant ~region (Bitvector.create i 32) in
                  let var, inputs =
                    Normalize_instructions.load_to_smt expr size en inputs varIndexes in
                  (Val.to_smt v var) @ acc, inputs) sub_m (acc, inputs)
        ) m ([], inputs)
    in
    match !s_init with
    | None -> env, inputs
    | Some s ->
      Env.fold (fun key sub_m (acc, inputs) ->
          match key with
          | Static_types.Array `Constant ->
            MemMap.fold (fun bv (_, en, v) (acc, inputs) ->
                let i = MemInterval.base_of bv in
                let size = MemInterval.size_of bv in
                let c =
                  match m with
                    None -> true
                  | Some m ->
                    try let _ = MemMap.find bv (Env.find (Static_types.Array `Constant) m) in
                      false
                    with Not_found -> true
                in
                if c then
                  let expr = Dba.Expr.constant (Bitvector.create i 32) in
                  let var, inputs = Normalize_instructions.load_to_smt expr size en inputs varIndexes in
                  Val.to_smt v var @ acc, inputs
                else acc, inputs) sub_m (acc, inputs)
          | Static_types.Array `Stack
          | Static_types.Array `Malloc ((_, _), _)
          | Static_types.Var (_, _) -> acc, inputs
        ) s (env, inputs)


  let refine_state m smt_env =
    match m with
    | None -> m
    | Some m ->
      let new_m = Env.fold (
          fun key sub_m acc ->
            match key with
            | Static_types.Var (n, _size) ->
              if n = "eax" || n = "ecx" || n = "ebx" || n = "edx" then
                let name = n ^ "0" in
                let new_sub_m =
                  MemMap.fold
                    (fun bv (a, en, v) acc ->
                       let new_v = Val.smt_refine v smt_env name in
                       Logger.debug "Refining %s: %a -> %a" name Val.pp v Val.pp new_v;
                       MemMap.add bv (a, en, new_v) acc)
                    sub_m sub_m
                in Env.add key new_sub_m acc
              else acc
            | Static_types.Array `Constant -> acc

            | Static_types.Array `Stack ->
              let new_sub_m =
                MemMap.fold
                  (fun bv (a, en, v) acc ->
                     match MemInterval.size_of bv with
                     | 4 ->
                       let i = MemInterval.base_of bv in
                       let addr =
                         Formula_pp.print_bv_term
                           (Formula.mk_bv_cst (Bitvector.create i 32)) in
                       let name = Format.asprintf "(load32_at memory0 %s)" addr in
                       let v' = Val.smt_refine v smt_env name in
                       Logger.debug  "Refining %s: %a -> %a" name Val.pp v Val.pp v';
                       MemMap.add bv (a, en, v') acc
                     | _ -> acc)
                  sub_m sub_m
              in Env.add key new_sub_m acc

            | Static_types.Array ((`Malloc ((_id, _), _))) -> acc
        ) m m in
      Some new_m

end
