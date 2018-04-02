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
open Region_bitvector
open High_level_predicate

module Malloc_status = Dba_types.Region.Map

(* FIXME: This is also in Simulate *)
let mk_sup from size =
  let open Bigint in
  let big_size = big_int_of_int size in
  sub_big_int (add_big_int from big_size) unit_big_int


module Make (Val1: Ai_sigs.Domain) (Val2: Ai_sigs.Domain) =
struct
  module SubEnv = BigInt.Map
  module UF = Union_find.Make (Val1)
  type env = (Val1.t * Val2.t) SubEnv.t Static_types.Env.t
  type t =  env option
  type equalities = UF.t
  type thresholds = int array * int array * int array * int array
  type elementsRecord = Region_bitvector.t list Dba_types.AddressStack.Map.t
  type naturalPredicatesRecord = (Dba.cond * Dba.cond) Dba_types.Caddress.Map.t


  let top = Some Static_types.Env.empty, High_level_predicate.empty, UF.create ()

  let bottom = None, High_level_predicate.bottom, UF.bottom

  let is_empty s = Utils.is_none s

  let s_init : env ref = ref Static_types.Env.empty
  let regs_in_expr_to_string _ _ _ = assert false
  let _set_s_init s =
    match s with
    | None -> s_init := Static_types.Env.empty
    | Some s -> s_init := s

  let pp ppf env =
    let open Format in
    match env  with
    | None -> fprintf ppf "{}"
    | Some _env ->
      fprintf ppf "@[<v 0>{";
      fprintf ppf "@ }@]"

  let pp_equalities _ppf _equalities = ()

  let to_string (s, equalities) =
    match s with
    | None -> "{}"
    | Some s ->
      let msg =
        Static_types.Env.fold (
          fun key sub_m acc ->
            match key with
            | Static_types.Var (s, _) ->
              let f _ (v1, v2) acc =
                let sv1 = (Val1.to_string v1) in
                let sv2 = (Val2.to_string v2) in
                (Format.asprintf "%s=(%s, %s) \n" s  sv1 sv2) ^ acc
              in
              SubEnv.fold f sub_m acc
            | Static_types.Array `Constant ->
              let f i (v1, v2) acc =
                let id = Bitvector.create i (Machine.Word_size.get ()) in
                let id = Bitvector.to_hexstring id in
                let sv1 = (Val1.to_string v1) in
                let sv2 = (Val2.to_string v2) in
                (Format.asprintf "Cst[%s]=(%s, %s)\n" id sv1 sv2) ^ acc
              in
              SubEnv.fold f sub_m acc
            | Static_types.Array `Stack ->
              let f i (v1, v2) acc =
                let id = Bitvector.create i (Machine.Word_size.get ()) in
                let id = Bitvector.to_hexstring id in
                let s1 = (Val1.to_string v1) in
                let s2 = (Val2.to_string v2) in
                (Format.asprintf "Stack[%s]=(%s, %s)\n" id s1 s2) ^ acc
              in
              SubEnv.fold f sub_m acc
            | Static_types.Array (`Malloc ((id, _), _)) ->
              let f i (v1, v2) acc =
                let o = Bitvector.create i (Machine.Word_size.get ()) in
                let o = Bitvector.to_hexstring o in
                let s1 = Val1.to_string v1 in
                let s2 = Val2.to_string v2 in
                Format.asprintf "Malloc%d[%s]=(%s, %s)\n" id o s1 s2
                ^ acc
              in
              SubEnv.fold f sub_m acc
        ) s ""
      in msg ^ (UF.to_string equalities)


  let elements v_exp1 v_exp2 =
    let aux_of_list l =
      let f acc i = Kset.K_set.add i acc in
      List.fold_left f Kset.K_set.empty l
    in
    let idx1 = try Some (Val1.elements v_exp1) with _ -> None in
    let idx2 = try Some (Val2.elements v_exp2) with _ -> None in
    begin match idx1, idx2 with
        None, None -> raise Kset.Elements_of_top
      | None, Some s -> s
      | Some s, None -> s
      | Some s1, Some s2 ->
        let u = Kset.K_set.union (aux_of_list s1) (aux_of_list s2) in
        Kset.K_set.elements u
    end


  let read key subkey s =
    try SubEnv.find subkey (Static_types.Env.find key s)
    with Not_found -> (Val1.universe, Val2.universe)

  let leq s1 s2 =
    match (s2, s1) with
      (_, None) -> true
    | (None, _) -> false
    | (Some s1, Some s2) ->
      (* for each constraint in s1 there
         should be a stricter constraint in s2 *)
      try
        let has_constraint key subkey (v11, v12) =
          let (v21, v22) = read key subkey s2 in
          if not (Val1.contains v11 v21) ||
             not (Val2.contains v12 v22) then raise Exit
        in
        let f key v = SubEnv.iter (has_constraint key) v in
        Static_types.Env.iter f s1;
        true
      with Exit -> false



  let join (s1, flgs1, equalities1) (s2, flgs2, equalities2) =
    match (s1, s2) with
      (None, s) -> (s, flgs2, equalities2)
    | (s, None) -> (s, flgs1, equalities1)
    | (Some s1, Some s2) ->
      let res = ref Static_types.Env.empty in
      let join_info key subkey (v11, v12) =
        let (v21, v22) = read key subkey s2 in
        let v = (Val1.join v11 v21, Val2.join v12 v22) in
        res := match key with
          | Static_types.Var _ ->
            let sub_s = SubEnv.add Bigint.zero_big_int v SubEnv.empty in
            Static_types.Env.add key sub_s !res
          | Static_types.Array r ->
            let sub_s =
              try Static_types.Env.find (Static_types.Array r) !res
              with Not_found -> SubEnv.empty
            in
            let sub_s = SubEnv.add subkey v sub_s in
            (Static_types.Env.add (Static_types.Array r) sub_s !res)
      in
      Static_types.Env.iter (fun key v -> SubEnv.iter (join_info key) v) s1;
      let s = Some !res in
      let flgs = High_level_predicate.join flgs1 flgs2 in
      let equalities = UF.create () in
      (s, flgs, equalities)



  let widen (s1, flgs1, equalities1) (s2, flgs2, equalities2) thresholds =
    match (s1, s2) with
      (None, s) -> (s, flgs2, equalities2)
    | (s, None) -> (s, flgs1, equalities1)
    | (Some s1, Some s2) ->
      let res = ref Static_types.Env.empty in
      let widen_info key subkey (v11, v12) =
        let (v21, v22) = read key subkey s2 in
        let v = (Val1.widen v11 v21 thresholds,
                 Val2.widen v12 v22 thresholds) in
        res := match key with
          | Static_types.Var _ ->
            let sub_s = SubEnv.add Bigint.zero_big_int v SubEnv.empty in
            Static_types.Env.add key sub_s !res
          | Static_types.Array r ->
            let sub_s =
              try Static_types.Env.find (Static_types.Array r) !res
              with Not_found -> SubEnv.empty
            in
            let sub_s = SubEnv.add subkey v sub_s in
            (Static_types.Env.add (Static_types.Array r) sub_s !res)
      in
      Static_types.Env.iter (fun key v -> SubEnv.iter (widen_info key) v) s1;
      let s =  Some !res in
      let flgs = High_level_predicate.join flgs1 flgs2 in
      let equalities = UF.create () in
      (s, flgs, equalities)



  let meet s1 s2 =
    match s1, s2 with
      (None, _) | (_, None) -> None
    | (Some s1, Some s2) ->
      let res = ref Static_types.Env.empty in
      let meet_info key subkey (v11, v12) =
        let v21, v22 = read key subkey s2 in
        let v = Val1.meet v11 v21, Val2.meet v12 v22 in
        res := match key with
          | Static_types.Var _  ->
            let sub_s = SubEnv.add Bigint.zero_big_int v SubEnv.empty in
            Static_types.Env.add key sub_s !res
          | Static_types.Array r ->
            let sub_s =
              try Static_types.Env.find (Static_types.Array r) !res
              with Not_found -> SubEnv.empty
            in
            let sub_s = SubEnv.add subkey v sub_s in
            (Static_types.Env.add (Static_types.Array r) sub_s !res)
      in
      Static_types.Env.iter (fun key v -> SubEnv.iter (meet_info key) v) s1;
      Some !res

  let _add_var x subx s =
    match s with
    | None -> None
    | Some s ->
      match x with
      | Static_types.Var _ ->
        let sub_m =
          SubEnv.add Bigint.zero_big_int (Val1.universe, Val2.universe) SubEnv.empty in
        Some (Static_types.Env.add x sub_m s)
      | Static_types.Array r ->
        let sub_m =
          try Static_types.Env.find (Static_types.Array r) s with Not_found -> SubEnv.empty in
        let sub_m = SubEnv.add subx (Val1.universe, Val2.universe) sub_m in
        Some (Static_types.Env.add (Static_types.Array r) sub_m s)


  let rec eval_expr expr s assumes global_regions : (Val1.t * Val2.t) * (Smt_bitvectors.smtBvExprAlt list)=
    match expr with
    | Dba.ExprVar (st,size, _) -> (
        let v =
          try load (Static_types.Var (st, size)) Bigint.zero_big_int s assumes global_regions, assumes
          with Not_found ->
          try load (Static_types.Var (st, size)) Bigint.zero_big_int !s_init assumes global_regions, assumes
          with Not_found -> raise (Errors.Uninitialized_variable st)
        in v
      )

    | Dba.ExprLoad (size, endianness, e) ->
      let append_value (v1, v2) (w1, w2) =
        match endianness with
        | Dba.BigEndian    -> Val1.concat v1 w1, Val2.concat v2 w2
        | Dba.LittleEndian -> Val1.concat w1 v1, Val2.concat w2 v2
      in
      let join_couple (v1, v2) (w1, w2) = Val1.join v1 w1, Val2.join v2 w2 in
      let (v_exp1, v_exp2), assumes = eval_expr e s assumes global_regions in
      let add_subenv elem s =
        let sub_s =
          let value =
            `Value (`Constant, Region_bitvector.bitvector_of elem) in
          SubEnv.singleton Bigint.zero_big_int
            (Val1.singleton value, Val2.singleton value)
        in Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_s s
      in
      let values =
        match elements v_exp1 v_exp2 with
        | [] -> Val1.universe, Val2.universe
        | indexes ->
          List.fold_right (fun elem acc ->
              let region = (Region_bitvector.region_of elem) in
              (* FIXME: check malloc size here *)
              let i = Region_bitvector.value_of elem in
              let s = add_subenv elem s in
              let find_at i s =
                try SubEnv.find i (Static_types.Env.find (Static_types.Array region) s)
                with Not_found ->
                try SubEnv.find i (Static_types.Env.find (Static_types.Array region) !s_init)
                with
                | Not_found ->
                  let v = Region_bitvector.default_get_byte_region_at i in
                  Val1.singleton v, Val2.singleton v
              in
              let open Bigint in
              let limit = mk_sup i size in
              let rec aux index acc elem =
                if gt_big_int index limit then acc
                else
                  let elem' =
                    let len = Region_bitvector.size_of elem in
                    add elem (`Value (`Constant, Bitvector.succ (Bitvector.zeros len))) in
                  let s = add_subenv elem' s in
                  aux
                    (succ_big_int index)
                    (append_value acc (find_at index s))
                    elem'
              in
              join_couple (aux (succ_big_int i) (find_at i s) elem) acc
            ) indexes (Val1.empty, Val2.empty)
      in values, assumes


    | Dba.ExprCst (r, bv) -> (Val1.singleton (`Value (r, bv)), Val2.singleton (`Value (r, bv))), assumes
    | Dba.ExprUnary (uop, expr) ->
      let (val1, val2), assumes = eval_expr expr s assumes global_regions in
      begin match uop with
          Dba.UMinus -> (Val1.neg val1, Val2.neg val2), assumes
        | Dba.Not -> (Val1.lognot val1, Val2.lognot val2), assumes
      end
    | Dba.ExprBinary (bop, expr1, expr2) ->
      let (val11, val12), assumes = (eval_expr expr1 s assumes global_regions) in
      let (val21, val22), assumes = (eval_expr expr2 s assumes global_regions) in
      begin match bop with
        | Dba.Plus -> Val1.add val11 val21, Val2.add val12 val22
        | Dba.Minus -> Val1.sub val11 val21, Val2.sub val12 val22
        | Dba.MultU -> Val1.umul val11 val21, Val2.umul val12 val22
        | Dba.MultS -> Val1.smul val11 val21, Val2.smul val12 val22
        (* | Dba.Power -> Val1.power val11 val21, Val2.power val12 val22 *)
        | Dba.DivU -> Val1.udiv val11 val21, Val2.udiv val12 val22
        | Dba.DivS -> Val1.sdiv val11 val21, Val2.sdiv val12 val22
        | Dba.ModU -> Val1.umod val11 val21, Val2.umod val12 val22
        | Dba.ModS -> Val1.smod val11 val21, Val2.smod val12 val22
        | Dba.Or -> Val1.logor val11 val21, Val2.logor val12 val22
        | Dba.And -> Val1.logand val11 val21, Val2.logand val12 val22
        | Dba.Xor -> Val1.logxor val11 val21, Val2.logxor val12 val22
        | Dba.Concat -> Val1.concat val11 val21, Val2.concat val12 val22
        | Dba.LShift -> Val1.lshift val11 val21, Val2.lshift val12 val22
        | Dba.RShiftU -> Val1.rshiftU val11 val21, Val2.rshiftU val12 val22
        | Dba.RShiftS	-> Val1.rshiftS val11 val21, Val2.rshiftS val12 val22
        | Dba.LeftRotate -> Val1.rotate_left val11 val21, Val2.rotate_left val12 val22
        | Dba.RightRotate -> Val1.rotate_right val11 val21, Val2.rotate_right val12 val22
        | Dba.Eq -> Val1.eq val11 val21, Val2.eq val12 val22
        (* TODO: check if two bit vectors of different sizes can be equal*)
        | Dba.Diff -> Val1.diff val11 val21, Val2.diff val12 val22
        | Dba.LeqU -> Val1.leqU val11 val21, Val2.leqU val12 val22
        | Dba.LtU -> Val1.ltU val11 val21, Val2.ltU val12 val22
        | Dba.GeqU -> Val1.geqU val11 val21, Val2.geqU val12 val22
        | Dba.GtU -> Val1.gtU val11 val21, Val2.gtU val12 val22
        | Dba.LeqS -> Val1.leqS val11 val21, Val2.leqS val12 val22
        | Dba.LtS -> Val1.ltS val11 val21, Val2.ltS val12 val22
        | Dba.GeqS -> Val1.geqS val11 val21, Val2.geqS val12 val22
        | Dba.GtS -> Val1.gtS val11 val21, Val2.gtS val12 val22
      end, assumes
    | Dba.ExprRestrict (expr, offset1, offset2) ->
      let (val1, val2), assumes = (eval_expr expr s assumes global_regions) in
      (Val1.restrict val1 offset1 offset2, Val2.restrict val2 offset1 offset2), assumes
    | Dba.ExprExtU (expr, size) ->
      let (val1, val2), assumes = (eval_expr expr s assumes global_regions) in
      (Val1.extension val1 size, Val2.extension val2 size), assumes
    | Dba.ExprExtS (expr, size) ->
      let (val1, val2), assumes = (eval_expr expr s assumes global_regions) in
      (Val1.signed_extension val1 size, Val2.signed_extension val2 size), assumes
    | Dba.ExprIte (cond, expr1, expr2) ->
      let cond, assumes = (eval_cond cond s assumes global_regions) in
      begin match cond with
        | Ternary.True  -> eval_expr expr1 s assumes global_regions 
        | Ternary.False -> eval_expr expr2 s assumes global_regions
        | Ternary.Unknown ->
          let (val11, val12), assumes = (eval_expr expr1 s assumes global_regions) in
          let (val21, val22), assumes = (eval_expr expr2 s assumes global_regions) in
          (Val1.join val11 val21, Val2.join val12 val22), assumes
      end
    | Dba.ExprAlternative (e_list, _) -> (* handle alternativeTag*)
      let eval e = eval_expr e s assumes global_regions in
      let eq ((v1, v2), _) ((w1, w2), _) = Val1.equal v1 w1 && Val2.equal v2 w2 in
      Dba_utils.eval_alternatives eval eq e_list

  and eval_cond c s  assumes global_regions =
    match c with
    | Dba.CondReif expr -> (
        try
          let (val1, val2), assumes = eval_expr expr s assumes global_regions in
          let c1 = Val1.is_true val1 assumes global_regions in
          let c2 = Val2.is_true val2 assumes global_regions in
          begin match c1, c2 with
            | Ternary.True, Ternary.True
            | Ternary.False, Ternary.False -> c1, assumes
            | Ternary.True, Ternary.False
            | Ternary.False, Ternary.True ->
              failwith "reducedProduct.ml: condition conflict"
            | c , Ternary.Unknown
            | Ternary.Unknown, c -> c, assumes
          end
        with Smt_bitvectors.Assume_condition smb ->
          let assumes = smb :: assumes in
          eval_cond c s assumes global_regions
      )
    | Dba.CondNot b ->
      let b, assumes = (eval_cond b s assumes global_regions) in
      Ternary.lognot b, assumes

    | Dba.CondAnd (b1, b2) ->
      let b1, assumes = (eval_cond b1 s assumes global_regions) in
      let b2, assumes = (eval_cond b2 s assumes global_regions) in
      Ternary.logand b1 b2, assumes

    | Dba.CondOr (b1, b2) ->
      let b1, assumes = (eval_cond b1 s assumes global_regions) in
      let b2, assumes = (eval_cond b2 s assumes global_regions) in
      Ternary.logor b1 b2, assumes

    | Dba.True -> Ternary.True, assumes
    | Dba.False -> Ternary.False, assumes

  (* Checking read of memory permissions here *)
  and load x i m assumes global_regions =
    match x with
    | Static_types.Var _ ->
      SubEnv.find Bigint.zero_big_int (Static_types.Env.find x m)
    | Static_types.Array r ->
      let c =
        (try Dba_types.Rights.find_read_right r !Concrete_eval.permis
         with Not_found -> Dba.True) in
      let b, _assumes = eval_cond c m assumes global_regions in
      match b with
      | Ternary.True ->
        SubEnv.find i (Static_types.Env.find (Static_types.Array r) m)
      | Ternary.False -> raise Errors.Read_permission_denied
      | Ternary.Unknown -> failwith "read permission in (True or false) case"

  (* Checking write to memory permissions here *)
  and store x i bv m assumes global_regions =
    match x with
    | Static_types.Var _ ->
      let sub_m = (SubEnv.add Bigint.zero_big_int bv SubEnv.empty) in
      (Static_types.Env.add x sub_m m)
    | Static_types.Array r ->
      let c = (try Dba_types.Rights.find_write_right r !Concrete_eval.permis
               with Not_found -> Dba.True) in
      let b, _ = (eval_cond c m assumes global_regions) in
      match b with
      | Ternary.True ->
        let submap =
          (try Static_types.Env.find (Static_types.Array r) m
           with Not_found -> SubEnv.empty)
          |> SubEnv.add i bv 
        in Static_types.Env.add (Static_types.Array r) submap m
      | Ternary.False -> raise Errors.Write_permission_denied
      |	Ternary.Unknown -> failwith "write permission in True or False case"


  let store_little_endian size v expr s assumes global_regions =
    let (v_exp1, v_exp2), assumes = eval_expr expr s assumes global_regions in
    let indexes = elements v_exp1 v_exp2 in
    if indexes = [] then failwith "reducedProduct.ml: elements of top"
    else
      List.fold_right (fun elem _acc ->
          let open Bigint in
          let region = Region_bitvector.region_of elem in
          let i = Region_bitvector.value_of elem in
          let check_region_size =
            match region with
            | `Constant | `Stack -> true
            | `Malloc (_, malloc_size) ->
              not (gt_big_int (add_big_int i (big_int_of_int (size - 1)))
                 malloc_size)
          in
          if (not check_region_size) || size < 1 then
            raise (Errors.Bad_bound "store, Little_endian case")
          else
            let m = ref s in
            let j= ref i in
            let sup = mk_sup i size in
            let v_expr = ref elem in
            while le_big_int !j sup do
              let sub_m =
                SubEnv.add Bigint.zero_big_int (Val1.singleton !v_expr, Val2.singleton !v_expr) SubEnv.empty
              in
              m := Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m !m;
              let of1 = 8 * int_of_big_int (sub_big_int !j i) in
              let of2 = of1 + 7 in
              m := store (Static_types.Array region) !j
                  (Val1.restrict (fst v) of1 of2, Val2.restrict (snd v) of1 of2)
                  !m assumes global_regions;
              v_expr := Region_bitvector.succ !v_expr;
              j := add_big_int !j unit_big_int
            done;
            !m  (* TODO : do the union with the existing elements at the same
                   location because the asignments can overlap *)
        ) indexes s


  let store_big_endian size v expr s assumes global_regions =
    let (v_exp1, v_exp2), assumes = eval_expr expr s assumes global_regions in
    let indexes = elements v_exp1 v_exp2 in
    if indexes = [] then failwith "reducedProduct.ml: elements of top"
    else
      List.fold_right (fun elem _acc ->
          let open Bigint in
          let region = Region_bitvector.region_of elem in
          let i = Region_bitvector.value_of elem in
          let check_region_size =
            match region with
            | `Constant | `Stack -> true
            | `Malloc (_, malloc_size) ->
              not (gt_big_int (add_big_int i (big_int_of_int (size - 1)))
                 malloc_size)

          in
          if (not check_region_size) || size < 1 then
            raise (Errors.Bad_bound "store, big_endian case")
          else
            let m = ref s in
            let j= ref i in
            let sup = mk_sup i size in
            let v_expr = ref elem in
            while le_big_int !j sup do
              let sub_m =
                SubEnv.singleton
                  Bigint.zero_big_int (Val1.singleton !v_expr,
                                       Val2.singleton !v_expr)
              in
              m := Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m !m;
              let of1 = 8 * ((size - 1) - int_of_big_int (sub_big_int !j i)) in
              let of2 = of1 + 7 in
              m := store (Static_types.Array region) !j
                  (Val1.restrict (fst v) of1 of2, Val2.restrict (snd v) of1 of2)
                  !m assumes global_regions;
              v_expr := Region_bitvector.succ !v_expr;
              j := add_big_int !j Bigint.unit_big_int
            done;
            !m  (* TODO : do the union with the existing elements at the same
                   location *)
        ) indexes s

  (* FIXME: refactor functions above *)
  let store = function
    | Dba.LittleEndian -> store_little_endian
    | Dba.BigEndian -> store_big_endian

  let assign lhs e (s: t) assumes global_regions : t * (Smt_bitvectors.smtBvExprAlt list) =
    match s with
      None -> None, assumes
    | Some s ->
      let v, assumes = eval_expr e s assumes global_regions in
      match lhs with
      | Dba.LhsVar (st, size, _) ->
        let sub_s = SubEnv.add (Bigint.zero_big_int) v SubEnv.empty in
        Some (Static_types.Env.add (Static_types.Var (st, size)) sub_s s), assumes
      | Dba.LhsVarRestrict (_st, _size, _of1, _of2) ->
        failwith "analyse.ml: restrict case not handled"

      | Dba.LhsStore (size, endianness, expr) ->
        Some (store endianness size v expr s assumes global_regions), assumes


  let rec guard cond s assumes global_regions =
    match s with
    | None -> None, assumes
    | Some m ->
      match cond with
      | Dba.CondReif expr ->
        (match expr with
         | Dba.ExprBinary (bop, exp1, exp2) ->
           (match bop with
              Dba.Eq | Dba.Diff | Dba.LeqU | Dba.LtU
            | Dba.GeqU | Dba.GtU	| Dba.LeqS	| Dba.LtS
            | Dba.GeqS | Dba.GtS ->
              let v_1, v_2 =
                let (val11, val12), assumes = (eval_expr exp1 m assumes global_regions) in
                let (val21, val22), _ = (eval_expr exp2 m assumes global_regions) in
                let g, d = Val1.guard bop val11 val21 in
                let g', d' = Val2.guard bop val12 val22 in
                (g, g'), (d, d')
              in
              let root_zero = SubEnv.singleton Bigint.zero_big_int in
              (match exp1, exp2 with
               | Dba.ExprVar (v1, size, _), Dba.ExprCst _ ->
                 let sub_m = root_zero v_1 in
                 Some (Static_types.Env.add (Static_types.Var (v1, size)) sub_m m), assumes
               | Dba.ExprCst _, Dba.ExprVar (v2, size, _) ->
                 let sub_m = root_zero v_2 in
                 Some (Static_types.Env.add (Static_types.Var (v2, size)) sub_m m), assumes
               | Dba.ExprVar (v1, _, _), Dba.ExprVar (v2, size, _) ->
                 let sub_m = root_zero v_1 in
                 let m = Static_types.Env.add (Static_types.Var (v1, size)) sub_m m in
                 let sub_m = root_zero v_2 in
                 Some (Static_types.Env.add (Static_types.Var (v2, size)) sub_m m), assumes
               | Dba.ExprVar (v, size, _),
                 Dba.ExprLoad (lsize, endianness, e) ->
                 let sub_m = root_zero v_1 in
                 let m = Static_types.Env.add (Static_types.Var (v, size)) sub_m m in
                 Some (store endianness lsize v_2 e m assumes global_regions),
                 assumes
               | Dba.ExprLoad (size, endianness, e),
                 Dba.ExprVar (v', size', _) ->
                 let sub_m = root_zero v_2 in
                 let m = Static_types.Env.add (Static_types.Var (v', size')) sub_m m in
                 Some (store endianness size v_1 e m assumes global_regions), assumes
               | Dba.ExprLoad (size1, endianness1, e1),
                 Dba.ExprLoad (size2, endianness2, e2) ->
                 let m = store endianness1 size1 v_1 e1 m assumes global_regions in
                 Some (store endianness2 size2 v_2 e2 m assumes global_regions), assumes
               | _, _ -> Some (m), assumes
              )
            | _ -> Some (m), assumes)
         | Dba.ExprUnary (uop, expr) ->
           ( match uop with
             | Dba.Not ->
               (match expr with
                | Dba.ExprBinary (bop, expr1, expr2) ->
                  (
                    match bop with
                    | Dba.Eq ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.Diff, expr1, expr2))) s assumes global_regions
                    | Dba.Diff ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.Eq, expr1, expr2))) s assumes global_regions
                    | Dba.LeqU ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.GtU, expr1, expr2))) s assumes global_regions
                    | Dba.LtU ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.GeqU, expr1, expr2))) s assumes global_regions
                    | Dba.GeqU ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.LtU, expr1, expr2))) s assumes global_regions
                    | Dba.GtU ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.LeqU, expr1, expr2))) s assumes global_regions
                    | Dba.LeqS ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.GtS, expr1, expr2))) s assumes global_regions
                    | Dba.LtS ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.GeqS, expr1, expr2))) s assumes global_regions
                    | Dba.GeqS ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.LtS, expr1, expr2))) s assumes global_regions
                    | Dba.GtS ->
                      guard (Dba.CondReif (Dba.ExprBinary (Dba.LeqS, expr1, expr2))) s assumes global_regions
                    | Dba.Or ->
                      guard (Dba.CondReif
                               (Dba.ExprBinary
                                  (Dba.And,
                                   Dba.ExprUnary (Dba.Not, expr1),
                                   Dba.ExprUnary (Dba.Not, expr2)))) s assumes global_regions
                    | Dba.And ->
                      guard (Dba.CondReif
                               (Dba.ExprBinary
                                  (Dba.Or,
                                   Dba.ExprUnary (Dba.Not, expr1),
                                   Dba.ExprUnary (Dba.Not, expr2)))) s assumes global_regions
                    | _ -> Some (m), assumes
                  )
                | _ -> Some (m), assumes )
             | _ -> Some (m), assumes)
         | _ -> Some (m), assumes)
      | Dba.CondNot b ->
        (
          match b with
          | Dba.CondReif expr ->
            guard (Dba.CondReif (Dba.ExprUnary (Dba.Not, expr))) s assumes global_regions
          | Dba.CondNot b -> guard b s assumes global_regions
          | Dba.CondAnd (_, _)
          | Dba.CondOr (_, _) -> Some (m), assumes
          | Dba.True -> Some (Static_types.Env.empty), assumes
          | Dba.False -> Some (m), assumes
        )

      | Dba.CondAnd (_, _)
      | Dba.CondOr (_, _)
      | Dba.True -> Some (m), assumes
      | Dba.False -> Some (Static_types.Env.empty), assumes



  let rec string_of_args args m assumes global_regions =
    (match args with
     | [] ->  ""
     | (Dba.Str s) :: tl ->
       (Scanf.unescaped s) ^ (string_of_args tl m assumes global_regions)
     | (Dba.Exp e) :: tl ->
       let v, assumes = eval_expr e m assumes global_regions in
       let temp = "(" ^ (Val1.to_string (fst v) ^ ", " ^ Val2.to_string (snd v)) ^ ")" in
       temp ^ (string_of_args tl m assumes global_regions))

  let check_exec_permission addr s assumes global_regions =
    match s with
      None -> failwith "unrelstate.ml: check exec permission with empyu state"
    | Some m ->
      let bv = addr.Dba.base in
      let s = Val1.singleton (`Value (`Constant, bv)), Val2.singleton (`Value (`Constant, bv)) in
      let sub_m = SubEnv.singleton Bigint.zero_big_int s  in
      let m = Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m m in
      let c =
        try Dba_types.Rights.find_exec_right `Constant !Concrete_eval.permis
        with Not_found ->  Dba.True
      in
      let b, _ = eval_cond c m assumes global_regions in
      match b with
      | Ternary.True -> addr
      | Ternary.False -> raise Errors.Exec_permission_denied
      | Ternary.Unknown -> failwith "exec permission in unknown case"


  let free expr s assumes global_regions =
    match s with
      None -> failwith "unrelstate.ml: free with empty state"
    | Some m ->
      let s, _assumes = eval_expr expr m assumes global_regions in
      let l = elements (fst s) (snd s) in
      List.iter (fun elem ->
          match elem with
          | `Value (`Malloc (id, malloc_size), bv) ->
            let st =
              try Malloc_status.find (`Malloc (id, malloc_size))
                    !Simulate.mallocs
              with Not_found -> failwith "Unbound free region"
            in begin
            match st with
            | Dba.Freeable when Bitvector.is_zero bv ->
              Simulate.mallocs := Malloc_status.add (`Malloc (id, malloc_size))
                  Dba.Freed !Simulate.mallocs
            | Dba.Freed -> raise Errors.Freed_variable_access
            | _ -> raise Errors.Invalid_free_address
            (* |	Restrict ((`Malloc (id, malloc_size), bv), _, _) as v ->  *)
            (*   raise (Unknown_value (Region_bitvector.string_of v)) *)
            end
          |	_ -> raise Errors.Invalid_free_region
        ) l;(* Perhaps we need to check empty set *)
      Some m


  let resolve_jump addrStack expr s flgs equalities recordMap assumes global_regions djumps_map =
    let _addr, cstack, loop = addrStack in
    match s with
    | None -> failwith "reduced_product.ml: resolve_jump with empty state"
    | Some m ->
      let s, _assumes = (eval_expr expr m assumes global_regions) in
      let l =
        if Ai_options.FailSoftMode.get () then
          let l1 =
            try Val1.elements (fst s)
            with
              Val1.Elements_of_top ->
              try Dba_types.AddressStack.Map.find addrStack recordMap
              with Not_found -> []
          in
          let l2 =
            try Val2.elements (snd s)
            with
              Val2.Elements_of_top ->
              try Dba_types.AddressStack.Map.find addrStack recordMap
              with Not_found -> []
          in
          l1 @ l2
        else
          elements (fst s) (snd s)
      in
      let locations, djumps_map = List.fold_right (fun elem (acc1, acc2) ->
          if Region_bitvector.region_of elem = `Constant then
            if Region_bitvector.size_of elem = Machine.Word_size.get () then
              begin
                let a = Dba_types.Caddress.block_start
                  @@ Region_bitvector.bitvector_of elem in
                let t1 = ((a, cstack, loop), Some m, flgs, equalities) :: acc1 in
                let t2 =
                  let s =
                    try Dba_types.AddressStack.Map.find addrStack acc2
                    with Not_found -> Dba_types.Caddress.Set.empty
                  in
                  Dba_types.AddressStack.Map.add addrStack (Dba_types.Caddress.Set.add a s) acc2 in
                t1, t2
              end
            else
              raise Errors.Bad_address_size
          else
            raise (Errors.Bad_region "Dynamic jump")
        ) l ([], djumps_map) in
      locations, (Dba_types.AddressStack.Map.add addrStack l recordMap), djumps_map

  let resolve_if cond s flgs equalities m1 m2 addr_suiv1 addr_suiv2 assumes global_regions =
    match s with
    | None -> failwith "reduced_product.ml: resolve_if with empty state"
    | Some m ->
      let cond, _assumes = eval_cond cond m assumes global_regions in
      match cond with
      | Ternary.True    -> [addr_suiv1, m1, flgs, equalities]
      | Ternary.False   -> [addr_suiv2, m2, flgs, equalities]
      | Ternary.Unknown ->
        [ addr_suiv1, m1, flgs, equalities;
          addr_suiv2, m2, flgs, equalities; ]


  let resolve_assume cond s flgs equalities addr_suiv assumes global_regions =
    match s with
    | None -> failwith "reduced_product.ml: assume with empty state"
    | Some m ->
      let condi, assumes = eval_cond cond m assumes global_regions in
      match condi with
      | Ternary.True 
      | Ternary.False ->
        let s, _assumes = guard cond s assumes global_regions in
        [addr_suiv, s, flgs, equalities]
      | Ternary.Unknown -> [addr_suiv, s, flgs, equalities]


  let resolve_assert cond s flgs equalities addr_suiv instr assumes global_regions =
    let addr, _cstack, _loop = addr_suiv in
    match s with
    | None -> failwith "reduced_product.ml: assert with empty state"
    | Some m ->
      let condi, assumes = eval_cond cond m assumes global_regions in
      let continue = Ternary.to_bool condi in
      if continue then
        let s, _assumes = guard cond s assumes global_regions in
        [addr_suiv, s, flgs, equalities]
      else Errors.assert_failure addr instr

  
  let resolve_nondet_assume lhslist cond s flgs equalities addr_suiv assumes global_regions =
    match s with
      None -> failwith "reduced_product.ml: resolve_nondet_assume with empty state"
    | Some s ->
      let rec update_memory_nondet lhslist s assumes global_regions =
        match lhslist with
        | [] -> s, assumes
        | [Dba.LhsVar (st, size, _)] ->
          let sub_s =
            SubEnv.add (Bigint.zero_big_int) (Val1.universe, Val2.universe) SubEnv.empty
          in (Static_types.Env.add (Static_types.Var (st, size)) sub_s s), assumes
        | [Dba.LhsVarRestrict (_st, _size, _of1, _of2)] ->
          failwith "analyse.ml: restrict case not handled"
        | [Dba.LhsStore (size, Dba.BigEndian, expr)] ->
          (store_big_endian size (Val1.universe, Val2.universe) expr s assumes global_regions), assumes
        | [Dba.LhsStore (size, Dba.LittleEndian, expr)] ->
          (store_little_endian size (Val1.universe, Val2.universe) expr s assumes global_regions), assumes
        | (Dba.LhsVar (st, size, _)) :: tl ->
          let m' =
            let sub_s =
              SubEnv.add (Bigint.zero_big_int) (Val1.universe, Val2.universe) SubEnv.empty
            in (Static_types.Env.add (Static_types.Var (st, size)) sub_s s)
          in update_memory_nondet tl m' assumes global_regions
        | Dba.LhsVarRestrict (_st, _size, _of1, _of2) :: _tl ->
          failwith "reduced_product.ml: restrict case not handled"
        | Dba.LhsStore (size, Dba.BigEndian, expr) :: tl ->
          let m' =
            store_big_endian size (Val1.universe, Val2.universe) expr s assumes global_regions in
          update_memory_nondet tl m' assumes global_regions
        | (Dba.LhsStore (size, Dba.LittleEndian, expr)) :: tl ->
          let m' = (store_little_endian size (Val1.universe, Val2.universe) expr s assumes global_regions) in
          update_memory_nondet tl m' assumes global_regions
      in
      let rec iterate cond iter assumes global_regions =
        if iter mod 100000 = 0 then
          Logger.debug "NONDET iteration num %d" iter;
        let m', assumes = update_memory_nondet lhslist s assumes global_regions in
        let condi, assumes = eval_cond cond m' assumes global_regions in
        match condi with
        | Ternary.True -> Some m', assumes
        | Ternary.False -> iterate cond (iter + 1) assumes global_regions
        | Ternary.Unknown -> guard cond (Some m') assumes global_regions
      in
      let op, assumes = iterate cond 0 assumes global_regions in
      [addr_suiv, op, flgs, equalities], assumes

  let resolve_nondet lhs _region s flgs equalities addr_suiv assumes global_regions =
    (* TODO : nondet in some region generates Top , not (region, Top) ? *)
    match s with
    | None -> failwith "resolve_nondet with empty state"
    | Some s ->
      let m = 
        match lhs with
        | Dba.LhsVar (st, size, _) ->
          let sub_s =
            SubEnv.add (Bigint.zero_big_int) (Val1.universe, Val2.universe) SubEnv.empty
          in Some (Static_types.Env.add (Static_types.Var (st, size)) sub_s s)
        | Dba.LhsVarRestrict (_st, _size, _of1, _of2) ->
          failwith "analyse.ml: restrict case not handled"
        | Dba.LhsStore (size, Dba.BigEndian, expr) ->
          Some (store_big_endian size (Val1.universe, Val2.universe) expr s assumes global_regions)
        | Dba.LhsStore (size, Dba.LittleEndian, expr) ->
          Some (store_little_endian size (Val1.universe, Val2.universe) expr s assumes global_regions) in
      [addr_suiv, m, flgs, equalities]


  let resolve_undef lhs s flgs equalities addr_suiv assumes global_regions =
    (* TODO : Kset can contain an undef value or
       it is trqnsformed to Top in this case *)
    match s with
    | None -> failwith "resolve_nondet with empty state"
    | Some s ->
      let m = match lhs with
          Dba.LhsVar (st, size, _) ->
          let sub_s =
            SubEnv.add (Bigint.zero_big_int) (Val1.singleton (`Undef (Dba_utils.computesize_dbalhs lhs)), Val2.singleton (`Undef (Dba_utils.computesize_dbalhs lhs)))
              SubEnv.empty
          in Some (Static_types.Env.add (Static_types.Var (st, size)) sub_s s)
        | Dba.LhsVarRestrict (_st, _size, _of1, _of2) ->
          failwith "analyse.ml: restrict case not handled"
        | Dba.LhsStore (size, Dba.BigEndian, expr) ->
          Some (store_big_endian size
                  (Val1.singleton (`Undef (Dba_utils.computesize_dbalhs lhs)), Val2.singleton (`Undef (Dba_utils.computesize_dbalhs lhs))) expr s assumes global_regions)
        | Dba.LhsStore (size, Dba.LittleEndian, expr) ->
          Some (store_little_endian size (Val1.singleton (`Undef (Dba_utils.computesize_dbalhs lhs)), Val2.singleton (`Undef (Dba_utils.computesize_dbalhs lhs))) expr s assumes global_regions) in
      [addr_suiv, m, flgs, equalities]

  let resolve_print args s flgs equalities addr_suiv assumes global_regions =
    match s with
    | None -> failwith "resolve_print: with empty state"
    | Some s ->
      Logger.debug "%s" (string_of_args args s assumes global_regions);
      [addr_suiv, Some s, flgs, equalities]




  let post abs_vals addrStack instr cache assumes global_regions djumps_map unrolled_loops _elements =
    let (addr, cstack, loop) = addrStack in
    let (m, flags, equalities) = abs_vals in
    let (recordMap, rcd_conds) = cache in
    match instr with
    | Dba.IkStop _ -> [], cache, assumes, djumps_map
    | Dba.IkAssign (lhs, expr, id_suiv) ->
      let op, assumes = (assign lhs expr m assumes global_regions) in
      let flags = update_flags lhs expr flags in
      [(Dba_types.Caddress.reid addr id_suiv, cstack, loop), op, flags, equalities], cache, assumes, djumps_map
    | Dba.IkMalloc (lhs, expr, id_suiv) ->
      let v, assumes =
        match m with
        | None -> failwith "unrelstate.ml: post malloc"
        | Some s -> eval_expr expr s assumes global_regions
      in
      incr Dba_types.malloc_id;
      let size =
        match (Val1.max (fst v)) with
        | `Value (`Constant, size) -> Bitvector.value_of size
        | _ -> match (Val2.max (snd v)) with
          | `Value (`Constant, size) -> Bitvector.value_of size
          | _ -> failwith "unrelstate.ml: malloc size"
      in
      let region = `Malloc ((!Dba_types.malloc_id, addr), size) in
      let bv = Bitvector.zeros (Machine.Word_size.get ()) in
      Simulate.mallocs :=
        Malloc_status.add region Dba.Freeable !Simulate.mallocs;
      let op, assumes = (assign lhs (Dba.ExprCst (region, bv)) m assumes global_regions) in
      [(Dba_types.Caddress.reid addr id_suiv, cstack, loop), op, flags, equalities], cache, assumes, djumps_map
    | Dba.IkFree (expr, id_suiv) ->
      [((check_exec_permission (Dba_types.Caddress.reid addr id_suiv) m assumes global_regions), cstack, loop),
       (free expr m assumes global_regions), flags, equalities], cache, assumes, djumps_map
    | Dba.IkSJump (Dba.JInner id_suiv, _call_return_tag) ->
      [(Dba_types.Caddress.reid addr id_suiv, cstack, loop), m, flags, equalities], cache, assumes, djumps_map
    | Dba.IkSJump (Dba.JOuter addr_suiv, _call_return_tag) ->
      [(addr_suiv, cstack, loop), m, flags, equalities], cache, assumes, djumps_map
    (********************************************************************)
    | Dba.IkDJump (expr, _call_return_tag) ->
      let a, rcd, djumps_map = resolve_jump addrStack expr m flags equalities recordMap assumes global_regions djumps_map in
      let cache = (rcd, rcd_conds) in
      a, cache, assumes, djumps_map
    (*******************************************************************)
    | Dba.IkIf (cond, Dba.JOuter next_addr, id) ->
      let cond, rcd_conds = retrieve_comparison cond flags addr rcd_conds in
      let m1, assumes = guard cond m assumes global_regions in
      let m2, assumes = guard (Dba.CondNot cond) m assumes global_regions in
      let a1 = (* Dba_utils.globalize_address addr *) next_addr in
      let a2 = Dba_types.Caddress.reid addr id in
      let unrolleds = Ai_utils.unrolled_loops_at_address addr unrolled_loops in
      let loop1, loop2 = Ai_utils.unroll_current_loop a1 loop unrolleds 100 in
      let addr_suiv1 = (a1, cstack, loop1) in
      let addr_suiv2 = (a2, cstack, loop2) in
      let a = resolve_if cond m flags equalities m1 m2 addr_suiv1 addr_suiv2 assumes global_regions in
      let cache = (recordMap, rcd_conds) in
      a, cache, assumes, djumps_map
    | Dba.IkIf (cond, Dba.JInner id_suiv1, id_suiv2) ->
      (* let loop, loop_a = loop in *)
      let cond, rcd_conds = retrieve_comparison cond flags addr rcd_conds in
      let m1, assumes = guard cond m assumes global_regions in
      let m2, assumes = guard (Dba.CondNot cond) m assumes global_regions in
      let a1 = Dba_types.Caddress.reid addr id_suiv1 in
      let a2 = Dba_types.Caddress.reid addr id_suiv2 in
      let loops =
        try Dba_types.Caddress.Map.find addr unrolled_loops
        with Not_found -> Dba_types.Caddress.Set.empty
      in
      let loop1, loop2 =
        if Dba_types.Caddress.Set.cardinal loops = 1
        then
          if Dba_types.Caddress.Set.mem a1 loops
          then min (loop + 1) 100, 0
          else 0, min (loop + 1) 100
        (* else if Basic_structs.Dba_types.Caddress.Set.cardinal loops = 2 *)
        (* then if loop = 0  *)
        (*   then min (loop + 1) 100, min (loop + 1) 100 *)
        (*   else  *)
        (*     if Basic_structs.Dba_types.Caddress.Set.mem a1 loops && Dba.compare_addresses a1 loop_a = 0 *)
        (*     then min (loop + 1) 100, 0 *)
        (*     else 0, min (loop + 1) 100 *)
        else loop, loop
      in
      let a1 = (a1, cstack, loop1) in
      let a2 = (a2, cstack, loop2) in
      let a = resolve_if cond m flags equalities m1 m2 a1 a2 assumes global_regions in
      let cache = (recordMap, rcd_conds) in
      a, cache, assumes, djumps_map
    | Dba.IkAssert (cond, id_suiv) ->
      let cond, _rcd_cond = retrieve_comparison cond flags addr rcd_conds in
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let a = resolve_assert cond m flags equalities a instr assumes global_regions in
      let cache = (recordMap, rcd_conds) in
      a, cache, assumes, djumps_map
    | Dba.IkAssume (cond, id_suiv) ->
      let cond, rcd_conds = retrieve_comparison cond flags addr rcd_conds in
      let a_suiv = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let a = resolve_assume cond m flags equalities a_suiv assumes global_regions in
      let cache = (recordMap, rcd_conds) in
      a, cache, assumes, djumps_map
    | Dba.IkNondetAssume (lhslist, cond, id_suiv) ->
      let cond, rcd_conds = retrieve_comparison cond flags addr rcd_conds in
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let op, assumes = (resolve_nondet_assume lhslist cond m flags equalities a assumes global_regions) in
      let cache = (recordMap, rcd_conds) in
      op, cache, assumes, djumps_map
    | Dba.IkNondet (lhs, region, id_suiv) ->
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      let op = (resolve_nondet lhs region m flags equalities a assumes global_regions) in
      op, cache, assumes, djumps_map
    | Dba.IkUndef (lhs, id_suiv) ->
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      (resolve_undef lhs m flags equalities a assumes global_regions), cache, assumes, djumps_map
    | Dba.IkPrint (args, id_suiv) ->
      let a = (Dba_types.Caddress.reid addr id_suiv, cstack, loop) in
      (resolve_print args m flags equalities a assumes global_regions), cache, assumes, djumps_map


  let get_initial_state inits =
    let addr = Dba_types.Caddress.block_start @@ Bitvector.zeros 32 in
    let cstack = [] in
    let loop = 0 in
    let addrStack = (addr, cstack, loop) in
    let rcd = Dba_types.AddressStack.Map.empty in
    let equalities = UF.create () in
    let conds = [] in
    let glbs = Dba_types.Caddress.Set.empty in
    let djmps = Dba_types.AddressStack.Map.empty in
    let flags = High_level_predicate.empty in
    let rcd_conds = Dba_types.Caddress.Map.empty in
    let unrolled_loops = Dba_types.Caddress.Map.empty in
    let cache = (rcd, rcd_conds) in
    let init_state m instr =
      let abs_vals = (m, flags, equalities) in
      post abs_vals addrStack instr cache conds glbs djmps unrolled_loops []
    in
    let f m instr =
      let l_res, _, _, _ = init_state m instr in
      let (_, res, _, _) = List.hd l_res in
      res
    in
    let m_top = Some Static_types.Env.empty in
    (List.fold_left f m_top inits)


  let _projection _ _ = failwith "not implemented"

  let env_to_smt_list _m _varIndexes _inputs =
    failwith "reduced_product.ml: env_to_smt"

  let refine_state _m _smt_env =
    failwith "reduced_product.ml: refine_state not yet implemented!"
end
