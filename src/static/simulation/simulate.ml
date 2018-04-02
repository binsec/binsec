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

open Region_bitvector
open Dba_types
open Concrete_state
open Concrete_eval
open Format
open Ast_builder

let mallocs =
  ref (Dba_types.Region.Map.empty : Dba.malloc_status Dba_types.Region.Map.t)

let new_malloc = ref (Bigint.big_int_of_int 0x80efa40)


let check_region_size i region size =
  match region with
  | `Constant -> true
  | `Malloc (_, malloc_size) ->
    not Bigint.(gt_big_int (Simulate_utils.mk_sup i size) malloc_size)
  | `Stack -> true

let update_memory lhs rbv m conds glbs:
  Concrete_state.concreteMap * Caddress.Set.t =
  let open Bigint in
  match lhs with
  | Dba.LhsVar (s, size, _) ->
    let v = Static_types.Var (s, size) in
    let m = Concrete_eval.write v zero_big_int rbv m conds glbs in
    m, glbs

  (* restrict assignment requires to load variable from memory *)
  | Dba.LhsVarRestrict (s, size, of1, of2) ->
    let x =
      try Concrete_eval.read (Static_types.Var (s, size)) zero_big_int m conds glbs
      with Not_found -> Simulate_utils.mk_undef_value size
    in
    let temp1 =
      if of1 = 0 then rbv
      else Region_bitvector.append rbv (restrict x 0 (of1 - 1))
    in
    let temp2 = restrict x (of2 + 1) (size - 1) in
    let v = Static_types.Var (s, size) in
    let tmp = Region_bitvector.append temp2 temp1 in
    let m = Concrete_eval.write v zero_big_int tmp m conds glbs in
    m, glbs

  | Dba.LhsStore (size, Dba.LittleEndian, expr) ->
    (* FIXME: this is the same case as BigEndian modulo *)
    let e, m = Concrete_eval.eval_expr expr m conds glbs, m in
    let region = region_of e in
    let i = value_of e in
    if check_region_size i region size
    then
      let m = ref m in
      let j = ref i in
      let sup = Simulate_utils.mk_sup i size in
      let v_exp = ref e in
      let glbs = ref glbs in
      while le_big_int !j sup do
        begin
          match region with
          | `Constant ->
            let a = Dba_types.Caddress.block_start @@ Bitvector.create !j 32
            in glbs := Caddress.Set.add a !glbs
          | _ -> ()
        end;
        let v = `Value (`Constant, Region_bitvector.bitvector_of !v_exp) in
        let sub_m = SubEnv.singleton zero_big_int v in
        m := Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m !m;
        let of1 = 8 * int_of_big_int (sub_big_int !j i) in
        let of2 = of1 + 7 in
        let array = Static_types.Array region in
        let res_rbv = restrict rbv of1 of2 in
        m := Concrete_eval.write array !j res_rbv !m conds !glbs;
        v_exp := Region_bitvector.succ !v_exp;
        j := add_big_int !j unit_big_int
      done;
      !m, !glbs
    else raise (Errors.Bad_bound "store, Little_endian case")

  | Dba.LhsStore (size, Dba.BigEndian, expr) ->
    let e, m = Concrete_eval.eval_expr expr m conds glbs, m in
    let region = region_of e in
    let i = value_of e in
    if (check_region_size i region size)
    then
      let m = ref m in
      let j = ref i in
      let sup = Simulate_utils.mk_sup i size in
      let v_exp = ref e in
      let glbs = ref glbs in
      while le_big_int !j sup do
        begin
          match region with
          | `Constant ->
            let a = Dba_types.Caddress.block_start @@ Bitvector.create !j 32
            in glbs := Caddress.Set.add a !glbs
          | _ -> ()
        end;
        let v = `Value (`Constant, Region_bitvector.bitvector_of !v_exp) in
        let sub_m = SubEnv.add zero_big_int v SubEnv.empty in
        m := Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m !m;
        let big_i = Bigint.int_of_big_int (Bigint.sub_big_int !j i) in
        let of1 = 8 * ((size - 1) - big_i) in
        let of2 = of1 + 7 in
        let array = Static_types.Array region in
        let res_rbv = restrict rbv of1 of2 in
        m := Concrete_eval.write array !j res_rbv !m conds !glbs;
        v_exp := Region_bitvector.succ !v_exp;
        j := Bigint.succ_big_int !j
      done;
      !m, !glbs
    else raise (Errors.Bad_bound "store, big_endian case")



let rec string_of_args args m conds glbs =
  (match args with
   | [] ->  ""
   | (Dba.Str s) :: tl ->
     (Scanf.unescaped s) ^ (string_of_args tl m conds glbs)
   | (Dba.Exp e) :: tl ->
     let temp = Concrete_eval.eval_expr e m conds glbs in
     let s_temp = Region_bitvector.to_string temp in
     s_temp ^ (string_of_args tl m conds glbs)
  )


let check_exec_permission addr m conds glbs =
  let v = `Value (`Constant, addr.Dba.base) in
  let sub_m = SubEnv.add Bigint.zero_big_int v SubEnv.empty in
  let m = Static_types.Env.add (Static_types.Var ("\\addr", Machine.Word_size.get ())) sub_m m in
  let c =
    try Dba_types.Rights.find (Dba_types.Rights.X, `Constant) !permis
    with Not_found ->  Dba.True
  in
  if (Concrete_eval.eval_cond c m conds glbs) then addr
  else raise Errors.Exec_permission_denied


let compute addr instr m conds djmps pp_conds glbs:
  Dba.address option *
  concreteMap *
  Smt_bitvectors.smtBvExprAlt list *
  Caddress.Set.t Caddress.Map.t *
  (Dba.address * Region_bitvector.t) list Caddress.Map.t *
  Caddress.Set.t =
  match instr with
  | Dba.IkStop (Some Dba.OK) ->
    None, m, conds, djmps, pp_conds, glbs

  | Dba.IkStop _ -> None, m, conds, djmps, pp_conds, glbs

  | Dba.IkAssign (lhs, expr, id_suiv) ->
    let addr_suiv = Dba_types.Caddress.reid addr id_suiv in
    let s1 = Some (check_exec_permission addr_suiv m conds glbs) in
    Logger.debug "%a" Dba_printer.Ascii.pp_instruction instr ;
    let rhs = Concrete_eval.eval_expr expr m conds glbs in
    let s2, glbs = update_memory lhs rhs m conds glbs in
    let s3 = conds in
    s1, s2, s3, djmps, pp_conds, glbs

  | Dba.IkMalloc (lhs, expr, id_suiv) ->
    incr Dba_types.malloc_id;
    let e_size = Concrete_eval.eval_expr expr m conds glbs in
    let size =
      match e_size  with
      | `Value (`Constant, bv) -> Bitvector.value_of bv
      | _ -> failwith "Simulate.ml: malloc ();"
    in
    let malloc_key  =
      if Simulate_options.SemanticsMode.flat ()
      then
        let a = Dba_types.Caddress.create (Bitvector.create !new_malloc 32) (-1) in
        `Malloc ((-1, a), Bigint.zero_big_int)
      else `Malloc ((!Dba_types.malloc_id, addr), size)
    in
    let region =
      if Simulate_options.SemanticsMode.flat () then `Constant
      else `Malloc ((!Dba_types.malloc_id, addr), size)
    in
    let bv =
      if Simulate_options.SemanticsMode.flat () then
        begin
          let res = !new_malloc in
          let big_size = Bigint.add_int_big_int 15 size in
          let big_a = Bigint.big_int_of_int 15 in
          let aligned_size = Bigint.mod_big_int big_size big_a in
          new_malloc := Bigint.add_big_int aligned_size !new_malloc;
          Bitvector.create res (Machine.Word_size.get ())
        end
      else Bitvector.zeros (Machine.Word_size.get ())
    in
    mallocs := Dba_types.Region.Map.add malloc_key Dba.Freeable !mallocs;
    let v = (`Value (region, bv)) in
    let m, glbs = update_memory lhs v m conds glbs in
    let a = Dba_types.Caddress.reid addr id_suiv in
    let addr_suiv = Some (check_exec_permission a m conds glbs) in
    addr_suiv, m, conds, djmps, pp_conds, glbs

  | Dba.IkFree (expr, id_suiv) ->
    let v = Concrete_eval.eval_expr expr m conds glbs in
    let rec aux v =
      (match v with
       | `SymbSmt smb ->
         let sz = Machine.Word_size.get () in
         let v = Region_bitvector.get_value smb sz conds glbs in
         aux v

       | `Value (`Malloc (id, malloc_size), bv) ->
         let st = Dba_types.Region.Map.find (`Malloc (id, malloc_size)) !mallocs
         in
         begin match st with
           | Dba.Freeable when Bitvector.is_zero bv ->
             begin
               let malloc = `Malloc (id, malloc_size) in
               mallocs :=
                 Dba_types.Region.Map.add malloc Dba.Freed !mallocs;
               let a = Dba_types.Caddress.reid addr id_suiv in
               let next = Some (check_exec_permission a m conds glbs) in
               next, m, conds, djmps, pp_conds, glbs
             end
           | Dba.Freed -> raise Errors.Freed_variable_access
           | _ -> raise Errors.Invalid_free_address
         end
       | `Value (`Constant, bv) ->
         let elmt = `Malloc ((-1, Dba_types.Caddress.create bv (-1)), Bigint.zero_big_int) in
         let st = Dba_types.Region.Map.find elmt !mallocs in
         begin
           match st with
           | Dba.Freeable ->
             mallocs :=
               Dba_types.Region.Map.add elmt Dba.Freed !mallocs;
             let a = Dba_types.Caddress.reid addr id_suiv in
             let next = Some (check_exec_permission a m conds glbs) in
             next, m, conds, djmps, pp_conds, glbs
           | Dba.Freed -> raise Errors.Freed_variable_access
         end
       | _ ->
         Format.eprintf "Invalid free : %a@." Region_bitvector.pp v;
         raise Errors.Invalid_free_region
      )
    in aux v

  | Dba.IkSJump (Dba.JOuter addr_suiv, _call_return_tag) ->
    let next = Some (check_exec_permission addr_suiv m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs

  | Dba.IkSJump (Dba.JInner id_suiv, _call_return_tag) ->
    let a = Dba_types.Caddress.reid addr id_suiv in
    let next = Some (check_exec_permission a m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs

  | Dba.IkDJump (expr, _call_return_tag) ->
    let rbv = Concrete_eval.eval_expr expr m conds glbs in
    let rec aux rbv  =
      match rbv with
       | `SymbSmt smb ->
         let sz = Machine.Word_size.get () in
         let v = Region_bitvector.get_value smb sz conds glbs in
         aux v
       | `Value (_r, b) ->
         let sz = Bitvector.size_of b in
         let b = Bitvector.value_of b  in
         let v = `Value (`Constant, (Bitvector.create b sz)) in
         let sub_m = SubEnv.add Bigint.zero_big_int v SubEnv.empty in
         let key = Static_types.Var ("\\addr", Machine.Word_size.get ()) in
         let m = Static_types.Env.add key sub_m m in
         let r = region_of rbv in
         let c =
           try Dba_types.Rights.find (Dba_types.Rights.X, r) !permis
           with Not_found -> Dba.True
         in
         if (Concrete_eval.eval_cond c m conds glbs) then (
           if r = `Constant then
             if size_of rbv = Machine.Word_size.get ()
             then
               begin
                 let target_addr =
                   Dba_types.Caddress.block_start @@
                   Region_bitvector.bitvector_of rbv in
                 let old_set =
                   try Caddress.Map.find addr djmps
                   with Not_found -> Caddress.Set.empty
                 in
                 let new_set = Caddress.Set.add target_addr old_set in
                 let djmps = Caddress.Map.add addr new_set djmps in
                 (Some target_addr), m, conds, djmps, pp_conds, glbs
               end
             else
               begin
                 Format.eprintf "size = %d" (size_of rbv);
                 raise Errors.Bad_address_size
               end
           else raise (Errors.Bad_region "Dynamic jump"))
         else raise Errors.Exec_permission_denied

       | _ -> raise (Errors.Unknown_value (to_string rbv))
    in
    aux rbv

  | Dba.IkIf (cond, (Dba.JOuter addr_suiv1), id_suiv2) ->
    (
      try
        if Concrete_eval.eval_cond cond m conds glbs then
          let a = check_exec_permission addr_suiv1 m conds glbs in
          let next = Some a in
          next, m, conds, djmps, pp_conds, glbs
        else
          let a = Dba_types.Caddress.reid addr id_suiv2 in
          let a = check_exec_permission a m conds glbs in
          let next = Some a in
          next, m, conds, djmps, pp_conds, glbs
      with
        Smt_bitvectors.Assume_condition c ->
        Random.self_init ();
        let random_value = Random.int 100 in
        let alternative_strategy () =
          let nc = Smt_bitvectors.SmtBvUnaryAlt (Smtlib2.SmtBvNot, c) in
          let bv_cond = `SymbSmt nc in
          let bv_cond_list =
            try Caddress.Map.find addr_suiv1 pp_conds
            with Not_found -> []
          in
          let l = bv_cond_list @ [(addr, bv_cond)] in
          let pp_conds = Caddress.Map.add addr_suiv1 l pp_conds in
          let nc = Dba.CondNot cond in
          let conds = Concrete_eval.add_smt_cond nc m conds glbs in
          let a = Dba_types.Caddress.reid addr id_suiv2 in
          let a = check_exec_permission a m conds glbs in
          let next = Some a in
          next, m, conds, djmps, pp_conds, glbs
        in
        match Simulate_options.ConditionalStrategy.get () with
        | None -> raise (Smt_bitvectors.Assume_condition c)
        | Some Simulate_options.ConditionalStrategy.Else -> alternative_strategy ()
        | Some Simulate_options.ConditionalStrategy.Normal ->
          if random_value mod 2 = 0 then alternative_strategy ()
          else
            let bv_cond = `SymbSmt c in
            let bv_cond_list =
              try Caddress.Map.find addr_suiv1 pp_conds
              with Not_found -> []
            in
            let l = bv_cond_list @ [(addr,bv_cond)] in
            let pp_conds = Caddress.Map.add addr_suiv1 l pp_conds in
            let conds = Concrete_eval.add_smt_cond cond m conds glbs in
            let a =check_exec_permission addr_suiv1 m conds glbs in
            let next = Some a in
            next, m, conds, djmps, pp_conds, glbs
    )

  | Dba.IkIf (cond, (Dba.JInner id_suiv1), id_suiv2) ->
    (
      try
        if (Concrete_eval.eval_cond cond m conds glbs) then
          let a = Dba_types.Caddress.reid addr id_suiv1 in
          let next = Some (check_exec_permission a m conds glbs) in
          next, m, conds, djmps, pp_conds, glbs
        else
          let a = Dba_types.Caddress.reid addr id_suiv2 in
          let next = Some (check_exec_permission a m conds glbs) in
          next, m, conds, djmps, pp_conds, glbs
      with
      | Smt_bitvectors.Assume_condition c ->
        let cond, next =
          let open Simulate_options in
          match ConditionalStrategy.get () with
          | None -> raise (Smt_bitvectors.Assume_condition c)
          | Some ConditionalStrategy.Else -> Dba.CondNot cond, id_suiv2
          | Some ConditionalStrategy.Normal -> cond, id_suiv1
        in
        let conds = Concrete_eval.add_smt_cond cond m conds glbs in
        let a = Dba_types.Caddress.reid addr next in
        let a = check_exec_permission a m conds glbs in
        let next = Some a in
        next, m, conds, djmps, pp_conds, glbs
    )

  | Dba.IkAssert (cond, id_suiv) ->
    if (Concrete_eval.eval_cond cond m conds glbs) then
      let a = Dba_types.Caddress.reid addr id_suiv in
      let a = check_exec_permission a m conds glbs in
      let next = Some a in
      next, m, conds, djmps, pp_conds, glbs
    else Errors.assert_failure addr instr

  | Dba.IkAssume (cond, id_suiv) ->
    let conds =
      match Simulate_options.ConditionalStrategy.get () with
      | None ->  conds
      | Some _ -> Concrete_eval.add_smt_cond cond m conds glbs
    in
    let a = Dba_types.Caddress.reid addr id_suiv in
    let next = Some (check_exec_permission a m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs

  | Dba.IkNondetAssume (lhslist, cond, id_suiv) ->

    let rec update_memory_nondet lhslist m conds glbs =
      match lhslist with
      | [] -> m, glbs
      | lhs :: lhss ->
        let v = Region_bitvector.non_deterministic `Constant (Dba_types.LValue.unsafe_bitsize lhs) in
        let m', glbs = update_memory lhs v m conds glbs in
        update_memory_nondet lhss m' conds glbs in
    let rec iterate cond iter =
      if iter <> 0 && iter mod 100000 = 0 then
        printf "NONDET iterartion num %d" iter;
      let m', glbs = update_memory_nondet lhslist m conds glbs in
      if (Concrete_eval.eval_cond cond m' conds glbs) then m'
      else iterate cond (iter + 1) in
    let m = iterate cond 0 in
    let a = Dba_types.Caddress.reid addr id_suiv in
    let next = Some (check_exec_permission a m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs

  | Dba.IkNondet (lhs, region, id_suiv) ->
    let m, glbs =
      match region with
      | `Malloc ((_id1, _addr1), _size1) ->
        let f_aux r x = (
          match r with
          | (`Malloc((_id2, _addr2), _size2)) -> x = Dba.Freeable
          | _ -> printf "freed malloc"; false
        ) in
        let available_mallocs =
          Dba_types.Region.Map.filter f_aux !mallocs in
        let r, _st =
          try Dba_types.Region.Map.choose available_mallocs
          with Not_found -> failwith "nondet: no malloc available"
        in
        let v = Region_bitvector.non_deterministic r (Dba_types.LValue.unsafe_bitsize lhs) in
        update_memory lhs v m conds glbs
      | `Constant
      | `Stack ->
        let v = Region_bitvector.non_deterministic region (Dba_types.LValue.unsafe_bitsize lhs) in
        update_memory lhs v m conds glbs
    in
    let a = Dba_types.Caddress.reid addr id_suiv in
    let next = Some (check_exec_permission a m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs

  | Dba.IkUndef (lhs, id_suiv) ->
    let v = Simulate_utils.mk_undef_value (Dba_utils.computesize_dbalhs lhs) in
    let m, glbs = update_memory lhs v m conds glbs in
    let a = Dba_types.Caddress.reid addr id_suiv in
    let next = Some (check_exec_permission a m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs

  | Dba.IkPrint (args, id_suiv) ->
    printf "%s%!" (string_of_args args m conds glbs);
    let a = Dba_types.Caddress.reid addr id_suiv in
    let next = Some (check_exec_permission a m conds glbs) in
    next, m, conds, djmps, pp_conds, glbs


let comp_check address instr m conds time djmps pp_conds glbs =
  (* FIXME: refactor *)
  let open Dba_printer.Ascii in
  let _a = asprintf "%a" pp_code_address address in
  try
    let t0 = Unix.gettimeofday () in
    let a, b, c, d, e, f =
      compute address instr m conds djmps pp_conds glbs in
    a, b, c, time +. (Unix.gettimeofday() -. t0), d, e, f
  with
  | Smt_bitvectors.Assume_condition smb ->
    let s_smb = Region_bitvector.to_string (`SymbSmt smb) in
    failwith (sprintf "Condition as symbolic expr: %s" s_smb)
  | Errors.Bad_condition s ->
    failwith (sprintf "Unable to get condition value: %s" s)
  | Errors.Invalid_address s ->
    failwith (sprintf "%s" s)
  | Errors.Assert_failure dinstr ->
    Logger.error "%@ %a" Dba_types.Statement.pp dinstr;
    exit 2
  | Errors.Assume_failure s ->
    failwith (sprintf "assume_failure %s" s)
  | Errors.Regions_conflict s ->
    failwith (sprintf "Regions_conflict %s" s)
  | Errors.Bad_region s ->
    failwith (sprintf "Bad_region %s" s)
  | Errors.Bad_bound s ->
    failwith (sprintf "Bad_bound %s" s)
  | Errors.Unbound_region_element s ->
    failwith (sprintf "Unbound_region_element %s" s)
  | Errors.Alternative_conflict_values ->
    failwith (sprintf "Alternative_conflict_values")
  | Errors.Unknown_value s ->
    failwith (sprintf "Unable to cast : %s" s)
  | Errors.Bad_concat s ->
    failwith (sprintf "concat of bad values : %s" s)
  | Errors.Read_permission_denied ->
    failwith (sprintf "Read access denied")
  | Errors.Write_permission_denied ->
    failwith (sprintf "Write access denied")
  | Errors.Exec_permission_denied ->
    failwith (sprintf "Exec permission denied")



let exec_inst a instsb m conds time djmps pp_conds glbs =
  let lb = try Caddress.Map.find a instsb with Not_found -> [] in
  let f (_add_suiv, m, conds, time, djmps, pp_conds, glbs) inst =
    comp_check a inst m conds time djmps pp_conds glbs
  in
  List.fold_left f (Some a, m, conds, time, djmps, pp_conds, glbs) lb


let add_stub_replace address instsr inst_map =
  try
    let chained_instr = Caddress.Map.find address instsr in
    Disasm_types.add_chained_instr chained_instr inst_map
  with Not_found ->
    inst_map

let display_step (a, l0) m insts djmps visited pp_conds =
  if Simulate_options.StepByStep.get () && a.Dba.id = 0 then begin
    Logger.debug "@[<v 0>%a:@ %a@]"
      Dba_printer.Ascii.pp_code_address a
      Concrete_state.display m;
    let ast = Ast_builder.make insts djmps in
    let cfg_dba =
      Ast_builder.cfg_dba_of_ast ast (Some l0) visited pp_conds in
    let cfg_opcode =
      Ast_builder.cfg_opcode_of_ast ast (Some l0) visited pp_conds in
    Cfgraph.Dot.output_graph_to_file "cfg_opcode.dot" cfg_opcode;
    Cfgraph.Dot.output_graph_to_file "cfg_dba.dot" cfg_dba;
    Format.printf "Press any key to continue ... @.";
    ignore(read_line ())
  end


let simulate l0 insts stops instsb instsr glbs =
  let mode_name = Simulate_options.SemanticsMode.to_string () in
  let trace_name = "out_" ^ mode_name ^ ".trace" in
  let oc = open_out trace_name in
  let trace_fmt = Format.formatter_of_out_channel oc in
  let rec aux a m stmts conds time djmps visited pp_conds glbs =
    let _pre_add, cur_add = a in
    let insts, instsb, instsr = stmts in
    match cur_add with
    | None ->
        close_out oc;
        m, insts, djmps, visited, pp_conds, conds, time
    | Some addr ->
      Format.fprintf trace_fmt "%a"
        Dba_printer.Ascii.pp_code_address addr;
      let visited = Caddress.Set.add addr visited in
      let _next_add, m, conds, time, djmps, pp_conds, glbs =
        exec_inst addr instsb m conds time djmps pp_conds glbs in
      let (insts:Disasm_types.pmap) = add_stub_replace addr instsr insts in
      let w_pts = Caddress.Map.empty in
      let unrolled_loops = Caddress.Map.empty in
      let w_delays = 0, Caddress.Map.empty in
      let (instr, _), insts, _w_pts, _unrolled_loops =
        Static_utils.update_instr_map l0 addr insts stops w_pts w_delays djmps unrolled_loops in
      let next_add, m, conds, time, djmps, pp_conds, glbs =
        comp_check addr instr m conds time djmps pp_conds glbs in

      display_step (addr, l0) m  insts djmps visited pp_conds;
      let a = cur_add, next_add in
      let stmts = insts, instsb, instsr in
      aux a m stmts conds time djmps visited pp_conds glbs
  in
  let m = Static_types.Env.empty in
  let djmps = Caddress.Map.empty in
  let visited = Caddress.Set.empty in
  let pp_conds = Caddress.Map.empty in
  let conds = [] in
  let time = 0. in
  let a = (None, (Some l0)) in
  let stmts = (insts, instsb, instsr) in
  aux a m stmts conds time djmps visited pp_conds glbs


let set_initial_state inst_inits perms =
  (* warning: permission rights do not apply for initialization *)
  Concrete_eval.perm := Dba_types.Region.Map.empty;
  Concrete_eval.permis := Dba_types.Rights.empty;
  let f (m, glbs) instr =
    try
      let l0 = !Dba_types.Caddress.default_init in
      let djmps = Caddress.Map.empty in
      let conds = Caddress.Map.empty in
      let (_, b, _, _, _, glbs) =
        compute l0 instr m [] djmps conds glbs in
      b, glbs
    with
      Errors.Write_permission_denied ->
      failwith ("Write access denied at initialization")
  in
  let m, glbs = (Static_types.Env.empty, Caddress.Set.empty) in
  let state, glbs = List.fold_left f (m, glbs) inst_inits in
  Concrete_eval.perm := fst perms;
  Concrete_eval.permis := snd perms;
  state, glbs


let display_simulation_results _mem _assumes time cfg_opcode cfg_dba =
  Cfgraph.Dot.output_graph_to_file "cfg_opcode.dot" cfg_opcode;
  Cfgraph.Dot.output_graph_to_file "cfg_dba.dot" cfg_dba;
  Logger.result
    "@[<v 0><info>: simulation time: \t %f@ \
      %.2f & %a & %d & %d@]"
    time time
    Region_bitvector.display_statistics ()
    (Simulate_utils.get_nb_undef_builds ())
    (Simulate_utils.get_undef_loads ())


let set_sim_outputs insts m djumps l0 visited pp_conds conds time =
  Logger.debug
    "@[<v 0>%a@]"
    (fun fmt map ->
       Caddress.Map.iter
         (fun key _ ->
           fprintf fmt "Dynamic jump : %a@ "
           Dba_printer.Ascii.pp_code_address key)
         map)
    djumps ;
  let ast = Ast_builder.make insts djumps in
  let l0 = Some l0 in
  let cfg_dba = cfg_dba_of_ast ast l0 visited pp_conds in
  let cfg_opcode = cfg_opcode_of_ast ast l0 visited pp_conds in
  display_simulation_results m conds time cfg_opcode cfg_dba


let fuzz_simulate l0 insts perms inits parameters =
  let open Infos in
  let stops = parameters.stops in
  let stub_before = parameters.prepend_stubs in
  let stub_replace = parameters.substitute_stubs in
  let count = ref 1 in
  let limit = Simulate_options.FuzzerIterations.get () in
  while !count <= limit do
    Logger.debug ~level:3 "Simulation step [%d/%d]" !count limit;
    Dba_types.malloc_id := 0;
    let m_init, glbs = set_initial_state inits perms in
    Concrete_state.m_init := m_init;
    let m, insts, djmps, visited, pp_conds, conds, time =
      simulate l0 insts stops stub_before stub_replace glbs
    in set_sim_outputs insts m djmps l0 visited pp_conds conds time;
    incr count
  done

let run ?(dba_file=None) ~configuration_file () =
  Logger.debug "Starting simulation ..." ;
  let parameters = Binsec_utils.read_optional_config_file configuration_file in
  let program =
    match dba_file with
    | None -> Binsec_utils.load_dba_definition (Machine.ISA.get ())
    | Some filename -> Binsec_utils.read_dba_file filename
  in
  let start_address =
    match Options.get_entry_point () with
    | None -> program.start_address
    | Some bint ->
       let bv = Bitvector.create bint (Machine.Word_size.get ()) in
       Dba_types.Caddress.block_start bv
  in
  fuzz_simulate
    start_address program.instructions
    program.permissions program.initializations
    parameters
