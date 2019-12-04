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

open Formula_pp
open Solver
open Formula_utils
open Path_predicate_env
open Path_predicate_optim
open! Basic_types
open Dse_options

exception Exit_path_predicate

let pp_vars f =
  Logger.result "@[<v 0>|------ Symbolic Store ------|@ ";
  SymbVar.iter
    (fun name (i,j,f) ->
       Logger.result "%s{%i, %i} = %s@ " name i j (print_bv_term f))
    f.vars;
  Logger.result "|----------------------------|@]@."

let optim_str f =
  let s = if f.optim_cst_prop then "C" else "" in
  let s = if f.optim_rebase then s^"R" else s in
  let s = if f.optim_row then s^"W" else s in
  let s = if f.optim_rowplus then s^"H" else s in
  let s = if f.optim_eq_prop then s^"E" else s in
  if s = "" then "NONE" else s

let base_varname s =
  Str.string_match (Str.regexp "^[a-zA-Z]*") s 0 |> ignore;
  Str.matched_string s

let _reset_symbvar f =
  {f with vars = SymbVar.empty}

let inc_varindex f (name:string): int SymbVar.t =
  try
    let value = SymbVar.find name f.varsindex in
    SymbVar.add name (value+1) f.varsindex
  with Not_found ->
    SymbVar.add name 1 f.varsindex

let get_varindex f (name:string): int =
  try
    SymbVar.find name f.varsindex
  with Not_found -> 0

let contains_variable f (name:string) =
  SymbVar.mem name f.vars

let print_simplifications f p1 p2 p3 =
  Logger.warning ~level:2 "@[<v 0>⎧in:[%s]@ ⎪row:[%s]@ ⎩out:[%s]@]"
    (String_utils.remove_newline (f p1))
    (String_utils.remove_newline (f p2))
    (String_utils.remove_newline (f p3))

let apply_optimizations_abvexpr f (new_name:string) f_expr =
  (* CONSTANT PROPAGATION *)
  let new_e = if f.optim_cst_prop then propagate_cst_abv f f_expr else f_expr in
  let new_e = if f.optim_rebase then rebase_abvexpr f new_e else new_e in (* REBASE OPTIM *)
  let new_e' = if f.optim_row then read_over_write_abv f new_e else new_e in (* READ over WRITE  *)
  let new_e = if f.optim_rowplus then read_over_write_hybrid_abv f new_e' else new_e' in (* ROW Hyrbid OPTIM *)
  let new_e = if f.optim_cst_prop then propagate_cst_abv f new_e else new_e in (* CONSTANTE PROPAGATION #2 *)
  let new_hybrid_mem, new_e'' = if f.optim_rowplus then update_hybrid_memory f new_name new_e else f.hybrid_memory, new_e in (* ROW Hybrid *)
  let new_e = if f.optim_cst_prop then propagate_cst_abv f new_e'' else new_e'' in (* CONSTANTE PROPAGATION #3 *)
  if not (Formula.equal_ax_term new_e' new_e) then
    print_simplifications print_ax_term new_e' new_e'' new_e;
  new_e, new_hybrid_mem

let apply_optimizations_expr f f_expr =
  let new_f = if f.optim_cst_prop then propagate_cst f f_expr else f_expr in (* CONSTANT PROPAGATION *)
  let new_f = if f.optim_rebase then rebase_expr f new_f else new_f in (* REBASE OPTIM *)
  let new_f' = if f.optim_row then read_over_write f new_f else new_f in (* READ over WRITE OPTIM *)
  let new_f' = if f.optim_cst_prop then propagate_cst f new_f' else new_f' in (* CONSTANT PROPAGATION *)
  let new_f'' = if f.optim_rowplus then read_over_write_hybrid f new_f' else new_f' in(* ROW Hybrid OPTIM *)
  let new_f = if f.optim_cst_prop then propagate_cst f new_f'' else new_f'' in (* CONSTANT PROPAGATION #2 *)
  if not (Formula.equal_bl_term new_f' new_f) then
    print_simplifications print_bl_term new_f' new_f'' new_f;
  new_f

let apply_optimizations_expr_list f l =
  List.fold_left (fun acc i ->
      let open Formula in
      let new_e = apply_optimizations_expr f i in
      match new_e.bl_term_desc with
      | BlTrue -> acc
      | _ -> i::acc
    ) [] l

(* ------- Memory functions ------- *)
let store_memory f ?(constraints=[]) mem_f =
  let open Formula in
  let new_name = "memory" ^ (string_of_int (get_varindex f "memory")) in
  let new_memory = ax_var new_name f.addr_size 8 in
  let new_mem, new_hybrid_mem = apply_optimizations_abvexpr f new_name mem_f in
  let constraints = apply_optimizations_expr_list f constraints in
  let newdef = mk_ax_def new_memory [] new_mem in
  let newoptimap = SymbVar.add new_name (f.global_counter, (newdef,constraints)) f.optim_map in
  let opu, opb, slts =
    List.fold_left (fun (opua,opba,sltsa) i ->
        let stats = bl_term_stats i in
        (opua+stats.unop,opba+stats.bnop,sltsa+stats.select))
      (0,0,0) constraints in
  let path =
    List.fold_left
      (fun seq bl -> Formula.push_front_assert bl seq)
      (Formula.push_front_define newdef f.path) constraints
  in
  {f with memory = Formula.mk_ax_var new_memory;
          varsindex=inc_varindex f "memory";
          path;
          nb_store=f.nb_store+1;
          nb_op=f.nb_op+opu+opb;
          nb_load=f.nb_load+slts;
          global_counter=f.global_counter+1;
          optim_map=newoptimap;
          nb_constraint=f.nb_constraint+(List.length constraints);
          hybrid_memory=new_hybrid_mem;
  }
(* --------------------------------- *)

(* ------- Variables functions ------ *)
let add_symbolic_variable f (name:string) ?(restrict_of=0) (low:int) (high:int) =
  Logger.debug ~level:2 "Create var %s{%d,%d}" name low high;
  let open Formula in
  let size = high-low+1 in
  let var = if restrict_of != 0 then bv_var name restrict_of else bv_var name size in
  let new_var = mk_bv_var var in
  let input = BvVar var in
  (* Create a new bitvector variable **declaration** as symbolic input *)
  let new_input = VarSet.add input f.inputs in
  let low, high = if restrict_of != 0 then 0,(restrict_of-1) else low, high in
  Logger.debug ~level:2 "Create var %s{%d,%d} := %s" name low high (print_bv_term new_var);
  (* Add it to the map *)
  let new_vars = SymbVar.add name (low, high, new_var) f.vars in
  let new_vari = SymbVar.add name 0 f.varsindex in
  (* Return the formula *)
  {f with inputs=new_input; vars=new_vars; varsindex=new_vari; nb_input=f.nb_input+1}


let apply_optimizations_bvexpr f f_expr =
  (* CONSTANTE PROPAGATION *)
  let new_e = if f.optim_cst_prop then propagate_cst_bv f f_expr else f_expr in
  (* REBASE OPTIM *)
  let new_e = if f.optim_rebase then rebase_bvexpr f new_e else new_e in
  (* READ over WRITE OPTIM *)
  let new_e' = if f.optim_row then read_over_write_bv f new_e else new_e in
  (* ROW Hyrbid OPTIM *)
  let new_e'' = if f.optim_rowplus then read_over_write_hybrid_bv f new_e' else new_e' in
  (* CONSTANTE PROPAGATION #2 *)
  let new_e = if f.optim_cst_prop then propagate_cst_bv f new_e'' else new_e'' in
  if not (Formula.equal_bv_term new_e' new_e) then
    print_simplifications print_bv_term new_e' new_e'' new_e;
  new_e



let add_constraint f constr =
  (* Add constraint in path only if it is symbolic(otherwise useless) *)
  let open Formula in
  let new_constr = apply_optimizations_expr f constr in
  let op_unary, op_binary, selects =
    let stats = bl_term_stats new_constr in
    stats.unop, stats.bnop, stats.select
  in
  if is_symbolic_bl_term new_constr
  && not (new_constr.bl_term_desc = BlTrue)
  && new_constr <> f.last_constraint
  then
    {f with path = Formula.push_front_assert new_constr f.path;
            last_constraint = new_constr;
            nb_constraint = f.nb_constraint + 1;
            nb_op = f.nb_op + op_unary + op_binary;
            nb_load = f.nb_load + selects}
  else f
(* Note: The last_constraint optim does not work for constraints attached to variable definition
   since we cannot know in advance if the variable definition will remain in the formula. *)

let change_variable f name fullsize low high ?(constraints=[]) e =
  let open Formula in
  let new_name = name ^ (string_of_int (get_varindex f name)) in
  let new_fullvar =
    mk_bv_var (bv_var new_name fullsize) in
  let exists = SymbVar.mem name f.vars in
  let l_old, h_old, var_f =
    if exists then SymbVar.find name f.vars
    else
      (Logger.debug ~level:2 "%s[%d]{%d,%d} does not exist add it.." name fullsize low high;
       0, fullsize-1, mk_bv_var (bv_var name fullsize))
  in
  let _new_vars = SymbVar.add name (l_old, h_old, new_fullvar) f.vars in
  let new_e =
    if l_old = low && h_old = high then e
    else if l_old <= low && high <= h_old then
      let tmp_e = if high < h_old
        then mk_bv_concat (mk_bv_extract {Interval.lo = high+1; Interval.hi=h_old} var_f) e
        else e in
      let tmp_e = if low > l_old
        then mk_bv_concat tmp_e (mk_bv_extract {Interval.lo=l_old; Interval.hi=low-1} var_f)
        else tmp_e in
      tmp_e
    else
      (pp_vars f;
       failwith (Printf.sprintf "disjoint variables %s store{%d,%d} old{%d,%d}\n"
                   name low high l_old h_old))
  in
  let new_e = apply_optimizations_bvexpr f new_e in
  match new_e.bv_term_desc with
  | BvCst _
  | BvFun _ when f.optim_eq_prop ->
    (* let _:unit = Printf.sprintf "Internalize %s:%s"  name (print_bv_term new_e) |> Logger.debug 0 in *)
    let new_vars = SymbVar.add name (l_old, h_old, new_e) f.vars in
    let new_f = List.fold_left (fun acc i -> add_constraint acc i) f constraints in
    {new_f with vars=new_vars}
  | _ ->
    let new_name = name ^ (string_of_int (get_varindex f name)) in
    let new_fullvar = bv_var new_name fullsize in
    let new_var = (l_old, h_old, mk_bv_var new_fullvar) in
    let newlet = mk_bv_def new_fullvar [] new_e in
    let newoptimap = SymbVar.add new_name (f.global_counter,(newlet,constraints)) f.optim_map in
    let gc = f.global_counter + 1 in
    let op_unary, op_binary, selects =
      let stats = bv_term_stats new_e in
      stats.unop, stats.bnop, stats.select
    in
    let opu, opb, slts =
      List.fold_left
        (fun (opua,opba,sltsa) i ->
           let stats = bl_term_stats i in
           opua + stats.unop, opba + stats.bnop,sltsa + stats.select)
        (0,0,0) constraints in
    let nbop = f.nb_op + op_unary + op_binary + opu + opb in
    let nbcst = f.nb_constraint + (List.length constraints) in
    let nbload = f.nb_load + selects + slts in
    let path =
      List.fold_left
        (fun seq bl -> Formula.push_front_assert bl seq)
        (Formula.push_front_define newlet f.path) constraints
    in
    if not exists && fullsize <> high - low + 1 then
      let tmp = add_symbolic_variable f name ~restrict_of:fullsize low high in
      { tmp with vars = SymbVar.add name new_var tmp.vars;
                 varsindex = inc_varindex f name;
                 path;
                 nb_let=f.nb_let+1;
                 nb_op = nbop; nb_load = nbload;
                 optim_map = newoptimap;
                 nb_constraint=nbcst;
                 global_counter=gc }
    else
      { f with vars = SymbVar.add name new_var f.vars;
               varsindex = inc_varindex f name;
               path;
               nb_let=f.nb_let+1;
               nb_op=nbop;
               nb_load=nbload;
               optim_map=newoptimap;
               nb_constraint=nbcst;
               global_counter=gc }


(* Create a new variable name *)
let new_tmpvar f size =
  let open Formula in
  let index = get_varindex f "tmp" in
  let new_varid = "tmp" ^ (string_of_int index) in
  let new_mem_var = bv_var new_varid size in
  { f with varsindex = inc_varindex f "tmp" }, mk_bv_var new_mem_var

let new_variable_name f (name:string): string * formula =
  let new_name = name^(string_of_int (get_varindex f name)) in
  new_name, {f with varsindex=inc_varindex f name}

let add_symbolic_input f (name:string) (size:int) =
  { f with inputs = Formula.(VarSet.add (BvVar (bv_var name size))) f.inputs ;
           nb_input = f.nb_input + 1}

let add_comment f comment =
  { f with path = Formula.push_front_comment comment f.path }


let add_initial_state f (init_st:int Basic_types.Addr64.Map.t) =
  let newpath = Formula.push_front_comment "Initial state" f.path in
  let path =
    Basic_types.Addr64.Map.fold
      (fun key value seq ->
         let open Formula in
         Formula.push_front_assert
           (mk_bv_equal
              (mk_select 1 f.memory
                 (mk_bv_cst (Bitvector.create (Bigint.big_int_of_int64 key) f.addr_size)))
              (mk_bv_cst (Bitvector.create (Bigint.big_int_of_int value) 8)))
           seq)
      init_st newpath in
  { f with path }


let get_var_or_create f (name:string) (fullsize:int) (lo:int) (hi:int) =
  let open Formula in
  match SymbVar.find name f.vars with
  | l, h, varf when (l = lo && h = hi) ->
    f, varf
  | l, h, varf when (l <= lo && hi <= h) ->
    f, mk_bv_extract {Interval.lo; Interval.hi} varf
  | _x ->
    failwith (Printf.sprintf "invalid variable size %s{%d,%d}" name lo hi)
  | exception Not_found ->
    let new_name,new_f = new_variable_name f name in
    let new_name = if new_name = name^"0" then name else new_name in
    let rest = if hi-lo+1 = fullsize then 0 else fullsize in
    let new_f = add_symbolic_variable new_f new_name ~restrict_of:rest lo hi in
    let new_var =
      if rest = 0
      then mk_bv_var (bv_var new_name (hi-lo+1))
      else mk_bv_extract {Interval.lo; Interval.hi} (mk_bv_var (bv_var new_name rest))
    in
    new_f, new_var


(* ------------------ For formula building ---------------- *)
let prune_useless_path_entries  _ ?(pruning=true) path pred =
  (* Warning: for this function to work path should iterated from end->begin.
   * It is the case by default as items are pushed on the head) *)
  if Formula_options.Flatten_memory.get ()
  then Errors.not_yet_implemented "flatten_memory"
  else
    let open Formula in
    let new_path,all_vars,kept_vars =
      Formula.fold_backward
        (fun item (pth,all,kpt as acc) ->
           match item.entry_desc with
           | Declare dc ->
             let var,name =
               (match dc.decl_desc with
                | BlDecl (v,_) -> BlVar v,v.bl_name
                | BvDecl (v,_) -> BvVar v,v.bv_name
                | AxDecl (v,_) -> AxVar v,v.ax_name)
             in
             if VarSet.mem var all || not pruning then
               Formula.push_back item path,all,String.Set.add name kpt
             else acc
           | Define df ->
             let var,name,vars =
               (match df.def_desc with
                | BlDef (v,_,bl) -> BlVar v,v.bl_name, bl_term_variables bl
                | BvDef (v,_,bv) -> BvVar v,v.bv_name, bv_term_variables bv
                | AxDef (v,_,ax) -> AxVar v,v.ax_name, ax_term_variables ax)
             in
             if VarSet.mem var all || not pruning then
               Formula.push_back item pth, VarSet.union all vars, String.Set.add name kpt
             else acc
           | Assert bl | Assume bl ->
             Formula.push_back item pth,
             VarSet.union all (bl_term_variables bl),
             kpt
           | Comment _ -> Formula.push_back item pth,all,kpt)
        path (Formula.empty, bl_term_variables pred, String.Set.empty)
    in
    let all_vars = to_stringmap all_vars in
    Logger.debug ~level:2 "Path:%d Vars:%d Kept:%d"
      (Formula.length new_path) (String.Set.cardinal all_vars) (String.Set.cardinal kept_vars);
    new_path, all_vars, kept_vars, VarSet.empty

let prune_useless_path_entries_prek inputs (ksteps:int) ?(pruning=true) path
    pred =
  (* Put directly all inputs so that they will be ignored in the no_pending_vars function *)
  let new_path,all_vars,kept_vars =
    let open Formula in
    let pth = ref Formula.empty in
    let all = ref (VarSet.union inputs (bl_term_variables pred)) in
    let kpt = ref String.Set.empty in
    let cnt = ref 0 in
    (try
       Formula.iter_backward
         (fun item ->
            if ksteps <> 0 && !cnt >= ksteps
            then
              (Logger.debug ~level:1 "limit reached.." ;
               raise Exit_path_predicate);
            incr cnt;
            match item.entry_desc with
            | Declare dc ->
              let var,name =
                (match dc.decl_desc with
                 | BlDecl (v,_) -> BlVar v,v.bl_name
                 | BvDecl (v,_) -> BvVar v,v.bv_name
                 | AxDecl (v,_) -> AxVar v,v.ax_name)
              in
              if VarSet.mem var !all || not pruning then
                (pth := Formula.push_back item !pth;
                 kpt := String.Set.add name !kpt)
            | Define df ->
              let var,name,vars =
                (match df.def_desc with
                 | BlDef (v,_,bl) -> BlVar v,v.bl_name, bl_term_variables bl
                 | BvDef (v,_,bv) -> BvVar v,v.bv_name, bv_term_variables bv
                 | AxDef (v,_,ax) -> AxVar v,v.ax_name, ax_term_variables ax)
              in
              if VarSet.mem var !all || not pruning then
                (pth := Formula.push_back item !pth;
                 all := VarSet.union !all vars;
                 kpt := String.Set.add name !kpt)
            | Assert bl | Assume bl ->
              pth := Formula.push_back item !pth;
              all  := VarSet.union !all (bl_term_variables bl)
            | Comment _ -> pth := Formula.push_back item !pth)
         path
     with Exit_path_predicate -> ());
    !pth, !all, !kpt
  in
  let all_vars = to_stringmap all_vars in
  Logger.debug ~level:2 "Path:%d Vars:%d Kept:%d"
    (Formula.length new_path) (String.Set.cardinal all_vars) (String.Set.cardinal kept_vars);
  new_path, all_vars, kept_vars, Formula.VarSet.empty


let prune_useless_inputs inputs (vars:String.Set.t) (pushed:String.Set.t) =
  Formula.VarSet.filter
    (fun i ->
       let name = Formula_utils.var_name i in
       String.Set.mem name vars && not(String.Set.mem name pushed))
    inputs

let get_missing_path_entries f (vars:String.Set.t) (kept:String.Set.t) =
  let inputs = to_stringmap f.inputs in
  (* Vars to backtrack in optim_map *)
  let missing_vars =
    String.Set.diff vars
      (String.Set.union kept (String.Set.union f.pushed_variable inputs))
  in
  let rec recurse allvars lets current_vars k =
    let open Formula in
    let lli =
      String.Set.fold
        (fun i acc ->
           if SymbVar.mem i f.optim_map
           then (SymbVar.find i f.optim_map)::acc
           else acc)
        current_vars []
    in (* get all lets *)
    let newvars =
      List.fold_left (fun acc (_, (df, csts)) ->
          (* Visit all lets to gather all the vars into them *)
          VarSet.union acc
            (List.fold_left
               (fun acc c -> VarSet.union acc (bl_term_variables c))
               (match df.def_desc with
                | BlDef (_,_,bl) -> bl_term_variables bl
                | BvDef (_,_,bv) -> bv_term_variables bv
                | AxDef (_,_,ax) -> ax_term_variables ax)
               csts))
        VarSet.empty lli
    in
    let newvars = to_stringmap newvars in
    (* vars that also need to be backtracked *)
    let remaining =
      String.Set.diff newvars
        (String.Set.union f.pushed_variable
           (String.Set.union current_vars
              (String.Set.union allvars inputs))) in
    (* Keep all vars so that we can filter inputs later on *)
    let all = String.Set.union allvars
        (String.Set.union current_vars newvars) in
    (* Return if there is no any other vars to backtrack *)
    let lets = lli @ lets in
    if String.Set.is_empty remaining then all, lets
    else begin
      Logger.debug ~level:2
        "recurse %d [%s]"
        k (String.Set.fold (fun acc i -> acc^" "^i) remaining "");
      recurse all lets remaining (k + 1)
    end
  in
  let allvars, lets = recurse vars [] missing_vars 0 in
  let final_lets =
    List.fold_left
      (fun seq (_, (df,ls)) ->
         List.fold_left
           (fun seq bl -> Formula.push_front_assert bl seq)
           (Formula.push_front_define df seq) ls)
      Formula.empty
      (List.sort (fun (i1,_) (i2,_) -> compare i1 i2) lets)
  in
  final_lets, allvars, f.inputs

let get_missing_path_entries_prek f (vars:String.Set.t) (kept:String.Set.t):
  path_t * String.Set.t * input_t =
  let inputs = ref f.inputs in
  let missing_vars = String.Set.diff vars (String.Set.union kept (String.Set.union f.pushed_variable (to_stringmap f.inputs))) in
  Logger.debug ~level:1
    "prek missing path entries: card kept:%d card missing:%d"
    (String.Set.cardinal kept) (String.Set.cardinal missing_vars);
  (* let vars_seen = ref String.Map.empty in *)
  let additional_vars = ref String.Set.empty in
  let lets =
    String.Set.fold
      (fun e acc ->
         let open Formula in
         let name = e in
         additional_vars := String.Set.add name !additional_vars;
         if VarSet.mem (BvVar (bv_var name 0)) !inputs ||
            VarSet.mem (AxVar (ax_var name 0 0)) !inputs
         then acc (* Does nothing if the var is already in inputs *)
         else begin
           Logger.warning ~level:2 "%s not found in inputs so add it [in optim_map:%b]"
             e (SymbVar.mem e f.optim_map);
           try
             let _,(df,_) = SymbVar.find e f.optim_map in
             (match df.def_desc with
              | BlDef (_,_,_) ->
                inputs := VarSet.add (BlVar (bl_var name)) !inputs; acc
              | BvDef (v,_,_) ->
                inputs := VarSet.add (BvVar (bv_var name v.bv_size)) !inputs; acc
              | AxDef (v,_,_) ->
                inputs := VarSet.add (AxVar (ax_var name v.idx_size v.elt_size)) !inputs;
                additional_vars := String.Set.add (base_varname name) !additional_vars;
                (* Link all the memoriy names on the same memory *)
                Formula.push_front_define
                  (mk_ax_def (ax_var e v.idx_size v.elt_size) []
                     (mk_ax_var (ax_var (base_varname name) v.idx_size v.elt_size)))
                  acc)
           with Not_found ->
             let _,_,sz = X86Util.reg_to_extract e in
             inputs := VarSet.add (BvVar (bv_var name (sz+1))) !inputs;
             acc(* Not so sure it should be ignored *)
         end
      ) missing_vars Formula.empty in
  lets, String.Set.union vars !additional_vars, !inputs

let append_to_file (file:string) (content:string): unit =
  let fd = open_out_gen [Open_append;Open_wronly;Open_text] 644 file in
  output_string fd content;
  close_out fd

let build_formula f predicate
    ?(ksteps=0) ?(forward=true) ?(pruning=true) ?(push=true) ?(dump="")
    (chan:out_channel)  =
  let open Formula in
  (* Generate the file of the formula *)
  let write_string s =
    Printf.fprintf chan "%s\n" s;
    if dump <> "" then append_to_file dump (s^"\n")
  in
  let predicate = apply_optimizations_expr f predicate in
  let pruning_function =
    if forward
    then prune_useless_path_entries
    else (prune_useless_path_entries_prek f.inputs) in
  (* Prune lets, and get all variables encountered with vars of lets kept *)
  let filtered_path, vars, vars_kept,inputs_additional =
    pruning_function ~pruning ksteps f.path predicate in
  let missing_path_f =
    if forward
    then get_missing_path_entries
    else get_missing_path_entries_prek in
  (* Get a set of the missing variable *)
  let missing_lets, vars, new_inputs = missing_path_f f vars vars_kept in
  let inputs = prune_useless_inputs new_inputs vars f.pushed_variable in
  let filtered_path = Formula.append missing_lets filtered_path in
  let nblets, nbcsts, vars_kept =
    Formula.fold_forward
      (fun i (num,numc,vars) ->
         match i.Formula.entry_desc with
         | Formula.Declare _
         | Formula.Comment _ ->
           num, numc, vars
         | Formula.Assert _ | Formula.Assume _ -> num, numc+1, vars
         | Formula.Define df ->
           let name = Formula_utils.def_name df in
           1 + num, numc, String.Set.add name vars)
      filtered_path (0, 0, String.Set.empty)
  in
  let inputs =
    if Formula_options.Flatten_memory.get ()
    then Formula.VarSet.union inputs inputs_additional
    else inputs in
  (* Write all symbolic input (free variables) *)
  write_string (Printf.sprintf "%s\n" (print_varset inputs));
  let onetime_comment = ref "" in
  let write_com_if_exist () =
    if !onetime_comment <> "" then
      (write_string (Printf.sprintf "\n; ------[ %s ]------" !onetime_comment);
       onetime_comment := "") in
  Formula.iter_forward
    (fun item ->
       match item.entry_desc with
       | Declare dc ->
         write_com_if_exist ();
         write_string
           (match dc.decl_desc with
            | BlDecl (v,_) ->
              Printf.sprintf "(declare-fun %s () Bool)"
                v.bl_name
            | BvDecl (v,_) ->
              Printf.sprintf "(declare-fun %s () (_ BitVec %d))"
                v.bv_name v.bv_size
            | AxDecl (v,_) ->
              Printf.sprintf "(declare-fun %s () (Array (_ BitVec %d) (_ BitVec %d)))"
                v.ax_name v.idx_size v.elt_size)
       | Define df ->
         write_com_if_exist ();
         write_string
           (match df.def_desc with
            | BlDef (v,_,bl) ->
              Printf.sprintf "(define-fun %s () Bool %s)"
                v.bl_name (print_bl_term bl)
            | BvDef (v,_,bv) ->
              Printf.sprintf "(define-fun %s () (_ BitVec %d) %s)"
                v.bv_name v.bv_size (print_bv_term bv)
            | AxDef (v,_,ax) ->
              Printf.sprintf "(define-fun %s () (Array (_ BitVec %d) (_ BitVec %d)) %s)"
                v.ax_name v.idx_size v.elt_size (print_ax_term  ax))
       | Assert bl | Assume bl ->
         write_com_if_exist ();
         write_string (Printf.sprintf "(assert %s)" (print_bl_term bl))
       | Comment s ->
         write_com_if_exist ();
         onetime_comment := s
    )
    filtered_path;
  if push then write_string "\n(push 1)\n";
  write_string (Printf.sprintf "(assert %s)\n" (print_bl_term predicate));
  let uop, bop, ld, st =
    Formula.fold_forward
      (fun entry (uop,bop,ld,st as acc) ->
         match entry.entry_desc with
         | Declare _ | Comment _ -> acc
         | Define df ->
           let stats =
             match df.def_desc with
             | BlDef (_,_,bl) -> bl_term_stats bl
             | BvDef (_,_,bv) -> bv_term_stats bv
             | AxDef (_,_,ax) -> ax_term_stats ax
           in
           uop+stats.unop,bop+stats.bnop,ld+stats.select,st+stats.store
         | Assert bl | Assume bl ->
           let stats = bl_term_stats bl in
           uop+stats.unop,bop+stats.bnop,ld+stats.select,st+stats.store)
      filtered_path (0,0,0,0)
  in
  let pushed =
    String.Set.union
      f.pushed_variable
      (String.Set.union vars_kept (to_stringmap inputs)) in
  let newinput = if pruning then f.inputs else Formula.VarSet.empty in
  let nbin = Formula.VarSet.cardinal inputs in
  let new_f =
    {f with pushed_variable=pushed; path=Formula.empty; inputs=newinput;
            nb_let=nblets; nb_load=ld; nb_store=st;
            nb_op=uop+bop; nb_input=nbin; nb_constraint=nbcsts}
  in
  new_f, None


let build_formula_file f pred
    ?(ksteps=0) ?(forward=true) ?(pruning=true) (name:string) =
  (* Generate the file of the formula *)
  let file = open_out name in
  Printf.fprintf file "%s\n\n" (print_header ());
  (* Write all symbolic input (free variables) *)
  let newf, status =
    build_formula f pred ~pruning ~push:false ~ksteps ~forward file in
  close_out file;
  newf, status

let build_formula_incremental f pred
    ?(ksteps=0) ?(forward=true) ?(pruning=true) ?(push=true)
    (session:solver_session) =
  build_formula f pred ~pruning ~push ~ksteps ~forward session.stdin
(* ------------------------------------------------------- *)


(* ------------- Stats functions ------------ *)
let get_stat_formula f: (int * int * int * int * int * int) =
  f.nb_input, f.nb_load, f.nb_store, f.nb_let, f.nb_op, f.nb_constraint

let pp_stat_formula f: unit =
  let i, ld, st, le, op, cst = get_stat_formula f in
  Logger.result
    "Inputs:%d  Load:%d  Store:%d  Vars:%d  Ops:%d  Constraints:%d@."
    i ld st le op cst
(* --------------------------------- *)
