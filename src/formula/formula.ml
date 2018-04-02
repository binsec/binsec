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

open Smtlib2print
open Solver
open Formula_type
open Formula_optim
open Common_piqi
open Smtlib2
open Smtlib2_visitor
open Basic_types

let last_k = ref 0

exception Exit_path_predicate

let pp_vars (f:formula): unit =
  Logger.result "@[<v 0>|------ Symbolic Store ------|@ ";
  SymbVar.iter
    (fun name (i,j,f) ->
       Logger.result "%s{%i, %i} = %s@ " name i j (smtbvexpr_to_string f))
    f.vars;
  Logger.result "|----------------------------|@]@."

let optim_str (f:formula): string =
  let s = if f.optim_cst_prop then "C" else "" in
  let s = if f.optim_rebase then s^"R" else s in
  let s = if f.optim_row then s^"W" else s in
  let s = if f.optim_rowplus then s^"H" else s in
  let s = if f.optim_eq_prop then s^"E" else s in
  if s = "" then "NONE" else s

let base_varname s =
  Str.string_match (Str.regexp "^[a-zA-Z]*") s 0 |> ignore;
  Str.matched_string s

let _reset_symbvar (f:formula): formula =
  {f with vars = SymbVar.empty}

let inc_varindex (f:formula) (name:string): int SymbVar.t =
  try
    let value = SymbVar.find name f.varsindex in
    SymbVar.add name (value+1) f.varsindex
  with Not_found ->
    SymbVar.add name 1 f.varsindex

let get_varindex (f:formula) (name:string): int =
  try
    SymbVar.find name f.varsindex
  with Not_found -> 0

let contains_variable (f:formula) (name:string) =
  SymbVar.mem name f.vars

let print_simplifications f p1 p2 p3 =
  Logger.warning ~level:2 "@[<v 0>⎧in:[%s]@ ⎪row:[%s]@ ⎩out:[%s]@]"
    (String_utils.remove_newline (f p1))
    (String_utils.remove_newline (f p2))
    (String_utils.remove_newline (f p3))

let apply_optimizations_abvexpr (f:formula) (new_name:string) (f_expr:smt_abv_expr) : smt_abv_expr * hybrid_mem_t =
  (* CONSTANT PROPAGATION *)
  let new_e = if f.optim_cst_prop then propagate_cst_abv f f_expr else f_expr in
  let new_e = if f.optim_rebase then rebase_abvexpr f new_e else new_e in (* REBASE OPTIM *)
  let new_e' = if f.optim_row then read_over_write_abv f new_e else new_e in (* READ over WRITE  *)
  let new_e = if f.optim_rowplus then read_over_write_hybrid_abv f new_e' else new_e' in (* ROW Hyrbid OPTIM *)
  let new_e = if f.optim_cst_prop then propagate_cst_abv f new_e else new_e in (* CONSTANTE PROPAGATION #2 *)
  let new_hybrid_mem, new_e'' = if f.optim_rowplus then update_hybrid_memory f new_name new_e else f.hybrid_memory, new_e in (* ROW Hybrid *)
  let new_e = if f.optim_cst_prop then propagate_cst_abv f new_e'' else new_e'' in (* CONSTANTE PROPAGATION #3 *)
  if not(abvexpr_equal new_e' new_e) then
    print_simplifications (smtabvexpr_to_string ~inline:false) new_e' new_e'' new_e;
  new_e, new_hybrid_mem

let apply_optimizations_expr (f:formula) (f_expr:smt_expr): smt_expr =
  let new_f = if f.optim_cst_prop then propagate_cst f f_expr else f_expr in (* CONSTANT PROPAGATION *)
  let new_f = if f.optim_rebase then rebase_expr f new_f else new_f in (* REBASE OPTIM *)
  let new_f' = if f.optim_row then read_over_write f new_f else new_f in (* READ over WRITE OPTIM *)
  let new_f' = if f.optim_cst_prop then propagate_cst f new_f' else new_f' in (* CONSTANT PROPAGATION *)
  let new_f'' = if f.optim_rowplus then read_over_write_hybrid f new_f' else new_f' in(* ROW Hybrid OPTIM *)
  let new_f = if f.optim_cst_prop then propagate_cst f new_f'' else new_f'' in (* CONSTANT PROPAGATION #2 *)
  if not(expr_equal new_f' new_f) then
    print_simplifications (smtexpr_to_string ~inline:false) new_f' new_f'' new_f;
  new_f

let apply_optimizations_expr_list (f:formula) (l:smt_expr list): smt_expr list =
  List.fold_left (fun acc i ->
      let new_e = apply_optimizations_expr f i in
      match new_e with
      | SmtTrue -> acc
      | _ -> i::acc
    ) [] l

(* ------- Memory functions ------- *)
let store_memory (f:formula) ?(constraints=[]) (mem_f:smt_abv_expr): formula =
  let new_name = "memory" ^ (string_of_int (get_varindex f "memory")) in
  let new_memory = SmtABvArray(new_name, f.addr_size, 8) in
  let new_mem, new_hybrid_mem = apply_optimizations_abvexpr f new_name mem_f in
  let constraints = apply_optimizations_expr_list f constraints in
  let newdef = VarDefinition(SmtABvArrayExpr new_memory, SmtABvArrayExpr new_mem, constraints) in
  let newoptimap = SymbVar.add new_name (f.global_counter,(SmtABvArrayExpr new_memory, SmtABvArrayExpr new_mem, constraints)) f.optim_map in
  let opu, opb, slts = List.fold_left (fun (opua,opba,sltsa) i -> let _,_, a,b,c,_ = stat_expr i in (opua+a,opba+b,sltsa+c)) (0,0,0) constraints in
  {f with memory=new_memory;
          varsindex=inc_varindex f "memory";
          path=newdef::f.path;
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
let add_symbolic_variable (f:formula) (name:string) ?(restrict_of=0) (low:int) (high:int): formula =
  Logger.debug ~level:2 "Create var %s{%d,%d}" name low high;
  let size = high-low+1 in
  let new_var = if restrict_of != 0 then SmtBvVar(name, restrict_of) else SmtBvVar(name, size) in
  let input = if restrict_of != 0 then SmtBv(name, restrict_of) else SmtBv(name, size) in
  let new_input = SmtVarSet.add input f.inputs in  (* Create a new bitvector variable **declaration** as symbolic input *)
  let low, high = if restrict_of != 0 then 0,(restrict_of-1) else low, high in
  Logger.debug ~level:2 "Create var %s{%d,%d} := %s" name low high (smtbvexpr_to_string new_var);
  let new_vars = SymbVar.add name (low, high, new_var) f.vars in (* Add it to the map *)
  let new_vari = SymbVar.add name 0 f.varsindex in
  {f with inputs=new_input; vars=new_vars; varsindex=new_vari; nb_input=f.nb_input+1}   (* Return the formula *)


let apply_optimizations_bvexpr (f:formula) (f_expr:smt_bv_expr): smt_bv_expr =
  let new_e = if f.optim_cst_prop then propagate_cst_bv f f_expr else f_expr in (* CONSTANTE PROPAGATION *)
  let new_e = if f.optim_rebase then rebase_bvexpr f new_e else new_e in (* REBASE OPTIM *)
  let new_e' = if f.optim_row then read_over_write_bv f new_e else new_e in (* READ over WRITE OPTIM *)
  let new_e'' = if f.optim_rowplus then read_over_write_hybrid_bv f new_e' else new_e' in (* ROW Hyrbid OPTIM *)
  let new_e = if f.optim_cst_prop then propagate_cst_bv f new_e'' else new_e'' in (* CONSTANTE PROPAGATION #2 *)
  if not (bvexpr_equal new_e' new_e) then
    print_simplifications (smtbvexpr_to_string ~inline:false) new_e' new_e'' new_e;
  new_e



let add_constraint (f:formula) (constr:smt_expr): formula =
  (* Add constraint in path only if it is symbolic(otherwise useless) *)
  let new_constr = apply_optimizations_expr f constr in
  let _, _, op_unary, op_binary, selects, _ = stat_expr new_constr in
  if is_symbolic_expr new_constr && not (smtexpr_is_true new_constr)
     && new_constr <> f.last_constraint
  then
    {f with path = Constraint new_constr :: f.path;
            last_constraint = new_constr;
            nb_constraint = f.nb_constraint + 1;
            nb_op = f.nb_op + op_unary + op_binary;
            nb_load = f.nb_load + selects}
  else
    f
(* Note: The last_constraint optim does not work for constraints attached to variable definition
   since we cannot know in advance if the variable definition will remain in the formula. *)

let change_variable (f:formula) (name:string) (fullsize:int) (low:int) (high:int) ?(constraints=[]) (e:smt_bv_expr): formula =
  let new_name = name ^ (string_of_int (get_varindex f name)) in
  let new_fullvar = SmtBvVar(new_name, fullsize) in
  let exists = SymbVar.mem name f.vars in
  let l_old, h_old, var_f =
    if exists then SymbVar.find name f.vars
    else
      (Logger.debug ~level:2 "%s[%d]{%d,%d} does not exist add it.." name fullsize low high;
       0, fullsize-1, SmtBvVar(name, fullsize))
  in
  let _new_vars = SymbVar.add name (l_old, h_old, new_fullvar) f.vars in
  let new_e =
    if l_old = low && h_old = high then e
    else if l_old <= low && high <= h_old then
      let tmp_e = if high < h_old then SmtBvBinary(SmtBvConcat, SmtBvUnary(SmtBvExtract(high+1,h_old),var_f), e) else e in
      let tmp_e = if low > l_old then SmtBvBinary(SmtBvConcat, tmp_e,  SmtBvUnary(SmtBvExtract(l_old,low-1),var_f)) else tmp_e in
      tmp_e
    else
      (pp_vars f;
       failwith (Printf.sprintf "disjoint variables %s store{%d,%d} old{%d,%d}\n" name low high l_old h_old))
  in
  let new_e = apply_optimizations_bvexpr f new_e in
  match new_e with
  | SmtBvCst(_)
  | SmtBvVar(_) when f.optim_eq_prop ->
    (* let _:unit = Printf.sprintf "Internalize %s:%s"  name (smtbvexpr_to_string new_e) |> Logger.debug 0 in *)
    let new_vars = SymbVar.add name (l_old, h_old, new_e) f.vars in
    let new_f = List.fold_left (fun acc i -> add_constraint acc i) f constraints in
    {new_f with vars=new_vars}
  | _ ->
    let new_name = name ^ (string_of_int (get_varindex f name)) in
    let new_fullvar = SmtBvVar(new_name, fullsize) in
    let new_var = (l_old, h_old, new_fullvar) in
    let newlet = VarDefinition(SmtBvExpr new_fullvar, SmtBvExpr new_e, constraints) in
    let newoptimap = SymbVar.add new_name (f.global_counter,(SmtBvExpr new_fullvar, SmtBvExpr new_e, constraints)) f.optim_map in
    let gc = f.global_counter + 1 in
    let _, _, op_unary, op_binary, selects, _ = stat_bvexpr new_e in
    let opu, opb, slts = List.fold_left (fun (opua,opba,sltsa) i -> let _,_, a,b,c,_ = stat_expr i in (opua+a,opba+b,sltsa+c)) (0,0,0) constraints in
    let nbop = f.nb_op+op_unary+op_binary+opu+opb in
    let nbcst = f.nb_constraint+(List.length constraints) in
    let nbload = f.nb_load+selects+slts in
    if not(exists) && fullsize != (high-low+1) then
      let tmp = add_symbolic_variable f name ~restrict_of:fullsize low high in
      { tmp with vars=SymbVar.add name new_var tmp.vars; varsindex=inc_varindex f name; path=newlet::f.path; nb_let=f.nb_let+1; nb_op=nbop; nb_load=nbload; optim_map=newoptimap; nb_constraint=nbcst; global_counter=gc}
    else
      {f with vars=SymbVar.add name new_var f.vars; varsindex=inc_varindex f name; path=newlet::f.path; nb_let=f.nb_let+1; nb_op=nbop; nb_load=nbload; optim_map=newoptimap; nb_constraint=nbcst;global_counter=gc}



let new_tmpvar (f:formula) (size:int) : formula * smt_bv_expr =  (* Create a new variable name *)
  let index = get_varindex f "tmp" in
  let new_varid = "tmp" ^ (string_of_int index) in
  let new_mem_var = SmtBvVar(new_varid, size) in
  {f with varsindex=inc_varindex f "tmp"}, new_mem_var

let new_variable_name (f:formula) (name:string): string * formula =
  let new_name = name^(string_of_int (get_varindex f name)) in
  new_name, {f with varsindex=inc_varindex f name}

let add_symbolic_input (f:formula) (name:string) (size:int): formula =
  {f with inputs=(SmtVarSet.add (SmtBv(name, size)) f.inputs); nb_input=f.nb_input+1}


let add_comment (f:formula) (comment:string): formula =
  {f with path=(Comment(comment))::f.path}


let add_initial_state (f:formula) (init_st:int Basic_types.Addr64.Map.t): formula =
  let newpath = (Comment("Initial state"))::f.path in
  let newpath = Basic_types.Addr64.Map.fold (fun key value acc ->
      (Constraint(SmtComp(SmtBvExpr(SmtABvSelect(f.memory,
                                                 SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 key) f.addr_size))),
                          SmtBvExpr(SmtBvCst(Bitvector.create (Bigint.big_int_of_int value) 8)))))::acc
    ) init_st newpath in
  {f with path=newpath}


let get_var_or_create (f:formula) (name:string) (fullsize:int) (low:int) (high:int): formula * smt_bv_expr =
  if SymbVar.mem name f.vars then
    let (l, h, var_f) = SymbVar.find name f.vars in
    match (l, h, var_f) with
    | (l, h, varf) when (l = low && h = high) -> f, varf
    | (l, h, varf) when (l <= low && high <= h) -> f, smtbv_extract varf low high
    | _ ->failwith (Printf.sprintf "invalid variable size %s{%d,%d}" name low high)
  else
    let new_name,new_f = new_variable_name f name in
    let new_name = if new_name = name^"0" then name else new_name in
    let rest = if high-low+1 = fullsize then 0 else fullsize in
    let new_f = add_symbolic_variable new_f new_name ~restrict_of:rest low high in
    let new_var = if rest = 0 then SmtBvVar(new_name,(high-low+1)) else SmtBvUnary(SmtBvExtract(low,high),SmtBvVar(new_name, rest)) in
    new_f, new_var


(* ------------------ For formula building ---------------- *)
let prune_useless_path_entries  _ ?(pruning=true) (path:path_t) (pred:smt_expr): path_t * String.Set.t * String.Set.t * SmtVarSet.t =
  (* Warning: for this function to work path should iterated from end->begin.
   * It is the case by default as items are pushed on the head) *)
  let visitor = new get_var_visitor in
  let flattener_visitor = new memory_flattener_visitor in
  visitor#visit_smt_expr pred;                (* visit the predicate to get variable used *)
  let vars_kept = ref String.Set.empty in
  let add_vars exprs = List.iter (fun i -> visitor#visit_smt_expr i) exprs in
  let add_name name = vars_kept := String.Set.add name !vars_kept in
  let new_path =
    List.fold_left (fun acc item ->
        match item with
        | Comment _ -> item::acc
        | VarDefinition(let_var,let_expr,csts) ->
          begin match let_var with
            | SmtBvExpr(SmtBvVar(name,_))
            | SmtABvArrayExpr(SmtABvArray(name,_,_)) ->
              if String.Set.mem name visitor#get_vars || not(pruning) then
                begin
                  add_name name;
                  let let_expr =
                    if !Options.flatten_memory
                    then flattener_visitor#visit_smt_expr let_expr
                    else let_expr
                  in
                  let csts =
                    if !Options.flatten_memory
                    then List.map (fun i -> flattener_visitor#visit_smt_expr i) csts
                    else csts
                  in
                  add_vars (let_expr::csts);
                  VarDefinition(let_var, let_expr, csts)::acc
                end
              else acc
            | _ -> acc
          end
        | Constraint cst ->
          let cst =
            if !Options.flatten_memory
            then flattener_visitor#visit_smt_expr cst
            else cst
          in
          add_vars [cst] ;
          Constraint(cst)::acc
      ) [] path
  in
  Logger.debug ~level:2
    "Path:%d Vars:%d Kept:%d"
    (List.length new_path) (String.Set.cardinal visitor#get_vars)
    (String.Set.cardinal !vars_kept);
  List.rev new_path, visitor#get_vars, !vars_kept, flattener_visitor#get_new_symbols ()

let prune_useless_path_entries_prek (inputs:SmtVarSet.t) (ksteps:int) ?(pruning=true) (path:path_t) (pred:smt_expr): path_t * String.Set.t * String.Set.t * SmtVarSet.t =
  let visitor = new get_var_visitor in
  visitor#visit_smt_expr pred;               (* visit the predicate to get variable used *)
  let vars_kept = ref String.Set.empty in
  Logger.debug ~level:2
    "Path length %d kept:%d"
    (List.length path) (String.Set.cardinal visitor#get_vars);
  let vars_defined = ref (to_stringmap inputs) in
  (* Put directly all inputs so that they will be ignored in the no_pending_vars function *)
  let counter = ref (0-1) in
  let stop_backwarding = ref false in
  let path_len = List.length path in
  let add_vars exprs = List.iter (fun i -> visitor#visit_smt_expr i) exprs in
  let add_name name = vars_kept := String.Set.add name !vars_kept in
  let new_path = ref [] in
  begin try
      List.iter (fun item ->
          if ksteps <> 0 && !counter >= ksteps then
            begin
              Logger.debug ~level:1 "limit reached.." ;
              last_k := ksteps;
              raise Exit_path_predicate
            end
          else
            match item with
            | Comment _ -> counter := !counter + 1 ; new_path := item::!new_path
            | VarDefinition(let_var,let_expr,csts) ->
              begin match let_var with
                | SmtBvExpr(SmtBvVar(name,_))
                | SmtABvArrayExpr(SmtABvArray(name,_,_)) ->
                  if String.Set.mem name visitor#get_vars || not(pruning) then
                    begin
                      add_name name;
                      add_vars (let_expr::csts);
                      vars_defined := String.Set.add name !vars_defined;
                      new_path := item::!new_path
                    end
                  else
                    ()
                | _ -> ()
              end
            | Constraint cst -> add_vars [cst] ; new_path := item::!new_path
        ) path;
    with Exit_path_predicate -> ()
  end;
  if not(!stop_backwarding) then last_k := path_len;
  Logger.debug ~level:2
    "Path:%d Vars:%d Kept:%d"
    (List.length !new_path)
    (String.Set.cardinal visitor#get_vars) (String.Set.cardinal !vars_kept);
  List.rev !new_path, visitor#get_vars, !vars_kept, SmtVarSet.empty


let prune_useless_inputs (inputs:SmtVarSet.t) (vars:String.Set.t) (pushed:String.Set.t): SmtVarSet.t =
  SmtVarSet.filter (fun i ->
      match i with
      | SmtBv(name, _)
      | SmtABv(name, _, _) -> String.Set.mem name vars && not(String.Set.mem name pushed)
    ) inputs

let get_missing_path_entries (f:formula) (vars:String.Set.t) (kept:String.Set.t): path_t * String.Set.t * input_t =
  let inputs = to_stringmap f.inputs in
  let missing_vars = String.Set.diff vars (String.Set.union kept (String.Set.union f.pushed_variable inputs)) in (* Vars to backtrack in optim_map *)
  let visitor = new get_var_visitor in
  let rec recurse allvars lets current_vars k =
    let lli = String.Set.fold (fun i acc -> if SymbVar.mem i f.optim_map then (SymbVar.find i f.optim_map)::acc else acc ) current_vars [] in (* get all lets *)
    let newvars =
      List.fold_left (fun acc (_, (_, e, csts)) ->
          (* Visit all lets to gather all the vars into them *)
          visitor#visit_smt_expr e;
          List.iter (fun c -> visitor#visit_smt_expr c) csts;
          let newvars = visitor#get_vars in
          visitor#clear ();
          String.Set.union acc newvars
        ) String.Set.empty lli
    in
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
  let final_lets = List.map (fun (_, (a, b, c)) -> VarDefinition(a,b,c)) (List.sort (fun (i1,_) (i2,_) -> compare i1 i2) lets) in
  final_lets, allvars, f.inputs

let get_missing_path_entries_prek (f:formula) (vars:String.Set.t) (kept:String.Set.t): path_t * String.Set.t * input_t =
  let inputs = ref f.inputs in
  let missing_vars = String.Set.diff vars (String.Set.union kept (String.Set.union f.pushed_variable (to_stringmap f.inputs))) in
  Logger.debug ~level:1
    "prek missing path entries: card kept:%d card missing:%d"
    (String.Set.cardinal kept) (String.Set.cardinal missing_vars);
  (* let vars_seen = ref String.Map.empty in *)
  let additional_vars = ref String.Set.empty in
  let lets = String.Set.fold (fun e acc ->
      let name = e in
      additional_vars := String.Set.add name !additional_vars;
      if SmtVarSet.mem (SmtBv(name,0)) !inputs || SmtVarSet.mem (SmtABv(name,0,0)) !inputs then
        acc (* Does nothing if the var is already in inputs *)
      else begin
        Logger.warning ~level:2 "%s not found in inputs so add it [in optim_map:%b]" e (SymbVar.mem e f.optim_map);
        try
          match SymbVar.find e f.optim_map with
          | (_, (SmtBvExpr(SmtBvVar(_, sz)),_,_)) ->
            inputs := SmtVarSet.add (SmtBv(name,sz)) !inputs;
            acc(* VarDefinition(SmtBvExpr(SmtBvVar(e,sz)), SmtBvExpr(SmtBvVar(name, sz)), [])::acc *)
          | (_,(SmtABvArrayExpr(SmtABvArray(_, szi, sz)),_,_)) ->
            additional_vars := String.Set.add (base_varname name) !additional_vars;
            inputs := SmtVarSet.add (SmtABv(base_varname name,szi,sz)) !inputs;
            VarDefinition(SmtABvArrayExpr(SmtABvArray(e,szi, sz)), SmtABvArrayExpr(SmtABvArray(base_varname name, szi, sz)), [])::acc
          (* Link all the memoriy names on the same memory *)
        | _ -> Logger.error "%s definitely not found..." e; acc
        with Not_found ->
          let _,_,sz = X86Util.reg_to_extract e in
          inputs := SmtVarSet.add (SmtBv(name,sz+1)) !inputs; acc(* Not so sure it should be ignored *)
      end
    ) missing_vars [] in
  lets, String.Set.union vars !additional_vars, !inputs

let append_to_file (file:string) (content:string): unit =
  let fd = open_out_gen [Open_append;Open_wronly;Open_text] 644 file in
  output_string fd content;
  close_out fd

let build_formula (f:formula) (predicate:smt_expr) ?(ksteps=0) ?(forward=true) ?(pruning=true) ?(push=true) ?(dump="") (chan:out_channel) (solver:solver_t): formula * Smtlib2.smt_result option =     (* Generate the file of the formula *)
  let write_string s =
    Printf.fprintf chan "%s\n" s;
    if dump <> "" then append_to_file dump (s^"\n")
  in
  last_k := ksteps;
  let predicate = apply_optimizations_expr f predicate in
  let pruning_function = if forward then prune_useless_path_entries else (prune_useless_path_entries_prek f.inputs) in
  let filtered_path, vars, vars_kept,inputs_additional = pruning_function ~pruning ksteps f.path predicate in (* Prune lets, and get all variables encountered with vars of lets kept *)
  let missing_path_f = if forward then get_missing_path_entries else get_missing_path_entries_prek in
  let missing_lets, vars, new_inputs = missing_path_f f vars vars_kept in (* Get a set of the missing variable *)
  let inputs = prune_useless_inputs new_inputs vars f.pushed_variable in
  let qed = Qed.create_qed_checker f in
  Qed.qed_check_entry qed (Constraint(predicate));
  List.iter (fun i -> Qed.qed_check_entry qed i) filtered_path;
  let filtered_path = missing_lets@(List.rev filtered_path) in
  let nblets, nbcsts, vars_kept = List.fold_left
      (fun (num,numc,vars) i ->
         match i with
         | Comment _ -> (num, numc, vars)
         | Constraint _ -> (num, numc+1, vars)
         | VarDefinition (i,_,l) ->
           begin match i with
             | SmtBvExpr(SmtBvVar(name,_))
             | SmtABvArrayExpr(SmtABvArray(name,_,_)) -> (1+num, numc+(List.length l), String.Set.add name vars)
             | _ -> (1+num, numc+(List.length l), vars)
           end)
      (0, 0, String.Set.empty) filtered_path
  in
  let inputs = if !Options.flatten_memory then SmtVarSet.union inputs inputs_additional else inputs in
  write_string (Printf.sprintf "%s\n" (smtvarset_to_string inputs)); (* Write all symbolic input (free variables) *)
  let inline = is_boolector solver in
  let visitor = new statistics_visitor in
  let onetime_comment = ref "" in
  let write_com_if_exist () = if not(!onetime_comment = "") then (write_string (Printf.sprintf "\n; ------[ %s ]------" !onetime_comment); onetime_comment := "") in
  let rec aux lets =
    match lets with
    | [] -> ()
    | (Comment s)::tl ->
      onetime_comment := s;
      aux tl
    | (Constraint cst)::tl ->
      write_com_if_exist ();
      visitor#visit_smt_expr cst;
      write_string (Printf.sprintf "(assert %s)" (smtexpr_to_string ~inline cst));
      aux tl
    | (VarDefinition(var, exp, csts))::tl ->
      write_com_if_exist ();
      visitor#visit_smt_expr exp;
      List.iter (fun i -> visitor#visit_smt_expr i) csts;
      let var_s = smtexpr_to_string ~inline var in
      let exp_s = smtexpr_to_string ~inline exp in
      begin match var with
        | SmtBvExpr(SmtBvVar(_, sz)) ->
          write_string (Printf.sprintf "(define-fun %s () (_ BitVec %d) %s)" var_s sz exp_s);
        | SmtABvArrayExpr(SmtABvArray(_, a, c)) ->
          write_string (Printf.sprintf "(define-fun %s () (Array (_ BitVec %d) (_ BitVec %d)) %s)" var_s a c exp_s);
        | _ -> failwith "impossible"
      end;
      List.iter (fun i -> write_string (Printf.sprintf "(assert %s)" (smtexpr_to_string ~inline i))) csts;
      aux tl
  in
  aux filtered_path;
  if push then write_string "\n(push 1)\n";
  write_string (Printf.sprintf "(assert %s)\n" (smtexpr_to_string ~inline predicate));
  let _, _, uop, bop, ld, st = visitor#get_stats in
  let pushed = String.Set.union f.pushed_variable (String.Set.union vars_kept (to_stringmap inputs)) in
  let newinput = if pruning then f.inputs else SmtVarSet.empty in
  let nbin = SmtVarSet.cardinal inputs in
  let new_f =
    {f with pushed_variable=pushed; path=[]; inputs=newinput; nb_let=nblets; nb_load=ld; nb_store=st;
            nb_op=uop+bop; nb_input=nbin; nb_constraint=nbcsts}
  in
  new_f, Qed.qed_get_status qed


let build_formula_file (f:formula) (pred:smt_expr) ?(ksteps=0) ?(forward=true) ?(pruning=true) (name:string) (solver:solver_t): formula * Smtlib2.smt_result option =     (* Generate the file of the formula *)
  let file = open_out name in
  Printf.fprintf file "%s\n\n" (smt_header ()); (* Write all symbolic input (free variables) *)
  if not(is_boolector solver) then Printf.fprintf file "%s\n\n" (smt_functions ());
  let newf, status = build_formula f pred ~pruning ~push:false ~ksteps ~forward file solver in
  close_out file;
  newf, status

let build_formula_incremental (f:formula) (pred:smt_expr) ?(ksteps=0) ?(forward=true) ?(pruning=true) ?(push=true) ?(file="") (session:solver_session) (solver:solver_t): formula * Smtlib2.smt_result option =
  build_formula f pred ~pruning ~push ~ksteps ~forward ~dump:file session.stdin solver
(* ------------------------------------------------------- *)


(* ------------- Stats functions ------------ *)
let get_stat_formula (f:formula): (int * int * int * int * int * int) =
  f.nb_input, f.nb_load, f.nb_store, f.nb_let, f.nb_op, f.nb_constraint

let pp_stat_formula (f:formula): unit =
  let i, ld, st, le, op, cst = get_stat_formula f in
  Logger.result
    "Inputs:%d  Load:%d  Store:%d  Vars:%d  Ops:%d  Constraints:%d@."
    i ld st le op cst
(* --------------------------------- *)
