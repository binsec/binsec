(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Path_predicate_env
open Path_predicate_formula
open Formula_utils
open Config_piqi
open Configuration
open Libcall_piqi
open Libcall_t
open Dse_options

(* ALLOC (id,size,addr), FREE (id,addr) *)
type heap_event_t =  ALLOC of (int * int64 * int64) | ALLOC_UAF of (int * int64 * int64) | FREE of (int*int64) | DFREE of (int*int64)
(*type heap_event_t = heap_event_elem_t Core.Std.Heap.t*)

let counter_alloc =
  let count = ref (-1) in
  fun () ->
    incr count;
    !count

exception DOUBLEFREE of int * int64

class uaf_detection (trace_config:Trace_config.t) =
  object(self) inherit InvertChild.invert_child trace_config

    val malloc_symb = "MALLOC"
    val malloc_uaf_id = ref 0
    val malloc_uaf_size = ref 0L
    val last_malloc =  ref "" (* works if only one allocation tracks *)
    val mutable uaf_detect = false
    val mutable alloc_addr = ref 0L
    val mutable free_addr = ref []
    val mutable use_addr   = ref []

    val mutable alloc_nth = ref (-1)
    val mutable free_nth = ref (-1)
    val mutable use_nth   = ref (-1)

    val mutable heap_events = ref []

    val alloc_size_tbl = Hashtbl.create 10

    method is_uaf () = uaf_detect

    method set_alloc addr= alloc_addr := addr
    method set_free addr= free_addr := (addr)::(!free_addr)
    method set_use addr= use_addr := (addr)::(!use_addr)

    method set_nth_alloc n= alloc_nth := n
    method set_nth_free n= free_nth := n
    method set_nth_use n= use_nth := n

    method call_malloc env (malloc:malloc_t option) loc key =
      let open Malloc_t in
      match malloc with
      | None -> ()
      | Some malloc ->
        if ((Int64.compare loc (!alloc_addr)) == 0  || key = (!alloc_nth)) then
          self#add_alloc env malloc.ret malloc.size true
        else
          self#add_alloc env malloc.ret malloc.size false

    method call_free free =
      let open Free_t in
      match free with
      | None -> ()
      | Some free -> self#add_free free.ptr


    method check_lib inst env key =
      let open Trace_type in
      List.iter
        (fun x ->
           match x with
           | Libcall l when l.ident = `malloc ->
             self#call_malloc env l.malloc inst.location key
           | Libcall l when l.ident = `free -> self#call_free l.free
           | Libcall _ | NextAddr _
           | RegRead (_, _)|RegWrite (_, _)|Syscall _
           | MemLoad (_, _) | MemStore (_, _)
           | Not_retrieved | Comment _ | Wave _  -> ()
        ) inst.concrete_infos


    method add_alloc env addr size symbolize =
      let id = counter_alloc() in
      let name = Printf.sprintf "%s%d" malloc_symb id in
      Hashtbl.add alloc_size_tbl addr (id,size);
      if symbolize then begin
        last_malloc:=name;
        heap_events := ALLOC_UAF (id,size,addr) :: !heap_events;
        malloc_uaf_id := id;
        malloc_uaf_size := size;
        Path_predicate_utils.symbolize_register "eax" ~is_full_name:true name env
      end
      else begin
        heap_events := ALLOC (id,size,addr) :: !heap_events;
        let fexpr =
          Formula.mk_bv_cst
            (Bitvector.create (Bigint.big_int_of_int64 addr)
               env.Path_predicate_env.formula.addr_size)
        in Path_predicate_utils.replace_register "eax" fexpr env
      end

    method add_free addr  =
      let id,_ =
        try Hashtbl.find alloc_size_tbl addr
        with Not_found -> (-1), 0L
      in
      let has_free =
        List.exists
          (fun e ->
             match e with
             | FREE(i,a) -> i = id && a = addr
             | ALLOC _ | ALLOC_UAF _ | DFREE _ -> false)
          !heap_events
      in
      (* check for double free *)
      heap_events := FREE (id,addr) :: !heap_events;
      if has_free then raise (DOUBLEFREE (id, addr))

    (* should do : 0<= (ADDR+sz) - addr <= sz in smt :) *)
    method add_uaf_check predicate (name,_) env =
      let open Formula in
      let get_addr_size e = e.Path_predicate_env.formula.addr_size in
      let _,chunk = get_var_or_create env.formula (!last_malloc) (get_addr_size env) 0 31 in
      let _,reg = get_var_or_create env.formula name env.Path_predicate_env.formula.addr_size 0 31 in
      let new_predicate = mk_bl_and predicate (mk_bv_distinct reg chunk) in
      new_predicate

    (* should do : 0<= (ADDR+sz) - addr <= sz in smt :) *)
    method add_free_check predicate env =
      let open Formula in
      let get_addr_size e = e.Path_predicate_env.formula.addr_size in
      (* Get the last val of memory *)
      let _,esp = get_var_or_create env.formula "esp" (get_addr_size env) 0 31 in
      let esp = mk_bv_add esp
          (mk_bv_cst (Bitvector.create (Bigint.big_int_of_int 0x4) (get_addr_size env)))
      in
      let _,chunk = get_var_or_create env.formula (!last_malloc) (get_addr_size env) 0 31 in
      let mem_name = "memory" ^ (string_of_int ((get_varindex env.formula "memory")-1)) in
      let load_esp =
        mk_select 4 (mk_ax_var (ax_var mem_name 0 (get_addr_size env))) esp
      in
      let new_predicate = mk_bl_and predicate (mk_bv_distinct load_esp chunk) in
      new_predicate

    (* useless *)
    method check_with_visitor name env =
      let _,reg = get_var_or_create env.formula name env.Path_predicate_env.formula.addr_size 0 31 in
      let vars = bv_term_variables reg |> to_stringmap in
      Basic_types.String.Set.iter (fun x  -> Logger.debug "%s " x) vars

    method! private visit_instr_after key inst env =
      try
        self#check_lib inst env key;
        Path_predicate.DoExec
      with
      | DOUBLEFREE(id,addr) ->
        Logger.debug "DoubleFree found at nth %d (addr: %Lx, id %d)" key addr id;
        uaf_detect <- true;
        Path_predicate.DoExec

    method! private visit_dbainstr_after (key:int) inst
        (dbainst:Dba_types.Statement.t) (env:Path_predicate_env.t)
      =
      let open Trace_type in
      let open Trace_config in
      let addr = inst.location in
      let solver = Formula_options.Solver.of_piqi
                     trace_config.configuration.solver in
      if List.exists (fun x -> Int64.compare addr x = 0) !free_addr
      || key = !free_nth then
        begin
          let formula_file = Printf.sprintf "formula-free-%d.smt2" key in
          let static_predicate = self#build_cond_predicate Dba.Expr._true env in
          let predicate = self#add_free_check static_predicate env in
          ignore @@
          Path_predicate_formula.build_formula_file env.formula predicate
            formula_file;
          let result, _model = Solver.solve_model formula_file solver in
          begin
            let open Formula in
            match result with
            | SAT ->
              Logger.debug "Free on other chunk at nth %d" key;
              Path_predicate.StopExec
            | UNSAT | TIMEOUT | UNKNOWN ->
              Path_predicate.DoExec
          end
        end
      else
        if List.exists (Int64.equal addr) !use_addr || key = !use_nth then
        let open Dba in
        match Dba_types.Statement.instruction dbainst with
        | Instr.Assign (Dba.LValue.Store(size,_,Dba.Expr.Var {Dba.name; _}),_,_) ->
          let formula_file = Printf.sprintf "formula-uaf-%d.smt2" key in
          let static_predicate =
            self#build_cond_predicate Dba.Expr._true env in
          let predicate = self#add_uaf_check static_predicate (name,size) env in
          ignore (Path_predicate_formula.build_formula_file env.formula predicate formula_file);
          let result, _model = Solver.solve_model formula_file solver in
          begin
            let open Formula in
            match result with
            | UNSAT ->
              Logger.debug "Uaf Found at nth %d" key;
              uaf_detect <- true;
              Path_predicate.StopExec
            | SAT | TIMEOUT | UNKNOWN ->
              Path_predicate.DoExec
          end
        | _ -> Path_predicate.DoExec (* not success to avoid fragile on this one :) *)
      else Path_predicate.DoExec

    method! private post_execution _env =
      (* REMOVED: always true predicate : if self#is_uaf () || true then *)
      Logger.debug "################\nHeap events : \n";
      let is_inf_eq x y = (Int64.compare x y) <=0 in
      let is_sup_eq x y = (Int64.compare x y) >=0 in
      let addr_begin = ref 0L in
      let addr_end = ref 0L in
      let root_passed = ref false in
      List.iter
        (fun x ->
           match x with
           | ALLOC(id,size,addr) ->
             Logger.debug "Alloc %d (addr 0x%Lx, sz 0x%Lx)\n" id addr size;
             if !root_passed then
               let addr_ = Int64.add addr size in
               if is_inf_eq !addr_begin addr && is_sup_eq addr !addr_begin
                  ||
                  is_inf_eq addr !addr_end && is_sup_eq addr_ !addr_end
               then Logger.debug "<--- intersection with root";
           | ALLOC_UAF(id,size,addr) ->
             addr_begin := addr;
             addr_end := Int64.add addr size;
             root_passed := true;
             Logger.debug "Alloc %d (addr 0x%Lx, sz 0x%Lx) <-- root\n" id addr size
           | FREE(id,addr) -> Logger.debug "Free  %d (addr 0x%Lx)\n" id addr
           | DFREE(id,addr) -> Logger.debug "(Double)Free  %d (addr 0x%Lx)\n" id addr
        ) (List.rev !heap_events);
      0


  end;;
