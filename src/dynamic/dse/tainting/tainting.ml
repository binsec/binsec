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


open Trace_type
open Taint_types

class tainting_engine (strat:taint_strategy) =
  object(self)

    val mutable all_mem_tainted = false
    val mutable memory_taint = Basic_types.Addr64.Map.empty
    val mutable variable_taint = Basic_types.String.Map.empty
    val mutable current_taint = TaintInfoSet.empty
    val mutable final_taint = []
    val mutable occurence_map = Basic_types.Addr64.Map.empty

    method private push_infos (t:taint_infos): unit =
      current_taint <- TaintInfoSet.add t current_taint

    method private is_syscall (instr:trace_inst): bool =
      instr.opcode = "\xcd\x80"
    (* Might should have sysenter, and calls to windows API *)

    method private syscall_taint_stub _ : taint_infos list = []
    (*    let _,syscall = get_syscall instr.concrete_infos in
          match syscall with
          | Stub -> []
          | Read(fd,addr,count,sz_read,data) -> self#sys_read_stub fd addr count sz_read data
          | Generic(_,_) -> [] *)

    method compute_taint_instr (instr:trace_inst) : TaintInfoSet.t list =
      if not(conc_infos_available instr.concrete_infos) then
        (Logger.info "Concrete infos not available skip taint calculation";
         List.map (fun _ -> TaintInfoSet.empty) instr.dbainstrs)
      else
        let occ =
          match Basic_types.Addr64.Map.find instr.location occurence_map with
          | n -> n + 1
          | exception Not_found -> 1
        in
        occurence_map <- Basic_types.Addr64.Map.add instr.location occ occurence_map;
        final_taint <- [];
        if self#is_syscall instr then
          begin
            let stub = self#syscall_taint_stub instr in
            let set = List.fold_left (fun acc i -> TaintInfoSet.add i acc) TaintInfoSet.empty stub in
            [set]
          end
        else begin
          List.iter (fun dbainst ->
              current_taint <- TaintInfoSet.empty;
                match Dba_types.Statement.instruction dbainst with
                | Dba.IkAssign(lhs,e,_) ->
                  begin
                    let res_expr = self#expr_to_taint e instr.concrete_infos in
                    match lhs with
                    | Dba.LhsVar(name, size, _) ->
                      begin
                        match res_expr with
                        | NoTaint -> self#untaint_register name
                        | _ ->
                          self#taint_register name (self#tI_to_tP res_expr);
                          let info =
                            LhsVar(
                              name, 0, size - 1,
                              self#fold_taint (self#tI_to_tP res_expr))
                          in self#push_infos info
                      end
                    | Dba.LhsVarRestrict(name, size, low, high) ->
                      begin
                        let t =
                          match res_expr with
                          | NoTaint ->
                            self#untaint_register name; res_expr
                          | _ ->
                            let source_taint = self#get_taint_register name in
                            let new_taint =
                              self#restrict_to_mix size low high source_taint res_expr in
                            self#taint_register name (self#tI_to_tP new_taint);
                            new_taint
                        in
                        self#push_infos (LhsVar(name,low,high,self#fold_taint (self#tI_to_tP t)))
                      end
                    | Dba.LhsStore(size, _, expr) ->
                      begin
                        let conc_addr = get_store_addr instr.concrete_infos in
                        let t_addr =
                          self#expr_to_taint expr instr.concrete_infos in
                        (match res_expr with
                         | NoTaint ->
                           begin
                             match strat with
                             | May ->
                               if Dba_types.Expr.is_symbolic expr
                               then all_mem_tainted <- false (* should do something else? *)
                             | Must -> if not (Dba_types.Expr.is_symbolic expr) then
                                 self#untaint_memory conc_addr size
                             | MustConc -> self#untaint_memory conc_addr size
                           end
                         | _ ->
                           begin
                             match strat with
                             | May ->
                               if Dba_types.Expr.is_symbolic expr then all_mem_tainted <- true
                             | Must ->
                               if not (Dba_types.Expr.is_symbolic expr) then
                                 self#taint_memory conc_addr size (self#tI_to_tP res_expr)
                             | MustConc -> self#taint_memory conc_addr size (self#tI_to_tP res_expr)
                           end);
                        self#push_infos (Store(self#fold_taint (self#tI_to_tP res_expr)));
                        self#push_infos (StoreAddr(t_addr))
                      end
                  end
                | Dba.IkDJump(e,_) -> ignore (self#expr_to_taint e instr.concrete_infos)
                | Dba.IkIf(c,_,_) -> ignore (self#cond_to_taint c instr.concrete_infos)
                | _ -> (); (* Don't give a f*** of all other instructions *)
              final_taint <- current_taint::final_taint
            ) instr.dbainstrs;
          List.rev final_taint
        end

    method private restrict_to_mix (size:int) (low:int) (high:int) (t_source:taint) (t_dest:taint): taint =
      let rec aux k low high =
        if k <= high && k > low then
          match t_dest with
          | TaintMix(l) -> (List.nth (List.rev l) k)::(aux (k-1) low high)
          | _ -> t_dest::(aux (k-1) low high)
        else
          let t =
            match t_source with
            | TaintMix(l) -> List.nth (List.rev l) k
            | _ -> t_source
          in
          if k != 0 then t::(aux (k-1) low high) else [t]
      in
      TaintMix(aux ((size+1)/8) ((low+1)/8) ((high+1)/8))

    method private fold_taint (t:taint):taint =
      match t with
      | TaintMix(l) ->
        if snd(List.fold_left (fun (acc,value) i -> (i, value && acc = i)) (List.hd l,true) l) then
          List.hd l
        else t
      | _ -> t

    method private tI_to_tP (t:taint): taint =
      match t with
      | TaintI -> TaintP
      | TaintMix(l) -> TaintMix(List.fold_right (fun i acc -> let res = match i with | TaintI -> TaintP | _ -> i in res::acc) l [])
      | _ -> t
    (* switch a taint to TaintP if it was TaintI *)

    method private sys_read_stub (_fd:int32) (addr:int64) (_count:int) (size_read:int) (_data:string): taint_infos list =
      let stub = [(Variable("eax", 0, 31, TaintI  ))] in
      self#taint_register "eax" TaintI;
      begin
        match strat with
        | May -> all_mem_tainted <- true
        | Must -> () (* forget that the whole memory can be tainted *)
        | MustConc -> self#taint_memory addr size_read TaintI(* Taint the
                                                                memory *)
      end;
      stub

    method private taint_memory (addr:int64) (size:int) (t:taint): unit =
      let rec taint_byte addr k t =
        let t_val = match t with | TaintMix(l) -> (List.nth (List.rev l) k) | _ -> t in
        memory_taint <- Basic_types.Addr64.Map.add (Int64.add addr (Int64.of_int k)) t_val memory_taint;
        if k > 0 then taint_byte addr (k-1) t
      in
      match t with
      | NoTaint -> self#untaint_memory addr size
      | _ -> taint_byte addr (size-1) t

    method private untaint_memory (addr:int64) (size:int): unit =
      let rec untaint_byte addr k =
        memory_taint <- Basic_types.Addr64.Map.remove (Int64.add addr (Int64.of_int k)) memory_taint;
        if k > 0 then untaint_byte addr (k-1)
      in
      untaint_byte addr (size-1)

    method private get_taint_memory (addr:int64) (size:int): taint =
      let get_taint address =
        if Basic_types.Addr64.Map.mem address memory_taint then
          Basic_types.Addr64.Map.find address memory_taint
        else
          NoTaint
      in
      let rec get_taint_list addr k =
        if k < 0 then
          []
        else
          let taint = get_taint (Int64.add addr (Int64.of_int k)) in
          taint::(get_taint_list addr (k-1))
      in
      let taint_list = get_taint_list addr (size-1) in
      self#fold_taint (TaintMix(taint_list))


    method private taint_register (name:string) (t:taint): unit =
      variable_taint <- Basic_types.String.Map.add name (self#fold_taint t) variable_taint

    method private untaint_register (name:string): unit =
      variable_taint <- Basic_types.String.Map.remove name variable_taint

    method private get_taint_register (name:string): taint =
      if Basic_types.String.Map.mem name variable_taint then
        Basic_types.String.Map.find name variable_taint
      else
        NoTaint

    method private at_least_one_byte_tainted (): bool =
      Basic_types.Addr64.Map.exists
        (fun _ v ->
          match v with
          | TaintI | TaintP ->  true
          | _ -> false
        ) memory_taint

    method is_tainted (t:taint): bool =
      match self#fold_taint t with
      | TaintI
      | TaintP -> true
      | TaintMix _ -> Logger.debug "is_tainted called on TaintMix\n"; false
      | _ -> false

    method expr_to_taint (e:Dba.expr) (infos:trace_concrete_infos list): taint =
      match e with
      | Dba.ExprVar(name,size,_) ->
        let taint = self#get_taint_register name in
        self#push_infos (Variable(name,0,size-1,taint));
        taint
      | Dba.ExprLoad(size, _, e) ->
        if all_mem_tainted then TaintP

        else
          let conc_addr = get_load_addr infos in
          let taint = self#get_taint_memory conc_addr size in
          let is_symb = Dba_types.Expr.is_symbolic e in
          let taint_addr = self#expr_to_taint e infos in
          let t =
            match strat with
            | May ->  if is_symb then
                if self#at_least_one_byte_tainted () then
                  TaintP
                else taint
              else taint
            | Must -> taint (* Check that we are not symbolic ? *)
            | MustConc -> taint
          in
          self#push_infos (Load(t));
          self#push_infos (LoadAddr(taint_addr));
          t

      | Dba.ExprCst(_,_) -> NoTaint

      | Dba.ExprUnary(_, e1) ->
        let taint = self#fold_taint (self#expr_to_taint e1 infos) in
        if self#is_tainted taint then (* Consider unary op to preserve taint.. *)
          TaintP
        else
          NoTaint

      | Dba.ExprBinary(_, e1, e2) ->
        let t1 = self#fold_taint (self#expr_to_taint e1 infos) in
        let t2 = self#fold_taint (self#expr_to_taint e2 infos) in
        if self#is_tainted t1 || self#is_tainted t2 then
          TaintP  (* For now a binary expr is considered tainted if at least one member is tainted *)
        else
          NoTaint

      | Dba.ExprRestrict(e1, low, high) ->
        begin
          let taint = (match e1 with | Dba.ExprVar(name, _,_) -> self#get_taint_register name | _ -> self#expr_to_taint e1 infos) in
          let taint_final =
            (match taint with (* In case of TaintMix try to return only exctracted bytes *)
             | TaintMix(l) ->
               let rec aux k low high =
                 if k >= 0 then
                   if k < high && k >= low then
                     (List.nth (List.rev l) k)::(aux (k-1) low high)
                   else aux (k-1) low high
                 else []
               in
               TaintMix(aux ((List.length l)-1) ((low+1)/8) ((high+1)/8))
             | _ -> taint)
          in
          begin
            match e1 with
            | Dba.ExprVar(name,_,_) -> self#push_infos (Variable(name,low,high,taint)); taint
            | _ -> taint_final
          end


        end
      | Dba.ExprExtU(e1,size) | Dba.ExprExtS(e1,size) ->
        let taint = self#expr_to_taint e1 infos in
        begin
          match taint with (* The point is should really consider the extension tainted ?? *)
          | TaintMix(l) -> let rec aux k t = if k != 0 then t::(aux (k-1) t) else [] in TaintMix((aux (size/8) (List.hd l))@l)
          | _ -> taint
        end

      | Dba.ExprIte(c,e1,e2) -> (* How should we propagate taint ? *)
        let tc1 = self#cond_to_taint c infos in
        let te1 = self#expr_to_taint e1 infos in
        let te2 = self#expr_to_taint e2 infos in
        if self#is_tainted tc1 && (self#is_tainted te1 || self#is_tainted te2) then TaintP else NoTaint
      (* For now consider the result if the condition is tainted and at least on of the two memebers *)

      | Dba.ExprAlternative(l,_) -> self#expr_to_taint (List.hd l) infos


    method private cond_to_taint (c:Dba.cond) (infos:trace_concrete_infos list): taint =
      match c with
      | Dba.CondReif(e) -> self#expr_to_taint e infos
      | Dba.CondNot(c1) -> self#cond_to_taint c1 infos
      | Dba.CondAnd(c1,c2) ->
        let tc1 = self#cond_to_taint c1 infos in
        let tc2 = self#cond_to_taint c2 infos in
        if self#is_tainted tc1 && self#is_tainted tc2 then TaintP else NoTaint
      | Dba.CondOr(c1,c2) ->
        let tc1 = self#cond_to_taint c1 infos in
        let tc2 = self#cond_to_taint c2 infos in
        if self#is_tainted tc1 || self#is_tainted tc2 then TaintP else NoTaint
      | Dba.True | Dba.False -> NoTaint

    method private print_mem (): unit =
      Basic_types.Addr64.Map.iter (fun k v -> Logger.debug "%Lx:%s " k (taint_to_string v)) memory_taint;

    method private print_vars (): unit =
      let vars = Basic_types.String.Map.fold (fun k v acc -> acc^" "^k^":"^(taint_to_string v)) variable_taint "" in
      Logger.debug "Vars:%s\n" vars

  end
