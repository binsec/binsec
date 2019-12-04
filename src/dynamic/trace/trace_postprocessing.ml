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

open Trace_type
open Trace_options

(*
TODO:
-> change the caching for trace loading
-> taking in account pattern like: cmp ; mov ; jcc
-> taking in account pattern using or
-> doing a backward use-def pass to remove unused flags (after the -nat-cond)
*)

let startswith (s:string) (item:string): bool =
  Str.string_match (Str.regexp ("^[ \t]*"^item)) s 0


let is_conditional_jump (opcode:string): bool =
  let jumps = ["jz";"ja";"jnbe";"jae";"jnb";"jnc";"jb";"jnae";"jc";"jbe";
               "jna";"je";"jne";"jnz";"jg";"jnle";"jge";"jnl";"jl";"jnge";"jle";"jng"] in
  List.fold_left (fun acc i -> acc || startswith opcode i) false jumps

type cmp_type =
  | Cmp of Dba.Expr.t * Dba.Expr.t
  | Sub of Dba.Expr.t
  | Test of Dba.Expr.t * Dba.Expr.t

exception Not_cmp_instruction

let get_comp_instruction inst: cmp_type =
  match inst.dbainstrs |> List.hd |> Dba_types.Statement.instruction with
  | Dba.Instr.Assign(Dba.(LValue.Var {name; size; info = opts}),
                     Dba.Expr.Binary(_,x,y),_) ->
    if startswith inst.mnemonic "cmp" then
      Cmp (x,y)
    else if startswith inst.mnemonic "test" then
      Test (x,y)
    else if startswith inst.mnemonic "sub" then
      Sub (Dba.Expr.var name size ~tag:opts)
    else
      raise Not_cmp_instruction
  | _ -> raise Not_cmp_instruction

let patch_jump_condition _ (opc:string) (cmp:cmp_type) =
  let mnemonic = Str.split (Str.regexp "[ \t]+") opc |> List.hd in
  let open Dba in
  match cmp with
  | Cmp(x,y) ->
    let op = match mnemonic with
      | "ja" | "jnbe" -> Dba.Expr.ugt
      | "jae" | "jnb" | "jnc" -> Dba.Expr.uge
      | "jb" | "jnae" | "jc" -> Dba.Expr.ult
      | "jbe" | "jna" -> Dba.Expr.ule
      | "je" | "jz" -> Dba.Expr.equal
      | "jne" | "jnz" -> Dba.Expr.diff
      | "jg" | "jnle" -> Dba.Expr.sgt
      | "jge" | "jnl" -> Dba.Expr.sge
      | "jl" | "jnge" -> Dba.Expr.slt
      | "jle" | "jng" -> Dba.Expr.sle
      | _ -> raise Not_cmp_instruction
    in op x y
  | Sub x' ->
    let size = Dba_utils.computesize_dbaexpr x' in
    let zero = Dba.Expr.zeros size in
    begin match mnemonic with
      | "ja" | "jnbe" | "jb" | "jnae" | "jc" | "jne" | "jnz" ->
        Dba.Expr.diff x' zero
      | "jae" | "jnb" | "jnc" | "jbe" | "jna" | "jge" | "jnl" | "jle" | "jng" ->
        Dba.Expr.one
      | "je" | "jz" -> Dba.Expr.equal x' zero
      | "jg" | "jnle" -> Dba.Expr.sgt x' zero
      | "jl" | "jnge" -> Dba.Expr.slt x' zero
      | _ -> raise Not_cmp_instruction
    end
  | Test(x,y) ->
    let size = Dba_utils.computesize_dbaexpr x in
    let zero = Dba.Expr.zeros size in
    let xAndy = Dba.Expr.logand x y in
    let eq_zero  = Expr.equal xAndy zero in
    let neq_zero = Expr.diff  xAndy zero in
    let xAndyLt0 =
      Expr.logand (Expr.slt x zero) (Expr.slt y zero) in
    begin match mnemonic with
      | "ja" | "jnbe" | "jne" | "jnz" -> neq_zero
      | "jae" | "jnb" | "jnc" -> Dba.Expr.one
      | "jb" | "jnae" | "jc" ->  Dba.Expr.zero
      | "jbe" | "jna" | "je" | "jz" -> eq_zero
      | "jg" | "jnle" ->
        Expr.logand neq_zero (Expr.sge xAndy zero)
      (* c *) (* Dba.CondAnd(neq_zero, xAndyLt0) *) (* FIXME: remettre la condition *)
      | "jge" | "jnl" ->
        Expr.logor (Dba.Expr.sge x zero) (Dba.Expr.sge y zero)
      | "jl" | "jnge" -> xAndyLt0
      | "jle" | "jng" -> Expr.logor eq_zero xAndyLt0
      | _ -> raise Not_cmp_instruction
    end

let nb_not_found = ref 0
let addr_not_found = ref Basic_types.Addr64.Set.empty
let nb_not_cmp = ref 0
let addr_not_cmp = ref Basic_types.Addr64.Set.empty
let nb_replaced = ref 0

let get_merge_stats () =
  !nb_not_found, !addr_not_found, !nb_not_cmp, !addr_not_cmp, !nb_replaced

let merge_natural_conditions (trace_instrs:trace_inst InstrMap.t) =
  let iter_trace key instr =
    if is_conditional_jump instr.mnemonic then
      try
        let cmp_instr = InstrMap.find (key-1) trace_instrs in
        let cmp = get_comp_instruction cmp_instr in
        match instr.dbainstrs with
        | { Dba_types.Statement.location;
            Dba_types.Statement.instruction = Dba.Instr.If(c,addr,off) } ::tl ->
          let new_cond = Dba_types.Statement.create
              location
              (Dba.Instr.ite (patch_jump_condition c instr.mnemonic cmp) addr off)
          in
          let new_concinfos =
            List.filter (fun i -> match i with NextAddr _ -> false | _ -> true)
              cmp_instr.concrete_infos
            @ instr.concrete_infos in
          let newdbainstrs, _ =
            match cmp with
            | Cmp(_,_) | Test(_,_) -> new_cond::tl, cmp_instr.dbainstrs
            | Sub(_) ->
              begin match List.rev cmp_instr.dbainstrs with
                | _ :: assign :: cmptl ->
                  let first = List.hd cmp_instr.dbainstrs in
                  first :: assign :: new_cond ::tl, List.rev cmptl
                | _ :: _
                | [] -> raise Not_cmp_instruction
              end
          in
          cmp_instr.dbainstrs <-
            (match cmp with
             | Cmp _
             | Test _ -> cmp_instr.dbainstrs
             | Sub _ -> List.rev cmp_instr.dbainstrs |> List.tl |> List.rev);
          (* Fixme: For now keep instr but would be great to remove them *)
          instr.dbainstrs <- newdbainstrs;
          instr.concrete_infos <- new_concinfos;
          incr nb_replaced;
        | _ :: _  -> Logger.warning "Jmp instruction not starting by If"
        | [] -> Logger.warning "No dba instructions in jmp instruction"
      with
      | Not_found ->
        incr nb_not_found;
        addr_not_found := Basic_types.Addr64.Set.add (Int64.of_int key) !addr_not_found
      | Not_cmp_instruction ->
        incr nb_not_cmp;
        addr_not_cmp := Basic_types.Addr64.Set.add instr.location !addr_not_cmp
  in
  InstrMap.iter iter_trace trace_instrs;
  trace_instrs
