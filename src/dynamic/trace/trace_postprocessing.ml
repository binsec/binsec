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
  | Cmp of Dba.expr * Dba.expr
  | Sub of Dba.expr
  | Test of Dba.expr * Dba.expr

exception Not_cmp_instruction

let get_comp_instruction (inst:trace_inst): cmp_type =
  match inst.dbainstrs |> List.hd |> Dba_types.Statement.instruction with
  | Dba.IkAssign(Dba.LhsVar(name,size,opts),Dba.ExprBinary(_,x,y),_) ->
    if startswith inst.opcode "cmp" then
      Cmp(x,y)
    else if startswith inst.opcode "test" then
      Test(x,y)
    else if startswith inst.opcode "sub" then
      Sub(Dba.ExprVar(name,size,opts))
    else
      raise Not_cmp_instruction
  | _ -> raise Not_cmp_instruction

let patch_jump_condition _ (opc:string) (cmp:cmp_type): Dba.cond =
  let mnemonic = Str.split (Str.regexp "[ \t]+") opc |> List.hd in
  match cmp with
  | Cmp(x,y) ->
    let op = match mnemonic with
    | "ja" | "jnbe" -> Dba.GtU
    | "jae" | "jnb" | "jnc" -> Dba.GeqU
    | "jb" | "jnae" | "jc" -> Dba.LtU
    | "jbe" | "jna" -> Dba.LeqU
    | "je" | "jz" -> Dba.Eq
    | "jne" | "jnz" -> Dba.Diff
    | "jg" | "jnle" -> Dba.GtS
    | "jge" | "jnl" -> Dba.GeqS
    | "jl" | "jnge" -> Dba.LtS
    | "jle" | "jng" -> Dba.LeqS
    | _ -> raise Not_cmp_instruction
    in
    Dba.CondReif(Dba.ExprBinary(op,x,y))
  | Sub(x') ->
    let size = Dba_utils.computesize_dbaexpr x' in
    let zero = Dba.ExprCst(`Constant, Bitvector.create (Bigint.zero_big_int) size) in
    begin match mnemonic with
    | "ja" | "jnbe" | "jb" | "jnae" | "jc" | "jne" | "jnz" -> Dba.CondReif(Dba.ExprBinary(Dba.Diff,x',zero))
    | "jae" | "jnb" | "jnc" | "jbe" | "jna" | "jge" | "jnl" | "jle" | "jng" -> Dba.True
    | "je" | "jz" -> Dba.CondReif(Dba.ExprBinary(Dba.Eq,x',zero))
    | "jg" | "jnle" -> Dba.CondReif(Dba.ExprBinary(Dba.GtS,x',zero))
    | "jl" | "jnge" -> Dba.CondReif(Dba.ExprBinary(Dba.LtS,x',zero))
    | _ -> raise Not_cmp_instruction
    end
  | Test(x,y) ->
    let size = Dba_utils.computesize_dbaexpr x in
    let zero = Dba.ExprCst(`Constant, Bitvector.create (Bigint.zero_big_int) size) in
    let xAndy = Dba.ExprBinary(Dba.And,x,y) in
    let eq_zero = Dba.CondReif(Dba.ExprBinary(Dba.Eq, xAndy, zero)) in
    let neq_zero = Dba.CondReif(Dba.ExprBinary(Dba.Diff, xAndy, zero)) in
    let xAndyLt0 = Dba.CondAnd(Dba.CondReif(Dba.ExprBinary(Dba.LtS,x,zero)),Dba.CondReif(Dba.ExprBinary(Dba.LtS,y,zero))) in
    begin match mnemonic with
    | "ja" | "jnbe" | "jne" | "jnz" -> neq_zero
    | "jae" | "jnb" | "jnc" -> Dba.True
    | "jb" | "jnae" | "jc" -> Dba.False
    | "jbe" | "jna" | "je" | "jz" -> eq_zero
    | "jg" | "jnle" -> Dba.CondAnd(neq_zero, Dba.CondReif(Dba.ExprBinary(Dba.GeqS, xAndy,zero))) (* c *) (* Dba.CondAnd(neq_zero, xAndyLt0) *) (* FIXME: remettre la condition *)
    | "jge" | "jnl" -> Dba.CondOr(Dba.CondReif(Dba.ExprBinary(Dba.GeqS,x,zero)),Dba.CondReif(Dba.ExprBinary(Dba.GeqS,y,zero)))
    | "jl" | "jnge" -> xAndyLt0
    | "jle" | "jng" -> Dba.CondOr(eq_zero, xAndyLt0)
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
    if is_conditional_jump instr.opcode then
      try
        let cmp_instr = InstrMap.find (key-1) trace_instrs in
        let cmp = get_comp_instruction cmp_instr in
        match instr.dbainstrs with
        | { Dba_types.Statement.location;
            Dba_types.Statement.instruction = Dba.IkIf(c,addr,off) } ::tl ->
          let new_cond = Dba_types.Statement.create
              location
              (Dba.IkIf(patch_jump_condition c instr.opcode cmp, addr, off))
          in
          let new_concinfos = (List.filter (fun i -> match i with NextAddr(_) -> false | _ -> true) cmp_instr.concrete_infos)@instr.concrete_infos in
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
          nb_replaced := !nb_replaced + 1
          (* if List.hd instr.dbainstrs = new_cond then nb_not_found := !nb_not_found +1  else nb_replaced := !nb_replaced + 1 *)
        | _ :: _  -> Logger.warning "Jmp instruction not starting by IkIf"
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
