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

(** DBA -> Formula decoder interface.  *)

module Instr_to_Formula= 
struct

  module F = Formula
    
  module Types = struct
    type binary = F.bv_term
    type boolean = F.bl_term
    type memory = F.ax_term
  end

  include Types

  module VarMap = Map.Make(String);;
  
  type state = {
    (* Associate to each register the representation of the bitvector
       that it contains; and a unique counter needed when we name the
       definitions.  *)
    vars: (int * binary) VarMap.t;

    (* mem and memcount are like vars, but for the memory. *)
    mem:  memory;
    memcount: int;

    (* The definitions that we have put so far. *)
    
    (* Note: currently we translate the assumes directly as assertions
       in the code, which works well only if we want a formula
       describing a single path. For path merging, we need a shared
       formula with a separate path-condition.  *)
    formula: F.formula;
  };;

  module State = struct

    type t = state

    let add_assertion cond state =
    {state with formula = Formula.push_front_assert cond state.formula}

    let add_comment comment state =
      {state with formula = Formula.push_front_comment comment state.formula}
    
    let clear state =
      (* Generate a new memory variable. *)
      let memcount = state.memcount + 1 in
      let memvar = F.ax_var ("memory" ^ (string_of_int memcount)) 32 8 in
      let formula = F.push_front_declare (F.decl @@ F.AxDecl(memvar,[])) state.formula in
      let mem = F.mk_ax_var memvar in

      (* Generate new variables. *)
      let vars,formula = VarMap.fold (fun var (i,b) (vars,formula) ->
          let i = i + 1 in
          let size = b.F.bv_term_size in
          let name = F.bv_var (var ^ (string_of_int i)) size in
          let formula = F.push_front_declare (F.decl @@ F.BvDecl(name,[])) formula in
          let term = F.mk_bv_var name in
          (VarMap.add var (i+1,term) vars, formula)
        ) state.vars (state.vars,formula) in
      { vars; formula; mem; memcount = memcount + 1 }
    ;;

  end

  let initial_state =
    let formula = F.empty in
    let memvar = F.ax_var "memory0" 32 8 in
    let formula = F.push_front_declare (F.decl @@ F.AxDecl(memvar,[])) formula in
    let mem = F.mk_ax_var memvar in
    {
      vars = VarMap.empty;
      mem; memcount = 1; formula
    }
  ;;
  
  let clear_memory = State.clear

  let get_formula x = x.formula
  
  let assume cond state =
    let state = State.add_assertion cond state in
    (), state

  let add_comment string state = State.add_comment string state
  ;;
  
  let unknown ~name ~size state =
    let var = F.bv_var name size in
    let formula = F.push_front_declare (F.decl @@ F.BvDecl(var,[])) state.formula in
    (F.mk_bv_var var,{state with formula})


  let unknown =
    let count = ref 0 in
    fun ~size state -> 
      let name = "unknown" ^ string_of_int (!count) in
      incr count;
      unknown ~name ~size state
  ;;

  
  let set_var ~size string bin state =
    let prev =
      try fst @@ VarMap.find string state.vars
      with Not_found -> 0
    in
    let next = prev + 1 in
    (* Put a definition. *)
    let var = F.bv_var (string ^ string_of_int next) size in
    let formula = F.push_front_define (F.def @@ F.BvDef(var,[],bin)) state.formula in
    let vars = VarMap.add string (next,F.mk_bv_var var) state.vars in
    {state with vars;formula}
  ;;

  let get_var ~size string state =
    try (snd @@ VarMap.find string state.vars,state)
    (* Ideally, we should initialize all registers with an unknown
       value. *)
    with Not_found ->
      let res,state = unknown ~size state in
      let vars = VarMap.add string (0,res) state.vars in
      res,{state with vars}
  ;;
  
  let store ~size _endian addr value state =
    let memvar = F.ax_var ("memory" ^ (string_of_int state.memcount)) 32 8 in
    let mem = F.mk_store (size/8) state.mem addr value in    
    let formula = F.push_front_define (F.def @@ F.AxDef(memvar,[],mem)) state.formula in
    let mem = F.mk_ax_var memvar in
    {state with mem;formula;memcount = state.memcount + 1}

  let load ~size _endian addr state =
    F.mk_select (size/8) state.mem addr,state
    
  let ite cond then_ else_ state =
    F.mk_bv_ite cond then_ else_, state
  ;;

  let bool_of_bin x state = F.mk_bv_equal x F.mk_bv_one,state
  let bin_of_bool x state =
    F.mk_bv_ite x F.mk_bv_one F.mk_bv_zero,state

    let undef = unknown

    module Boolean = struct
      include Types

      let false_ state = F.mk_bl_false,state
      let true_ state = F.mk_bl_true,state
      let (||) a b state = F.mk_bl_or a b, state
      let (&&) a b state = F.mk_bl_and a b, state
      let not a state = F.mk_bl_not a, state                           

    end

    module Binary = struct
      include Types

      let biconst ~size i state = 
        F.mk_bv_cst (Bitvector.create i size),state


      let bvar2 op =
        let f:size:int -> binary -> binary -> State.t -> binary * State.t = 
          fun ~size a b state ->
            let _ = size in
            op a b, state in
        f
      ;;

      let bimul =  bvar2 F.mk_bv_mul
      let bisub =  bvar2 F.mk_bv_sub
      let biadd =  bvar2 F.mk_bv_add
      let blshr  = bvar2 F.mk_bv_lshr
      let bashr  = bvar2 F.mk_bv_ashr
      let bshl =   bvar2 F.mk_bv_shl
      let biurem = bvar2 F.mk_bv_urem
      let biudiv = bvar2 F.mk_bv_udiv
      let bisrem = bvar2 F.mk_bv_srem
      let bisdiv = bvar2 F.mk_bv_sdiv
      let bxor =   bvar2 F.mk_bv_xor
      let bor =    bvar2 F.mk_bv_or
      let band =   bvar2 F.mk_bv_and

      let bv_right_rotate ~size _a _b state =
        Xtrasec_options.Logger.warning "bv_right_rotate is imprecise";
        unknown ~size state

      let bv_left_rotate ~size _a _b state =
        Xtrasec_options.Logger.warning "bv_left_rotate is imprecise";
        unknown ~size state

      
      let buext ~size ~oldsize a state =
        F.mk_bv_zero_extend (size - oldsize) a,state

      let bsext ~size ~oldsize a state =
        F.mk_bv_sign_extend (size - oldsize) a,state

      let bextract ~lo ~hi ~oldsize a state =
        if lo == 0 && hi == (oldsize-1) then a,state
        else F.mk_bv_extract Interval.{lo;hi} a,state
      ;;

      let bconcat ~size1 ~size2 a b state =
        let _ = size1 and _ = size2 in
        F.mk_bv_concat a b, state
      ;;

      let bpred op =
        let f:size:int -> binary -> binary -> State.t -> boolean * State.t = 
          fun ~size a b state -> let _ = size in op a b, state in
        f
      ;;

      let biult = bpred F.mk_bv_ult
      let biule = bpred F.mk_bv_ule
      let bislt = bpred F.mk_bv_slt
      let bisle = bpred F.mk_bv_sle
      let beq =   bpred F.mk_bv_equal

    end


    
end
