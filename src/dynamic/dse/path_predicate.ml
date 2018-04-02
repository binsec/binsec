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

open Formula
open Formula_type
open Config_piqi
open Solver
open Smtlib2
open Smtlib2_visitor
open Policy_engine
open Policy_type
open Options
open Tainting
open Decode_utils
open Dba_utils
open Common_piqi
open Memory_t


module Pp = Dba_printer.EICUnicode
let tmp_vars =
  Basic_types.String.Set.of_list ["stemp";"btemp";"dtemp";"temp";
                                  "res8"; "res16"; "res32"; "res64";
                                  "temp8"; "temp16"; "temp32"; "temp64"]

let is_tmp_variable (name:string) = Basic_types.String.Set.mem name tmp_vars

let get_cmd_content = List_utils.hd_hd


let rec path_to dbainstrs (offset:int) (acc:Dba_types.Statement.t list): Dba_types.Statement.t list =
  (* Function that compute the path to take within a DBA block to reach the given offset
     The result is the list of DBA instr within the DBA to reach offset (excluded).
  *)
  let open Dba_types.Statement in
  let keep_from l i = List.filter (fun li -> li.location.Dba.id >= i) l in
  match dbainstrs with
  | { location = { Dba.id = x; _}; instruction = inst } as dba_inst :: tl ->
    if x >= offset then List.rev (dba_inst :: acc)
    else
      begin match inst with
      | Dba.IkSJump (Dba.JInner off, _) when off > x ->
        let new_list = keep_from tl off in
        path_to new_list offset (dba_inst::acc)
      | Dba.IkIf (_, Dba.JInner off1, off2) ->
        let l1 = keep_from tl off1 in
        let l2 = keep_from tl off2 in
        let path1 = path_to l1 offset (dba_inst::acc) in
        let path2 = path_to l2 offset (dba_inst::acc) in
        let exists_in = List.exists (fun li -> li.location.Dba.id  = offset) in
        let bool1 = exists_in path1 and bool2 = exists_in path2 in
        if bool1 && bool2 then
          Logger.warning "path_to %d: offset present in both branches.." offset;
        if bool1 && bool2 then
          if List.length path1 < List.length path2 then path1 else path2
        else if bool1 && not(bool2) then
          path1
        else if not(bool1) && bool2 then
          path2
        else
          (Logger.error "path_to: target offset %d not found .." offset;
          path1)
      | Dba.IkStop _ -> List.rev (dba_inst::acc)
      | _ ->
        let l = dba_inst :: acc in
        if x >= offset then List.rev l else path_to tl offset l
      end
  | [] -> List.rev acc

exception Stop_analysis

type trace_visit_action =
  | SkipExec
  | DoExec
  | StopExec
(*| DoChildrenPost of a ... ?
  | SkipChildrenPost of a .. *)



class type dse_analysis_t = object
    val mutable trace : Trace_type.trace
    val mutable default_formula_file : string
    val mutable config : Config_piqi.configuration
    val mutable do_compute_taint : bool
    val mutable cur_key_inst: int
    val is_remote : bool
    val trace_name : string

    (* --- Used internally by Binsec --- *)
    method get_taint : unit -> Tainting.tainting_engine
    method is_taint_computed : unit -> bool
    method expr_to_smt :
      Dba.expr -> ?apply_cs:bool -> Path_pred_env.t ->
      Smtlib2.smt_bv_expr * Smtlib2.smt_expr list
    method get_current_dbacodeaddress : unit -> Dba.address
    method exec : Dba_types.Statement.t -> Path_pred_env.t -> unit
    method get_current_concrete_infos : unit -> Trace_type.trace_concrete_infos list
    method concretize_expr_bv : Dba.expr -> ?is_lhs:bool -> Path_pred_env.t -> Bitvector.t
    method concretize_cond : Dba.cond -> Path_pred_env.t -> bool
    (* ----------------------------------- *)

    (* --- External methods --- *)
    method compute : int
    method solve_predicate :
      Smtlib2.smt_expr ->
      ?print_stat:bool -> ?name:string -> ?push:bool -> ?pop:bool -> ?prek:int ->
      ?pruning:bool -> ?get_model:bool -> Path_pred_env.t ->
      Smtlib2.smt_result * Smt_model.t * float
    (* ------------------------- *)

    (* --- methods overridable by child class --- *)
    method private visit_instr_before : int -> Trace_type.trace_inst -> Path_pred_env.t -> trace_visit_action
    method private visit_instr_after : int -> Trace_type.trace_inst -> Path_pred_env.t -> trace_visit_action
    method private visit_dbainstr_before : int -> Trace_type.trace_inst -> Dba_types.Statement.t ->
      Path_pred_env.t -> trace_visit_action
    method private visit_dbainstr_after : int -> Trace_type.trace_inst -> Dba_types.Statement.t ->
      Path_pred_env.t -> trace_visit_action
    method private pre_execution : Path_pred_env.t -> unit
    method private post_execution : Path_pred_env.t -> int
    method private input_message_received : string -> string -> unit
    method private visit_metadata : Trace_type.trace_inst -> Trace_type.metadata -> Path_pred_env.t -> unit
    (* ------------------------------------------ *)

    (* --- Methods callable by child class --- *)
    method private send_message : string -> string -> unit
    method private compare_address : Dba.expr -> Bitvector.t -> ?apply_cs:bool -> Path_pred_env.t -> Smtlib2.smt_expr
    method private is_symbolic_expression : Dba.expr -> Path_pred_env.t -> bool
    method private is_symbolic_condition : Dba.cond -> Path_pred_env.t -> bool
    method private build_address_comparison_predicate : Dba.expr -> Bitvector.t -> ?apply_cs:bool -> Path_pred_env.t -> Smtlib2.smt_expr
    method private add_witness_variable : string -> Dba.expr -> ?apply_cs:bool -> Path_pred_env.t -> unit
    method private build_witness_bitvector_comparison_predicate : string -> int -> Bitvector.t -> Smtlib2.smt_expr
    method private build_witness_expr_comparison_predicate : string -> int -> Dba.expr -> ?apply_cs:bool -> Path_pred_env.t -> Smtlib2.smt_expr
    method private build_cond_predicate : Dba.cond -> Path_pred_env.t -> Smtlib2.smt_expr
    method private build_multiple_condition_predicate : Smtlib2.smt_expr list -> Smtlib2.smt_expr
    (* --------------------------------------- *)
    end


class dse_analysis (input_config:trace_analysis_config) =
object(self)

  val mutable config = input_config.configuration
  val mutable trace = Trace_type.empty_trace
  val mutable cur_key_inst = 0          (* nth instruction in the trace *)
  val mutable cur_inst = Trace_type.empty_inst     (* current asm instruction *)
  val mutable cur_dbainst =
    let loc = Dba_types.Caddress.block_start
      @@ Bitvector.zeros (Machine.Word_size.get ())
    and instr = Dba.IkStop None in
    Dba_types.Statement.create loc instr

  val mutable occur_map = Basic_types.Int64.Map.empty(* Map that contain the current number of appearance of a given address in the trace *)
  val mutable cur_k = 0                 (* Number of occurence for the current instruction (taken from occup_map) *)
  val mutable nth_dbainst = 0           (* Current dba instruction analyse in the trace_inst *)
  val mutable assignmap = Basic_types.String.Map.empty (* Track assign within dba block. Keep first assign offset for each var *)
  val mutable tainting = new tainting_engine Taint_types.MustConc
  val mutable do_compute_taint = false
  val mutable policy =
    Policy_engine.parse_policy_from_string_list input_config.configuration.Configuration.policy

  val mutable fallback = Symb
  val mutable rotate_fallback = Symb
  val mutable pending_constraints = []

  val mutable stat_conc = 0
  val mutable stat_symb = 0

  val mutable solver_session = default_session ()
  val mutable default_formula_file = Filename.temp_file "binsec_path_predicate" ".smt2"

  val mutable solve_incrementally = input_config.configuration.Configuration.incremental
  val mutable timeout = Int32.to_int input_config.configuration.Configuration.timeout
  val is_remote = (match input_config.trace_input with | Chunked(_,_) -> false | Stream _ -> true)
  val trace_name = (match input_config.trace_input with
                    | Chunked(_,_) -> Filename.basename input_config.trace_file |> Filename.chop_extension
                    | Stream _ -> "stream")
  
  method get_taint _ =
    (* Trick to make taint engine available for the policy_engine *)
    tainting

  method get_current_dbacodeaddress (): Dba.address =
    Dba_types.Statement.location cur_dbainst

  method is_taint_computed _ =
    do_compute_taint

  method get_current_concrete_infos _ =
    cur_inst.Trace_type.concrete_infos

  method private get_comm _ =
    Printf.sprintf "%Lx #%d %s"
      cur_inst.Trace_type.location cur_key_inst cur_inst.Trace_type.opcode

(* ---------------------------------------------------------------------- *)
(* ------------------- Different from traceAnalysis.ml ------------------ *)
  method private backtrack_var (name:string) (low:int) (high:int) (offset:int) (env:Path_pred_env.t)=
    (* Retrieve the path to offset within the current DBA block *)
    let path = path_to cur_inst.Trace_type.dbainstrs offset [] in
    (* Remove the current offset *)
    let path = List_utils.drop 1 (List.rev path) in
    let var =
      Dba.ExprVar(name,high-low+1,
                  if is_tmp_variable name then Some Dba.Temp else None) in
    let rec recurse_path l acc =
      let open Dba_types.Statement in
      match l with
      | { location = _; instruction =  ins } :: tl ->
        begin match ins with
          | Dba.IkAssign(Dba.LhsVar(n, sz, opt), e, _) ->
            (* Create a var out of the lhs *)
            let r_var = Dba.ExprVar(n, sz, opt) in
            (* Replace r_var by e into the current expr(acc) *)
            recurse_path tl (Dba_utils.substitute_dba_expr r_var e acc)
          | Dba.IkAssign(Dba.LhsVarRestrict(n, sz, low, hig), e, _) ->
            let r_var = Dba.ExprRestrict(Dba.ExprVar(n, sz, None), low, hig) in
            recurse_path tl (Dba_utils.substitute_dba_expr r_var e acc)
          | _ -> recurse_path tl acc
        end
      | [] -> acc
    in
    let full_e = recurse_path path var in (* Get the full DBA expr *)
    nth_dbainst <- 0; (* Set the current offset (so that concretize will consider to be offset 0) *)
    let final_bv = self#concretize_expr_bv full_e env in (* Concretize *)
    Logger.debug ~level:2 "%s backtracked to: %a = %a" name Pp.pp_expr full_e Bitvector.pp_hex final_bv;
    nth_dbainst <- offset; (* Push back the right offset *)
    final_bv


  method concretize_expr_bv (e:Dba.expr) ?(is_lhs=false) (env:Path_pred_env.t): Bitvector.t =
    let conc_var name low high env =
      try
        if is_tmp_variable name then (* if temporary variable *)
          self#backtrack_var name low high nth_dbainst env
        else
          let smtcst = self#concretize_var name low high env in
          match smtcst with
          | SmtBvCst bv -> bv
          | _ -> failwith "impossible"
      with
      | Trace_type.Not_found_in_concrete_infos _ ->
        Logger.error "%d %s %s: %a"
          cur_key_inst cur_inst.Trace_type.opcode
         (Decode_utils.string_to_hex cur_inst.Trace_type.opcode_bytes)
         Dba_types.Statement.pp cur_dbainst;
        failwith (name^" cannot be concretized because not in concrete infos")
    in
    let concrete_infos = cur_inst.Trace_type.concrete_infos in
    match e with
    | Dba.ExprBinary(Dba.Plus, (* Hack to concretize the whole address rather than every var, in case of gdt *)
      Dba.ExprLoad(_,_, Dba.ExprBinary(Dba.Plus, Dba.ExprVar("gdt",_,_) ,_)), _) ->
      let addr =
        if is_lhs then Trace_type.get_store_addr concrete_infos
        else Trace_type.get_load_addr concrete_infos in
      let bv_addr = Bitvector.create (Bigint.big_int_of_int64 addr) env.Path_pred_env.addr_size in
      Logger.debug ~level:0 "Heuristic concretize %a -> %Lx"  Pp.pp_expr e addr;
      bv_addr
    | Dba.ExprVar(name,sz,_) -> conc_var name 0 (sz-1) env
    | Dba.ExprLoad(sz, _, e) ->
      let addr_v = self#concretize_expr_bv e env |> Bitvector.value_of in
      let raw_content =
        Trace_type.get_load_content
          (Bigint.int64_of_big_int addr_v) sz concrete_infos in
      let c_content = string_to_big_int raw_content in
      Bitvector.create c_content (sz*8)
    | Dba.ExprCst(`Constant, bv) -> bv
    | Dba.ExprUnary(uop, e) ->
      begin match uop with
      | Dba.UMinus -> Bitvector.neg (self#concretize_expr_bv e env)
      | Dba.Not -> Bitvector.lognot (self#concretize_expr_bv e env)
      end
    | Dba.ExprBinary(bop, e1, e2) ->
      let apply_int op bv1 bv2 = op bv1 (Bitvector.size_of bv2) in
      let apply_of_bool op bv1 bv2 = Bitvector.of_bool (op bv1 bv2) in
      let f =
        match bop with
        | Dba.Plus        -> Bitvector.add
        | Dba.Minus       -> Bitvector.sub
        | Dba.MultU       -> Bitvector.umul
        | Dba.MultS       -> Bitvector.smul
        | Dba.DivU        -> Bitvector.udiv
        | Dba.DivS        -> Bitvector.sdiv
        | Dba.ModU        -> Bitvector.umod
        | Dba.ModS        -> Bitvector.smod
        | Dba.Or          -> Bitvector.logor
        | Dba.And         -> Bitvector.logand
        | Dba.Xor         -> Bitvector.logxor
        | Dba.Concat      -> Bitvector.append
        | Dba.LShift      -> apply_int Bitvector.shift_left
        | Dba.RShiftU     -> apply_int Bitvector.shift_right
        | Dba.RShiftS     -> apply_int Bitvector.shift_right_signed
        | Dba.LeftRotate  -> apply_int Bitvector.rotate_left
        | Dba.RightRotate -> apply_int Bitvector.rotate_right
        | Dba.Eq          -> apply_of_bool Bitvector.equal
        | Dba.Diff        -> apply_of_bool Bitvector.diff
        | Dba.LeqU        -> apply_of_bool Bitvector.ule
        | Dba.LtU         -> apply_of_bool Bitvector.ult
        | Dba.GeqU        -> apply_of_bool Bitvector.uge
        | Dba.GtU         -> apply_of_bool Bitvector.ugt
        | Dba.LeqS        -> apply_of_bool Bitvector.sle
        | Dba.LtS         -> apply_of_bool Bitvector.slt
        | Dba.GeqS        -> apply_of_bool Bitvector.sge
        | Dba.GtS         -> apply_of_bool Bitvector.sgt
      in
      let bv1 = self#concretize_expr_bv e1 env in
      let bv2 = self#concretize_expr_bv e2 env in
      begin try
        f bv1 bv2
      with Division_by_zero ->
        Logger.warning "Division by Zero caught while concretizing (replace by 0)";
        Bitvector.zeros (Bitvector.size_of bv1)
      end
    | Dba.ExprRestrict(Dba.ExprVar(n, _, _), l, h) when not (is_tmp_variable n) ->
      conc_var n l h env
    | Dba.ExprRestrict(e, l, h) ->
      Bitvector.extract (self#concretize_expr_bv e env) l h
    | Dba.ExprExtU(e, sz) ->
      Bitvector.extend (self#concretize_expr_bv e env) sz
    | Dba.ExprExtS(e, sz) ->
      Bitvector.extend_signed (self#concretize_expr_bv e env) sz
    | Dba.ExprIte(c, e1, e2) ->
      if self#concretize_cond c env then
        self#concretize_expr_bv e1 env
      else
        self#concretize_expr_bv e2 env
    | Dba.ExprAlternative(l,_) ->
      begin match l with
      | [] -> failwith "ExpreAlternative cannot be empty.."
      | e :: _ -> self#concretize_expr_bv e env
      end
    | _ -> failwith "not supported"

  method concretize_cond (c:Dba.cond) (env:Path_pred_env.t): bool =
    match c with
    | Dba.CondReif e ->
      let bv = self#concretize_expr_bv e env in
      if Bitvector.size_of bv <> 1 then
        failwith "Cond reification cannot be more than 1 bit long"
      else
        if Bitvector.is_zero bv then false
        else if Bitvector.is_one bv then true
        else failwith "Value cannot be other than 0 or 1"
    | Dba.CondNot c -> not(self#concretize_cond c env)
    | Dba.CondAnd(c1,c2) -> (self#concretize_cond c1 env) && (self#concretize_cond c2 env)
    | Dba.CondOr(c1,c2) -> (self#concretize_cond c1 env) || (self#concretize_cond c2 env)
    | Dba.True -> true
    | Dba.False -> false

  method private maj_variable (name:string) (size:Dba.size) (low:int) (high:int) (e:Dba.expr) (env:Path_pred_env.t): unit =
    let f_expr = self#expr_to_f e env in
    let constraints = List.rev pending_constraints in
    env.Path_pred_env.formula <- change_variable env.Path_pred_env.formula name size low high ~constraints f_expr

  method private inject_symbol_in_register (name:string) (reg_name:string) (low:int) (high:int) (env:Path_pred_env.t): unit =
    let size = high-low+1 in
    let new_var = SmtBvVar(name, size) in
    let new_f = add_symbolic_input env.Path_pred_env.formula name size in
    env.Path_pred_env.formula <- change_variable new_f reg_name (high-low+1) low high new_var

  method private inject_symbol_in_register_and_conc (name:string) (reg_name:string) (low:int) (high:int) (env:Path_pred_env.t): unit =
    self#inject_symbol_in_register name reg_name low high env;
    let c = self#concretize_var reg_name low high env in
    let f = SmtComp(SmtBvExpr(SmtBvBinary(SmtBvComp,SmtBvVar(name, high-low+1),
                                          c)), Smtlib2.one) in
    let new_f = add_constraint env.Path_pred_env.formula f in
    env.Path_pred_env.formula <- new_f

  method private maj_memory (fexpr:smt_abv_expr) (env:Path_pred_env.t): unit =
    env.Path_pred_env.formula <- store_memory env.Path_pred_env.formula ~constraints:(List.rev pending_constraints) fexpr

  method private symbolize_expr (e:Dba.expr) (env:Path_pred_env.t): smt_bv_expr =
    Logger.debug ~level:2 "Symbolise:%a" Pp.pp_expr e;
    stat_symb <- stat_symb +1;
    let size = computesize_dbaexpr e in
    let name = "expr_at_"^(string_of_int cur_key_inst)^"_"^(string_of_int nth_dbainst) in
    env.Path_pred_env.formula <- add_symbolic_input env.Path_pred_env.formula name size;
    SmtBvVar(name, size)

  method private concretize_expr (e:Dba.expr) ?(is_lhs=false) (env:Path_pred_env.t): smt_bv_expr =
    let c, sz =
      let bv = self#concretize_expr_bv e ~is_lhs env in (* Eval dba expression to get a constant *)
      Bitvector.value_of bv, Bitvector.size_of bv in
    Logger.debug ~level:2 "Concretize:%a  --> (%Lx,%d)" Pp.pp_expr e (Bigint.int64_of_big_int c) sz;
    stat_conc <- stat_conc +1; (* increment the number of expr concretized *)
    let f = self#expr_to_f e env in (* Build the constraint *)
    let bv = Bitvector.create c sz in
    (if not (bvexpr_equal f (SmtBvCst bv)) then
      let cst = SmtComp(SmtBvExpr(SmtBvBinary(SmtBvComp, f, SmtBvCst bv)), Smtlib2.one) in
      pending_constraints <- cst::pending_constraints);
    SmtBvCst bv (* Return the result as smt constant *)

  method private expr_to_f (e:Dba.expr) ?(is_lhs=false) (env:Path_pred_env.t): smt_bv_expr =
    if env.Path_pred_env.toplevel then
      (env.Path_pred_env.toplevel <- false;
      let action, _ = eval_expr_policy policy cur_dbainst e (self :> Path_pred_env.dse_analysis_sig_t) in
      match action with
      | Symb ->
        env.Path_pred_env.toplevel <- true; (* Experimentaly needed *)
        self#symbolize_expr e env
      | KeepOrConc ->
        fallback <- Conc;
        self#expr_to_f e env
      | KeepOrSymb ->
        fallback <- Symb;
        self#expr_to_f e env
      | Conc ->
        self#concretize_expr e ~is_lhs env)
    else begin
      env.Path_pred_env.toplevel <- true;
      let open Dba in
      match e with
      | Dba.ExprVar(name, size, _) -> self#var_to_f name 0 (size-1) env
      | Dba.ExprLoad(size, indien, e) ->  self#load_to_f e size indien env
      | Dba.ExprRestrict(e, i, j) ->
        (match e with
        | Dba.ExprVar(name, sz, _) -> self#var_to_f ~restrict_of:sz name i j env
        | _ -> SmtBvUnary(SmtBvExtract(i, j), self#expr_to_f e env))
      | Dba.ExprCst(region, bv) ->
        (match region with                                          (* Do not handle regions *)
        | `Constant -> SmtBvCst bv
        | _ -> failwith "region not supported")
      | Dba.ExprUnary(op, e) -> SmtBvUnary(Dba_to_smtlib.unary op, self#expr_to_f e env)
      | Dba.ExprBinary(op, e1, e2) -> (
        let expr1 = self#expr_to_f e1 env in
        let expr2 = self#expr_to_f e2 env in
        match op with
        | LShift|RShiftU|RShiftS ->
          (* Check size of the two bitvectors because DBA accept different but not smtlib *)
          let size1 = size_bvexpr expr1 in
          let size2 = size_bvexpr expr2 in
          if size1 = size2 then
            SmtBvBinary(Dba_to_smtlib.binary op, expr1, expr2)
          else if size1 > size2 then
            SmtBvBinary(Dba_to_smtlib.binary op, expr1, SmtBvUnary(SmtBvZeroExtend(size1-size2), expr2))
          else
            SmtBvBinary(Dba_to_smtlib.binary op, expr1, SmtBvUnary(SmtBvExtract(0,size1-1), expr2))
        | (LeqU|LtU|GeqU|GtU|LeqS|LtS|GeqS|GtS) -> (* Create formula to return #b1 or #b0 instead of a boolean *)
            SmtBvIte(SmtBvExpr(SmtBvBinary(Dba_to_smtlib.binary op, expr1, expr2)), Smtlib2.bvone , Smtlib2.bvzero )
        | LeftRotate ->
            if not (is_symbolic_bvexpr expr2) then
              let value = Dba_utils.Expr.fold_expr e2 |> Bitvector.value_of in
              SmtBvUnary(SmtBvRotateL(Bigint.int_of_big_int value), expr1 )
            else
              self#rotate_noncst_to_f e env
        | RightRotate ->
          if not (is_symbolic_bvexpr expr2) then
            let value = Dba_utils.Expr.fold_expr e2 |> Bitvector.value_of in
              SmtBvUnary(SmtBvRotateR(Bigint.int_of_big_int value), expr1 )
            else
              self#rotate_noncst_to_f e env
        | _ ->
            SmtBvBinary(Dba_to_smtlib.binary op, expr1, expr2))     (* Normal case *)
      | Dba.ExprExtU(e, size) ->
        let expr = self#expr_to_f e env in
        let s = size_bvexpr expr in
          SmtBvUnary(SmtBvZeroExtend(size-s), expr)
      | Dba.ExprExtS(e, size) ->
        let expr = self#expr_to_f e env in
        let s = size_bvexpr expr in
          SmtBvUnary(SmtBvSignExtend(size-s), expr)
      | Dba.ExprIte(c, e1, e2) ->
        SmtBvIte(self#cond_to_f c  env,
                 self#expr_to_f e1 env,
                 self#expr_to_f e2 env)
      | Dba.ExprAlternative(e_l, _) -> self#expr_to_f (List.hd e_l) env
      (*Alternative allow to specify multiple expr to represention an operation
        so they all have the same semantic (so take the first) *)
    end

method private var_to_f (name:string) ?(restrict_of=0) (low:int) (high:int) (env:Path_pred_env.t): smt_bv_expr =
  if contains_variable env.Path_pred_env.formula name then
    (* will never need to create since we already tested it exists *)
    snd (get_var_or_create env.Path_pred_env.formula name restrict_of low high)
  else
    match fallback with
    | Conc ->
      Logger.debug ~level:2 "Use fallback Conc %s" name;
      stat_conc <- stat_conc + 1;
      self#concretize_var name low high env
    | Symb ->
      Logger.debug ~level:2 "Use fallback Symb for %s[%d] (%d,%d)" name restrict_of low high;
      stat_symb <- stat_symb + 1;
      let new_f, f_expr = get_var_or_create env.Path_pred_env.formula name restrict_of low high in
      env.Path_pred_env.formula <- new_f;
      f_expr
    | _ -> failwith "Fallback cannot be other than Conc Symb"

method private rotate_noncst_to_f (e:Dba.expr) (env:Path_pred_env.t): smt_bv_expr =
  match rotate_fallback with
  | Conc
  | KeepOrConc ->
    Logger.debug ~level:1 "Rotate with expr shift concretize: %a" Pp.pp_expr e;
    self#concretize_expr e env
  | Symb
  | KeepOrSymb ->
    Logger.debug ~level:1 "Rotate with expr shift symbolize: %a" Pp.pp_expr e;
    self#symbolize_expr e env

(* ----------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------- *)

method private concretize_var (name:string) (low:int) (high:int) (env:Path_pred_env.t)
  : Smtlib2.smt_bv_expr =
  let open Trace_type in
  let reg = X86Util.string_of_register_restrict name low high in
  let value =
    if Basic_types.String.Map.mem name assignmap then (* If var present in assigned var (for the current DBA block) *)
      let first,last = Basic_types.String.Map.find name assignmap in (* get offset of the first and last assign within DBA block *)
      if first < nth_dbainst then
        if last < nth_dbainst then (* If we are using a var after its last assign takes its output value (written) *)
          Trace_type.get_reg_value reg ~read:false cur_inst.concrete_infos
        else begin
          Logger.debug ~level:3 "var %s already assigned so backtrack %d < %d!" name first nth_dbainst;
          Bigint.int64_of_big_int (Bitvector.value_of (self#backtrack_var name low high nth_dbainst env))
        end
      else (* Read var before its first assign so take the read value *)
        Trace_type.get_reg_value reg ~read:true cur_inst.concrete_infos
    else
      Trace_type.get_reg_value reg ~read:true cur_inst.concrete_infos
    in
    let mask1 = Int64.shift_left Int64.one (high-low+1) in
    let mask = Int64.shift_left (Int64.pred mask1) low in
    let value = Int64.shift_right_logical (Int64.logand value mask) low in
    SmtBvCst(Bitvector.create (Bigint.big_int_of_int64 value) (high-low+1))


method private exec_dynjump (e:Dba.expr) (env:Path_pred_env.t): unit =
  let open Trace_type in
  try
    let nextaddr =
      try
        get_next_address_bv cur_inst.concrete_infos env.Path_pred_env.addr_size
      with Not_found_in_concrete_infos _ ->
        if InstrMap.mem (cur_key_inst+1) trace.instrs then
          let next_i = InstrMap.find (cur_key_inst+1) trace.instrs in
          let next = next_i.location in
          (Bitvector.create (Bigint.big_int_of_int64 next) env.Path_pred_env.addr_size)
        else Bitvector.zeros env.Path_pred_env.addr_size
    in (* create bitvector from int64 *)
    if not (Bitvector.is_zero nextaddr) then (* if 0 we are on the last instr in the trace *)
      begin
        let f = self#compare_address e nextaddr env in
        Logger.debug ~level:3 "Add djmp constraint:%s" (Smtlib2print.smtexpr_to_string f);
        let new_f = add_constraint env.Path_pred_env.formula f in
        env.Path_pred_env.formula <- new_f (* modify the environnement *)
      end
  with Not_found_in_concrete_infos _ ->
    failwith "Next address not found while trying to execute djmp"

method private heuristic_local_jump (cond:Dba.cond) (off1:int) (off2:int) (next:Bitvector.t): int =
  let dba_instrs = cur_inst.Trace_type.dbainstrs in
  let inst1 = Dba_types.Statement.instruction (List.nth dba_instrs off1) in
  let inst2 = Dba_types.Statement.instruction (List.nth dba_instrs off2) in
  let next_ok = not(Bitvector.is_zeros next) in
  match inst1, inst2 with
  | (Dba.IkStop _), _ when next_ok -> Logger.warning ~level:1 "Heuristic: choose else branch local if" ; off2 (* if there is a next and one of the target is a stop got the other *)
  | _, (Dba.IkStop _) when next_ok -> Logger.warning ~level:1 "Heuristic: choose else branch local if" ; off1
  | _,_ ->
    begin match cond with
    | Dba.CondReif(Dba.ExprVar("DF", 1, None)) -> Logger.warning ~level:1 "Heuristic: choose DF=0 for local if" ; off2 (* consider that that we iterate string left to right *)
    | _ -> (* Else keep the shortest path to the exit (goto other instr) *)
      let l_path1 = path_to dba_instrs off1 [] |> List.length in
      let l_path2 = path_to dba_instrs off2 [] |> List.length in
      if l_path1 < l_path2 then
        (Logger.warning ~level:1 "Heuristic: choose if branch (local if)" ; off1)
      else
        (Logger.warning ~level:1 "Heuristic: choose else branch (local if)" ; off2)
    end

(* Update the path predicate in case of If (jump) *)
method private exec_if (cond:Dba.cond) (codeaddr:Dba.jump_target) (offset2:int) (env:Path_pred_env.t): unit =
  let concrete_infos = cur_inst.Trace_type.concrete_infos in
  let open Trace_type in
  try
    let nextaddr =
      try
        get_next_address_bv cur_inst.concrete_infos env.Path_pred_env.addr_size
      with Not_found_in_concrete_infos _ ->
        if InstrMap.mem (cur_key_inst+1) trace.instrs then
          let next_i = InstrMap.find (cur_key_inst+1) trace.instrs in
          let next = next_i.location in
          (Bitvector.create (Bigint.big_int_of_int64 next) env.Path_pred_env.addr_size)
        else Bitvector.zeros env.Path_pred_env.addr_size
    in (* create bitvector from int64 *)
    let new_f =
      match codeaddr with
      | Dba.JInner offset1 ->
        let f_cond = self#cond_to_f cond env in
        let good_offset = 
          if (is_concrete_infos_retrieved concrete_infos) || not(Dba_types.Condition.is_symbolic cond) then
            if self#concretize_cond cond env then offset1 else offset2
          else
            self#heuristic_local_jump cond offset1 offset2 nextaddr
        in
        let constraint_f = if good_offset = offset1 then f_cond else SmtNot(f_cond) in
        nth_dbainst <- good_offset;
        add_constraint env.Path_pred_env.formula constraint_f
      | Dba.JOuter addr ->
        let f_cond = self#cond_to_f cond env in            (* Convert condition into formula *)
        (* let _ = Logger.debug "Add constraint %s" (Smtlib2print.smtexpr_to_string f_cond) in *)
        let f_cond =
          if Bigint.compare_big_int (Dba_types.Caddress.base_value addr) (Bitvector.value_of nextaddr) = 0
          then f_cond else SmtNot f_cond
        in add_constraint env.Path_pred_env.formula f_cond
    in
    env.Path_pred_env.formula <- new_f (* modify the environnement *)
  with Not_found_in_concrete_infos _ ->
    failwith "Next address not found while trying to execute if"

method private map_assignmmap (instrs:Dba_types.Statement.t list): (int * int) Basic_types.String.Map.t =
  fst (List.fold_left (fun (acc,c) i ->
      match Dba_types.Statement.instruction i with
      | Dba.IkAssign(Dba.LhsVar(name,_,_), _, _) ->
        (* Dot not take in account extract
           (can cause problem if we want to backtrack larger variable) *)
        (* | IkAssign(LhsVarRestrict(name,_,_,_)) -> *)
        if not(Basic_types.String.Map.mem name acc) then
          (Basic_types.String.Map.add name (c,c) acc,c+1)
        else
          let f, _ = Basic_types.String.Map.find name acc in
          (Basic_types.String.Map.add name (f,c) acc,c+1)
      | _ -> (acc,c+1)
    ) (Basic_types.String.Map.empty,0) instrs)

method private init_trace (): bool =
  match input_config.trace_input with
  | Chunked(chan,r_all) ->
    let tr = Trace_loader.load_partial_trace_from_file ~load_all:r_all chan in
    trace <- tr;
    true
  | Stream _ident ->
    let content1 = Network_io.receive true in
    let cmd1, msg1 = get_cmd_content content1 in
    let content2 = Network_io.receive true in
    let cmd2, msg2 = get_cmd_content content2 in
    begin
      match cmd1,cmd2 with
      | "TRACE_HEADER","TRACE_CHUNK" ->
        trace <- Trace_loader.load_partial_trace_from_string msg1 msg2; true
      | _ ->
        Logger.error "Unknown command sequence:%s, %s" cmd1 cmd2;
        false
    end

method private fini_trace (): unit =
  match input_config.trace_input with
  | Chunked(chan, _) -> flush_all (); close_in chan
  | Stream _ -> ()


method private complete_trace (keep_existing:bool): unit =
  match input_config.trace_input with
  | Chunked(chan, _) ->
    begin match Trace_loader.parse_chunk chan with
      | Some ch ->
        trace <- Trace_loader.complete_partial_trace trace ch keep_existing
      | None -> trace <- {trace with Trace_type.complete=true}
    end
  | Stream _ident ->
    try
      Logger.debug ~level:0 "Try to complete trace";
      let content = Network_io.receive true in
      let cmd, msg = get_cmd_content content in
      self#input_message_received cmd msg
    with Unix.Unix_error(err,s1,_) ->
      Logger.error "Socket closed [%s]:%s" (Unix.error_message err) s1;
      trace <- {trace with Trace_type.complete=true}

method private check_messages (): unit =
  match input_config.trace_input with
  | Chunked _ -> ()
  | Stream _ident ->
    begin try
        let content = Network_io.receive false  in
        let cmd, msg = get_cmd_content content in
        self#input_message_received cmd msg
      with
      | Unix.Unix_error(Unix.EAGAIN, _, _) -> ()
      | Unix.Unix_error(err,s1,_) ->
        Logger.error "Error on the wire [%s]:%s" (Unix.error_message err) s1
    end

method private input_message_received (cmd:string) (data:string): unit =
  match cmd with
  | "TRACE_CHUNK" ->
    let buf = Piqirun.init_from_string data in
    let chunk = Trace_piqi.parse_chunk_t buf in
    Logger.warning "chunk received! (%d)" (List.length chunk.Trace_piqi.Chunk_t.body);
    trace <- Trace_loader.complete_partial_trace trace chunk true
  | "END" ->
    Logger.debug ~level:1 "END received";
    trace <- {trace with Trace_type.complete=true}
  | _ -> Logger.warning "Unhandled message [%s]" cmd

method private send_message (cmd:string) (data:string): unit =
  match input_config.trace_input with
  | Chunked(_,_) -> ()
  | Stream ident ->
    begin try
        Logger.debug ~level:1 "Will send %s:%s" cmd data;
        Network_io.send_client_message ident ~block:true cmd data
      with
      | Unix.Unix_error(Unix.EAGAIN, _, _) -> ()
      | Unix.Unix_error(err,s1,_) ->
        Logger.error "Error on the wire [%s]:%s" (Unix.error_message err) s1
    end

method private process_dbainstr ftrans dbainst env =
  let current_nth = nth_dbainst in
  match self#visit_dbainstr_before cur_key_inst cur_inst dbainst env with
  | SkipExec -> ()
  | StopExec -> raise Stop_analysis
  | DoExec ->
    begin
      ftrans dbainst env;
      match self#visit_dbainstr_after cur_key_inst cur_inst dbainst env with
      | SkipExec | StopExec -> raise Stop_analysis
      | DoExec -> () (* Just keep going *)
    end;
    if current_nth == nth_dbainst then
      nth_dbainst <- nth_dbainst + 1

method private process_instr ftrans (key:int) inst (env:Path_pred_env.t) =
  let open Trace_type in
  cur_key_inst <- key;
  cur_inst <- inst;
  if Basic_types.Int.Map.mem key trace.metadatas then
  List.iter (fun meta -> self#visit_metadata inst meta env;) (Basic_types.Int.Map.find key trace.metadatas);
  let loc = inst.location in
  occur_map <-
    if Basic_types.Int64.Map.mem loc occur_map
    then Basic_types.Int64.Map.add loc
        ((Basic_types.Int64.Map.find loc occur_map) + 1) occur_map
    else Basic_types.Int64.Map.add loc 1 occur_map;
  cur_k <- Basic_types.Int64.Map.find loc occur_map;
  nth_dbainst <- 0;
  assignmap <- self#map_assignmmap inst.dbainstrs;
  if do_compute_taint then ignore(tainting#compute_taint_instr inst) else ();
  match self#visit_instr_before cur_key_inst cur_inst env with
  | SkipExec -> ()
  | StopExec -> raise Stop_analysis
  | DoExec ->
    env.Path_pred_env.formula <- add_comment env.Path_pred_env.formula (self#get_comm()); (* Add the current instruction in comment in the formula *)
    if inst.decoded then
      begin
        List.iteri
          (fun i dbainst ->
             if i = nth_dbainst then self#process_dbainstr ftrans dbainst env
          ) inst.dbainstrs;
        if is_libcall inst.concrete_infos then
          let open Configuration in
          Libcall_stubs.apply_libcall_policy
            config.libcalls
            cur_key_inst inst
            config.callcvt config.default_action env
        else ();
        match self#visit_instr_after cur_key_inst cur_inst env with
        | SkipExec | StopExec ->
          (* Consider both SkipExec meaning to stop the analysis *)
          raise Stop_analysis
        | DoExec -> () (* Just keep going *)
      end
    else
    if Syscall_stubs.is_syscall_opcode inst.opcode then
      (* Try to see if there is any policy for the given syscall *)
      Syscall_stubs.dispatch_syscall config.Configuration.syscalls inst env
    else
      (* Try to see if there is any instruction policy for this unhandled instruction *)
      Instruction_stubs.dispatch_instruction config.Configuration.instrs inst env

method private add_initial_state (env:Path_pred_env.t): unit =
  List.iter (fun mem_t ->
      Logger.debug ~level:0 "Initial state at %Lx:%s" (mem_t.addr) (string_to_hex mem_t.value);
      let bytes = string_to_int_list mem_t.value in
      let init =
        List.fold_left
          (fun (acc, i) v ->
             Basic_types.Addr64.Map.add (Int64.add (mem_t.addr) i) v acc,
             Int64.add i 1L
          )
          (Basic_types.Addr64.Map.empty, 0L) bytes
        |> fst
      in
      env.Path_pred_env.formula <- add_initial_state env.Path_pred_env.formula init
    ) config.Configuration.initial_state

method private create_env (): Path_pred_env.t =
  Path_pred_env.new_env (self :> Path_pred_env.dse_analysis_sig_t) config
    ~cst_pro:config.Configuration.optim_cstprop
    ~rebase:config.Configuration.optim_rebase
    ~row:config.Configuration.optim_row
    ~row_plus:config.Configuration.optim_rowplus
    ~eq_prop:config.Configuration.optim_eqprop
    trace.Trace_type.address_size

method compute : int =
  if not(self#init_trace ()) then -1
  else
  let env = self#create_env () in
  self#add_initial_state env;
  if solve_incrementally then
    solver_session <-
      start_interactive
        ~file:default_formula_file
        ~timeout input_config.configuration.Configuration.solver;
  if policy = [] then Logger.warning "No policy provided (use default)";
  self#pre_execution env;
  let rec process_trace key =
    let open Trace_type in
    if Trace_type.InstrMap.mem key trace.Trace_type.instrs then begin
      let inst = Trace_type.InstrMap.find key trace.instrs in
      self#process_instr self#exec key inst env;
      self#check_messages();
      process_trace (key+1)
    end
    else begin
      if not(trace.complete) then self#complete_trace false;
      if trace.complete then
        raise Stop_analysis
      else
        process_trace key
    end
  in
  try process_trace 0
  with Stop_analysis ->
    self#fini_trace ();
    nth_dbainst <- nth_dbainst - 1;
    self#post_execution env


method private exec_nondet (lhs:Dba.lhs) (env:Path_pred_env.t): unit =
  (* Cosmetic overwriting of exec_nondet to put more explicit names for symbols on syscalls *)
  let name, new_f =
    let open Trace_type in
    if is_syscall cur_inst.concrete_infos then
      let name = get_syscall_name cur_inst.concrete_infos in
      let lhsname =
        match Dba_types.LValue.name_of lhs with
        | Some name -> name
        | None -> "store"
      in lhsname^"_"^name^"_"^(string_of_int cur_key_inst), env.Path_pred_env.formula
    else
      new_variable_name env.Path_pred_env.formula "nondet"
  in
  let size = Dba_types.LValue.unsafe_bitsize lhs in
  env.Path_pred_env.formula <- fst(get_var_or_create new_f name size 0 (size-1));
  self#exec_lhs lhs (Dba.ExprVar(name,size,None)) env

  (* Method abstracting solving via file or incremental mode *)
method solve_predicate
    (pred:smt_expr) ?(print_stat=false) ?(name="") ?(push=true) ?(pop=true)
    ?(prek=(-1)) ?(pruning=true) ?(get_model=true) (env:Path_pred_env.t):
  Smtlib2.smt_result * Smt_model.t * float =
  let name = if name = "" then default_formula_file else name in
  let solver = config.Configuration.solver in
  if solve_incrementally then
    (let newf, status = build_formula_incremental env.Path_pred_env.formula pred ~push ~pruning ~file:name solver_session solver in
     env.Path_pred_env.formula <- newf;
     let res =
       match status with
       | Some UNSAT -> Logger.debug "use Qed status"; UNSAT, Smt_model.empty, 0.0
       | _ -> solve_incremental_model_time solver_session ~file:name ~get_model solver
     in
     if pop then Solver.pop ~file:name solver_session;
     res)
  else
    (let newf, status =
       if prek <> -1 then
         build_formula_file env.Path_pred_env.formula pred name ~forward:false ~pruning ~ksteps:prek solver
       else
         build_formula_file env.Path_pred_env.formula pred name ~pruning solver
     in
     if print_stat then
       Logger.info "Inputs:%d Vars:%d Load:%d Store:%d Csts:%d"
         newf.nb_input newf.nb_let newf.nb_load newf.nb_store newf.nb_constraint;
     match status with
     | Some(UNSAT) -> Logger.debug "use Qed status"; UNSAT, Smt_model.empty, 0.0
     | _ -> solve_model_time ~timeout:timeout ~get_model name solver)


method private visit_instr_before _ _ _ = DoExec

method private visit_instr_after _ _ _ = DoExec

method private visit_dbainstr_before _ _ _ _ =  DoExec

method private visit_dbainstr_after _ _ _ _ =  DoExec

method private post_execution _ = 0

method private pre_execution _ = ()

method private visit_metadata _inst _meta _env = ()

(* ----------------------------------------------- *)
(* ---------------- CommonAnalysis --------------- *)
method private logical_load (addr:smt_bv_expr) (size:Dba.size) (env:Path_pred_env.t): smt_bv_expr = (* size in BYTES *)
  let rec concat_select_symb sz =
    (* Recursive function to concat each byte read into a single bitvector *)
    let to_add =  size-sz in
    let new_addr =
      if to_add = 0 then addr
      else smtbv_add_int addr to_add in
    match sz with
    | 1 -> SmtABvSelect(env.Path_pred_env.formula.memory, new_addr)
    | 4 -> SmtABvLoad32(env.Path_pred_env.formula.memory, addr)
    | n -> SmtBvBinary(SmtBvConcat, concat_select_symb (n - 1),
                       SmtABvSelect(env.Path_pred_env.formula.memory, new_addr))
  in
  concat_select_symb size

method private load_to_f (e:Dba.expr) (size:Dba.size) endian (env:Path_pred_env.t): smt_bv_expr =
  (* Convert a load in expression and update memory accordingly *)
  match endian with
  | Dba.BigEndian -> failwith "Big endian not implemented\n"
  | Dba.LittleEndian ->
    let expr_f = self#expr_to_f e env in
    self#logical_load expr_f size env

(* Convert a condition into formula
   TODO: Check for converting bvcmp X 1 into = *)
method private cond_to_f (c:Dba.cond) (env:Path_pred_env.t): smt_expr =
  match c with
  | Dba.CondReif(e) ->
     let expr = self#expr_to_f e env in SmtComp(SmtBvExpr expr, Smtlib2.one)
  | Dba.CondNot(c) -> SmtNot(self#cond_to_f c env)
  | Dba.CondAnd(c1, c2) ->
     SmtAnd(self#cond_to_f c1 env, self#cond_to_f c2 env)
  | Dba.CondOr(c1, c2) ->
     SmtOr(self#cond_to_f c1 env, self#cond_to_f c2 env)
  | Dba.True -> SmtTrue
  | Dba.False -> SmtFalse

method private logical_store (f_addr:smt_bv_expr) (f_expr:smt_bv_expr) size env=
  (* Recursive function to create store on multiple index (by extracting each
      byte of the bv) *)
  let rec aux_store_symb sz data l h =
    let to_add =  size - sz in
    let new_addr =
      if to_add = 0 then f_addr
      else smtbv_add_int f_addr to_add in
    match sz with
    | 1 -> SmtABvStore(env.Path_pred_env.formula.memory, new_addr, smtbv_extract data l h)
    | n -> SmtABvStore(aux_store_symb (n - 1) data (l + 8) (h + 8),
                       new_addr, smtbv_extract data l h)
  in
  match size with
  | 1 -> self#maj_memory (SmtABvStore(env.Path_pred_env.formula.memory, f_addr, f_expr)) env
  | 4 -> self#maj_memory (SmtABvStore32(env.Path_pred_env.formula.memory, f_addr, f_expr)) env
  | n -> let new_formula, new_var = new_tmpvar env.Path_pred_env.formula (size_bvexpr f_expr) in
    let content_f = aux_store_symb n new_var 0 7 in
    let f = SmtABvLet([(SmtBvExpr new_var, SmtBvExpr f_expr)], content_f) in
    env.Path_pred_env.formula <- new_formula;
    self#maj_memory f env

method private exec_store (e_addr:Dba.expr) (size:Dba.size) (e:Dba.expr) endian (env:Path_pred_env.t): unit =
  (* Purely symbolic implementation *)
  match endian with
  | Dba.BigEndian -> failwith "Big endianness not yet implemented"
  | Dba.LittleEndian ->
    let f_expr = self#expr_to_f e ~is_lhs:false env in  (* Formula of data to write *)
    let f_addr = self#expr_to_f e_addr ~is_lhs:true env in
    self#logical_store f_addr f_expr size env

method private exec_lhs lhs e (env:Path_pred_env.t): unit =
  match lhs with
  | Dba.LhsVar(name, size, _) ->            self#maj_variable name size 0 (size-1) e env
  | Dba.LhsVarRestrict(name, size, i, j) -> self#maj_variable name size i j e env
  | Dba.LhsStore(size, indien, e_addr) ->   self#exec_store e_addr size e indien env

method exec (instr:Dba_types.Statement.t) (env:Path_pred_env.t): unit =
  pending_constraints <- [];
  cur_dbainst <- instr;
  match Dba_types.Statement.instruction instr with
  | Dba.IkAssign (lhs, expr, _) -> self#exec_lhs lhs expr env
  | Dba.IkSJump (Dba.JOuter(_), _) -> ()
  | Dba.IkSJump (Dba.JInner off, _) -> if off > nth_dbainst then nth_dbainst <- off
  | Dba.IkDJump (e, _) -> self#exec_dynjump e env
  | Dba.IkMalloc (_, _, _) -> ()
  | Dba.IkFree (_, _) -> ()
  | Dba.IkIf (cond, codeaddr, off) -> self#exec_if cond codeaddr off env
  | Dba.IkStop _ -> ()
  | Dba.IkAssert (_, _) -> ()
  | Dba.IkAssume (_, _) -> ()
  | Dba.IkNondetAssume (_, _, _) -> ()
  | Dba.IkNondet (lhs, `Constant, _) -> self#exec_nondet lhs env
  | Dba.IkNondet (_, _, _) -> ()
  | Dba.IkUndef (_, _) -> ()
  | Dba.IkPrint (_, _) -> ()


(* ------ Internal functions ------ *)
method private compare_address (expr:Dba.expr) (addr:Bitvector.t) ?(apply_cs=true) (env:Path_pred_env.t): smt_expr = (* Perform a comparison of a formula to an address *)
  let f_expr = self#tricky_expr_to_f "dumb" expr ~apply_cs env in
  let f_addr = SmtBvCst addr in (* Convert the address into formula *)
  SmtComp(SmtBvExpr(SmtBvBinary(SmtBvComp, f_expr, f_addr)), one)
(* -------------------------------- *)


(* ---- Function meant to be used by compute to drive the analyse ---- *)
method expr_to_smt (e:Dba.expr) ?(apply_cs=true) (env:Path_pred_env.t): smt_bv_expr * smt_expr list =
  let size = computesize_dbaexpr e in
  let stub_inst =
    let instr = Dba.IkAssign(Dba.LhsVar("dumb", size,None), e, nth_dbainst+1) in
    Dba_types.Statement.set_instruction cur_dbainst instr in
  let saved_inst = cur_dbainst in
  cur_dbainst <- stub_inst;(* Dba.Temporarily replace current inst to make the policy engine matchable *)
  let saved_cst = pending_constraints in
  pending_constraints <- [];
  let f_expr = self#expr_to_f e {env with Path_pred_env.toplevel=apply_cs} in
  let new_cst = pending_constraints in
  cur_dbainst <- saved_inst;
  pending_constraints <- saved_cst;
  f_expr, new_cst

method private tricky_expr_to_f (name:string) ?(apply_cs=true) (e:Dba.expr) (env:Path_pred_env.t): smt_bv_expr =
  let size = computesize_dbaexpr e in
  let inst = Dba.IkAssign(Dba.LhsVar(name, size,None), e, nth_dbainst+1) in
  let stub_inst = Dba_types.Statement.set_instruction cur_dbainst inst in
  let saved_inst = cur_dbainst in
  cur_dbainst <- stub_inst; (* Dba.Temporarily replace current inst to make the policy engine matchable *)
  let f_expr = self#expr_to_f e {env with Path_pred_env.toplevel=apply_cs} in
  cur_dbainst <- saved_inst;
  f_expr

method private tricky_cond_to_f (c:Dba.cond) ?(apply_cs=true
                                              ) (env:Path_pred_env.t): smt_expr
  =
  let inst =
    Dba_types.Instruction.ite c
      (Dba.JOuter (Dba_types.Caddress.block_start @@ Bitvector.zeros 32))
      (nth_dbainst+1) in
  let stub_inst =
    Dba_types.Statement.set_instruction cur_dbainst inst in
  let saved_inst = cur_dbainst in
  cur_dbainst <- stub_inst; (* Temporarily replace current inst to make the policy engine matchable *)
  let f_expr = self#cond_to_f c {env with Path_pred_env.toplevel=apply_cs} in
  cur_dbainst <- saved_inst;
  f_expr


method private is_symbolic_expression (e:Dba.expr) (env:Path_pred_env.t): bool =   (* Return True if the formula is symbolic (contraints free vars, or select) *)
  let f = self#expr_to_f e {env with Path_pred_env.toplevel=false} in                          (* update=false allow not to update memory, and symbolic vars (just generate formula) *)
  is_symbolic_bvexpr f

method private is_symbolic_condition (c:Dba.cond) (env:Path_pred_env.t): bool =  (* Return True if the formula is symbolic (contraints free vars, or select) *)
  let f = self#cond_to_f c {env with Path_pred_env.toplevel=false} in                (* update=false allow not to update memory, and symbolic vars (just generate formula) *)
  is_symbolic_expr f

method private build_cond_predicate (c:Dba.cond) (env:Path_pred_env.t): smt_expr =
  self#tricky_cond_to_f c {env with Path_pred_env.toplevel=false}

method private build_address_comparison_predicate (e:Dba.expr) (addr:Bitvector.t) ?(apply_cs=true) (env:Path_pred_env.t): smt_expr =
  self#compare_address e addr {env with Path_pred_env.toplevel=apply_cs}

method private add_witness_variable (name:string) (e:Dba.expr) ?(apply_cs=true) (env:Path_pred_env.t): unit = (* Note would have returned env if env was not mutable *)
  (* Does not add the variable in the symbolic store because it is not meant to be used elsewhere *)
  let size = computesize_dbaexpr e in
  let f_expr = self#tricky_expr_to_f name e ~apply_cs env in
  let new_var = SmtBvVar(name, size) in
  let new_f = add_symbolic_input env.Path_pred_env.formula name size in
  let new_f = add_constraint new_f (SmtComp(SmtBvExpr(new_var), SmtBvExpr(f_expr))) in
  env.Path_pred_env.formula <- new_f (* modify the environnement *)

method private build_witness_bitvector_comparison_predicate (name:string) (size:int) (bitvec:Bitvector.t): smt_expr = (* Perform a comparison of a formula to a bitvector (that can be an address) *)
  (* Not really generic, really oriented to negate addresses to force jumping somewhere else *)
  let witness_var = SmtBvExpr(SmtBvVar(name,size)) in
  let f_val = SmtBvCst bitvec in (* Convert the address into formula *)
  SmtComp(witness_var, SmtBvExpr(f_val))

method private build_witness_expr_comparison_predicate (name:string) (size:int) (e:Dba.expr) ?(apply_cs=true) env: smt_expr =
  let f_expr = self#tricky_expr_to_f name e ~apply_cs env in
  let witness_f = SmtBvExpr(SmtBvVar(name, size)) in
  SmtComp(witness_f, SmtBvExpr(f_expr))

method private build_multiple_condition_predicate (conds:smt_expr list): smt_expr =     (* Generate the path predicate from the list, by folding each item from the end *)
  let f_tmp = List.fold_left (fun tok cond -> SmtAnd(replace_token_expr tok cond, SmtToken)) SmtToken conds in
  let f = replace_token_expr f_tmp SmtTrue in
  f
  (* ---------------------------------------------------------------------- *)

end
