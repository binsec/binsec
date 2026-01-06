(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module V = Virtual_address
module A = Dba_types.Caddress

module Status = struct
  type t = Unknown | Unreachable | Clear | Opaque of bool

  let pp ppf = function
    | Unknown -> Format.pp_print_string ppf "unknown"
    | Unreachable -> Format.pp_print_string ppf "unreachable"
    | Clear -> Format.pp_print_string ppf "clear"
    | Opaque true -> Format.pp_print_string ppf "opaque branch"
    | Opaque false -> Format.pp_print_string ppf "opaque fallthrough"
end

module Directive = struct
  type t =
    | ExpectAt of Virtual_address.t * Status.t
    | SkipJump of Virtual_address.t
    | ProcessCall of Virtual_address.t
end

module Query_stat = Metrics.Query ()

module P =
  Binsec_symbolic.Path.Make (Query_stat) (Binsec_symbolic.Default.State)

type ('a, 'b) fiber = ('a, 'b) Binsec_sse.Types.fiber
type node = ([ `All ], P.t) fiber

module Env = struct
  type t = {
    cfg : Ghidra_cfg.t;  (** The Ghidra cfg *)
    ims : string Virtual_address.Htbl.t;  (** Instruction mnemonic storage *)
    ctp : Virtual_address.Set.t;  (** Call to process *)
    bbt : node Dba_types.Caddress.Htbl.t;  (** Basic bloc table *)
    dis : Dba.Instr.t Dba_types.Caddress.Htbl.t;  (** Dba instruction storage *)
    dap : Dba_types.Caddress.t list Dba_types.Caddress.Htbl.t;
        (** Dba address predecessors *)
    opa : bool Dba_types.Caddress.Htbl.t;  (** Opaque predicate addresses *)
  }

  let empty =
    {
      cfg = Ghidra_cfg.create ();
      ims = V.Htbl.create 0;
      ctp = V.Set.empty;
      bbt = A.Htbl.create 0;
      dis = A.Htbl.create 0;
      dap = A.Htbl.create 0;
      opa = A.Htbl.create 0;
    }
end

let k_env : Env.t P.key = P.declare_field Env.empty
let k_conditions : bool list P.key = P.declare_field []
let k_jump_targets : V.t list P.key = P.declare_field []

module Path = struct
  include P

  let init :
      env:Env.t ->
      conditions:bool list ->
      jump_targets:Virtual_address.t list ->
      t =
   fun ~env ~conditions ~jump_targets ->
    let path = create () in
    set path k_env env;
    set path k_conditions conditions;
    set path k_jump_targets jump_targets;
    path
end

module As = Compiler.Cse
module VarSet = Dba_types.Var.Set

let handle_jump : Path.t As.t -> VarSet.t -> int Dba.jump_target -> VarSet.t =
 fun bb vars jmp ->
  match jmp with
  | JInner _ -> vars
  | JOuter _ ->
      VarSet.filter
        (fun var ->
          match var.info with
          | Temp ->
              As.forget bb var;
              false
          | _ -> true)
        vars

let goto : A.t -> int Dba.jump_target -> A.t =
 fun addr target ->
  match target with JInner n -> A.reid addr n | JOuter addr -> addr

let commit : Path.t As.t -> node -> node =
 fun bb succ ->
  let placeholder : ([ `Assume ], 'a) fiber =
    Assume { test = Dba.Expr.one; succ = Compiler.invalid_successor }
  in
  Compiler.relink
    ~pred:
      (As.commit bb ~pred:(match placeholder with Assume _ as head -> head))
    succ;
  let (Assume { succ; _ }) = placeholder in
  succ

module rec Driver : sig
  include
    Interpreter.DRIVER
      with type path = Path.t
       and type outcome = (Path.value, Binsec_sse.Types.status) Result.t

  val start : path -> A.t -> outcome
end = struct
  type path = Path.t
  type outcome = (Path.value, Binsec_sse.Types.status) Result.t

  let step : path -> ([ `Step ], path) fiber -> outcome =
   fun path (Step { addr; succ; _ }) ->
    Path.set_pc path addr;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let assign : path -> ([ `Assign ], path) fiber -> outcome =
   fun path (Assign { var; rval; succ }) ->
    Path.assign path var rval;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let clobber : path -> ([ `Clobber ], path) fiber -> outcome =
   fun path (Clobber { var; succ }) ->
    Path.clobber path var;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let load : path -> ([ `Load ], path) fiber -> outcome =
   fun path (Load { var; base; addr; dir; succ }) ->
    Path.load path var base ~addr dir;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let store : path -> ([ `Store ], path) fiber -> outcome =
   fun path (Store { base; addr; dir; rval; succ }) ->
    Path.store path base ~addr rval dir;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let symbolize : path -> ([ `Symbolize ], path) fiber -> outcome =
   fun path (Symbolize { var; succ }) ->
    Path.symbolize path var;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let apply : path -> ([ `Apply ], path) fiber -> outcome =
   fun path (Apply { f; succ }) ->
    f path;
    (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let assume : path -> ([ `Assume ], path) fiber -> outcome =
   fun path (Assume { test; succ }) ->
    match Path.State.assume (Path.get_value path test) (Path.state path) with
    | None -> Error Unsatisfiable_assumption
    | Some state ->
        Path.set_state path state;
        (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let check : path -> ([ `Assert ], path) fiber -> outcome =
   fun path (Assert { test; succ }) ->
    match Path.State.assume (Path.get_value path test) (Path.state path) with
    | None -> Error Unsatisfiable_assumption
    | Some state ->
        Path.set_state path state;
        (Interpreter.dispatch [@tailcall]) path succ (module Driver)

  let ite : path -> ([ `Branch ], path) fiber -> outcome =
   fun path (Branch { test; taken; fallthrough }) ->
    let test = Path.get_value path test in
    match Path.get path k_conditions with
    | [] -> Ok test
    | value :: conditions -> (
        match
          Path.State.assume
            (if value then test else Path.Value.unary Not test)
            (Path.state path)
        with
        | None -> Error Unsatisfiable_assumption
        | Some state ->
            Path.set path k_conditions conditions;
            Path.set_state path state;
            (Interpreter.dispatch [@tailcall]) path
              (if value then taken else fallthrough)
              (module Driver))

  let rec resolve : Env.t -> A.t -> node =
   fun env addr ->
    Options.Logger.debug ~level:2 "resolve (%a,%d)" A.pp_base addr addr.id;
    try A.Htbl.find env.bbt addr
    with Not_found ->
      let node = line env (As.empty (module Path)) VarSet.empty addr in
      A.Htbl.add env.bbt addr node;
      node

  and line : Env.t -> Path.t As.t -> VarSet.t -> A.t -> node =
   fun env bb vars addr ->
    match A.Htbl.find env.dis addr with
    | Assign (Var v, e, n) ->
        As.assign bb v e;
        line env bb (VarSet.add v vars) (A.reid addr n)
    | Assign (Restrict (v, { lo; hi }), e, n) ->
        As.assign bb v (Dba_types.Expr.complement e ~lo ~hi v);
        line env bb (VarSet.add v vars) (A.reid addr n)
    | Assign (Store (_, dir, ptr, base), e, n) ->
        As.store bb base dir ~addr:ptr e;
        line env bb vars (A.reid addr n)
    | Nondet (Var v, n) ->
        As.symbolize bb v;
        line env bb (VarSet.add v vars) (A.reid addr n)
    | Undef (Var v, n) ->
        As.clobber bb v;
        line env bb (VarSet.add v vars) (A.reid addr n)
    | ( Nondet (Restrict (v, { hi; lo }), n)
      | Undef (Restrict (v, { hi; lo }), n) ) as inst ->
        let size' = hi - lo + 1 in
        let name' = Format.sprintf "%%entropy%%%d" size' in
        let var' = Dba.Var.temporary name' (Size.Bit.create size') in
        let rval = Dba_types.Expr.complement (Dba.Expr.v var') ~lo ~hi v in
        (match inst with
        | Nondet _ -> As.symbolize bb var'
        | _ -> As.clobber bb var');
        As.assign bb v rval;
        line env bb (VarSet.add var' (VarSet.add v vars)) (A.reid addr n)
    | ( Nondet (Store (bytes, dir, ptr, base), n)
      | Undef (Store (bytes, dir, ptr, base), n) ) as inst ->
        let size' = 8 * bytes in
        let name' = Format.sprintf "%%entropy%%%d" size' in
        let var' = Dba.Var.temporary name' (Size.Bit.create size') in
        let rval = Dba.Expr.v var' in
        (match inst with
        | Nondet _ -> As.symbolize bb var'
        | _ -> As.clobber bb var');
        As.store bb base dir ~addr:ptr rval;
        line env bb (VarSet.add var' vars) (A.reid addr n)
    | SJump (target, _) ->
        line env bb (handle_jump bb vars target) (goto addr target)
    | If (test, target, n) ->
        commit bb
          (Branch
             {
               test;
               taken = continue (goto addr target);
               fallthrough = continue (A.reid addr n);
             })
    | DJump (target, _) -> commit bb (Jump target)
    | Stop _ ->
        commit bb
          (Tail_call (Fun.const (Binsec_sse.Types.Signal (Error "stop"))))
    | Assert (test, n) ->
        As.check bb test;
        line env bb vars (A.reid addr n)
    | Assume (test, n) ->
        As.assume bb test;
        line env bb vars (A.reid addr n)

  and continue : A.t -> node =
   fun addr ->
    Tail_call (fun path -> Continue (resolve (Path.get path k_env) addr))

  let goto : path -> ([ `Goto ], path) fiber -> outcome =
   fun path (Goto addr) ->
    (Interpreter.dispatch [@tailcall]) path
      (resolve (Path.get path k_env) (A.of_virtual_address addr))
      (module Driver)

  let jump : path -> ([ `Jump ], path) fiber -> outcome =
   fun path (Jump target) ->
    let target = Path.get_value path target in
    match Path.get path k_jump_targets with
    | [] -> Error (Error "jump")
    | addr :: jump_targets -> (
        match
          Path.State.assume
            (Path.Value.binary Eq target
               (Path.Value.constant
                  (Bitvector.create (V.to_bigint addr)
                     (Path.Value.sizeof target))))
            (Path.state path)
        with
        | None -> Error Unsatisfiable_assumption
        | Some state ->
            Path.set path k_jump_targets jump_targets;
            Path.set_state path state;
            goto path (Goto addr))

  let rec resume : path -> Path.t Binsec_sse.Types.continuation -> outcome =
   fun path k ->
    match k with
    | Continue succ ->
        (Interpreter.dispatch [@tailcall]) path succ (module Driver)
    | Tail_call f -> resume path (f path)
    | Call _ | Return | Return_to _ | Signal _ | Fork _ | Merge _ ->
        Error (Error "resume")

  let call : path -> ([ `Call ], path) fiber -> outcome =
   fun _ _ -> Error (Error "resume")

  let tail_call : path -> ([ `Tail_call ], path) fiber -> outcome =
   fun path (Tail_call f) -> resume path (f path)

  let start : path -> A.t -> outcome =
   fun path addr ->
    (Interpreter.dispatch [@tailcall]) path
      (resolve (Path.get path k_env) addr)
      (module Driver)
end
