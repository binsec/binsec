(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

let solvers =
  let open Formula_options in
  [ Bitwuzla; Boolector; Z3; CVC4; Yices ]

let map =
  let open Formula_options in
  let open Smt_options in
  function
  | Auto | Bitwuzla_native -> assert false
  | Bitwuzla_smtlib -> Bitwuzla
  | Boolector_smtlib -> Boolector
  | Z3_smtlib -> Z3
  | CVC4_smtlib -> CVC4
  | Yices_smtlib -> Yices

let get_solver_factory () =
  let open Formula_options in
  let open Smt_options in
  match Smt_options.SMTSolver.get () with
  | (Smt_options.Auto | Smt_options.Bitwuzla_native) when Smt_bitwuzla.available
    ->
      (module Native_solver.Solver : Solver_sig.FACTORY)
  | Auto -> (
      try
        let solver = List.find Prover.ping solvers in
        Logger.info "Found %a in the path." Prover.pp solver;
        Solver.set solver;
        (module Smt2_solver.Solver : Solver_sig.FACTORY)
      with Not_found -> Logger.fatal "No SMT solver found.")
  | Bitwuzla_native ->
      Logger.fatal "Native bitwuzla binding is required but not available."
  | solver when Prover.ping (map solver) ->
      Solver.set (map solver);
      (module Smt2_solver.Solver : Solver_sig.FACTORY)
  | solver ->
      Logger.fatal "%a is required but not available in path." Prover.pp
        (map solver)

exception Undef of Types.Var.t

exception Uninterp of string

exception Unknown = Types.Unknown

exception Non_unique = Types.Non_unique

exception Non_mergeable = Types.Non_mergeable

type 'a test = 'a Types.test =
  | True of 'a
  | False of 'a
  | Both of { t : 'a; f : 'a }

(* utils *)
let byte_size = Natural.to_int Basic_types.Constants.bytesize

module BiMap = Basic_types.BigInt.Map
module NiTbl = Basic_types.Int.Htbl
module Sname = Suid
open Sexpr
module BiItM = Imap
module BvSet = Set.Make (Expr)
module S = Basic_types.String.Map
module I = Basic_types.Int.Map
module R = Basic_types.Int.Htbl

module State (F : Solver_sig.FACTORY) (QS : Types.QUERY_STATISTICS) = struct
  type t = {
    constraints : Expr.t list;
    (* reversed sequence of assertions *)
    constset : BvSet.t;
    vsymbols : Expr.t I.t;
    (* collection of visible symbols *)
    varrays : Memory.t S.t;
    (* collection of visible arrays *)
    vmemory : Memory.t;
    (* visible memory *)
    fid : Sname.t;
    (* unique indice counter *)
    fvariables : Expr.t list S.t;
    (* collection of free variables *)
    farrays : Memory.t S.t;
    (* collection of free array *)
    ilocs : (Z.t * Loader_buf.t) BiItM.t;
    (* set of initialized memory locations *)
    model : Model.t; (* a model that satisfy constraints *)
  }

  let pp ppf state =
    Model.pp ppf state.fvariables
      (Kernel_options.Machine.word_size ())
      state.model

  let empty () =
    {
      constraints = [];
      constset = BvSet.empty;
      vsymbols = I.empty;
      varrays = S.empty;
      vmemory = Memory.Root;
      fid = Sname.(incr zero);
      (* zero is reserved for initial memory *)
      fvariables = S.empty;
      farrays = S.empty;
      ilocs = BiItM.empty;
      model = Model.empty ();
    }

  let fresh (Var { name; size; label; _ } : Types.Var.t) state =
    let v = Expr.var (Sname.to_string state.fid) size name in
    let fid = Sname.incr state.fid in
    let h =
      match S.find name state.fvariables with
      | exception Not_found -> [ v ]
      | h -> v :: h
    in
    let fvariables = S.add name h state.fvariables in
    let vsymbols = I.add label v state.vsymbols in
    { state with vsymbols; fid; fvariables }

  let alloc name state =
    let symbol = Memory.Symbol name in
    {
      state with
      varrays = S.add name symbol state.varrays;
      farrays = S.add name symbol state.farrays;
    }

  let assign' (Var { label; _ } : Types.Var.t) value state =
    { state with vsymbols = I.add label value state.vsymbols }

  let write' ~addr value dir state =
    { state with vmemory = Memory.write ~addr value dir state.vmemory }

  let store' name ~addr value dir state =
    try
      let ar = S.find name state.varrays in
      {
        state with
        varrays = S.add name (Memory.write ~addr value dir ar) state.varrays;
      }
    with Not_found -> raise_notrace (Uninterp name)

  let read ~addr bytes dir state = Memory.read ~addr bytes dir state.vmemory

  let select name ~addr bytes dir state =
    try Memory.read ~addr bytes dir (S.find name state.varrays)
    with Not_found -> raise_notrace (Uninterp name)

  let memcpy ~addr len orig state =
    let base = Bv.value_of addr in
    let ilocs = BiItM.add ~base len (Bv.value_of addr, orig) state.ilocs in
    let vmemory =
      Memory.source ~addr:(Expr.constant addr) ~len orig state.vmemory
    in
    { state with ilocs; vmemory }

  module Engine (Solver : Solver_sig.S) = struct
    type result = Unsat | Sat of t

    let extract_memory state =
      match Solver.get_array Memory.Root with
      | (exception Not_found) | [||] -> (BiTbl.create 0, state.constraints)
      | assignment ->
          let memory = BiTbl.create (Array.length assignment)
          and addr_space = Kernel_options.Machine.word_size () in
          let constraints =
            Array.fold_left
              (fun constraints (addr, value) ->
                match BiItM.find addr state.ilocs with
                | exception Not_found ->
                    BiTbl.add memory addr value;
                    constraints
                | base, img ->
                    let offset = Z.to_int (Z.sub addr base) in
                    let value' =
                      Char.unsafe_chr
                        (if offset < Bigarray.Array1.dim img then
                         Bigarray.Array1.get img offset
                        else 0)
                    in
                    if value <> value' then
                      Expr.(
                        equal
                          (load 1 LittleEndian
                             (constant (Bv.create addr addr_space))
                             Memory.Root)
                          (constant (Bv.of_char value')))
                      :: constraints
                    else constraints)
              state.constraints assignment
          in
          (memory, constraints)

    let extract_array name =
      match Solver.get_array name with
      | (exception Not_found) | [||] -> BiTbl.create 0
      | assignment ->
          let array = BiTbl.create (Array.length assignment) in
          Array.iter
            (fun (addr, value) -> BiTbl.add array addr value)
            assignment;
          array

    let extract_arrays state =
      let arrays = StTbl.create 5 in
      S.iter
        (fun name symbol -> StTbl.add arrays name (extract_array symbol))
        state.farrays;
      arrays

    let extract_vars state =
      let vars = BvTbl.create 32 in
      S.iter
        (fun _ ->
          List.iter (fun bv ->
              match Solver.get bv with
              | exception Not_found -> ()
              | x ->
                  BvTbl.add vars bv
                    (Bitvector.create Solver.(get_value x) (Expr.sizeof bv))))
        state.fvariables;
      vars

    let rec force_lazy_init constraints state =
      if constraints == state.constraints = false then
        match constraints with
        | [] -> ()
        | eq :: constraints ->
            let addr, value =
              match eq with
              | Binary
                  { f = Eq; x = Load { addr = Cst addr; _ }; y = Cst value; _ }
                ->
                  (Bitvector.value_of addr, Bitvector.value_of value)
              | _ -> assert false
            in
            Solver.set_memory ~addr value;
            force_lazy_init constraints state

    let enumerate =
      let rec iter state e expr size n enum =
        if n = 0 then enum
        else
          match Solver.check_sat () with
          | Unknown ->
              QS.Solver.incr_err ();
              raise Unknown
          | Unsat ->
              QS.Solver.incr_unsat ();
              enum
          | Sat ->
              QS.Solver.incr_sat ();
              let memory, constraints = extract_memory state in
              if constraints == state.constraints = false then (
                force_lazy_init constraints state;
                iter { state with constraints } e expr size n enum)
              else
                let x = Solver.get_value expr in
                let b = Bv.create x size in
                let cond = Expr.equal e (Expr.constant b) in
                let state' =
                  {
                    state with
                    constraints = cond :: constraints;
                    constset = BvSet.add cond state.constset;
                    model = (extract_vars state, memory, extract_arrays state);
                  }
                in
                Solver.neq expr x;
                iter state e expr size (n - 1) ((b, state') :: enum)
      in
      fun e ?(n = (1 lsl Expr.sizeof e) - 1) ?(except = []) state ->
        let size = Expr.sizeof e in
        let expr = Solver.bind state.fid e state.constraints in
        let init =
          let bv = Model.eval state.model e in
          if List.mem bv except then []
          else (
            QS.Preprocess.incr_const ();
            Solver.neq expr (Bitvector.value_of bv);
            let cond = Expr.equal e (Expr.constant bv) in
            [
              ( bv,
                {
                  state with
                  constraints = cond :: state.constraints;
                  constset = BvSet.add cond state.constset;
                } );
            ])
        in
        List.iter (fun bv -> Solver.neq expr (Bitvector.value_of bv)) except;
        iter state e expr size (n - 1) init

    let check_sat =
      let rec check_sat_true state =
        match Solver.check_sat () with
        | Unknown -> raise Unknown
        | Unsat -> Unsat
        | Sat ->
            let memory, constraints = extract_memory state in
            if constraints == state.constraints = false then (
              force_lazy_init constraints state;
              check_sat_true { state with constraints })
            else
              Sat
                {
                  state with
                  model = (extract_vars state, memory, extract_arrays state);
                }
      in
      fun state ->
        Solver.put state.fid state.constraints;
        check_sat_true state

    let close () = Solver.close ()
  end

  let assume' cond state =
    if Expr.is_equal cond Expr.one then (
      QS.Preprocess.incr_sat ();
      Some state)
    else if Expr.is_equal cond Expr.zero then (
      QS.Preprocess.incr_unsat ();
      None)
    else if BvSet.mem cond state.constset then (
      QS.Preprocess.incr_sat ();
      Some state)
    else if BvSet.mem (Expr.lognot cond) state.constset then (
      QS.Preprocess.incr_unsat ();
      None)
    else
      let state =
        {
          state with
          constraints = cond :: state.constraints;
          constset = BvSet.add cond state.constset;
        }
      in
      if Bitvector.zero = Model.eval state.model cond then (
        QS.Solver.start_timer ();
        let open Engine (F ()) in
        let r =
          match check_sat state with
          | exception Unknown ->
              QS.Solver.incr_err ();
              raise Unknown
          | Unsat ->
              QS.Solver.incr_unsat ();
              None
          | Sat state ->
              QS.Solver.incr_sat ();
              Some state
        in
        close ();
        QS.Solver.stop_timer ();
        r)
      else (
        QS.Preprocess.incr_sat ();
        Some state)

  let test' cond state =
    if Expr.is_equal cond Expr.one then (
      QS.Preprocess.incr_sat ();
      True state)
    else if Expr.is_equal cond Expr.zero then (
      QS.Preprocess.incr_unsat ();
      False state)
    else if BvSet.mem cond state.constset then (
      QS.Preprocess.incr_sat ();
      True state)
    else if BvSet.mem (Expr.lognot cond) state.constset then (
      QS.Preprocess.incr_unsat ();
      False state)
    else
      let t =
        {
          state with
          constraints = cond :: state.constraints;
          constset = BvSet.add cond state.constset;
        }
      in
      let ncond = Expr.lognot cond in
      let f =
        {
          state with
          constraints = ncond :: state.constraints;
          constset = BvSet.add ncond state.constset;
        }
      in
      let e = Model.eval state.model cond in
      let s = if Bv.is_zero e then t else f in
      QS.Solver.start_timer ();
      let open Engine (F ()) in
      let r =
        match check_sat s with
        | exception Unknown ->
            QS.Solver.incr_err ();
            raise Unknown
        | Unsat ->
            QS.Solver.incr_unsat ();
            if Bv.is_zero e then False f else True t
        | Sat state ->
            QS.Solver.incr_sat ();
            if Bv.is_zero e then Both { t = state; f }
            else Both { t; f = state }
      in
      close ();
      QS.Solver.stop_timer ();
      r

  let enumerate =
    let with_solver e ?n ?except state =
      QS.Solver.start_timer ();
      let open Engine (F ()) in
      let r = enumerate e ?n ?except state in
      close ();
      QS.Solver.stop_timer ();
      r
    in
    fun e ?n ?(except = []) state ->
      match (e, n) with
      | Expr.Cst bv, _ when List.mem bv except = false ->
          QS.Preprocess.incr_const ();
          [ (bv, state) ]
      | Expr.Cst _, _ ->
          QS.Preprocess.incr_const ();
          []
      | _, Some 1 ->
          let bv = Model.eval state.model e in
          if List.mem bv except then with_solver e ?n ~except state
          else (
            QS.Preprocess.incr_const ();
            let cond = Expr.equal e (Expr.constant bv) in
            [
              ( bv,
                {
                  state with
                  constraints = cond :: state.constraints;
                  constset = BvSet.add cond state.constset;
                } );
            ])
      | _, _ -> with_solver e ?n ~except state

  let merge t t' =
    if t == t' then t
    else if
      t.fid = t'.fid
      && t.fvariables == t'.fvariables
      && t.farrays == t'.farrays && t.ilocs == t'.ilocs
    then
      match (t.constraints, t'.constraints) with
      | c :: constraints, c' :: constraints'
        when constraints == constraints' && Expr.is_equal c (Expr.lognot c') ->
          let constset = BvSet.remove c t.constset
          and vsymbols =
            if t.vsymbols == t'.vsymbols then t.vsymbols
            else
              I.merge
                (fun _ o0 o1 ->
                  match (o0, o1) with
                  | Some e0, Some e1 ->
                      if Expr.is_equal e0 e1 then o0
                      else Some (Expr.ite c e0 e1)
                  | (Some _ | None), (Some _ | None) ->
                      raise_notrace Non_mergeable)
                t.vsymbols t'.vsymbols
          and varrays =
            if t.varrays == t'.varrays then t.varrays
            else
              S.merge
                (fun _ o0 o1 ->
                  match (o0, o1) with
                  | Some a0, Some a1 -> Some (Memory.merge c a0 a1)
                  | (Some _ | None), (Some _ | None) ->
                      raise_notrace Non_mergeable)
                t.varrays t'.varrays
          and vmemory = Memory.merge c t.vmemory t'.vmemory
          and fid = t.fid
          and fvariables = t.fvariables
          and farrays = t.farrays
          and ilocs = t.ilocs
          and model = t.model in
          {
            constraints;
            constset;
            vsymbols;
            varrays;
            vmemory;
            fid;
            fvariables;
            farrays;
            ilocs;
            model;
          }
      | _ -> raise_notrace Non_mergeable
    else raise_notrace Non_mergeable

  module Translate = struct
    let rec expr symbolic_state (e : Types.Expr.t) =
      match e with
      | Cst bv -> Expr.constant bv
      | Var { label; _ } -> (
          try I.find label symbolic_state.vsymbols
          with Not_found -> raise_notrace (Undef (Term.to_var_exn e)))
      | Load { len; dir; addr; label = Root; _ } ->
          let addr = expr symbolic_state addr in
          read ~addr len dir symbolic_state
      | Load { len; dir; addr; label = Symbol name; _ } ->
          let addr = expr symbolic_state addr in
          select name ~addr len dir symbolic_state
      | Unary { f; x; _ } ->
          let x = expr symbolic_state x in
          Expr.unary f x
      | Binary { f; x; y; _ } ->
          let x = expr symbolic_state x in
          let y = expr symbolic_state y in
          Expr.binary f x y
      | Ite { c; t; e; _ } ->
          let c = expr symbolic_state c in
          let t = expr symbolic_state t in
          let e = expr symbolic_state e in
          Expr.ite c t e
  end

  let rec get_value ?(check_unique = false) e t =
    match Translate.expr t e with
    | exception Undef var -> get_value ~check_unique e (fresh var t)
    | exception Uninterp name -> get_value ~check_unique e (alloc name t)
    | Cst bv -> bv
    | _ when check_unique -> raise Non_unique
    | e -> Model.eval t.model e

  let rec assume e t =
    try
      let e = Translate.expr t e in
      assume' e t
    with
    | Undef var -> assume e (fresh var t)
    | Uninterp name -> assume e (alloc name t)

  let rec test e t =
    try
      let e = Translate.expr t e in
      test' e t
    with
    | Undef var -> test e (fresh var t)
    | Uninterp name -> test e (alloc name t)

  let rec split_on e ?n ?except t =
    try
      let e = Translate.expr t e in
      enumerate e ?n ?except t
    with
    | Undef var -> split_on e ?n ?except (fresh var t)
    | Uninterp name -> split_on e ?n ?except (alloc name t)

  let rec assign name e t =
    try
      let e = Translate.expr t e in
      assign' name e t
    with
    | Undef var -> assign name e (fresh var t)
    | Uninterp name' -> assign name e (alloc name' t)

  let rec write ~addr value dir t =
    try
      let addr = Translate.expr t addr in
      let value = Translate.expr t value in
      write' ~addr value dir t
    with
    | Undef var -> write ~addr value dir (fresh var t)
    | Uninterp name -> write ~addr value dir (alloc name t)

  let rec store name ~addr value dir t =
    try
      let addr = Translate.expr t addr in
      let value = Translate.expr t value in
      store' name ~addr value dir t
    with
    | Undef var -> store name ~addr value dir (fresh var t)
    | Uninterp name' -> store name ~addr value dir (alloc name' t)

  let pp_smt (target : Types.target) ppf t =
    let module P = Smt2_solver.Printer in
    let ctx =
      P.create ~debug:(fun ~name ~label -> label ^ name) ~next_id:t.fid ()
    in
    (* visit assertions *)
    List.iter (P.visit_bl ctx) t.constraints;
    (* visit terms *)
    let defs =
      match target with
      | Slice defs ->
          let rec proceed defs t =
            try
              List.map
                (fun (expr, name) ->
                  let expr = Translate.expr t expr in
                  P.visit_bv ctx expr;
                  (expr, name))
                defs
            with Undef var -> proceed defs (fresh var t)
          in
          proceed defs t
      | Env revenv ->
          P.visit_ax ctx t.vmemory;
          List.rev
            (I.fold
               (fun id expr defs ->
                 P.visit_bv ctx expr;
                 (expr, R.find revenv id) :: defs)
               t.vsymbols [])
    in
    Format.pp_open_vbox ppf 0;
    (* print declarations *)
    P.pp_print_decls ppf ctx;
    (* print definitions *)
    P.pp_print_defs ppf ctx;
    List.iter
      (fun (bv, name) ->
        Format.fprintf ppf "@[<h>(define-fun %s () (_ BitVec %d)@ " name
          (Expr.sizeof bv);
        P.pp_print_bv ctx ppf bv;
        Format.fprintf ppf ")@]@ ")
      defs;
    (match target with
    | Env _ ->
        Format.fprintf ppf
          "@[<h>(define-fun memory () (Array (_ BitVec %d) (_ BitVec 8))@ %a)@]"
          (Kernel_options.Machine.word_size ())
          (P.pp_print_ax ctx) t.vmemory
    | Slice _ -> ());
    (* print assertions *)
    List.iter
      (fun bl ->
        Format.pp_open_hbox ppf ();
        Format.pp_print_string ppf "(assert ";
        P.pp_print_bl ctx ppf bl;
        Format.pp_print_char ppf ')';
        Format.pp_close_box ppf ();
        Format.pp_print_space ppf ())
      t.constraints;
    Format.pp_close_box ppf ()

  let as_ascii ~name t =
    let buf = Buffer.create 16 in
    List.iter (fun var ->
        assert (Expr.sizeof var mod byte_size = 0);
        let rec iter bv =
          let size = Bitvector.size_of bv in
          if size = byte_size then Buffer.add_char buf (Bitvector.to_char bv)
          else
            let byte = Bitvector.extract bv { Interval.lo = 0; hi = 7 } in
            Buffer.add_char buf (Bitvector.to_char byte);
            iter (Bitvector.extract bv { Interval.lo = 8; hi = size - 1 })
        in
        iter (Model.eval t.model var))
    @@ List.rev @@ S.find name t.fvariables;
    Buffer.contents buf

  let as_c_string ~name t =
    try
      let ar = S.find name t.varrays in
      let buf = Buffer.create 16 in
      let rec iter addr =
        let byte =
          Model.eval t.model (Memory.read ~addr 1 Machine.LittleEndian ar)
        in
        if Bitvector.is_zeros byte then Buffer.contents buf
        else (
          Buffer.add_char buf (Bitvector.to_char byte);
          iter (Expr.addi addr 1))
      in
      iter (Expr.zeros (Kernel_options.Machine.word_size ()))
    with Not_found -> ""
end
