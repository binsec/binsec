(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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

open Options
open Types

let byte_size = Natural.to_int Basic_types.Constants.bytesize

let byteswap e =
  let rec loop e size e' =
    if size = 0 then e'
    else
      loop e (size - 8)
        (Formula.mk_bv_concat
           (Formula.mk_bv_extract { lo = size - 8; hi = size - 1 } e)
           e')
  in
  let size = Formula_utils.bv_size e in
  loop e (size - 8) (Formula.mk_bv_extract { lo = size - 8; hi = size - 1 } e)

module VMap = Dba_types.Var.Map

module State (Solver : Smt_sig.Solver) (QS : QUERY_STATISTICS) : RAW_STATE =
struct
  module Uid = struct
    type t = int

    let zero = 0
    let succ = ( + ) 1
    let compare = ( - )
  end

  type t = {
    mutable formula : Formula.formula;
    (* SMT2 formula *)
    vsymbols : Formula.bv_term VMap.t;
    (* collection of visible symbols *)
    vmemory : Formula.ax_term;
    (* visible memory *)
    mutable fid : int;
    (* unique indice counter *)
    fmemory : Formula.ax_var;
    (* initial memory *)
    model : Smt_model.t; (* a model that satisfy constraints *)
  }
  (** Symbolic state *)

  let memory_name = "__memory"

  module Value = struct
    type t = Formula.bv_term

    let kind = Abstract
    let constant = Formula.mk_bv_cst

    let var id name size =
      Formula.mk_bv_var (Formula.bv_var (Printf.sprintf "%s_0%d" name id) size)

    let ite cond then_smt else_smt =
      Formula.(mk_bv_ite (mk_bv_equal cond mk_bv_one) then_smt else_smt)

    let as_bv bop e1 e2 = Formula.(mk_bv_ite (bop e1 e2) mk_bv_one mk_bv_zero)
    let rotate_right_const n = Formula.mk_bv_rotate_right n
    let rotate_left_const n = Formula.mk_bv_rotate_left n

    let rotate shift_func rev_shift_func const_rot_func value shift =
      let open Formula in
      match shift.bv_term_desc with
      | BvCst x ->
          let op = Bitvector.value_of x |> Z.to_int |> const_rot_func in
          op value
      | _ ->
          let part1 = shift_func value shift
          and shift_size = Formula_utils.bv_size shift
          and value_size = Formula_utils.bv_size value |> Z.of_int in
          let value_size =
            Bitvector.create value_size shift_size |> mk_bv_cst
          in
          let offset = mk_bv_sub value_size shift in
          let part2 = rev_shift_func value offset in
          mk_bv_or part1 part2

    let rotate_right =
      rotate Formula.mk_bv_lshr Formula.mk_bv_shl rotate_right_const

    let rotate_left =
      rotate Formula.mk_bv_shl Formula.mk_bv_lshr rotate_left_const

    let binary (op : Dba.Binary_op.t) =
      match op with
      | Plus -> Formula.mk_bv_add
      | Minus -> Formula.mk_bv_sub
      | Mult -> Formula.mk_bv_mul
      | DivU -> Formula.mk_bv_udiv
      | DivS -> Formula.mk_bv_sdiv
      | ModU -> Formula.mk_bv_urem
      | ModS -> Formula.mk_bv_smod
      | Eq -> as_bv Formula.mk_bv_equal
      | Diff -> as_bv Formula.mk_bv_distinct
      | LeqU -> as_bv Formula.mk_bv_ule
      | LtU -> as_bv Formula.mk_bv_ult
      | GeqU -> as_bv Formula.mk_bv_uge
      | GtU -> as_bv Formula.mk_bv_ugt
      | LeqS -> as_bv Formula.mk_bv_sle
      | LtS -> as_bv Formula.mk_bv_slt
      | GeqS -> as_bv Formula.mk_bv_sge
      | GtS -> as_bv Formula.mk_bv_sgt
      | Xor -> Formula.mk_bv_xor
      | And -> Formula.mk_bv_and
      | Or -> Formula.mk_bv_or
      | Concat -> Formula.mk_bv_concat
      | LShift -> Formula.mk_bv_shl
      | RShiftU -> Formula.mk_bv_lshr
      | RShiftS -> Formula.mk_bv_ashr
      | LeftRotate -> rotate_left
      | RightRotate -> rotate_right

    let unary_op (op : Term.unary Term.operator) =
      match op with
      | Not -> Formula.BvNot
      | Minus -> Formula.BvNeg
      | Sext size -> Formula.BvSignExtend size
      | Uext size -> Formula.BvZeroExtend size
      | Restrict it -> Formula.BvExtract it

    let unary op e = Formula.mk_bv_unop (unary_op op) e

    let binary_op (op : Term.binary Term.operator) =
      match op with
      | Plus -> Dba.Binary_op.Plus
      | Minus -> Dba.Binary_op.Minus
      | Mul -> Dba.Binary_op.Mult
      | Udiv -> Dba.Binary_op.DivU
      | Umod -> Dba.Binary_op.ModU
      | Sdiv -> Dba.Binary_op.DivS
      | Smod -> Dba.Binary_op.ModS
      | Or -> Dba.Binary_op.Or
      | And -> Dba.Binary_op.And
      | Xor -> Dba.Binary_op.Xor
      | Concat -> Dba.Binary_op.Concat
      | Lsl -> Dba.Binary_op.LShift
      | Lsr -> Dba.Binary_op.RShiftU
      | Asr -> Dba.Binary_op.RShiftS
      | Rol -> Dba.Binary_op.LeftRotate
      | Ror -> Dba.Binary_op.RightRotate
      | Eq -> Dba.Binary_op.Eq
      | Diff -> Dba.Binary_op.Diff
      | Ule -> Dba.Binary_op.LeqU
      | Ult -> Dba.Binary_op.LtU
      | Uge -> Dba.Binary_op.GeqU
      | Ugt -> Dba.Binary_op.GtU
      | Sle -> Dba.Binary_op.LeqS
      | Slt -> Dba.Binary_op.LtS
      | Sge -> Dba.Binary_op.GeqS
      | Sgt -> Dba.Binary_op.GtS

    let binary op e1 e2 = binary (binary_op op) e1 e2
  end

  let lookup (v : Dba.Var.t) t =
    try VMap.find v t.vsymbols with Not_found -> raise (Undef v)

  let read ~addr bytes (dir : Machine.endianness) t =
    let array = t.vmemory in
    let content = Formula.mk_select bytes array addr in
    let content =
      match dir with LittleEndian -> content | BigEndian -> byteswap content
    in
    (content, t)

  let select _ ~addr:_ _ _ _ = Errors.not_yet_implemented "arrays"

  let empty () =
    let word_size = Kernel_options.Machine.word_size () in
    let fmemory = Formula.ax_var (memory_name ^ "_0") word_size byte_size in
    {
      formula =
        Formula.empty
        |> Formula.push_back_declare @@ Formula.mk_ax_decl fmemory [];
      vsymbols = VMap.empty;
      vmemory = Formula.mk_ax_var fmemory;
      fid = 1;
      fmemory;
      model = Smt_model.empty;
    }

  let do_optimization ?(keep = Formula.VarSet.empty) fm =
    let level = 3 in
    if Formula.VarSet.is_empty keep then Logger.debug ~level "Optimize"
    else
      Logger.debug ~level
        "@[<v 2>Optimize but keep intact these variables:@ %a@]"
        Formula_pp.pp_varset keep;
    Formula_transformation.optimize_from_options ?is_controlled:None ~keep fm

  let alloc ~array:_ _ = Errors.not_yet_implemented "arrays"

  let assign (lval : Dba.Var.t) value state =
    let value_size = Formula_utils.bv_size value in
    let var =
      Formula.bv_var (Printf.sprintf "%s_%d" lval.name state.fid) value_size
    in
    let fid = state.fid + 1 in
    let vsymbols = VMap.add lval (Formula.mk_bv_var var) state.vsymbols in
    let formula =
      state.formula
      |> Formula.push_front_define @@ Formula.mk_bv_def var [] value
    in
    { state with formula; vsymbols; fid }

  let write ~addr value (dir : Machine.endianness) state =
    let value =
      match dir with LittleEndian -> value | BigEndian -> byteswap value
    in
    let addr_size = Formula_utils.bv_size addr
    and write_size = Formula_utils.bv_size value / 8 in
    let layer =
      Formula.ax_var
        (Printf.sprintf "%s_%d" memory_name state.fid)
        addr_size byte_size
    in
    let fid = state.fid + 1 in
    let vmemory = Formula.mk_ax_var layer in
    let formula =
      state.formula
      |> Formula.push_front_define @@ Formula.mk_ax_def layer []
         @@ Formula.mk_store write_size state.vmemory addr value
    in
    { state with formula; vmemory; fid }

  let store _ ~addr:_ _ _ _ = Errors.not_yet_implemented "arrays"

  let memcpy ~addr size img state =
    let reader = Lreader.of_zero_extend_buffer img in
    let chunk = Lreader.Read.read reader size in
    let addr_size = Bitvector.size_of addr in
    let layer =
      Formula.ax_var
        (Printf.sprintf "%s_%d" memory_name state.fid)
        addr_size byte_size
    in
    let fid = state.fid + 1 in
    let vmemory = Formula.mk_ax_var layer in
    let formula =
      state.formula
      |> Formula.push_front_define @@ Formula.mk_ax_def layer []
         @@ Formula.mk_store size state.vmemory (Formula.mk_bv_cst addr)
              (Formula.mk_bv_cst chunk)
    in
    { state with formula; vmemory; fid }

  module Solver = struct
    let extract_model session vars memory =
      let model = Smt_model.create () in
      List.iter
        (fun var ->
          Smt_model.add_var model
            (Formula_utils.bv_var_name var)
            (Solver.get_bv_value session (Formula.mk_bv_var var)))
        vars;
      Array.iter
        (fun (addr, value) -> Smt_model.add_memcell model addr value)
        (Solver.get_ax_values session (Formula.mk_ax_var memory));
      model

    let declare_var session marked var vars =
      match (var : Formula.var) with
      | BvVar bv_var when not (Formula.VarSet.mem var marked) ->
          Solver.put session (Formula.mk_declare (Formula.mk_bv_decl bv_var []));
          bv_var :: vars
      | _ -> vars

    let with_solver formula f =
      QS.Solver.start_timer ();
      let session = Solver.open_session () in
      let vars, _ =
        Formula.fold_forward
          (fun entry (vars, marked) ->
            let vars, marked =
              match entry.entry_desc with
              | Declare { decl_desc = BvDecl (bv_var, _); _ } ->
                  (bv_var :: vars, Formula.VarSet.add (BvVar bv_var) marked)
              | Declare { decl_desc = BlDecl _ | AxDecl _; _ } -> (vars, marked)
              | Define { def_desc = BvDef (bv_var, _, bv_term); _ } ->
                  let deps = Formula_utils.bv_term_variables bv_term in
                  ( Formula.VarSet.fold (declare_var session marked) deps vars,
                    Formula.VarSet.add (BvVar bv_var)
                      (Formula.VarSet.union deps marked) )
              | Define { def_desc = BlDef (bl_var, _, bl_term); _ } ->
                  let deps = Formula_utils.bl_term_variables bl_term in
                  ( Formula.VarSet.fold (declare_var session marked) deps vars,
                    Formula.VarSet.add (BlVar bl_var)
                      (Formula.VarSet.union deps marked) )
              | Define { def_desc = AxDef (ax_var, _, ax_term); _ } ->
                  let deps = Formula_utils.ax_term_variables ax_term in
                  ( Formula.VarSet.fold (declare_var session marked) deps vars,
                    Formula.VarSet.add (AxVar ax_var)
                      (Formula.VarSet.union deps marked) )
              | Assert bl_term | Assume bl_term ->
                  let deps = Formula_utils.bl_term_variables bl_term in
                  ( Formula.VarSet.fold (declare_var session marked) deps vars,
                    Formula.VarSet.union deps marked )
              | Comment _ -> (vars, marked)
            in
            Solver.put session entry;
            (vars, marked))
          formula ([], Formula.VarSet.empty)
      in
      let r = f session vars in
      Solver.close_session session;
      QS.Solver.stop_timer ();
      r

    let check_satistifiability formula memory =
      with_solver formula (fun session vars ->
          match Solver.check_sat session with
          | Formula.SAT ->
              QS.Solver.incr_sat ();
              Logger.debug ~level:4 "SMT query resulted in SAT";
              Some (extract_model session vars memory)
          | Formula.UNSAT ->
              QS.Solver.incr_unsat ();
              Logger.debug ~level:4 "SMT query resulted in UNSAT";
              None
          | Formula.UNKNOWN | Formula.TIMEOUT ->
              QS.Solver.incr_err ();
              Logger.warning ~level:0 "SMT query resulted in UNKNOWN";
              raise Unknown)

    let enumerate e ?(n = 1 lsl Formula_utils.bv_size e) formula memory =
      with_solver formula (fun session vars ->
          let rec loop e' n enum =
            if n = 0 then enum
            else
              match Solver.check_sat session with
              | Formula.SAT ->
                  QS.Solver.incr_sat ();
                  let bv = Solver.get_bv_value session e' in
                  Logger.debug ~level:5
                    "Solver returned %a ; %d solutions still to be found"
                    Bitvector.pp_hex bv (n - 1);
                  let model = extract_model session vars memory in
                  Solver.put session @@ Formula.mk_assert
                  @@ Formula.mk_bv_distinct e (Formula.mk_bv_cst bv);
                  loop e' (n - 1) ((bv, model) :: enum)
              | Formula.UNSAT ->
                  QS.Solver.incr_unsat ();
                  Logger.debug ~level:4 "Solver returned UNSAT";
                  enum
              | Formula.UNKNOWN | Formula.TIMEOUT ->
                  QS.Solver.incr_err ();
                  Logger.warning ~level:0 "SMT query resulted in UNKNOWN";
                  raise Unknown
          in
          loop e n [])
  end

  let keep state =
    VMap.fold
      (fun _ e k ->
        match e with
        | { Formula.bv_term_desc = Formula.BvFun (v, []); _ } ->
            Formula.VarSet.add (Formula.BvVar v) k
        | _ -> assert false)
      state.vsymbols
    @@
    match state.vmemory with
    | { Formula.ax_term_desc = Formula.AxFun (v, []); _ } ->
        Formula.VarSet.add (Formula.AxVar v)
        @@ Formula.VarSet.singleton (Formula.AxVar state.fmemory)
    | _ -> assert false

  let get_value e state =
    let size = Formula_utils.bv_size e in
    let var = Formula.bv_var (Printf.sprintf "__value_%d" state.fid) size in
    let formula =
      state.formula |> Formula.push_front_define @@ Formula.mk_bv_def var [] e
    in
    let keep = Formula.VarSet.add (Formula.BvVar var) (keep state) in
    let formula = do_optimization ~keep formula in
    state.formula <- Option.get (Formula.pop_front formula);
    match Formula.peek_front formula with
    | Some
        {
          entry_desc =
            Formula.Define
              {
                def_desc =
                  Formula.BvDef (v, _, { bv_term_desc = Formula.BvCst bv; _ });
                _;
              };
          _;
        } ->
        assert (v = var);
        QS.Preprocess.incr_const ();
        Logger.debug ~level:4 "Value of %a resolved to constant %a"
          Formula_pp.pp_bv_term e Bitvector.pp bv;
        bv
    | _ -> raise Non_unique

  let assume e state =
    let e = Formula.mk_bv_equal e Formula.mk_bv_one in
    let var = Formula.bl_var (Printf.sprintf "__assume_%d" state.fid) in
    let fid = state.fid + 1 in
    let formula =
      state.formula |> Formula.push_front_define @@ Formula.mk_bl_def var [] e
    in
    let keep = Formula.VarSet.add (Formula.BlVar var) @@ keep state in
    let formula = do_optimization ~keep formula in
    match Formula.peek_front formula with
    | Some
        {
          entry_desc =
            Formula.Define
              {
                def_desc =
                  Formula.BlDef (v, _, { bl_term_desc = Formula.BlTrue; _ });
                _;
              };
          _;
        } ->
        assert (v = var);
        QS.Preprocess.incr_true ();
        Some { state with formula; fid }
    | Some
        {
          entry_desc =
            Formula.Define
              {
                def_desc =
                  Formula.BlDef (v, _, { bl_term_desc = Formula.BlFalse; _ });
                _;
              };
          _;
        } ->
        assert (v = var);
        QS.Preprocess.incr_false ();
        None
    | _ -> (
        let formula =
          Formula.push_front_assert (Formula.mk_bl_var var) formula
        in
        match Solver.check_satistifiability formula state.fmemory with
        | Some model -> Some { state with formula; fid; model }
        | None -> None)

  let test e state =
    let e = Formula.mk_bv_equal e Formula.mk_bv_one in
    let var = Formula.bl_var (Printf.sprintf "__assume_%d" state.fid) in
    let fid = state.fid + 1 in
    let formula =
      state.formula |> Formula.push_front_define @@ Formula.mk_bl_def var [] e
    in
    let keep = Formula.VarSet.add (Formula.BlVar var) @@ keep state in
    let formula = do_optimization ~keep formula in
    match Formula.peek_front formula with
    | Some
        {
          entry_desc =
            Formula.Define
              {
                def_desc =
                  Formula.BlDef (v, _, { bl_term_desc = Formula.BlTrue; _ });
                _;
              };
          _;
        } ->
        assert (v = var);
        QS.Preprocess.incr_true ();
        True { state with formula; fid }
    | Some
        {
          entry_desc =
            Formula.Define
              {
                def_desc =
                  Formula.BlDef (v, _, { bl_term_desc = Formula.BlFalse; _ });
                _;
              };
          _;
        } ->
        assert (v = var);
        QS.Preprocess.incr_false ();
        False { state with formula; fid }
    | _ -> (
        let formula = Formula.push_front_assert (Formula.mk_bl_var var) formula
        and formula' =
          Formula.push_front_assert
            (Formula.mk_bl_not (Formula.mk_bl_var var))
            formula
        in
        match
          ( Solver.check_satistifiability formula state.fmemory,
            Solver.check_satistifiability formula' state.fmemory )
        with
        | Some model, Some model' ->
            Both
              {
                t = { state with formula; fid; model };
                f = { state with formula = formula'; fid; model = model' };
              }
        | Some model, None -> True { state with formula; fid; model }
        | None, Some model' ->
            False { state with formula = formula'; fid; model = model' }
        | None, None -> raise Unknown)

  let enumerate e ?n ?(except = []) state =
    let size = Formula_utils.bv_size e in
    let var = Formula.bv_var (Printf.sprintf "__enum_%d" state.fid) size in
    let fid = state.fid + 1 in
    let formula =
      state.formula |> Formula.push_front_define @@ Formula.mk_bv_def var [] e
    in
    let keep = Formula.VarSet.add (Formula.BvVar var) @@ keep state in
    let formula = do_optimization ~keep formula in
    match Formula.peek_front formula with
    | Some
        {
          entry_desc =
            Formula.Define
              {
                def_desc =
                  Formula.BvDef (v, _, { bv_term_desc = Formula.BvCst bv; _ });
                _;
              };
          _;
        } ->
        assert (v = var);
        if Bitvector.is_one bv then QS.Preprocess.incr_true ()
        else if Bitvector.is_zero bv then QS.Preprocess.incr_false ()
        else QS.Preprocess.incr_const ();
        Logger.debug ~level:4 "Enumeration of %a resolved to constant %a"
          Formula_pp.pp_bv_term e Bitvector.pp bv;
        [ (bv, { state with formula; fid }) ]
    | _ ->
        let evar = Formula.mk_bv_var var in
        let formula =
          List.fold_left
            (fun f bv ->
              Formula.push_front_assert
                (Formula.mk_bv_distinct evar (Formula.mk_bv_cst bv))
                f)
            formula except
        in
        List.map (fun (bv, model) ->
            let formula =
              formula
              |> Formula.push_front_assert
                 @@ Formula.mk_bv_equal evar (Formula.mk_bv_cst bv)
            in
            (bv, { state with formula; fid; model }))
        @@ Solver.enumerate evar ?n formula state.fmemory

  let get_a_value e t =
    match enumerate e ~n:1 t with
    | [ (bv, t') ] ->
        t.fid <- t'.fid;
        t.formula <- t'.formula;
        bv
    | _ -> raise Unknown

  let merge ~parent:_ _ _ = raise Non_mergeable

  let assertions t =
    Formula.fold_forward
      (fun (e : Formula.entry) r ->
        match e with
        | { entry_desc = Assert b; _ } ->
            Formula.mk_bv_ite b Formula.mk_bv_one Formula.mk_bv_zero :: r
        | _ -> r)
      t.formula []

  let pp ppf state = Smt_model.pp ppf state.model

  let pp_smt slice ppf state =
    match slice with
    | None -> Formula_pp.pp_formula ppf state.formula
    | Some l ->
        let keep, state =
          List.fold_left
            (fun (keep, state) (e, n) ->
              let state =
                let value_size = Formula_utils.bv_size e in
                let var = Formula.bv_var n value_size in
                let formula =
                  state.formula
                  |> Formula.push_front_define @@ Formula.mk_bv_def var [] e
                in
                { state with formula }
              in
              match Formula.peek_front state.formula with
              | Some
                  {
                    entry_desc =
                      Formula.Define { def_desc = Formula.BvDef (v, _, _); _ };
                    _;
                  } ->
                  (Formula.VarSet.add (Formula.BvVar v) keep, state)
              | _ -> assert false)
            (Formula.VarSet.empty, state)
            l
        in
        Formula_pp.pp_formula ppf (do_optimization ~keep state.formula)

  let to_formula { formula; _ } = formula
  let downcast _ = None
end

type Options.Engine.t += Legacy

let () =
  Options.Engine.register "legacy" Legacy (fun () ->
      (module State ((val Smt_solver.get_solver ()))))
