(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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

module S = Basic_types.String.Map

module State (Solver : Smt_sig.Solver) (QS : QUERY_STATISTICS) : STATE = struct
  type t = {
    formula : Formula.formula;
    (* SMT2 formula *)
    vsymbols : Formula.bv_term S.t;
    (* collection of visible symbols *)
    vmemory : Formula.ax_term;
    (* visible memory *)
    fid : int;
    (* unique indice counter *)
    fvariables : Formula.bv_var list S.t;
    (* collection of free variables *)
    fmemory : Formula.ax_var;
    (* initial memory *)
    model : Smt_model.t; (* a model that satisfy constraints *)
  }
  (** Symbolic state *)

  let memory_name = "__memory"

  module Value = struct
    type t = Formula.bv_term

    let constant = Formula.mk_bv_cst

    let lookup (v : Dba.Var.t) t =
      try S.find v.name t.vsymbols with Not_found -> raise (Undef v)

    let read ~addr bytes (dir : Machine.endianness) t =
      let array = t.vmemory in
      let content = Formula.mk_select bytes array addr in
      let content =
        match dir with LittleEndian -> content | BigEndian -> byteswap content
      in
      (content, t)

    let select _ ~addr:_ _ _ _ = raise (Errors.not_yet_implemented "arrays")

    let ite cond then_smt else_smt =
      Formula.(mk_bv_ite (mk_bv_equal cond mk_bv_one) then_smt else_smt)

    let unary e (op : Dba.Unary_op.t) =
      match op with
      | Not -> Formula.mk_bv_not
      | UMinus -> Formula.mk_bv_neg
      | Sext n -> Formula.mk_bv_sign_extend (n - Dba.Expr.size_of e)
      | Uext n -> Formula.mk_bv_zero_extend (n - Dba.Expr.size_of e)
      | Restrict interval -> Formula.mk_bv_extract interval

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

    let rec eval (e : Dba.Expr.t) state =
      match e with
      | Var { info = Symbol (_, (lazy bv)); _ } | Cst bv -> Formula.mk_bv_cst bv
      | Var v -> lookup v state
      | Load (bytes, dir, e, None) ->
          let smt_e = eval e state in
          fst (read ~addr:smt_e bytes dir state)
      | Load _ -> raise (Errors.not_yet_implemented "arrays")
      | Binary (bop, lop, rop) as e ->
          Logger.debug ~level:6 "Translating binary %a"
            Dba_printer.Ascii.pp_bl_term e;
          let l_smt_e = eval lop state in
          let r_smt_e = eval rop state in
          binary bop l_smt_e r_smt_e
      | Unary (uop, e) ->
          let v = eval e state in
          unary e uop v
      | Ite (c, then_e, else_e) ->
          let cond = eval c state in
          let then_smt = eval then_e state in
          let else_smt = eval else_e state in
          ite cond then_smt else_smt

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

  let empty () =
    let word_size = Kernel_options.Machine.word_size () in
    let fmemory = Formula.ax_var (memory_name ^ "_0") word_size byte_size in
    {
      formula =
        Formula.empty
        |> Formula.push_back_declare @@ Formula.mk_ax_decl fmemory [];
      vsymbols = S.empty;
      vmemory = Formula.mk_ax_var fmemory;
      fid = 1;
      fvariables = S.empty;
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

  let fresh ({ name; size; _ } : Dba.Var.t) state =
    let v = Formula.bv_var (Printf.sprintf "%s_%d" name state.fid) size in
    let fid = state.fid + 1 in
    let h =
      match S.find name state.fvariables with
      | exception Not_found -> [ v ]
      | h -> v :: h
    in
    let fvariables = S.add name h state.fvariables in
    let vsymbols = S.add name (Formula.mk_bv_var v) state.vsymbols in
    let formula =
      state.formula |> Formula.push_front_declare @@ Formula.mk_bv_decl v []
    in
    { state with formula; vsymbols; fid; fvariables }

  let alloc ~array:_ _ = raise (Errors.not_yet_implemented "arrays")

  let assign name value state =
    let value_size = Formula_utils.bv_size value in
    let var =
      Formula.bv_var (Printf.sprintf "%s_%d" name state.fid) value_size
    in
    let fid = state.fid + 1 in
    let vsymbols = S.add name (Formula.mk_bv_var var) state.vsymbols in
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

  let store _ ~addr:_ _ _ _ = raise (Errors.not_yet_implemented "arrays")

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
      S.iter
        (fun _ ->
          List.iter (fun var ->
              Smt_model.add_var model
                (Formula_utils.bv_var_name var)
                (Solver.get_bv_value session (Formula.mk_bv_var var))))
        vars;
      Array.iter
        (fun (addr, value) -> Smt_model.add_memcell model addr value)
        (Solver.get_ax_values session (Formula.mk_ax_var memory));
      model

    let with_solver formula f =
      QS.Solver.start_timer ();
      let session = Solver.open_session () in
      Formula.iter_forward (Solver.put session) formula;
      let r = f session in
      Solver.close_session session;
      QS.Solver.stop_timer ();
      r

    let check_satistifiability formula vars memory =
      with_solver formula (fun session ->
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

    let enumerate e ?(n = 1 lsl Formula_utils.bv_size e) formula vars memory =
      with_solver formula (fun session ->
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

  let get_value e state =
    let size = Formula_utils.bv_size e in
    let var = Formula.bv_var (Printf.sprintf "__value_%d" state.fid) size in
    let formula =
      state.formula |> Formula.push_front_define @@ Formula.mk_bv_def var [] e
    in
    let keep = Formula.VarSet.singleton (Formula.BvVar var) in
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
        QS.Preprocess.incr_const ();
        Logger.debug ~level:4 "Value of %a resolved to constant %a"
          Formula_pp.pp_bv_term e Bitvector.pp bv;
        bv
    | _ -> raise Non_unique

  let keep state =
    S.fold
      (fun _ e k ->
        match e with
        | { Formula.bv_term_desc = Formula.BvFun (v, []); _ } ->
            Formula.VarSet.add (Formula.BvVar v) k
        | _ -> assert false)
      state.vsymbols
    @@ S.fold
         (fun _ l k ->
           List.fold_left
             (fun k v -> Formula.VarSet.add (Formula.BvVar v) k)
             k l)
         state.fvariables
    @@
    match state.vmemory with
    | { Formula.ax_term_desc = Formula.AxFun (v, []); _ } ->
        Formula.VarSet.add (Formula.AxVar v)
        @@ Formula.VarSet.singleton (Formula.AxVar state.fmemory)
    | _ -> assert false

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
        QS.Preprocess.incr_sat ();
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
        QS.Preprocess.incr_unsat ();
        None
    | _ -> (
        let formula =
          Formula.push_front_assert (Formula.mk_bl_var var) formula
        in
        match
          Solver.check_satistifiability formula state.fvariables state.fmemory
        with
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
        QS.Preprocess.incr_sat ();
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
        QS.Preprocess.incr_unsat ();
        False { state with formula; fid }
    | _ -> (
        let formula = Formula.push_front_assert (Formula.mk_bl_var var) formula
        and formula' =
          Formula.push_front_assert
            (Formula.mk_bl_not (Formula.mk_bl_var var))
            formula
        in
        match
          ( Solver.check_satistifiability formula state.fvariables state.fmemory,
            Solver.check_satistifiability formula' state.fvariables
              state.fmemory )
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
        if Bitvector.is_one bv then QS.Preprocess.incr_sat ()
        else if Bitvector.is_zero bv then QS.Preprocess.incr_unsat ()
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
        @@ Solver.enumerate evar ?n formula state.fvariables state.fmemory

  let merge _ _ = raise Non_mergeable

  let pp ppf state = Smt_model.pp ppf state.model

  let pp_smt slice ppf state =
    match slice with
    | None -> Formula_pp.pp_formula ppf state.formula
    | Some l ->
        let keep, state =
          List.fold_left
            (fun (keep, state) (e, n) ->
              let state = assign n (Value.eval e state) state in
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

  let as_ascii ~name state =
    let buf = Buffer.create 16 in
    List.iter (fun var ->
        let name = Formula_utils.bv_var_name var in
        match Smt_model.find_variable state.model name with
        | None -> Buffer.add_char buf '.'
        | Some bv ->
            assert (Bitvector.size_of bv mod byte_size = 0);
            let rec iter bv =
              let size = Bitvector.size_of bv in
              if size = byte_size then
                Buffer.add_char buf (Bitvector.to_char bv)
              else
                let byte = Bitvector.extract bv { Interval.lo = 0; hi = 7 } in
                Buffer.add_char buf (Bitvector.to_char byte);
                iter (Bitvector.extract bv { Interval.lo = 8; hi = size - 1 })
            in
            iter bv)
    @@ List.rev
    @@ S.find name state.fvariables;
    Buffer.contents buf

  let as_c_string ~name:_ _ = raise (Errors.not_yet_implemented "arrays")

  let assign ({ name; _ } : Dba.Var.t) state = assign name state

  let to_formula { formula; _ } = formula
end
