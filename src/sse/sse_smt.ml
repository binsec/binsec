(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

open Sse_options

module Query_stats = struct
  type t = {
    unsat : int;
    sat   : int;
    err   : int;
    enums : int;
    time  : float;
    }

  let empty = { unsat = 0; sat = 0; err = 0; time = 0.; enums = 0; }
  let total q = q.unsat + q.sat + q.err

  let avg_time q =
    q.time /. (float @@ total q)

  let add_enumeration q =
    { q with enums = q.enums + 1 }

  let add q time res =
    let q = {q with time = q.time +. time } in
    match res with
    | Formula.UNSAT -> { q with unsat = q.unsat + 1 }
    | Formula.SAT -> { q with sat = q.sat + 1 }
    | _ ->  { q with err = q.err + 1 }

  let pp ppf q =
    let open Format in
    fprintf ppf
    "@[<v 0>\
     @[<h>sat            %d@]@,\
     @[<h>unsat          %d@]@,\
     @[<h>unknown        %d@]@,\
     @[<h>total          %d@]@,\
     @[<h>enumerations   %d@]@,\
     @[<h>time           %.2f@]@,\
     @[<h>average        %.2f@]@,\
     @]"
    q.sat q.unsat q.err (total q)
    q.enums
    q.time (avg_time q)

  module Q = struct
    let value = ref empty

    let _reset () = value := empty
    let add time res = value := add !value time res
    let add_enumeration () = value := add_enumeration !value
    let pp ppf () = pp ppf !value
  end

  include Q
end

module Solver = struct
  open Sse_types

  let with_solver path_state f =
    let timeout = Formula_options.Solver.Timeout.get () in
    let file =
      if Sse_options.SmtDir.is_set() then Some (Sse_utils.temp_file ())
      else None in
    let solver = Formula_options.Solver.get () in
    let session = Solver.Session.create ?file ~timeout solver in
    Logger.debug ~level:5 "Running %s %@ %a"
      (Prover.name_of solver) Path_state.pp_loc path_state;
    try
      Path_state.prepare_solver_in_state path_state session;
      let v = Utils.time (fun () -> f session) in
      Solver.Session.destroy session;
      Some v
    with
    | Failure msg ->
      Logger.warning "SMT solver failed on %s" msg;
      Solver.Session.destroy session;
      if not (Sse_options.KeepGoing.get())
      then begin
        Logger.error
          "@[<v 0>\
           @[SMT solver failed in %a@ with message:@].@ \
           @[%s@]@ \
           @[Aborting. Use -keep-going to ignore@]@]"
          (Print_utils.pp_opt Format.pp_print_string) file msg;
        failwith msg
      end;
      None
    | e ->
      Solver.Session.destroy session;
      raise e

  let no_address = Basic_types.Int64.Set.empty

  let get_addresses_to_load session path_state =
    if not (LoadSections.is_set () || LoadROSections.get())
    then no_address
    else
      let model = Solver.Session.get_model session in
      let addresses = Smt_model.memory_addresses model in
      let loader = Kernel_functions.get_img () in
      let keep_addr addr =
        (* load addr iff
          *  (we don't have loaded it yet)
          *  && (it is in a (read-only || specified in cmdline) section)
        *)
        not (Path_state.address_belongs_to_init ~addr path_state) &&
          let address = Int64.to_int addr in
          match Loader_utils.find_section_by_address ~address loader with
          | None -> false
          | Some section ->
             let name = Loader.Section.name section in
             Basic_types.String.Set.mem name (LoadSections.get()) ||
               LoadROSections.get() &&
                 Loader.Section.(has_flag Loader_types.Read section &&
                                   not (has_flag Loader_types.Write section))
      in
      let to_load = List.filter keep_addr addresses in
      Basic_types.Int64.Set.(List.fold_left (fun set x-> add x set) empty to_load)

  let maybe_load_and_recurse f result path_state to_load =
    if to_load = no_address then result, path_state else begin
      Logger.debug ~level:1 "at %a:@ loading addresses @ %a"
        Path_state.pp_loc path_state
        (fun fmt ->
           Basic_types.Int64.Set.iter
             (fun x -> Format.fprintf fmt "0x%Lx@ " x)
        )
        to_load;
      let path_state =
        Basic_types.Int64.Set.fold
          (fun addr ps ->
             Path_state.with_init_mem_at ps ~addr ~size:1
          ) to_load path_state
      in
      f path_state
    end

  let rec check_satistifiability path_state =
    let result, to_load =
      with_solver path_state
        (fun session ->
          let result = Solver.Session.check_sat session in
          result, match result with
          | Formula.SAT -> get_addresses_to_load session path_state
          | _ -> no_address)
      |> function
         | Some (time, r) -> Query_stats.add time (fst r); r
         | None -> Formula.UNKNOWN, no_address
    in
    let log = match result with
      | Formula.SAT
      | Formula.UNSAT -> Logger.debug ~level:4
      | _ -> Logger.warning ~level:0 in
    log "SMT query resulted in %a" Formula_pp.pp_status result;
    maybe_load_and_recurse check_satistifiability result path_state to_load

  let get_model path_state =
    let f session =
      match Solver.Session.check_sat session with
      | Formula.SAT -> Solver.Session.get_model session
      | _ -> failwith "we ask the solver a model when formula is unsat"
    in
    match with_solver path_state f with
    | None -> None
    | Some (_, r) -> Some r



  let rec enumerate_values n expr path_state =
    Query_stats.add_enumeration ();
    let rec loop acc to_load n solver =
      match n with
      | 0 ->
        begin
          acc, to_load
        end
      | _ ->
        begin
          begin match acc with
            | [] -> ()
            | x :: _ ->
              Formula.(mk_bv_distinct (mk_bv_cst x) expr)
              |> Formula.mk_assert
              |> Solver.Session.put_entry solver
          end;
          match Solver.Session.check_sat solver with
          | Formula.SAT ->
            let bv = Solver.Session.get_value solver (Formula.mk_bv_term expr) in
            Logger.debug ~level:5 "Solver returned %a ; \
                                   %d solutions still to be found"
              Bitvector.pp_hex bv
              (n-1);
            let to_load' = get_addresses_to_load solver path_state in
            loop (bv :: acc)
              (Basic_types.Int64.Set.union to_load to_load') (n - 1) solver
          | res ->
            begin
              Logger.debug ~level:4 "Solver returned %a"
                Formula_pp.pp_status res;
              acc, to_load
            end
        end
    in
    let values, to_load =
      with_solver path_state (loop [] no_address n)
      |> function
        | None -> [], no_address
        | Some (_, v) -> v
    in
    let values, path_state =
      maybe_load_and_recurse (enumerate_values n expr) values path_state to_load
    in
    if List.length values = n then
      Logger.warning "Found as many solutions for@ %a@ as asked.@ \
                      Possibly incomplete solution set."
        Formula_pp.pp_bv_term expr;
    values, path_state
end

module Translate = struct
  open Dba
  open Sse_types


  let unary e = function
    | Unary_op.Not    -> Formula.mk_bv_not
    | Unary_op.UMinus -> Formula.mk_bv_neg
    | Unary_op.Sext n -> Formula.mk_bv_sign_extend (n - Dba.Expr.size_of e)
    | Unary_op.Uext n -> Formula.mk_bv_zero_extend (n - Dba.Expr.size_of e)
    | Unary_op.Restrict interval -> Formula.mk_bv_extract interval


  let as_bv bop e1 e2 =
    Formula.(mk_bv_ite (bop e1 e2) (mk_bv_one) (mk_bv_zero))

  let rotate_right_const n = Formula.mk_bv_rotate_right n
  let rotate_left_const n = Formula.mk_bv_rotate_left n

  let rotate shift_func rev_shift_func const_rot_func value shift =
    let open Formula in
    match shift.bv_term_desc with
    | BvCst x ->
      let op = Bitvector.value_of x |> Bigint.int_of_big_int |> const_rot_func in
      op value
    | _ ->
      let part1 = shift_func value shift
      and shift_size = Formula_utils.bv_size shift
      and value_size = Formula_utils.bv_size value |> Bigint.big_int_of_int in
      let value_size = Bitvector.create value_size shift_size |> mk_bv_cst in
      let offset = mk_bv_sub value_size shift in
      let part2 = rev_shift_func value offset in
      mk_bv_or part1 part2

  let rotate_right = rotate Formula.mk_bv_lshr Formula.mk_bv_shl rotate_right_const
  let rotate_left = rotate Formula.mk_bv_shl Formula.mk_bv_lshr rotate_left_const


  let binary op =
    let open Binary_op in
    match op with
    | Plus   -> Formula.mk_bv_add
    | Minus  -> Formula.mk_bv_sub
    | Mult   -> Formula.mk_bv_mul
    | DivU   -> Formula.mk_bv_udiv
    | DivS   -> Formula.mk_bv_sdiv
    | ModU   -> Formula.mk_bv_urem
    | ModS   -> Formula.mk_bv_srem
    | Eq     -> as_bv (Formula.mk_bv_equal)
    | Diff   -> as_bv (Formula.mk_bv_distinct)
    | LeqU   -> as_bv (Formula.mk_bv_ule)
    | LtU    -> as_bv (Formula.mk_bv_ult)
    | GeqU   -> as_bv (Formula.mk_bv_uge)
    | GtU    -> as_bv (Formula.mk_bv_ugt)
    | LeqS   -> as_bv (Formula.mk_bv_sle)
    | LtS    -> as_bv (Formula.mk_bv_slt)
    | GeqS   -> as_bv (Formula.mk_bv_sge)
    | GtS    -> as_bv (Formula.mk_bv_sgt)
    | Xor    -> Formula.mk_bv_xor
    | And    -> Formula.mk_bv_and
    | Or     -> Formula.mk_bv_or
    | Concat -> Formula.mk_bv_concat
    | LShift -> Formula.mk_bv_shl
    | RShiftU -> Formula.mk_bv_lshr
    | RShiftS -> Formula.mk_bv_ashr
    | LeftRotate -> rotate_left
    | RightRotate -> rotate_right


  let rec expr symbolic_state e =
    let smt_unary = unary and smt_binary = binary in
    let open Dba.Expr in
    match e with
    | Var (name, bitsize, _) ->
      Sse_symbolic.State.get_bv symbolic_state name (Size.Bit.create bitsize)
    | Cst (_, bv) -> Formula.mk_bv_cst bv
    | Load (bytes, _endianness, e) ->
      let smt_e = expr symbolic_state e in
      let mem = Sse_symbolic.State.get_memory symbolic_state in
      Formula.mk_select bytes mem smt_e
    | Binary (bop, lop, rop) as e ->
      Logger.debug ~level:6 "Translating binary %a" Dba_printer.Ascii.pp_bl_term e;
      let l_smt_e = expr symbolic_state lop and r_smt_e = expr symbolic_state rop in
      smt_binary bop l_smt_e r_smt_e
    | Unary (uop, e) ->
      (expr symbolic_state e) |> smt_unary e uop
    | Ite (c, then_e, else_e) ->
      Formula.(mk_bv_ite
                 (mk_bv_equal (expr symbolic_state c) (mk_bv_one))
                 (expr symbolic_state then_e)
                 (expr symbolic_state else_e))

  open Sse_symbolic
  let lvalue_with_rval_update symbolic_state logical_rval = function
    | LValue.Var (name, bitsize, _) ->
      name, Formula.bv_sort bitsize, Formula.mk_bv_term logical_rval
    | LValue.Restrict (name, bitsize, {Interval.lo; Interval.hi}) ->
      let size = Size.Bit.create bitsize in
      let t = Formula.bv_sort bitsize in
      let svar = State.get_bv symbolic_state name size in
      let concat_lo = lo - 1 and concat_hi = hi + 1 in
      let max_bit = bitsize - 1 in
      let rval =
        let open Formula in
        match concat_lo < 0, concat_hi > max_bit with
        | false, false ->
          mk_bv_concat
            (mk_bv_extract {Interval.lo=concat_hi; Interval.hi=max_bit} svar)
            (mk_bv_concat logical_rval
               (mk_bv_extract {Interval.lo=0; Interval.hi=concat_lo} svar))
        | true, false ->
          mk_bv_concat
            (mk_bv_extract {Interval.lo=concat_hi; Interval.hi=max_bit} svar)
            logical_rval
        | false, true ->
          mk_bv_concat
            logical_rval
            (mk_bv_extract {Interval.lo=0; Interval.hi=concat_lo} svar)
        | true, true -> logical_rval
      in name, t, Formula.mk_bv_term rval
    | LValue.Store (sz, _, e) ->
      let mem = State.get_memory symbolic_state in
      let value = logical_rval in
      let index = expr symbolic_state e in
      memory_name, memory_type, Formula.(mk_ax_term (mk_store sz mem index value))

  let assignment lvalue rvalue path_state =
    let symstate = Path_state.symbolic_state path_state in
    let logical_rval_base = expr symstate rvalue in
    let name, var_type, logical_rval =
      lvalue_with_rval_update symstate logical_rval_base lvalue in
    Path_state.update_symbolic_state name var_type logical_rval path_state

  let assign lval rval symstate =
    let logical_rval_base = expr symstate rval in
    let name, var_type, logical_rval =
      lvalue_with_rval_update symstate logical_rval_base lval in
    Sse_symbolic.State.assign symstate name var_type logical_rval

  let nondet_count = ref 0

  let nondet lvalue path_state =
    let symstate = Path_state.symbolic_state path_state in
    let size = LValue.size_of lvalue in
    incr nondet_count;
    let var = Formula.bv_var ("unknown" ^ string_of_int !nondet_count) size in
    Sse_symbolic.Store.add_entry symstate.Sse_symbolic.State.store
    @@ Formula.entry @@ Formula.Declare(Formula.mk_bv_decl var []);
    let logical_rval_base = Formula.mk_bv_var var in
    let name, var_type, logical_rval =
      lvalue_with_rval_update symstate logical_rval_base lvalue in
    Path_state.update_symbolic_state name var_type logical_rval path_state

  let assume cond state =
    assert(Expr.size_of cond = 1);
    let tr_cond =
      Formula.(mk_bv_equal
                 (expr (Path_state.symbolic_state state) cond)
                 mk_bv_one)
    in Path_state.add_assertion tr_cond state
  ;;

end
