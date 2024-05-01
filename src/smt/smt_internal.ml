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

module type Session = sig
  val put_entry : Formula.entry -> unit
  val bv_lookup : Formula.bv_term -> Bitvector.t
  val ax_lookup : Formula.ax_term -> (Bitvector.t * Bitvector.t) array
  val check_sat : timeout:int -> Formula.status
  val close : unit -> unit
end

module Session (Solver : Libsolver.S) : Session = struct
  module Bl = Solver.Bl
  module Bv = Solver.Bv
  module Ax = Solver.Ax
  module BlH = Formula.BlTermHashtbl
  module BvH = Formula.BvTermHashtbl
  module AxH = Formula.AxTermHashtbl
  module I = Basic_types.Int.Htbl

  module P = Hashtbl.Make (struct
    type t = int * int

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

  let bl_mapping = BlH.create 64
  let bv_mapping = BvH.create 64
  let ax_mapping = AxH.create 64
  let ax_sort = P.create 8

  let ax_sort idx elt =
    let p = (idx, elt) in
    try P.find ax_sort p
    with Not_found ->
      let sort = Ax.sort ~idx elt in
      P.add ax_sort p sort;
      sort

  let bl_bnop_map : Formula.bl_bnop -> Bl.t -> Bl.t -> Bl.t = function
    | BlImply -> Bl.implies
    | BlAnd -> Bl.logand
    | BlOr -> Bl.logor
    | BlXor -> Bl.logxor

  let bl_comp_map : Formula.bl_comp -> Bl.t -> Bl.t -> Bl.t = function
    | BlEqual -> Bl.equal
    | BlDistinct -> Bl.diff

  let rec repeat n bv_term acc =
    if n = 0 then acc else repeat (n - 1) bv_term (Bv.append bv_term acc)

  let bv_unop_map : Formula.bv_unop -> Bv.t -> Bv.t = function
    | BvNot -> Bv.lognot
    | BvNeg -> Bv.neg
    | BvRepeat n -> fun bv_term -> repeat (n - 1) bv_term bv_term
    | BvZeroExtend n -> Bv.uext n
    | BvSignExtend n -> Bv.sext n
    | BvRotateLeft n -> (Fun.flip Bv.rotate_lefti) n
    | BvRotateRight n -> (Fun.flip Bv.rotate_righti) n
    | BvExtract { hi; lo } -> Bv.extract ~hi ~lo

  let bv_bnop_map : Formula.bv_bnop -> Bv.t -> Bv.t -> Bv.t = function
    | BvConcat -> Bv.append
    | BvAnd -> Bv.logand
    | BvNand -> Bv.lognand
    | BvOr -> Bv.logor
    | BvNor -> Bv.lognor
    | BvXor -> Bv.logxor
    | BvXnor -> Bv.logxnor
    | BvCmp -> fun x y -> Bl.to_bv (Bv.equal x y)
    | BvAdd -> Bv.add
    | BvSub -> Bv.sub
    | BvMul -> Bv.mul
    | BvUdiv -> Bv.udiv
    | BvSdiv -> Bv.sdiv
    | BvUrem -> Bv.urem
    | BvSrem -> Bv.srem
    | BvSmod -> Bv.smod
    | BvShl -> Bv.shift_left
    | BvAshr -> Bv.shift_right_signed
    | BvLshr -> Bv.shift_right

  let bv_comp_map : Formula.bv_comp -> Bv.t -> Bv.t -> Bl.t = function
    | BvEqual -> Bv.equal
    | BvDistinct -> Bv.diff
    | BvUlt -> Bv.ult
    | BvUle -> Bv.ule
    | BvUgt -> Bv.ugt
    | BvUge -> Bv.uge
    | BvSlt -> Bv.slt
    | BvSle -> Bv.sle
    | BvSgt -> Bv.sgt
    | BvSge -> Bv.sge

  let ax_comp_map : Formula.ax_comp -> Ax.t -> Ax.t -> Bl.t = function
    | AxEqual -> Ax.equal
    | AxDistinct -> Ax.diff

  let rec mk_select sz ax_term bv_term acc =
    if sz = 0 then acc
    else
      mk_select (sz - 1) ax_term (Bv.succ bv_term)
        (Bv.append (Ax.select ax_term bv_term) acc)

  let mk_select sz ax_term bv_term =
    mk_select (sz - 1) ax_term (Bv.succ bv_term) (Ax.select ax_term bv_term)

  let rec mk_store bindings elt_size sz ax_term bv_term0 bv_term1 =
    if elt_size = sz then Ax.store ax_term bv_term0 (bv_map bindings bv_term1)
    else
      mk_store bindings elt_size (sz - elt_size)
        (Ax.store ax_term bv_term0
           (bv_map bindings
              (Formula.mk_bv_extract
                 { Interval.hi = elt_size - 1; lo = 0 }
                 bv_term1)))
        (Bv.succ bv_term0)
        (Formula.mk_bv_extract { Interval.hi = sz - 1; lo = elt_size } bv_term1)

  and bind defs bindings =
    let bl_mapping = BlH.create 8
    and bv_mapping = BvH.create 8
    and ax_mapping = AxH.create 8 in
    List.iter (put_define [ (bl_mapping, bv_mapping, ax_mapping) ]) defs;
    (bl_mapping, bv_mapping, ax_mapping) :: bindings

  and bl_lookup bl_term = function
    | [] -> BlH.find bl_mapping bl_term
    | (bl_mapping, _, _) :: bindings -> (
        try BlH.find bl_mapping bl_term
        with Not_found -> bl_lookup bl_term bindings)

  and bv_lookup bv_term = function
    | [] -> BvH.find bv_mapping bv_term
    | (_, bv_mapping, _) :: bindings -> (
        try BvH.find bv_mapping bv_term
        with Not_found -> bv_lookup bv_term bindings)

  and ax_lookup ax_term = function
    | [] -> AxH.find ax_mapping ax_term
    | (_, _, ax_mapping) :: bindings -> (
        try AxH.find ax_mapping ax_term
        with Not_found -> ax_lookup ax_term bindings)

  and bl_map bindings (bl_term : Formula.bl_term) =
    try bl_lookup bl_term bindings
    with Not_found ->
      let bl_term' =
        match bl_term.bl_term_desc with
        | BlTrue -> Bl.top
        | BlFalse -> Bl.bot
        | BlFun _ -> assert false
        | BlLet (defs, bl_term) -> bl_map (bind defs bindings) bl_term
        | BlUnop (BlNot, bl_term) -> Bl.lognot (bl_map bindings bl_term)
        | BlBnop (op, bl_term0, bl_term1) ->
            (bl_bnop_map op) (bl_map bindings bl_term0)
              (bl_map bindings bl_term1)
        | BlComp (op, bl_term0, bl_term1) ->
            (bl_comp_map op) (bl_map bindings bl_term0)
              (bl_map bindings bl_term1)
        | BvComp (op, bv_term0, bv_term1) ->
            (bv_comp_map op) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | AxComp (op, ax_term0, ax_term1) ->
            (ax_comp_map op) (ax_map bindings ax_term0)
              (ax_map bindings ax_term1)
        | BlIte (bl_term0, bl_term1, bl_term2) ->
            Bl.ite (bl_map bindings bl_term0) (bl_map bindings bl_term1)
              (bl_map bindings bl_term2)
      in
      BlH.add bl_mapping bl_term bl_term';
      bl_term'

  and bv_map bindings (bv_term : Formula.bv_term) =
    try bv_lookup bv_term bindings
    with Not_found ->
      let bv_term' =
        match bv_term.bv_term_desc with
        | BvCst bv -> Bv.value (Bitvector.size_of bv) (Bitvector.value_of bv)
        | BvFun ({ bv_name; bv_size; _ }, []) -> Bv.const bv_size bv_name
        | BvFun _ -> assert false
        | BvLet (defs, bv_term) -> bv_map (bind defs bindings) bv_term
        | BvUnop (op, bv_term) -> (bv_unop_map op) (bv_map bindings bv_term)
        | BvBnop (op, bv_term0, bv_term1) ->
            (bv_bnop_map op) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | BvIte (bl_term, bv_term0, bv_term1) ->
            Bv.ite (bl_map bindings bl_term) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | Select (1, ax_term, bv_term) ->
            Ax.select (ax_map bindings ax_term) (bv_map bindings bv_term)
        | Select (sz, ax_term, bv_term) ->
            mk_select sz (ax_map bindings ax_term) (bv_map bindings bv_term)
      in
      BvH.add bv_mapping bv_term bv_term';
      bv_term'

  and ax_map bindings (ax_term : Formula.ax_term) =
    try ax_lookup ax_term bindings
    with Not_found ->
      let ax_term' =
        match ax_term.ax_term_desc with
        | AxFun _ -> assert false
        | AxLet (defs, ax_term) -> ax_map (bind defs bindings) ax_term
        | AxIte (bl_term, ax_term0, ax_term1) ->
            Ax.ite (bl_map bindings bl_term) (ax_map bindings ax_term0)
              (ax_map bindings ax_term1)
        | Store (1, ax_term, bv_term0, bv_term1) ->
            Ax.store (ax_map bindings ax_term) (bv_map bindings bv_term0)
              (bv_map bindings bv_term1)
        | Store (sz, ax_term, bv_term0, bv_term1) ->
            mk_store bindings ax_term.elt_term_size
              (sz * ax_term.elt_term_size)
              (ax_map bindings ax_term) (bv_map bindings bv_term0) bv_term1
      in
      AxH.add ax_mapping ax_term ax_term';
      ax_term'

  and[@warning "-8"] put_define
      ((bl_mapping, bv_mapping, ax_mapping) :: _ as bindings)
      (def : Formula.def) =
    match def.def_desc with
    | BlDef (bl_var, _, bl_term) ->
        BlH.add bl_mapping (Formula.mk_bl_var bl_var) (bl_map bindings bl_term)
    | BvDef (bv_var, _, bv_term) ->
        BvH.add bv_mapping (Formula.mk_bv_var bv_var) (bv_map bindings bv_term)
    | AxDef (ax_var, _, ax_term) ->
        AxH.add ax_mapping (Formula.mk_ax_var ax_var) (ax_map bindings ax_term)

  let put_declare : Formula.decl -> unit = function
    | { decl_desc = BlDecl (({ bl_name; _ } as bl_var), _); _ } ->
        BlH.add bl_mapping (Formula.mk_bl_var bl_var) (Bl.const bl_name)
    | { decl_desc = BvDecl (({ bv_name; bv_size; _ } as bv_var), _); _ } ->
        BvH.add bv_mapping (Formula.mk_bv_var bv_var) (Bv.const bv_size bv_name)
    | {
        decl_desc = AxDecl (({ ax_name; idx_size; elt_size; _ } as ax_var), _);
        _;
      } ->
        AxH.add ax_mapping (Formula.mk_ax_var ax_var)
          (Ax.const (ax_sort idx_size elt_size) ax_name)

  let put_define = put_define [ (bl_mapping, bv_mapping, ax_mapping) ]

  let put_entry (entry : Formula.entry) =
    match entry.entry_desc with
    | Declare decl -> put_declare decl
    | Define def -> put_define def
    | Assert bl_term | Assume bl_term ->
        Solver.assert_formula (bl_map [] bl_term)
    | Comment _ -> ()

  let bv_lookup e =
    Bitvector.create (Solver.get_bv_value (bv_map [] e)) e.bv_term_size

  let ax_lookup e =
    let addr_size, elem_size = Formula_utils.ax_size e in
    Array.of_list
      (Solver.fold_ax_values
         (fun addr value values ->
           (Bitvector.create addr addr_size, Bitvector.create value elem_size)
           :: values)
         (ax_map [] e) [])

  let check_sat ~timeout =
    let timeout = if timeout = 0 then None else Some (float_of_int timeout) in
    match Solver.check_sat ?timeout () with
    | Sat -> Formula.SAT
    | Unsat -> Formula.UNSAT
    | Unknown -> Formula.UNKNOWN

  let check_sat ~timeout =
    match Utils.time (fun () -> check_sat ~timeout) with
    | time, r ->
        Smt_options.Logger.debug "solver returned %a in %fs"
          Formula_pp.pp_status r time;
        r

  let close = Solver.close
end

module Make (Factory : Libsolver.F) = struct
  type t = (module Session) * float

  let queries = ref 0

  type time = { mutable sec : float }

  let cumulated_time = { sec = 0. }
  let query_stat () = !queries
  let time_stat () = cumulated_time.sec

  let open_session () =
    let t = Unix.gettimeofday () in
    ((module Session (Factory ()) : Session), t)

  let check_sat (solver, _) =
    incr queries;
    let timeout = Formula_options.Solver.Timeout.get () in
    let module S = (val solver : Session) in
    S.check_sat ~timeout

  let get_bv_value (solver, _) e =
    let module S = (val solver : Session) in
    S.bv_lookup e

  let get_ax_values (solver, _) e =
    let module S = (val solver : Session) in
    S.ax_lookup e

  let put (solver, _) e =
    let module S = (val solver : Session) in
    S.put_entry e

  let close_session (solver, t) =
    let module S = (val solver : Session) in
    S.close ();
    cumulated_time.sec <- cumulated_time.sec +. Unix.gettimeofday () -. t;
    Smt_options.Logger.debug "solver session closed in %fs"
      (Unix.gettimeofday () -. t)

  let check_sat_and_close solver =
    let res = check_sat solver in
    close_session solver;
    res
end
