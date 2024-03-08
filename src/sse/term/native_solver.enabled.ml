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

module Bv = Sexpr.Bv
module Expr = Sexpr.Expr
module Memory = Sexpr.Memory
module StTbl = Sexpr.StTbl
module BvTbl = Sexpr.BvTbl
module AxTbl = Sexpr.AxTbl
module NiTbl = Basic_types.Int.Htbl
module BiMap = Basic_types.BigInt.Map

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module Solver () : Solver_sig.S = struct
  module Solver = Bitwuzla.Incremental ()
  module T = Term
  open Solver

  type nonrec result = result = Sat | Unsat | Unknown
  type memory = (bv, bv) ar term
  type nonrec term = bv term

  module Context = struct
    type t = {
      fvariables : Expr.t StTbl.t;
      farrays : Memory.t StTbl.t;
      st_cons : bv sort NiTbl.t;
      bv_cons : term BvTbl.t;
      ax_cons : memory AxTbl.t;
      addr_space : int;
    }

    let create addr_space =
      {
        fvariables = StTbl.create 16;
        farrays = StTbl.create 4;
        st_cons = NiTbl.create 8;
        bv_cons = BvTbl.create 128;
        ax_cons = AxTbl.create 64;
        addr_space;
      }
  end

  open Context

  let visit_sort ctx sz =
    try NiTbl.find ctx.st_cons sz
    with Not_found ->
      let st = Sort.bv sz in
      NiTbl.add ctx.st_cons sz st;
      st

  and visit_select =
    let rec iter len concat index array res =
      if len = 0 then res
      else
        iter (len - 1) concat (Term.Bv.succ index) array
          (concat (Term.Ar.select array index) res)
    in
    fun len dir index array ->
      let concat =
        match dir with
        | Machine.LittleEndian -> Term.Bv.append
        | Machine.BigEndian -> Fun.flip Term.Bv.append
      in
      iter (len - 1) concat (Term.Bv.succ index) array
        (Term.Ar.select array index)

  and visit_binop =
    let open Expr in
    function
    | Plus -> Term.Bv.add
    | Minus -> Term.Bv.sub
    | Mul -> Term.Bv.mul
    | Udiv -> Term.Bv.udiv
    | Sdiv -> Term.Bv.sdiv
    | Umod -> Term.Bv.urem
    | Smod -> Term.Bv.srem
    | Or -> Term.Bv.logor
    | And -> Term.Bv.logand
    | Xor -> Term.Bv.logxor
    | Concat -> Term.Bv.append
    | Lsl -> Term.Bv.shift_left
    | Lsr -> Term.Bv.shift_right_logical
    | Asr -> Term.Bv.shift_right
    | Rol -> Term.Bv.rotate_left
    | Ror -> Term.Bv.rotate_right
    | Eq -> Term.equal
    | Diff -> Term.distinct
    | Ule -> Term.Bv.ule
    | Ult -> Term.Bv.ult
    | Uge -> Term.Bv.uge
    | Ugt -> Term.Bv.ugt
    | Sle -> Term.Bv.sle
    | Slt -> Term.Bv.slt
    | Sge -> Term.Bv.sge
    | Sgt -> Term.Bv.sgt

  let rec unroll_store array index x s =
    if s = 8 then Term.Ar.store array index x
    else
      unroll_store
        (Term.Ar.store array index (Term.Bv.extract ~hi:7 ~lo:0 x))
        (Term.Bv.succ index)
        (Term.Bv.extract ~hi:(s - 1) ~lo:8 x)
        (s - 8)

  let rec visit_bv ctx bv =
    try BvTbl.find ctx.bv_cons bv
    with Not_found ->
      let e =
        match bv with
        | Var { name; size; _ } ->
            StTbl.add ctx.fvariables name bv;
            Term.const (visit_sort ctx size) name
        | Load { len; dir; addr; label; _ } ->
            let sort = visit_sort ctx (Expr.sizeof addr) in
            let index = visit_bv ctx addr and array = visit_ax ctx sort label in
            visit_select len dir index array
        | Cst bv ->
            let size = Bv.size_of bv and value = Bv.value_of bv in
            let st = visit_sort ctx size in
            Term.Bv.of_z st value
        | Unary { f = Not; x; _ } -> Term.Bv.lognot (visit_bv ctx x)
        | Unary { f = Minus; x; _ } -> Term.Bv.neg (visit_bv ctx x)
        | Unary { f = Uext n; x; _ } -> Term.Bv.zero_extend n (visit_bv ctx x)
        | Unary { f = Sext n; x; _ } -> Term.Bv.sign_extend n (visit_bv ctx x)
        | Unary { f = Restrict { lo; hi }; x; _ } ->
            Term.Bv.extract ~hi ~lo (visit_bv ctx x)
        | Binary { f; x; y; _ } ->
            (visit_binop f) (visit_bv ctx x) (visit_bv ctx y)
        | Ite { c; t; e; _ } ->
            Term.ite (visit_bv ctx c) (visit_bv ctx t) (visit_bv ctx e)
      in
      BvTbl.add ctx.bv_cons bv e;
      e

  and visit_ax ctx index (ax : Memory.t) =
    try AxTbl.find ctx.ax_cons ax
    with Not_found ->
      let a =
        match ax with
        | Root ->
            Term.const
              (Sort.ar index (visit_sort ctx byte_size))
              Suid.(to_string zero)
        | Symbol name ->
            StTbl.add ctx.farrays name ax;
            Term.const (Sort.ar index (visit_sort ctx byte_size)) name
        | Layer { addr; store; over; _ } ->
            let base = visit_bv ctx addr and array = visit_ax ctx index over in
            Sexpr.Store.fold
              (fun i value array ->
                if Sexpr.Chunk.is_hunk value then array
                else
                  let value = Sexpr.Chunk.to_term value in
                  let s = Expr.sizeof value in
                  let x = visit_bv ctx value
                  and index = Term.Bv.(add base (of_z index i)) in
                  unroll_store array index x s)
              array store
      in
      AxTbl.add ctx.ax_cons ax a;
      a

  let ctx = create (Kernel_options.Machine.word_size ())

  let put _ constraints =
    List.iter (fun bl -> assert' (visit_bv ctx bl)) constraints

  let bind _ e constraints =
    let e = visit_bv ctx e in
    put () constraints;
    e

  let iter_free_variables f = StTbl.iter f ctx.fvariables
  let iter_free_arrays f = StTbl.iter f ctx.farrays
  let get e = BvTbl.find ctx.bv_cons e

  let set_memory ~addr value =
    let sort = visit_sort ctx ctx.addr_space in
    assert'
      (Term.equal
         (Term.Ar.select
            (visit_ax ctx sort Memory.root)
            (Term.Bv.of_z sort addr))
         (Term.Bv.of_z (visit_sort ctx byte_size) value))

  let neq e x = assert' (Term.distinct e (Term.Bv.of_z (Term.sort e) x))

  let get_array ar =
    let assignment, _ =
      Term.Ar.assignment (get_value (AxTbl.find ctx.ax_cons ar))
    in
    Array.map
      (fun (addr, value) ->
        ( Term.Bv.assignment addr,
          Char.unsafe_chr (Z.to_int (Term.Bv.assignment value)) ))
      assignment

  let get_value x = Term.Bv.assignment (get_value x)
  let timeout = Formula_options.Solver.Timeout.get ()
  let check_sat () = Smt_bitwuzla_utils.watchdog ~timeout check_sat ()
  let close () = unsafe_close ()
end
