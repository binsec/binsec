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

module Bitvector = Binsec.Bitvector

module Interrupt : sig
  val default : float -> int
  val timeout : float -> int
end = struct
  let check timestamp = Float.compare (Unix.gettimeofday ()) timestamp

  let interrupt f x =
    match Unix.sigpending () with
    | [] | (exception Invalid_argument _) -> f x
    | l when List.mem Sys.sigint l -> 1
    | _ -> f x

  let zero = Fun.const 0
  let default = interrupt zero
  let timeout = interrupt check
end

let watchdog ~timeout (f : ?interrupt:('a -> int) * 'a -> 'b) =
  let interrupt =
    if timeout = 0. then (Interrupt.default, 0.)
    else (Interrupt.timeout, Unix.gettimeofday () +. timeout)
  in
  f ~interrupt

module Session () : Common.S = struct
  include Bitwuzla.Incremental ()

  let bv_sort =
    let module Tbl = Binsec.Basic_types.Int.Htbl in
    let tbl = Tbl.create 32 in
    fun sz ->
      try Tbl.find tbl sz
      with Not_found ->
        let sort = Sort.bv sz in
        Tbl.add tbl sz sort;
        sort

  module Bl = struct
    type t = bv term

    let const name = Term.const Sort.bool name
    let top = Term.Bl.true'
    let bot = Term.Bl.false'
    let lognot = Term.Bl.lognot
    let logand = Term.Bl.logand
    let logor = Term.Bl.logor
    let logxor = Term.Bl.logxor
    let ite = Term.ite
    let equal = Term.equal
    let diff = Term.distinct
    let implies = Term.Bl.implies
    let to_bv = Fun.id
  end

  module Bv = struct
    type t = bv term

    let const size name = Term.const (bv_sort size) name
    let value sz bv = Term.Bv.of_z (bv_sort sz) bv
    let equal = Term.equal
    let diff = Term.distinct
    let ule = Term.Bv.ule
    let uge = Term.Bv.uge
    let ult = Term.Bv.ult
    let ugt = Term.Bv.ugt
    let sle = Term.Bv.sle
    let sge = Term.Bv.sge
    let slt = Term.Bv.slt
    let sgt = Term.Bv.sgt
    let add = Term.Bv.add
    let sub = Term.Bv.sub
    let mul = Term.Bv.mul
    let neg = Term.Bv.neg
    let udiv = Term.Bv.udiv
    let umod = Term.Bv.urem
    let urem = Term.Bv.urem
    let sdiv = Term.Bv.sdiv
    let smod = Term.Bv.smod
    let srem = Term.Bv.srem
    let logand = Term.Bv.logand
    let logor = Term.Bv.logor
    let lognot = Term.Bv.lognot
    let logxor = Term.Bv.logxor
    let lognand = Term.Bv.lognand
    let lognor = Term.Bv.lognor
    let logxnor = Term.Bv.logxnor
    let shift_left = Term.Bv.shift_left
    let shift_right = Term.Bv.shift_right_logical
    let shift_right_signed = Term.Bv.shift_right
    let rotate_left = Term.Bv.rotate_left
    let rotate_right = Term.Bv.rotate_right
    let rotate_lefti = Term.Bv.rotate_lefti
    let rotate_righti = Term.Bv.rotate_righti
    let append = Term.Bv.append
    let extract = Term.Bv.extract
    let uext = Term.Bv.zero_extend
    let sext = Term.Bv.sign_extend
    let ite = Term.ite
    let succ = Term.Bv.succ
    let to_bl = Fun.id
  end

  module Ax = struct
    type t = (bv, bv) ar term
    type nonrec sort = (bv, bv) ar sort

    let sort ~idx elm = Sort.ar (bv_sort idx) (bv_sort elm)
    let const = Term.const
    let store = Term.Ar.store
    let select = Term.Ar.select
    let ite = Term.ite
    let equal = Term.equal
    let diff = Term.distinct
  end

  let assert_formula bl = assert' bl
  let push () = push 1
  let pop () = pop 1

  let check_sat ?(timeout = 0.) () : Common.status =
    match watchdog ~timeout check_sat () with
    | Sat -> Sat
    | Unsat -> Unsat
    | Unknown -> Unknown

  let check_sat_assuming ?(timeout = 0.) e : Common.status =
    match watchdog ~timeout check_sat_assuming ?names:None [| e |] with
    | Sat -> Sat
    | Unsat -> Unsat
    | Unknown -> Unknown

  let get_bv_value bv = Term.Bv.assignment (get_value bv)

  let fold_ax_values f ax v =
    Array.fold_left
      (fun v (addr, value) ->
        f (Term.Bv.assignment addr) (Term.Bv.assignment value) v)
      v
      (fst (Term.Ar.assignment (get_value ax)))

  let close = unsafe_close
end

let factory = Some (module Session : Common.F)
