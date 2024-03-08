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

module type S = sig
  type t

  val zero : t
  (** Zero (hard-wired) aka x0 *)

  val ra : t
  (** Return address / x1 *)

  val sp : t
  val gp : t
  val tp : t

  val fp : t
  (** Same as s0 *)

  val a0 : t
  val a1 : t
  val a2 : t
  val a3 : t
  val a4 : t
  val a5 : t
  val a6 : t
  val a7 : t
  val t0 : t
  val t1 : t
  val t2 : t
  val t3 : t
  val t4 : t
  val t5 : t
  val t6 : t
  val s0 : t
  val s1 : t
  val s2 : t
  val s3 : t
  val s4 : t
  val s5 : t
  val s6 : t
  val s7 : t
  val s8 : t
  val s9 : t
  val s10 : t
  val s11 : t
  val name : t -> string
  val size : t -> int
  val num : t -> int
  val bvnum : t -> Bitvector.t
  val of_string : string -> t option
  val of_int_exn : int -> t
  val of_int : int -> t option
  val expr : t -> Dba.Expr.t
  val lval : t -> Dba.LValue.t
end

module Mode = struct
  [@@@warning "-37"]

  type t = M32 | M64 | M128

  let m32 = M32
  and m64 = M64
  and m128 = M128

  let is_m32 = function M32 -> true | M64 | M128 -> false
  let is_m64 = function M64 -> true | M32 | M128 -> false
  let is_m128 = function M128 -> true | M32 | M64 -> false
  let size = function M32 -> 32 | M64 -> 64 | M128 -> 128
end

module type RegisterSize = sig
  val size : Mode.t
end

module Register (M : RegisterSize) = struct
  type register = Dba.Var.t
  type t = register

  (* The local hashtable holding the information for this architecture *)
  module H = struct
    include Basic_types.String.Htbl

    let h = create 7
    let get = find h
    let set = add h
  end

  let regs base last =
    let rec aux acc m =
      if m > last then List.rev acc
      else
        let name = base ^ string_of_int m in
        aux (name :: acc) (m + 1)
    in
    aux [] 0

  let canonical_registers =
    ("zero" :: "ra" :: "sp" :: "gp" :: "tp" :: regs "a" 7)
    @ regs "t" 7 @ regs "s" 11

  (* The original, non-ABI given names for RISCV *)
  let _x_registers = regs "x" 31

  (* Add registers to the hashtbl *)
  let add_all () =
    let info = Dba.Var.Tag.Register
    and bitsize = Size.Bit.create (Mode.size M.size) in
    List.iter
      (fun r -> H.set r (Dba.Var.create r ~bitsize ~tag:info))
      canonical_registers

  let _ = add_all ()

  let zero = H.get "zero"
  and ra = H.get "ra"
  and sp = H.get "sp"
  and gp = H.get "gp"
  and tp = H.get "tp"
  and a0 = H.get "a0"
  and a1 = H.get "a1"
  and a2 = H.get "a2"
  and a3 = H.get "a3"
  and a4 = H.get "a4"
  and a5 = H.get "a5"
  and a6 = H.get "a6"
  and a7 = H.get "a7"
  and t0 = H.get "t0"
  and t1 = H.get "t1"
  and t2 = H.get "t2"
  and t3 = H.get "t3"
  and t4 = H.get "t4"
  and t5 = H.get "t5"
  and t6 = H.get "t6"
  and s0 = H.get "s0"
  and s1 = H.get "s1"
  and s2 = H.get "s2"
  and s3 = H.get "s3"
  and s4 = H.get "s4"
  and s5 = H.get "s5"
  and s6 = H.get "s6"
  and s7 = H.get "s7"
  and s8 = H.get "s8"
  and s9 = H.get "s9"
  and s10 = H.get "s10"
  and s11 = H.get "s11"

  let fp = s0

  (* The x table below relates ABI-named registers to their "physical" name *)
  let x =
    [|
      zero;
      ra;
      sp;
      gp;
      tp;
      t0;
      t1;
      t2;
      s0;
      s1;
      a0;
      a1;
      a2;
      a3;
      a4;
      a5;
      a6;
      a7;
      s2;
      s3;
      s4;
      s5;
      s7;
      s7;
      s8;
      s9;
      s10;
      s11;
      t3;
      t4;
      t5;
      t6;
    |]

  let name t = t.Dba.Var.name
  and size t = t.Dba.Var.size
  and expr t = Dba.Expr.v t
  and lval t = Dba.LValue.v t

  let of_string name =
    match H.get name with t -> Some t | exception Not_found -> None

  let of_int_exn n = Array.get x n
  let of_int n = match of_int_exn n with t -> Some t | exception _ -> None

  let num t =
    let exception Found of int in
    try
      Array.iteri (fun i r -> if r = t then raise (Found i)) x;
      raise Not_found
    with Found i -> i

  let bvnum t = Bitvector.of_int ~size:(Mode.size M.size) (num t)
end
