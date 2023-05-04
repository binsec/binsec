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

open Types

type 'a t =
  | Hook : {
      addr : Virtual_address.t;
      info : string;
      mutable succ : [ `All ] t;
    }
      -> [< `Label | `All ] t
  | Exec : {
      addr : Virtual_address.t;
      info : string;
      n : int;
      others : (Virtual_address.t * string) list;
      mutable succ : [ `All ] t;
    }
      -> [< `Label | `All ] t
  | Assign : {
      var : Var.t;
      rval : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `All ] t
  | Clobber : { var : Var.t; mutable succ : [ `All ] t } -> [< `All ] t
  | Load : {
      var : Var.t;
      base : A.t;
      dir : Machine.endianness;
      addr : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `All ] t
  | Store : {
      base : A.t;
      dir : Machine.endianness;
      addr : Expr.t;
      rval : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `All ] t
  | Symbolize : { var : Var.t; mutable succ : [ `All ] t } -> [< `All ] t
  | Assume : {
      test : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `Assume | `All ] t
  | Assert : {
      test : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `Assert | `All ] t
  | Branch : {
      test : Expr.t;
      mutable taken : [ `All ] t;
      mutable fallthrough : [ `All ] t;
    }
      -> [< `Branch | `All ] t
  | Goto : {
      addr : Virtual_address.t;
      mutable preds : (bool * [ `All ] t) list;
    }
      -> [< `All ] t
  | Jump : Expr.t -> [< `Jump | `All ] t
  | Halt : [< `All ] t
  | Probe : {
      kind : Probe.t;
      mutable succ : [ `All ] t;
    }
      -> [< `Probe | `All ] t
  | Cut : [< `All ] t
  | Die : string -> [< `All ] t

val addr : [ `Label ] t -> Virtual_address.t

val assign : Dba.LValue.t -> Dba.Expr.t -> [ `All ] t -> [ `All ] t

val nondet : Dba.LValue.t -> [ `All ] t -> [ `All ] t

val of_dhunk : Dhunk.t -> [ `All ] t

val of_script : ?continue:[ `All ] t -> Script.Instr.t list -> [ `All ] t

val mk_cut :
  Virtual_address.t -> string -> Expr.t option -> [ `All ] t -> [ `Label ] t

val mk_assume :
  Virtual_address.t -> string -> Expr.t -> [ `All ] t -> [ `Label ] t

val mk_assert :
  Virtual_address.t -> string -> Expr.t -> [ `All ] t -> [ `Label ] t

val mk_reach :
  Virtual_address.t ->
  string ->
  int ->
  Expr.t option ->
  int ->
  Output.t list ->
  [ `All ] t ->
  [ `Label ] t

val mk_enumerate :
  Virtual_address.t ->
  string ->
  int ->
  Output.format ->
  Expr.t ->
  int ->
  [ `All ] t ->
  [ `Label ] t

val relink : ?taken:bool -> pred:[ `All ] t -> [ `All ] t -> unit
