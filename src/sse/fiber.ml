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

open Types

module type S = sig
  type builtin

  type 'a t =
    | Debug : { msg : string; mutable succ : [ `All ] t } -> [< `All ] t
    | Print : { output : Output.t; mutable succ : [ `All ] t } -> [< `All ] t
    | Step : {
        addr : Virtual_address.t;
        n : int;
        mutable succ : [ `All ] t;
      }
        -> [< `All ] t
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
    | Goto : Virtual_address.t -> [< `All ] t
    | Jump : Expr.t -> [< `Jump | `All ] t
    | Halt : [< `All ] t
    | Probe : {
        kind : Probe.t;
        mutable succ : [ `All ] t;
      }
        -> [< `Probe | `All ] t
    | Builtin : { f : builtin; mutable succ : [ `All ] t } -> [ `All ] t
    | Cut : [< `All ] t
    | Die : string -> [< `All ] t
end
