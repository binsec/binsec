(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Status : sig
  type t = Unknown | Unreachable | Clear | Opaque of bool

  val pp : Format.formatter -> t -> unit
end

module Directive : sig
  type t =
    | ExpectAt of Virtual_address.t * Status.t
    | SkipJump of Virtual_address.t
    | ProcessCall of Virtual_address.t
end

module Query_stat : Symbolic.Metrics.S

module rec Path : sig
  include Symbolic.Path.S

  val init :
    env:Env.t ->
    conditions:bool list ->
    jump_targets:Virtual_address.t list ->
    t
end

and Env : sig
  type t = {
    cfg : Ghidra_cfg.t;  (** The Ghidra cfg *)
    ims : string Virtual_address.Htbl.t;  (** Instruction mnemonic storage *)
    ctp : Virtual_address.Set.t;  (** Call to process *)
    bbt : ([ `All ], Path.t) Binsec_sse.Types.fiber Dba_types.Caddress.Htbl.t;
        (** Basic bloc table *)
    dis : Dba.Instr.t Dba_types.Caddress.Htbl.t;  (** Dba instruction storage *)
    dap : Dba_types.Caddress.t list Dba_types.Caddress.Htbl.t;
        (** Dba address predecessors *)
    opa : bool Dba_types.Caddress.Htbl.t;  (** Opaque predicate addresses *)
  }
end

type node = ([ `All ], Path.t) Binsec_sse.Types.fiber

module Driver : sig
  include
    Interpreter.DRIVER
      with type path = Path.t
       and type outcome = (Path.value, Binsec_sse.Types.status) Result.t

  val start : path -> Dba.address -> outcome
end
