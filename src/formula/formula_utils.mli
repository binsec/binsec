(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

(** High level formula manipulation *)

open Smtlib2

(** {2 Registers/Variables} *)

(** [concretize_register name value env] concretize
    the register [name] with the value [value]. This
    function solely add a constraint on the current
    register value. *)
val concretize_register : string -> smt_bv_expr -> Path_pred_env.t -> unit

(** replace the register value with the given [smt_bv_expr]
    @deprecated use [logicalize_register] instead
*)
val replace_register : string -> smt_bv_expr -> Path_pred_env.t -> unit

(** convert the DBA expression in SMT and then call [replace_register] *)
val logicalize_register : string -> Dba.expr -> Path_pred_env.t -> unit

(** [symbolize_register name ~is_full_name prefix] create
    a new symbol and assign it in the register [name]. 
    [~is_full_name] define only use [prefix] as symbol 
    name otherwise concatenate [prefix] and [name]*)
val symbolize_register : string -> ?is_full_name:bool -> string -> Path_pred_env.t -> unit

(** symbolize the register and then concretize it. It
    has in effect to constraint the new symbol with the
    concrete value. (reset history) *)
val symbolize_and_then_concretize_register : string -> smt_bv_expr -> string -> Path_pred_env.t -> unit


(** {2 Memory} *)

(** [replace_memory addr data env] perform a store operation of
    every bytes of [data] starting at [addr]
    @deprecated use [logicalize_memory] instead *)
val replace_memory : int64 -> string -> Path_pred_env.t -> unit

(** [concretize_memory dba_addr data env] add constraints on the
    memory at the logical address [dba_addr] for every bytes
    of [data] *)
val concretize_memory : Dba.expr -> string -> Path_pred_env.t -> unit

(** [symbolize_memory_one_octet addr prefix env] symbolize the the byte
    at address [addr] and prefix the new input symbol by [prefix] *)
val symbolize_memory_one_octet : int64 -> string -> Path_pred_env.t -> unit

(** [symbolize_memory dba_addr prefix ~i size env] symbolize the memory
    starting at the address [dba_addr] up to [size]. All input symbols
    created will be prefixed by [prefix] *)
val symbolize_memory : Dba.expr -> string -> ?i:int -> int -> Path_pred_env.t -> unit

(** [logicalize_memory addr content] put [content] at [addr]
    by performing a logical store in memory *)
val logicalize_memory : Dba.expr -> Dba.expr -> Path_pred_env.t -> unit