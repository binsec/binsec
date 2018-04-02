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

val eval_expr :
  Dba.expr -> Region_bitvector.t Concrete_state.SubEnv.t Static_types.Env.t ->
  Smt_bitvectors.smtBvExprAlt list -> Dba_types.Caddress.Set.t 
  -> Region_bitvector.t

val eval_cond : Dba.cond ->
  Region_bitvector.t Concrete_state.SubEnv.t Static_types.Env.t ->
  Smt_bitvectors.smtBvExprAlt list -> Dba_types.Caddress.Set.t -> bool

val write :
  Static_types.extended_variable -> Bigint.t ->
  Region_bitvector.t ->
  Region_bitvector.t Concrete_state.SubEnv.t Static_types.Env.t ->
  Smt_bitvectors.smtBvExprAlt list ->
  Dba_types.Caddress.Set.t ->
  Region_bitvector.t Concrete_state.SubEnv.t Static_types.Env.t

val read :
  Static_types.extended_variable -> Bigint.t ->
  Region_bitvector.t Concrete_state.SubEnv.t Static_types.Env.t ->
           Smt_bitvectors.smtBvExprAlt list ->
           Dba_types.Caddress.Set.t -> Region_bitvector.t

val add_smt_cond : Dba.cond -> Region_bitvector.t Concrete_state.SubEnv.t Static_types.Env.t  -> Smt_bitvectors.smtBvExprAlt list ->
  Dba_types.Caddress.Set.t -> Smt_bitvectors.smtBvExprAlt list

val perm : Dba_types.permissions list Dba_types.Region.Map.t ref
val permis : Dba.cond Dba_types.Rights.t ref
