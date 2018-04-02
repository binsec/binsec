(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open TypeTraceDSE
open TypeHistoryDSE

(* module CriteriaAsDFS : TypeCriteriaDSE (TraceDSE_v) (HistoryDSE_v) = *)
module CriteriaAsDefault = functor (TraceDSE_v:TypeTraceDSE) -> functor (HistoryDSE_v:TypeHistoryDSE) ->
struct
  type conf_criteria = string
  type trace_t = TraceDSE_v.trace_t                    
  type child_t = TraceDSE_v.child_t                    
  type history_t  = HistoryDSE_v(TraceDSE_v).history_t 

  let init_criteria _c = ()
  (* we explore all execution paths, so trace verdict is always false *)
  let verdict _trace = false

  (* we explore all execution paths, so stop criteria is always false *)
  let stop_criteria _history _invalid_children  = false
end
