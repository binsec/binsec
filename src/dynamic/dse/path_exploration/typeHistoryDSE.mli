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

module type TypeHistoryDSE = functor (TraceDSE_v:TypeTraceDSE) ->
sig
  type history_t

  val init_history : unit -> history_t

  val contains : history_t -> TraceDSE_v.trace_t -> bool

  val update_history : TraceDSE_v.trace_t -> history_t -> history_t

  val sat_child : history_t -> TraceDSE_v.child_t -> unit
  
  val unsat_child : history_t -> TraceDSE_v.child_t -> unit

  val print_status : history_t -> unit

end
