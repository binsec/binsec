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

module CriteriaEIPRewrite :
  functor (TraceDSE_v:TypeTraceDSE.TypeTraceDSE) ->
  functor (HistoryDSE_v:TypeHistoryDSE.TypeHistoryDSE) ->
  sig
    type conf_criteria = string
    val init_criteria : conf_criteria -> unit
    val verdict       : conf_criteria -> Conf_exploration.conf_t -> bool
    val stop_criteria : HistoryDSE_v(TraceDSE_v).history_t -> TraceDSE_v.child_t list -> bool
end
