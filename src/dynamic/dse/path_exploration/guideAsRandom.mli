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

module GuideAsRandom :
  functor (TraceDSE_v:TypeTraceDSE.TypeTraceDSE) ->
  functor (HistoryDSE_v:TypeHistoryDSE.TypeHistoryDSE) ->
    TypeGuideDSE.S
      with type trace_t = TraceDSE_v.trace_t
       and type child_t = TraceDSE_v.child_t
       and type history_t = HistoryDSE_v(TraceDSE_v).history_t
       and type score_input_t = string (*list_event, alloc,free,use *)



