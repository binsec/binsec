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

(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016                                                    *)
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

module CriteriaEIPRewrite = functor (TraceDSE_v:TypeTraceDSE) -> functor (HistoryDSE_v:TypeHistoryDSE) ->
struct
  type conf_criteria = string

  let init_criteria _c = ()
  
  let verdict trace trace_config =
    let config = Conf_exploration.build_analysis_configuration trace trace_config in 
    let analyzer = new EipRewrite.eip_rewrite config in
    analyzer#init_entries ();
    ignore(analyzer#compute);
    match analyzer#is_eip_rewrite with
    | true ->
        Logger.debug "Buffer overflow found !@ Check : ";
        List.iter (fun x -> Logger.debug "%s " x) (analyzer#get_new_conf_files ());
        true
    | false -> false

  (* we explore all execution paths, so stop criteria is always false *)
  let stop_criteria _ _ = false
end
