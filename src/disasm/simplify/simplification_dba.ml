(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

let simplify_dba inst_map =
  match Simplification_options.Simplification.get () with
  | Simplification_options.No_simplification -> inst_map
  | _ ->
    begin
      let simplify () =
        Simplification_dba_prog.remove_mustkill_lfp inst_map |>
        Simplification_dba_block.block_simplifications |>
        Simplification_dba_prog.remove_goto
      in
      Simplification_options.Logger.debug "Starting DBA simplification ...";
      let stats = Simplification_dba_utils.statistics inst_map in
      let t, res = Utils.time simplify in
      if Simplification_options.Display_statistics.get () &&
         not (Dba_types.Caddress.Map.is_empty res) then
        Simplification_options.Logger.info "%a"
          (Simplification_dba_utils.display_results stats res) t;
      res
    end
