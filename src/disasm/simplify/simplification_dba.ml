(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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
  match Disasm_options.Simplification.get () with
  | Disasm_options.No_simplification -> inst_map
  | _ ->
    begin
      let simplify () =
        Simplification_dba_prog.remove_mustkill_lfp inst_map |>
        Simplification_dba_block.block_simplifications |>
        Simplification_dba_prog.remove_goto
      in
      Logger.debug "Starting DBA simplification ...";
      let initsize, _initgoto, itemps, iflags =
        Simplification_dba_utils.statistics inst_map in
      Ai_options.initsize := !Ai_options.initsize + initsize;
      Ai_options.itemps := !Ai_options.itemps + itemps;
      Ai_options.iflags := !Ai_options.iflags + iflags;
      let t, res = Utils.time simplify in
      if Simplification_options.Display_statistics.get () &&
         not (Dba_types.Caddress.Map.is_empty res) then
        Logger.info "%a"
          (Simplification_dba_utils.display_results res) t;
      res
    end
