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

let simplify_dba inst_map =
  if !Disasm_options.simpl
  then
    begin
      let simplify () =
        Simplification_dba_prog.remove_mustkill_lfp inst_map |>
        Simplification_dba_block.block_simplifications |>
        Simplification_dba_prog.remove_goto
      in
      Logger.debug "Starting DBA simplification ...";
      let initsize, _initgoto, itemps, iflags =
        Simplification_dba_utils.statistics inst_map in
      Options.initsize := !Options.initsize + initsize;
      Options.itemps := !Options.itemps + itemps;
      Options.iflags := !Options.iflags + iflags;
      let t, res = Utils.time simplify in
      if !Options.display_statistics &&
         not (Dba_types.Caddress.Map.is_empty res) then
        Logger.info "%a"
          (Simplification_dba_utils.display_results res) t;
      res
    end
  else
    inst_map
