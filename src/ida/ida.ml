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

open Ida_options

module IO = Ida_options
module ICG = Ida_cg
module IC = Ida_cfg.C
module IG = Ida_cfg.G

let callgraph_dot_file = "callgraph.dot"

let parse_cg () =
  let time, cg =
    Utils.time
      (fun () ->
        let cg_file = Filename.concat (Sys.getcwd ()) callgraph_dot_file in
        ICG.Parse.build_cg ~cg_file) in
  Logger.result "Parsing CG #nodes: %d, #edges: %d, time: %.2f (s)"
    (ICG.nb_vertex cg) (ICG.nb_edges cg) time;
  cg
;;


(* Produces dot files *)
let parse_cfg ~simple ~ida_file =
  let time, (g, cfg) =
    Utils.time (fun () ->
        let g = Ida_cfg.do_cfg ~simple ~ida_file in
        g, IG.graph g) in
  Logger.result "Parsing CFG #nodes: %d, #edges: %d, time: %.2f (s)"
    (IC.nb_vertex cfg) (IC.nb_edges cfg) time;
  g
;;

let run () =
  if IO.is_enabled () then
    let simple = IO.IdaSimpleCfg.get () in
    let ida_file = IO.IdaOutputFile.get () in
    ignore @@ parse_cg ();
    ignore @@ parse_cfg ~simple ~ida_file;
;;

let _ =
  Cli.Boot.enlist ~name:"IDA + disassembly" ~f:run;
;;
