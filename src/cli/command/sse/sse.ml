(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

open Options

let register_plugin = Plugins.register

let () =
  register_plugin ~is_enabled:Options.KillFlagsAtReturn.get (fun () ->
      (module Opt.Flags));
  register_plugin ~is_enabled:QMerge.get (fun () ->
      (module Binsec_sse_quick_merge.Plugin))

let get_worklist : unit -> (module Worklist.S) =
 fun () ->
  match Search_heuristics.get () with
  | Dfs -> (module Worklist.Dfs)
  | Bfs -> (module Worklist.Bfs)
  | Nurs ->
      let seed =
        match Seed.get_opt () with
        | Some s -> s
        | None ->
            let v = Random.bits () in
            Logger.info "Random search seed is %d" v;
            Seed.set v;
            v
      in
      Random.init seed;
      (module Worklist.Nurs)

let run () =
  if is_enabled () && Kernel_options.ExecFile.is_set () then
    let module R =
      Exec.Run
        (struct
          let filename = Kernel_options.ExecFile.get ()
          let isa = Some (Kernel_options.Machine.get ())
          let img = Kernel_functions.get_img ()

          let fs =
            let map =
              Option.fold ~none:Fun.id
                ~some:(fun root path -> Filename.concat root path)
                (Options.Sysroot.get_opt ())
            in
            fun path ->
              match Unix.openfile (map path) [ Unix.O_RDONLY ] 0 with
              | exception Unix.Unix_error _ -> raise Not_found
              | file_descr ->
                  let buffer =
                    Bigarray.array1_of_genarray
                      (Unix.map_file file_descr Int8_unsigned C_layout false
                         [| -1 |])
                  in
                  Unix.close file_descr;
                  buffer

          let assembler : (module Compiler.ASSEMBLER) =
            if Cse.get () then (module Compiler.Cse)
            else (module Compiler.Default)

          let trace : Compiler.trace =
            let debug_level = Options.Logger.get_debug_level () in
            if debug_level >= 40 then Ir
            else if debug_level >= 2 then Assembly
            else No

          let transient_enum = Options.TransientEnum.get ()
          let max_depth = Options.MaxDepth.get ()
          let enumeration_limit = Options.JumpEnumDepth.get ()
          let smt_backend = Smt_options.backend (Smt_options.Solver.get ())

          let smt_timeout =
            match Smt_options.Timeout.get_opt () with
            | Some 0. -> None
            | opt -> opt

          let smt_multichecks = Smt_options.Multichecks.get ()
          let smt_dumpdir = Smt_options.DumpDir.get_opt ()
          let missing_symbols = Options.MissingSymbol.get ()
          let timeout = Options.Timeout.get_opt ()
          let entry = Kernel_functions.get_ep ()

          let script =
            match ScriptFiles.get () with
            | [] -> Fun.const []
            | files -> Fun.flip Script.read_files files

          let plugins = Plugins.list ()
        end)
        ((val Engine.get_factory ()))
        ((val get_worklist ()))
        ()
    in
    R.unit

let _ = Cli.Boot.enlist ~name:"SSE" ~f:run
