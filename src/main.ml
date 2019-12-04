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

(* Ugly hack to help ocamlbuild's dependency computation ... *)
open! Ai
open! Bw_main
open! Sse
open! Dse
open! Drun
open! Disasm
open! Server
open! Simulate
open! Formula_main
open! Test
open! Kernel_core
open! Binpatcher
open! Xtrasec
open! Smtlib_utils
open! Concrete
open! Ida
open! Dwarf


let exclude_suffixes = [".smt"; ".smt2"]
let set_machdep_on_need () =
  match Kernel_options.ExecFile.get_opt () with
  | None -> ()
  | Some filename ->
     if not (File_utils.has_suffix ~suffixes:exclude_suffixes filename)
     then
       match Kernel_options.Machine.get () with
       | Machine.Unknown ->
          Kernel_functions.Loader.set_arch_from_file ~filename
       | _ -> ()


let set_command_line extra =
  let rec loop = function
    | [] ->
       if not @@ Kernel_options.ExecFile.is_set () then
         if not @@ Kernel_options.Version.get () then
         Kernel_options.Logger.debug "No file set"
    | arg :: args ->
       (* There are 2 valid reasons for being in the extra arg list: either it
          is a (binary) file that needs to be handled by BINSEC, or it is a
          old-style command line switch. The conditional below takes care of
          that.
        *)
       if Sys.file_exists arg then
         if Kernel_options.ExecFile.is_set () then
           let filename = Kernel_options.ExecFile.get () in
           Kernel_options.Logger.fatal
             "File %s was previously set. Cannot set to %s"
             filename arg
         else Kernel_options.ExecFile.set arg
       else Cli.Boot.maybe_enable arg;
       loop args
  in loop extra


let main () =
  (* General initialization of the random number generator *)
  Random.self_init ();
  Kernel_core.read_configuration_file ();
  Cli.parse () |> set_command_line |> set_machdep_on_need ;
  Cli.Boot.run ()
;; (* these ;; are necessary if we want to avoid an uglier let () = main ()
      below *)

main ()
