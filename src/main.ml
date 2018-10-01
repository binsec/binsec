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


let exclude_suffixes = [".smt"; ".smt2"]
let set_machdep_on_need () =
  match Kernel_options.ExecFile.get_opt () with
  | None -> ()
  | Some filename ->
     if not (File_utils.has_suffix ~suffixes:exclude_suffixes filename)
     then
       let module KM = Kernel_options.Machine in
       match KM.ISA.get () with
       | Machine.Unknown ->
          Kernel_functions.Loader.set_arch_from_file ~filename
       | _ -> ()


let set_command_line extra =
  let rec loop = function
    | [] ->
       if not @@ Kernel_options.ExecFile.is_set () then
         Kernel_options.Logger.warning "No file set"
    | arg :: args ->
       if Sys.file_exists arg then
         if Kernel_options.ExecFile.is_set () then
           let filename = Kernel_options.ExecFile.get () in
           Kernel_options.Logger.fatal
             "File %s was previously set. Cannot set to %s"
             filename arg;
           exit 1
         else Kernel_options.ExecFile.set arg
       else (
         (* Logger.info "Found arg %s ... ignoring for now" arg; *)
         (* maybe this is is an switch like in previous versions of BINSEC
            This is here for compatibility reasons. But it might be a good idea
            to keep it that way anyway.
          *)
         Cli.Boot.maybe_enable arg;
       );
       loop args
  in loop extra


let main () =
  (* General initialization of the random number generator *)
  Random.self_init ();
  Kernel_core.read_configuration_file ();
  Cli.parse () |> set_command_line |> set_machdep_on_need ;
  Cli.Boot.run ()
;;

main ()
