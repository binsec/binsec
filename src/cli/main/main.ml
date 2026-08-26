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

let exclude_suffixes = [ ".smt"; ".smt2" ]

let set_machdep_on_need () =
  match Kernel_options.ExecFile.get_opt () with
  | None -> ()
  | Some filename -> (
      if
        not
          (Binsec_base.File_utils.has_suffix ~suffixes:exclude_suffixes filename)
      then
        match Kernel_options.Machine.get () with
        | Machine.Unknown ->
            Kernel_functions.Loader.set_arch_from_file ~filename
        | _ -> ())

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
              "@[\"%s\" is set to be analized. @ \"%s\" is an unexpected \
               argument.@]"
              filename arg
          else Kernel_options.ExecFile.set arg
        else Cli.Boot.maybe_enable arg;
        loop args
  in
  loop extra

let dynlink_error_tips : Dynlink.error -> string option = function
  | Inconsistent_implementation _ | Inconsistent_import _ ->
      Some "try running $(dune clean && dune build && dune install)"
  | Module_already_loaded _ | Library's_module_initializers_failed _ ->
      Some "please contact the developers"
  | Not_a_bytecode_file _ | Unavailable_unit _ | Unsafe_file | Linking_error _
  | Corrupted_interface _ | Cannot_open_dynamic_library _
  | Private_library_cannot_implement_interface _ ->
      None

(** Load a color profile, priority to the command line. *)
let load_color_profile () =
  match Sys.getenv_opt "BINSEC_COLOR_PROFILE" with
  | None -> ()
  | Some string -> Kernel_options.Color_profile.set string

(** Set colors in terminal *)
let switch_color_mode () =
  Kernel_options.Logger.set_color
    (Cli.is_ansi_code_supported && Kernel_options.Color.get ())

let main () =
  (* General initialization of the random number generator *)
  Random.self_init ();
  Kernel_core.read_configuration_file ();

  (***************************)
  try
    (* First activation to color Cli.parse operations *)
    switch_color_mode ();
    (* From the env, update the initial state *)
    load_color_profile ();
    Cli.parse () |> set_command_line |> set_machdep_on_need;
    (* Second activation to color or not, depend if options being passed, Plugins operations *)
    switch_color_mode ();
    (* no need to handle it as a signal, the calling function will still raise a Sys_error "EPIPE" *)
    Sys.(set_signal sigpipe Signal_ignore);
    Cli.Boot.run ()
  with Dynlink.Error err ->
    Kernel_options.Logger.error
      "@[<v>Dynlink error: \"%s\"@ This may indicate that a package is out of \
       sync with your latest@ compiled version of Binsec.@]"
      (Dynlink.error_message err);
    Option.iter
      (Kernel_options.Logger.info "@[<v>Tips: %s@]")
      (dynlink_error_tips err)
;;

(* these ;; are necessary if we want to avoid an uglier let () = main ()
   below *)

main ()
