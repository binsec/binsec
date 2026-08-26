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

open Kernel_options

(* we can't use the option framework yet, as options have not been parsed yet. *)
let config_file = ref None

let _ =
  let len = Array.length Sys.argv in
  let rec loop i =
    if i < len then
      if Sys.argv.(i) = "-config" then (
        let config_filename = Sys.argv.(i + 1) in
        (* for other parts of the code to read after option parsing *)
        Kernel_options.Config_file.set config_filename;
        (* for us *)
        config_file := Some config_filename)
      else loop (i + 1)
  in
  loop 0

let read_configuration_file () =
  match !config_file with
  | None -> () (* Maybe_TODO : Use default file ? *)
  | Some filename -> ignore @@ Cli.parse_configuration_file ~filename

let binary_descr () =
  if Describe_binary.get () && ExecFile.is_set () then
    Logger.result "@]@\n@[<v>%a" Kernel_functions.Loader.pp_loader_summary
      (ExecFile.get ())

let version () =
  if Version.get () then (
    Logger.set_log_level "result";
    Format.printf "Binsec version %%VERSION%%\n";
    exit 0)

let style : Binsec_base.Logger.font -> Format.formatter -> string -> unit =
 fun font ppf str ->
  Format.pp_open_stag ppf (Binsec_base.Logger.Font font);
  Format.pp_print_string ppf str;
  Format.pp_close_stag ppf ()

let print_color_documentation () =
  if Color_help.get () then (
    Logger.set_log_level "result";
    if Cli.is_ansi_code_supported then (
      Logger.set_color true;
      Format.printf
        "@[<v>The following color profiles are available:%a@ @ Use the %a \
         environment variable to automatically select a color profile.@ Tip: \
         add this variable to your %a to make your choice permanent.@."
        (fun ppf profiles ->
          let max_name_size =
            List.fold_left
              (fun size (profile : Binsec_base.Logger.Profile.t) ->
                max size (String.length profile.name))
              0 profiles
          in
          List.iter
            (fun (profile : Binsec_base.Logger.Profile.t) ->
              Format.fprintf ppf "@ %-*s: %a %a %a %a %a %a %a" max_name_size
                profile.name (style profile.debug) "debug" (style profile.step)
                "step" (style profile.info) "info" (style profile.result)
                "result" (style profile.warning) "warning" (style profile.error)
                "error" (style profile.fatal) "fatal")
            profiles)
        Binsec_base.Logger.Profile.all_profiles
        (style (Default, Bold))
        "BINSEC_COLOR_PROFILE"
        (style (Default, Bold))
        ".bashrc";
      exit 0)
    else (
      Format.printf
        "@[<v>Your terminal does not support ANSI colors or color output is \
         disabled.@ Color options will be ignored.@]@.";
      exit 0))

let _ =
  Cli.Boot.enlist ~name:"binary description" ~f:binary_descr;
  Cli.Boot.enlist ~name:"version description" ~f:version;
  Cli.Boot.enlist ~name:"color description" ~f:print_color_documentation
