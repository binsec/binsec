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

include Cli.Options (struct
  let name = "smtlib"
  let shortname = name
end)

module Model_from_file = Builder.String_option (struct
  let name = "model-from-file"
  let doc = "Parse model from given file."
end)

let test_model_parsing () =
  if Model_from_file.is_set () then (
    let filename = Model_from_file.get () in
    Logger.debug "Parsing SMT model from %s" filename;
    let parser = Smtlib.Lang.Parser.model in
    let lexer = Smtlib.Lang.Lexer.token in
    let premodel = Parse_utils.read_file ~parser ~lexer ~filename in
    Logger.debug "@[Parsed model@\n%a@]" Smtlib.Lang.Printer.pp_model premodel;
    let model = Smtlib.Formula.Model.extract premodel in
    let section_name =
      match Kernel_functions.get_img () with
      | img -> (
          fun address ->
            match Loader_utils.find_section_by_address ~address img with
            | None -> None
            | Some section -> Some (Loader.Section.name section))
      | exception Failure _ -> fun _ -> None
    in
    Logger.result "%a"
      (Smtlib.Formula.Model.pp_with_sections section_name)
      model)

let _ = Cli.Boot.enlist ~name:"SMT model parser test" ~f:test_model_parsing
