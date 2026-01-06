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

module Logger = Logger

module Lang = struct
  include Smtlib
  module Location = Location
  module Lexer = Smtlib_lexer
  module Parser = Smtlib_parser
  module Printer = Smtlib_pp
  module Utils = Smtlib_utils
end

module Formula = struct
  include Formula
  include Formula_pp
  module To_smtlib = Formula_to_smtlib
  module Transformation = Formula_transformation
  module Model = Formula_model
  module Solver = Formula_solver
  module Utils = Formula_utils
  module Logger = Formula_logger
end

module Bindings = Binsec_smtlib_bindings

module Solver = struct
  type status = Session.status = Sat | Unsat | Unknown

  include Solver
  module Session = Session

  type backend =
    | None : backend
    | Text : {
        session : (module Session.S with type arg = 'a);
        arg : 'a;
      }
        -> backend
    | Binding : {
        factory : (module Bindings.OPEN);
        complete_fold_ax_values : bool;
            (** [complete_fold_ax_values] is [true] if there is an explicit value for each accessed address, [false] when there can exist a missed default value.  *)
      }
        -> backend

  let default_backend () =
    match Bindings.bitwuzla_cxx with
    | Some factory -> Binding { factory; complete_fold_ax_values = false }
    | None -> (
        match Bindings.bitwuzla_c with
        | Some factory -> Binding { factory; complete_fold_ax_values = true }
        | None -> (
            match Bindings.z3 with
            | Some factory ->
                Binding { factory; complete_fold_ax_values = false }
            | None -> (
                match
                  List.find Solver.ping [ Bitwuzla; Boolector; Yices; Z3; CVC4 ]
                with
                | solver ->
                    Text { session = (module Session.Spawn); arg = solver }
                | exception Not_found -> None)))
end
