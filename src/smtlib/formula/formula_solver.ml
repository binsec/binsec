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

open Formula_pp
module Logger = Formula_logger

let pp_bv_term_list =
  Format.pp_print_list ~pp_sep:Format.pp_print_space pp_bv_term

type 'a command =
  | PutEntry : Formula.entry -> unit command
  | CheckSat : float -> Formula.status command
  | GetModel : Formula_model.t command
  | GetBvValue : Formula.bv_term list -> Bitvector.t list command
  | GetAxValue : Formula.ax_term -> (Bitvector.t * Bitvector.t) array command

module Command = struct
  type 'a t = 'a command

  let pp : type a. Format.formatter -> a t -> unit =
   fun ppf command ->
    match command with
    | PutEntry en -> Format.fprintf ppf "@[%a@]" pp_entry en
    | CheckSat _ -> Format.pp_print_string ppf "(check-sat)"
    | GetModel -> Format.pp_print_string ppf "(get-model)"
    | GetBvValue values ->
        Format.fprintf ppf "@[<hov 2>(get-value@ (%a))@]" pp_bv_term_list values
    | GetAxValue value ->
        Format.fprintf ppf "@[<hov 2>(get-value@ (%a))@]" pp_ax_term value

  let put_entry en = PutEntry en
  let check_sat timeout = CheckSat timeout
  let get_model = GetModel
  let get_bv_value vs = GetBvValue vs
  let get_ax_value v = GetAxValue v
end

module Make (S : Session.S) = struct
  open Formula

  type t = S.t

  let create ~theory arg =
    let session = S.open_session arg in
    S.put session (Formula_pp.pp_header ~theory) ();
    session

  let pp ppf session = Format.fprintf ppf "Session \"%a\"" S.pp session
  let destroy session = S.close_session session

  let write session command =
    S.put session
      (fun ppf command -> Format.fprintf ppf "@[<h>%a@]@." Command.pp command)
      command

  let run : type a. t -> a command -> a =
   fun session command ->
    match command with
    | PutEntry _ -> write session command
    | CheckSat timeout -> (
        match S.check_sat session ~timeout with
        | Sat -> SAT
        | Unsat -> UNSAT
        | Unknown -> UNKNOWN)
    | GetModel -> Formula_model.extract (S.get_model session)
    | GetBvValue values ->
        List.map
          (fun (_, (term : Smtlib.term)) ->
            match term with
            | { term_desc = TermSpecConstant cst; _ } ->
                Formula_model.extract_value cst
            | _ -> Logger.fatal "unexpected get-value output")
          (S.get_value session pp_bv_term_list values)
    | GetAxValue value ->
        let model = Formula_model.create () in
        Formula_model.add_memory_term model
          (S.get_value session pp_ax_term value);
        Formula_model.memory_bindings model

  let put_entry session en = run session (Command.put_entry en)
  let check_sat ~timeout session = run session (Command.check_sat timeout)
  let get_model session = run session Command.get_model
  let get_bv_value session values = run session (Command.get_bv_value values)
  let get_ax_value session value = run session (Command.get_ax_value value)
end
