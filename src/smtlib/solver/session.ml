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

type status = Binsec_smtlib_bindings.status = Sat | Unsat | Unknown

let status_str : status -> string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"

let pp_status : Format.formatter -> status -> unit =
 fun ppf status -> Format.pp_print_string ppf (status_str status)

module type S = sig
  type t
  type arg

  val open_session : arg -> t
  val put : t -> (Format.formatter -> 'a -> unit) -> 'a -> unit
  val comment : t -> (Format.formatter -> 'a -> unit) -> 'a -> unit
  val check_sat : t -> timeout:float -> status

  val check_sat_assuming :
    t -> timeout:float -> (Format.formatter -> 'a -> unit) -> 'a -> status

  val get_value :
    t ->
    (Format.formatter -> 'a -> unit) ->
    'a ->
    (Smtlib.term * Smtlib.term) list

  val get_model : t -> Smtlib.model
  val close_session : t -> unit
  val pp : Format.formatter -> t -> unit
end

let dummy_formatter () = Format.make_formatter (fun _ _ _ -> ()) Fun.id

let clear_formatter formatter =
  Format.pp_set_formatter_output_functions formatter (fun _ _ _ -> ()) Fun.id

let pp_comment :
    Format.formatter -> (Format.formatter -> 'a -> unit) -> 'a -> unit =
 fun ppf pp a ->
  Format.pp_open_vbox ppf 0;
  List.iter
    (fun line ->
      if String.length line > 0 then (
        Format.pp_print_string ppf "; ";
        Format.pp_print_string ppf line;
        Format.pp_print_space ppf ()))
    (String.split_on_char '\n' (Format.asprintf "%a" pp a));
  Format.pp_close_box ppf ();
  Format.pp_print_flush ppf ()

module Spawn : S with type arg = Solver.t = struct
  type t = {
    mutable state : bool;
    pid : Subprocess.t;
    stdout : in_channel;
    formatter : Format.formatter;
    cmd : Solver.Command.t;
  }

  type arg = Solver.t

  let close t =
    if t.state then (
      t.state <- false;
      ignore @@ Subprocess.close t.pid)

  let open_session solver =
    let cmd = Solver.command ~incremental:true 0 solver in
    Logger.debug "Openning session: %a" Solver.Command.pp cmd;
    let pid = Subprocess.spawn ~pdeathsig:Sys.sigkill cmd in
    let stdin = Subprocess.stdin pid and stdout = Subprocess.stdout pid in
    let session =
      { state = true; pid; stdout; formatter = dummy_formatter (); cmd }
    in
    Format.pp_set_formatter_output_functions session.formatter
      (fun str pos len ->
        try output_substring stdin str pos len
        with Sys_error _ ->
          clear_formatter session.formatter;
          close session)
      (fun () ->
        try flush stdin
        with Sys_error _ ->
          clear_formatter session.formatter;
          close session);
    Format.fprintf session.formatter
      "@[<v 0>(set-option :print-success false)@ @]@[<hov>";
    session

  let put { formatter; _ } ppf a = ppf formatter a
  let comment { formatter; _ } ppf a = pp_comment formatter ppf a
  let close_session t = close t

  let rec parse_check_with_deadline t fd ~deadline : status =
    if Float.is_infinite deadline then
      parse_check_line t fd ~deadline Float.minus_one
    else
      let timeout = deadline -. Unix.gettimeofday () in
      if Float.sign_bit timeout then (
        close t;
        Unknown)
      else parse_check_line t fd ~deadline timeout

  and parse_check_line t fd ~deadline timeout : status =
    match Unix.select [ fd ] [] [] timeout with
    | [ _ ], _, _ ->
        Unix.set_nonblock fd;
        parse_check_line_nonblock t fd ~deadline
    | _ ->
        close t;
        Unknown

  and parse_check_line_nonblock t fd ~deadline : status =
    match input_line t.stdout with
    | "sat" ->
        Unix.clear_nonblock fd;
        Sat
    | "unsat" ->
        Unix.clear_nonblock fd;
        Unsat
    | "unknown" ->
        Unix.clear_nonblock fd;
        Unknown
    | "" -> parse_check_line_nonblock t fd ~deadline
    | s ->
        Logger.error "Solver returned: %s" s;
        close t;
        Unknown
    | exception Sys_blocked_io ->
        Unix.clear_nonblock fd;
        parse_check_with_deadline t fd ~deadline
    | (exception End_of_file) | (exception Sys_error _) ->
        close t;
        Unknown

  let parse_check t ~timeout =
    parse_check_with_deadline t
      (Unix.descr_of_in_channel t.stdout)
      ~deadline:
        (if Float.is_infinite timeout then timeout
         else Unix.gettimeofday () +. timeout)

  let check_sat t ~timeout : status =
    if t.state then (
      Format.fprintf t.formatter "(check-sat)@.@[<hov>";
      parse_check t ~timeout)
    else Unknown

  let check_sat_assuming t ~timeout pp e : status =
    if t.state then (
      Format.fprintf t.formatter "(check-sat-assuming (%a))@.@[<hov>" pp e;
      parse_check t ~timeout)
    else Unknown

  let get_value t pp x =
    Format.fprintf t.formatter "(get-value (%a))@.@[<hov>" pp x;
    let lexbuf = Lexing.from_channel t.stdout in
    Smtlib_parser.ivalue Smtlib_lexer.token lexbuf

  let get_model t =
    Format.pp_print_string t.formatter "(get-model)";
    Format.pp_print_flush t.formatter ();
    Format.pp_open_hovbox t.formatter 0;
    let lexbuf = Lexing.from_channel t.stdout in
    Smtlib_parser.model Smtlib_lexer.token lexbuf

  let pp ppf t = Solver.Command.pp ppf t.cmd
end

module Dump : S with type arg = string = struct
  type t = {
    filename : string;
    channel : out_channel;
    formatter : Format.formatter;
  }

  type arg = string

  let open_session filename =
    let channel = open_out filename in
    let formatter = dummy_formatter () in
    Format.pp_set_formatter_output_functions formatter
      (fun str pos len ->
        try output_substring channel str pos len
        with Sys_error _ ->
          clear_formatter formatter;
          close_out_noerr channel)
      (fun () ->
        try flush channel
        with Sys_error _ ->
          clear_formatter formatter;
          close_out_noerr channel);
    { filename; channel; formatter }

  let put { formatter; _ } pp a = pp formatter a
  let comment { formatter; _ } pp a = pp_comment formatter pp a

  let check_sat { formatter; _ } ~timeout:_ =
    Format.pp_print_string formatter "(check-sat)";
    Format.pp_print_space formatter ();
    Format.pp_print_flush formatter ();
    Unknown

  let check_sat_assuming { formatter; _ } ~timeout:_ pp e =
    Format.fprintf formatter "(check-sat-assuming (%a))@." pp e;
    Unknown

  let get_value { formatter; _ } pp x =
    Format.fprintf formatter "(get-value (%a))@." pp x;
    []

  let get_model { formatter; _ } : Smtlib.model =
    Format.pp_print_string formatter "(get-model)";
    Format.pp_print_flush formatter ();
    { model_commands = []; model_loc = Location.none }

  let close_session session =
    clear_formatter session.formatter;
    close_out_noerr session.channel

  let pp ppf { filename; _ } = Format.pp_print_string ppf filename
end

module Carbon_copy (Main : S) : S with type arg = Main.arg * string = struct
  type t = { main : Main.t; copy : Dump.t }
  type arg = Main.arg * string

  let echo_status : Dump.t -> status -> status =
   fun copy status ->
    Dump.comment copy pp_status status;
    status

  let open_session (arg, filename) =
    let main = Main.open_session arg and copy = Dump.open_session filename in
    { main; copy }

  let put { main; copy } pp a =
    let r = Format.asprintf "%a" pp a in
    Dump.put copy Format.pp_print_string r;
    Main.put main Format.pp_print_string r

  let comment { main; copy } pp a =
    let r = Format.asprintf "%a" pp a in
    Dump.comment copy Format.pp_print_string r;
    Main.comment main Format.pp_print_string r

  let check_sat { main; copy } ~timeout =
    ignore (Dump.check_sat copy ~timeout);
    echo_status copy (Main.check_sat main ~timeout)

  let check_sat_assuming { main; copy } ~timeout pp e =
    let r = Format.asprintf "%a" pp e in
    ignore (Dump.check_sat_assuming copy ~timeout Format.pp_print_string r);
    echo_status copy
      (Main.check_sat_assuming main ~timeout Format.pp_print_string r)

  let get_value { main; copy } pp e =
    let r = Format.asprintf "%a" pp e in
    ignore (Dump.get_value copy Format.pp_print_string r);
    let l = Main.get_value main Format.pp_print_string r in
    Dump.comment copy
      (fun ppf list ->
        Format.fprintf ppf "@[<v 2>(%a@])@ "
          (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (k, v) ->
               Format.fprintf ppf "(%a %a)" Smtlib_pp.pp_term k
                 Smtlib_pp.pp_term v))
          list)
      l;
    l

  let get_model { main; copy } =
    ignore (Dump.get_model copy);
    let m = Main.get_model main in
    Dump.comment copy Smtlib_pp.pp_model m;
    m

  let close_session { main; copy } =
    Dump.close_session copy;
    Main.close_session main

  let pp ppf { main; copy } =
    Format.fprintf ppf "%a (copy in %a)" Main.pp main Dump.pp copy
end
