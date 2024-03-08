(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
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
open Types

module Make (E : EXPLORATION_STATISTICS) (Q : QUERY_STATISTICS) = struct
  type mode = Logging | Drawing

  module Buffer = struct
    type t = {
      content : Bytes.t;
      mutable insert_pos : int;
      mutable flush_pos : int;
    }

    let create size =
      { content = Bytes.create size; insert_pos = 0; flush_pos = 0 }

    let flush out_string out_flush t =
      if t.insert_pos > 0 then (
        if t.flush_pos = 0 then
          out_string (Bytes.unsafe_to_string t.content) 0 t.insert_pos
        else (
          out_string (Bytes.unsafe_to_string t.content) 0 t.flush_pos;
          out_flush ();
          let n = t.insert_pos - t.flush_pos in
          if n > 0 then
            out_string (Bytes.unsafe_to_string t.content) t.flush_pos n;
          t.flush_pos <- 0);
        t.insert_pos <- 0)

    let out_string mutex out_string out_flush t s p n =
      Mutex.lock mutex;
      let insert_pos = t.insert_pos + n and capacity = Bytes.length t.content in
      if insert_pos > capacity then (
        Curses.def_prog_mode ();
        Curses.endwin ();
        flush out_string out_flush t;
        if n > capacity then out_string s p n
        else (
          Bytes.blit_string s p t.content 0 n;
          t.insert_pos <- n);
        Curses.reset_prog_mode ();
        ignore @@ Curses.refresh ())
      else (
        Bytes.blit_string s p t.content t.insert_pos n;
        t.insert_pos <- insert_pos);
      Mutex.unlock mutex

    let out_flush mutex t () =
      Mutex.lock mutex;
      t.flush_pos <- t.insert_pos;
      Mutex.unlock mutex

    let out_spaces mutex out_string out_flush t n =
      Mutex.lock mutex;
      let insert_pos = t.insert_pos + n and capacity = Bytes.length t.content in
      if insert_pos > capacity then (
        Curses.def_prog_mode ();
        Curses.endwin ();
        flush out_string out_flush t;
        if n > capacity then out_string (String.make n ' ') 0 n
        else (
          Bytes.unsafe_fill t.content 0 n ' ';
          t.insert_pos <- n);
        Curses.reset_prog_mode ();
        ignore @@ Curses.refresh ())
      else (
        Bytes.unsafe_fill t.content t.insert_pos n ' ';
        t.insert_pos <- insert_pos);
      Mutex.unlock mutex
  end

  let template =
    [|
      "+- Exploration ----------------------++- Reasoning ----------------+";
      "|+- Paths --------------------------+||+- Assertions -------------+|";
      "||              total               ||||      total               ||";
      "||            pending               ||||     failed               ||";
      "||      completed/cut               |||+--------------------------+|";
      "||       discontinued               |||+- Simplifications --------+|";
      "|+----------------------------------+|||      total               ||";
      "|+- Topology -----------------------+|||       true               ||";
      "||   branching points               ||||      false               ||";
      "||  max reached depth               ||||       enum               ||";
      "||                                  |||+--------------------------+|";
      "|+----------------------------------+||+- SMT queries ------------+|";
      "|+- ASM instructions ---------------+|||      total               ||";
      "||             unique               ||||        sat               ||";
      "||      total visited               ||||      unsat               ||";
      "||    emulation speed               ||||    timeout               ||";
      "|+----------------------------------+||+--------------------------+|";
      "+------------------------------------++----------------------------+";
      "+- Execution time -------------------------------------------------+";
      "|                                                                  |";
      "|               ratio                                              |";
      "|                                             total                |";
      "+------------------------------------------------------------------+";
    |]

  type opened_state = {
    lock : unit -> unit;
    unlock : unit -> unit;
    mutable interrupt : bool;
    mutable mode : mode;
    window : Curses.window;
    mutable drawn : bool;
    restore_stdout_fof : Format.formatter_out_functions;
    logging_stdout_fof : Format.formatter_out_functions;
    drawing_stdout_fof : Format.formatter_out_functions;
    restore_stderr_fof : Format.formatter_out_functions;
    logging_stderr_fof : Format.formatter_out_functions;
    drawing_stderr_fof : Format.formatter_out_functions;
    flush_buffers : unit -> unit;
    mutable instructions : int;
    mutable total_time : float;
  }

  type t = Closed | Opened of opened_state * Thread.t

  let state = ref Closed

  module Draw = struct
    let acs = Lazy.from_fun Curses.get_acs_codes

    let template y x template =
      Array.iteri
        (fun i line -> ignore @@ Curses.mvaddstr (y + i) x line)
        template

    let named_box name y x c l =
      let acs = Lazy.force acs in
      let y' = y + l - 1 and x' = x + c - 1 in
      ignore @@ Curses.mvaddch y x acs.ulcorner;
      ignore @@ Curses.mvaddch y x' acs.urcorner;
      ignore @@ Curses.mvaddch y' x acs.llcorner;
      ignore @@ Curses.mvaddch y' x' acs.lrcorner;
      ignore @@ Curses.move y (x + 1);
      ignore @@ Curses.addch acs.hline;
      ignore @@ Curses.addch (Char.code ' ');
      Curses.attr_on Curses.WA.bold;
      ignore @@ Curses.addstr name;
      Curses.attr_off Curses.WA.bold;
      ignore @@ Curses.addch (Char.code ' ');
      for _ = x + String.length name + 5 to x' do
        ignore @@ Curses.addch acs.hline
      done;
      ignore @@ Curses.move y' (x + 1);
      for _ = x + 2 to x' do
        ignore @@ Curses.addch acs.hline
      done;
      for i = y + 1 to y' - 1 do
        ignore @@ Curses.mvaddch i x acs.vline;
        ignore @@ Curses.mvaddch i x' acs.vline
      done

    let int y x i = ignore @@ Curses.mvaddstr y x (Printf.sprintf "%10d" i)

    let time y x t =
      ignore
      @@ Curses.mvaddstr y x
           (let h = int_of_float (t /. 3600.) in
            let m = int_of_float (t /. 60.) - (60 * h) in
            let s = int_of_float t - (3600 * h) - (60 * m) in
            if h > 0 then Printf.sprintf "%3dh%02dm%02ds" h m s
            else if m > 0 then Printf.sprintf "%6dm%02ds" m s
            else
              let ms = int_of_float (t *. 1000.) - (1000 * s) in
              if s > 0 then Printf.sprintf "%7d.%1ds" s (ms / 100)
              else Printf.sprintf "%8dms" ms)

    let ips y x f =
      ignore
      @@ Curses.mvaddstr y x
           (if Float.compare f 1e9 >= 1 then
              let g = int_of_float (f /. 1e9) in
              let m = int_of_float (f /. 1e8) - (10 * g) in
              Printf.sprintf "%4d.%1dGIPS" g m
            else if Float.compare f 1e6 >= 1 then
              let m = int_of_float (f /. 1e6) in
              let k = int_of_float (f /. 1e5) - (10 * m) in
              Printf.sprintf "%4d.%1dMIPS" m k
            else if Float.compare f 1e3 >= 1 then
              let k = int_of_float (f /. 1e3) in
              let z = int_of_float (f /. 1e2) - (10 * k) in
              Printf.sprintf "%4d.%1dkIPS" k z
            else Printf.sprintf "%7dIPS" (int_of_float f))

    let percent y x p = ignore @@ Curses.mvaddstr y x (Printf.sprintf "%9d%%" p)
  end

  let draw state =
    if not state.drawn then (
      Draw.template 0 0 template;
      Draw.named_box "Exploration" 0 0 38 18;
      Draw.named_box "Paths" 1 1 36 6;
      Draw.named_box "Topology" 7 1 36 5;
      Draw.named_box "ASM instructions" 12 1 36 5;
      Draw.named_box "Reasoning" 0 38 30 18;
      Draw.named_box "Assertions" 1 39 28 4;
      Draw.named_box "Simplifications" 5 39 28 6;
      Draw.named_box "SMT queries" 11 39 28 6;
      Draw.named_box "Execution time" 18 0 68 5;
      state.drawn <- true);
    let total_time = E.get_time () in

    let paths = E.get_paths ()
    and completed_paths = E.get_completed_paths ()
    and unknown_paths = E.get_unknown_paths ()
    and branches = E.get_branches ()
    and total_asserts = E.get_total_asserts ()
    and failed_asserts = E.get_failed_asserts ()
    and max_depth = E.get_max_depth ()
    and instructions = E.get_instructions ()
    and unique_insts = E.get_unique_insts ()
    and pre_true = Q.Preprocess.get_true ()
    and pre_false = Q.Preprocess.get_false ()
    and pre_enum = Q.Preprocess.get_const ()
    and sol_sat = Q.Solver.get_sat ()
    and sol_unsat = Q.Solver.get_unsat ()
    and sol_err = Q.Solver.get_err ()
    and sol_time = Q.Solver.get_time () in

    Draw.int 2 25 paths;
    Draw.int 3 25 (paths - completed_paths - unknown_paths);
    Draw.int 4 25 completed_paths;
    Draw.int 5 25 unknown_paths;

    Draw.int 8 25 branches;
    Draw.int 9 25 max_depth;

    Draw.int 13 25 unique_insts;
    Draw.int 14 25 instructions;
    Draw.ips 15 25
      (float_of_int (instructions - state.instructions)
      /. (total_time -. state.total_time));

    Draw.int 2 55 total_asserts;
    Draw.int 3 55 failed_asserts;

    Draw.int 6 55 (pre_true + pre_false + pre_enum);
    Draw.int 7 55 pre_true;
    Draw.int 8 55 pre_false;
    Draw.int 9 55 pre_enum;

    Draw.int 12 55 (sol_sat + sol_unsat + sol_err);
    Draw.int 13 55 sol_sat;
    Draw.int 14 55 sol_unsat;
    Draw.int 15 55 sol_err;

    Draw.time 19 25 (total_time -. sol_time);
    Draw.time 19 55 sol_time;

    let reason_ratio = int_of_float (100. *. sol_time /. total_time) in
    let exec_ratio = 100 - reason_ratio in

    Draw.percent 20 25 exec_ratio;
    Draw.percent 20 55 reason_ratio;

    Draw.time 21 55 total_time;

    state.instructions <- instructions;
    state.total_time <- total_time;

    ignore @@ Curses.refresh ()

  let update state =
    match state.mode with Logging -> () | Drawing -> draw state

  let switch_mode state =
    match state.mode with
    | Logging ->
        let y, x = Curses.getmaxyx state.window in
        if y < 23 || x < 68 then (
          state.drawn <- false;
          Logger.error "Monitoring mode requires a window of at least 68x23.")
        else (
          Format.(
            pp_set_formatter_out_functions std_formatter
              state.drawing_stdout_fof;
            pp_set_formatter_out_functions err_formatter
              state.drawing_stderr_fof);
          Curses.reset_prog_mode ();
          ignore @@ Curses.curs_set 0;
          draw state;
          state.mode <- Drawing)
    | Drawing ->
        Format.(
          pp_set_formatter_out_functions std_formatter state.logging_stdout_fof;
          pp_set_formatter_out_functions err_formatter state.logging_stderr_fof);
        Curses.def_prog_mode ();
        Curses.endwin ();
        state.flush_buffers ();
        ignore @@ Curses.cbreak ();
        ignore @@ Curses.curs_set 1;
        state.mode <- Logging

  let rec listen state =
    state.lock ();
    if state.interrupt then state.unlock () else key_handler state false false

  and key_handler state switch resize =
    let c = Curses.getch () in
    if c = -1 then (
      let switch =
        if resize then (
          match state.mode with
          | Logging ->
              Curses.reset_prog_mode ();
              Curses.endwin ();
              ignore @@ Curses.cbreak ();
              switch
          | Drawing ->
              switch_mode state;
              true)
        else switch
      in
      if switch then switch_mode state else update state;
      state.unlock ();
      Thread.delay 1.;
      listen state)
    else if c = Char.code ' ' then key_handler state true resize
    else if c = Curses.Key.resize then key_handler state switch true
    else key_handler state switch resize

  let init_formatters locked mutex formatter =
    let restore_fof = Format.pp_get_formatter_out_functions formatter () in
    let logging_fof =
      Format.
        {
          out_string =
            (fun s p n ->
              let lockable = not !locked in
              if lockable then (
                Mutex.lock mutex;
                locked := true);
              restore_fof.out_string s p n;
              if lockable then (
                locked := false;
                Mutex.unlock mutex));
          out_flush =
            (fun () ->
              let lockable = not !locked in
              if lockable then (
                Mutex.lock mutex;
                locked := true);
              restore_fof.out_flush ();
              if lockable then (
                locked := false;
                Mutex.unlock mutex));
          out_newline =
            (fun () ->
              let lockable = not !locked in
              if lockable then (
                Mutex.lock mutex;
                locked := true);
              restore_fof.out_newline ();
              restore_fof.out_string "\r" 0 1;
              if lockable then (
                locked := false;
                Mutex.unlock mutex));
          out_spaces =
            (fun n ->
              let lockable = not !locked in
              if lockable then (
                Mutex.lock mutex;
                locked := true);
              restore_fof.out_spaces n;
              if lockable then (
                locked := false;
                Mutex.unlock mutex));
          out_indent =
            (fun n ->
              let lockable = not !locked in
              if lockable then (
                Mutex.lock mutex;
                locked := true);
              restore_fof.out_indent n;
              if lockable then (
                locked := false;
                Mutex.unlock mutex));
        }
    in
    let background_buffer = Buffer.create 4096 in
    let flush_buffer () =
      Buffer.flush restore_fof.out_string restore_fof.out_flush
        background_buffer
    in
    let drawing_fof =
      Format.
        {
          out_string =
            Buffer.out_string mutex restore_fof.out_string restore_fof.out_flush
              background_buffer;
          out_flush = Buffer.out_flush mutex background_buffer;
          out_newline =
            (fun () ->
              Buffer.out_string mutex restore_fof.out_string
                restore_fof.out_flush background_buffer "\n" 0 1);
          out_spaces =
            Buffer.out_spaces mutex restore_fof.out_string restore_fof.out_flush
              background_buffer;
          out_indent =
            Buffer.out_spaces mutex restore_fof.out_string restore_fof.out_flush
              background_buffer;
        }
    in
    (restore_fof, logging_fof, drawing_fof, flush_buffer)

  let init () =
    let enabled = Monitor.get ()
    and debug = Logger.is_debug_enabled ()
    and tty = Unix.(isatty stdout) in
    if enabled && debug && tty then
      Logger.warning
        "Monitoring screen is disabled when debug mode is set to avoid blink \
         issues."
    else if enabled && tty then (
      Logger.info "TTY: press [space] to switch between log and monitor modes.";
      let mutex = Mutex.create () in
      Mutex.lock mutex;
      let locked = ref false in
      let lock () =
        Mutex.lock mutex;
        locked := true
      and unlock () =
        locked := false;
        Mutex.unlock mutex
      in
      let ( restore_stdout_fof,
            logging_stdout_fof,
            drawing_stdout_fof,
            flush_stdout_buffer ) =
        init_formatters locked mutex Format.std_formatter
      and ( restore_stderr_fof,
            logging_stderr_fof,
            drawing_stderr_fof,
            flush_stderr_buffer ) =
        init_formatters locked mutex Format.err_formatter
      in
      let flush_buffers () =
        flush_stdout_buffer ();
        flush_stderr_buffer ()
      in
      Format.(
        pp_set_formatter_out_functions std_formatter logging_stdout_fof;
        pp_set_formatter_out_functions err_formatter logging_stderr_fof);
      let window =
        let stdscr = Curses.stdscr () in
        if stdscr = Curses.null_window then Curses.initscr () else stdscr
      in
      ignore @@ Curses.noecho ();
      ignore @@ Curses.nodelay window true;
      ignore @@ Curses.addstr "Hello World!";
      ignore @@ Curses.refresh ();
      Curses.def_prog_mode ();
      Curses.endwin ();
      ignore @@ Curses.cbreak ();
      let opened_state =
        {
          lock;
          unlock;
          interrupt = false;
          mode = Logging;
          window;
          drawn = false;
          restore_stdout_fof;
          logging_stdout_fof;
          drawing_stdout_fof;
          restore_stderr_fof;
          logging_stderr_fof;
          drawing_stderr_fof;
          flush_buffers;
          instructions = 0;
          total_time = 0.;
        }
      in
      let thread = Thread.create listen opened_state in
      state := Opened (opened_state, thread);
      Mutex.unlock mutex)
    else ()

  let release () =
    match !state with
    | Closed -> ()
    | Opened (opened_state, thread) ->
        opened_state.interrupt <- true;
        Thread.join thread;
        ignore @@ Curses.nocbreak ();
        ignore @@ Curses.refresh ();
        Curses.endwin ();
        opened_state.flush_buffers ();
        Format.(
          pp_set_formatter_out_functions std_formatter
            opened_state.restore_stdout_fof;
          pp_set_formatter_out_functions err_formatter
            opened_state.restore_stderr_fof)
end
