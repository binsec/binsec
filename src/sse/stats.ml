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

type time = { mutable sec : float }

module Exploration () : Types.EXPLORATION_STATISTICS_FULL = struct
  let paths = ref 1

  let counter =
    let halt = ref 0
    and cut = ref 0
    and unsatisfiable_assumption = ref 0
    and assertion_failed = ref 0
    and max_depth = ref 0
    and enumeration_limit = ref 0
    and unresolved_formula = ref 0
    and non_executable_code = ref 0
    and die = ref 0 in
    fun (status : Types.status) ->
      match status with
      | Halt -> halt
      | Cut -> cut
      | Unsatisfiable_assumption -> unsatisfiable_assumption
      | Assertion_failed -> assertion_failed
      | Max_depth -> max_depth
      | Enumeration_limit -> enumeration_limit
      | Unresolved_formula -> unresolved_formula
      | Non_executable_code -> non_executable_code
      | Die -> die

  let completed_paths = ref 0
  let unknown_paths = ref 0
  let total_asserts = ref 0
  let failed_asserts = ref 0
  let branches = ref 0
  let max_depth = ref 0
  let instructions = ref 0
  let unique_insts = Virtual_address.Htbl.create 256
  let init_time = { sec = Unix.gettimeofday () }

  let reset () =
    paths := 1;
    completed_paths := 0;
    unknown_paths := 0;
    total_asserts := 0;
    failed_asserts := 0;
    branches := 0;
    max_depth := 0;
    instructions := 0;
    List.iter
      (fun status -> counter status := 0)
      [
        Halt;
        Cut;
        Unsatisfiable_assumption;
        Assertion_failed;
        Max_depth;
        Enumeration_limit;
        Unresolved_formula;
        Non_executable_code;
        Die;
      ];
    Virtual_address.Htbl.clear unique_insts;
    init_time.sec <- Unix.gettimeofday ()

  let time () = Unix.gettimeofday () -. init_time.sec
  let get_time = time
  let get_paths () = !paths
  let get_pending_paths () = !paths - !completed_paths - !unknown_paths
  let get_completed_paths () = !completed_paths
  let get_unknown_paths () = !unknown_paths
  let get_status status = !(counter status)
  let get_total_asserts () = !total_asserts
  let get_failed_asserts () = !failed_asserts
  let get_branches () = !branches
  let get_max_depth () = !max_depth
  let get_instructions () = !instructions
  let get_unique_insts () = Virtual_address.Htbl.length unique_insts
  let add_path () = incr paths

  let terminate_path (status : Types.status) =
    incr (counter status);
    match status with
    | Halt | Cut | Assertion_failed | Unsatisfiable_assumption ->
        incr completed_paths
    | Max_depth | Enumeration_limit | Unresolved_formula | Non_executable_code
    | Die ->
        incr unknown_paths

  let add_assert () = incr total_asserts
  let add_failed_assert () = incr failed_asserts
  let add_branch () = incr branches
  let update_depth d = if !max_depth < d then max_depth := d
  let add_instructions n = instructions := !instructions + n
  let register_address addr = Virtual_address.Htbl.replace unique_insts addr ()

  let pp ppf () =
    Format.fprintf ppf
      "@[<v 0>@[<h>total paths                      %d@]@,\
       @[<h>completed/cut paths              %d@]@,\
       @[<h>pending paths                    %d@]@,\
       @[<h>stale paths                      %d@]@,\
       @[<h>failed assertions                %d@]@,\
       @[<h>branching points                 %d@]@,\
       @[<h>max path depth                   %d@]@,\
       @[<h>visited instructions (unrolled)  %d@]@,\
       @[<h>visited instructions (static)    %d@]@,\
       @]"
      !paths !completed_paths
      (!paths - !completed_paths - !unknown_paths)
      !unknown_paths !failed_asserts !branches !max_depth !instructions
      (Virtual_address.Htbl.length unique_insts)

  let to_toml () =
    Toml.Min.of_key_values
      [
        (Toml.Min.key "paths", Toml.Types.TInt !paths);
        (Toml.Min.key "completed_paths", Toml.Types.TInt !completed_paths);
        (Toml.Min.key "unknown_paths", Toml.Types.TInt !unknown_paths);
        (Toml.Min.key "total_asserts", Toml.Types.TInt !total_asserts);
        (Toml.Min.key "failed_asserts", Toml.Types.TInt !failed_asserts);
        (Toml.Min.key "branches", Toml.Types.TInt !branches);
        (Toml.Min.key "max_depth", Toml.Types.TInt !max_depth);
        (Toml.Min.key "instructions", Toml.Types.TInt !instructions);
        ( Toml.Min.key "unique_insts",
          Toml.Types.TInt (Virtual_address.Htbl.length unique_insts) );
      ]
end

module Query () : Types.QUERY_STATISTICS = struct
  module Preprocess = struct
    let ctrue = ref 0
    let cfalse = ref 0
    let const = ref 0
    let get_true () = !ctrue
    let get_false () = !cfalse
    let get_const () = !const
    let total () = !ctrue + !cfalse + !const
    let incr_true () = incr ctrue
    let incr_false () = incr cfalse
    let incr_const () = incr const

    let reset () =
      ctrue := 0;
      cfalse := 0;
      const := 0

    let pp ppf () =
      let open Format in
      fprintf ppf
        "@[<v 2>@[<h>Preprocessing simplifications@]@,\
         @[<h>total          %d@]@,\
         @[<h>true           %d@]@,\
         @[<h>false          %d@]@,\
         @[<h>constant enum  %d@]@]" (total ()) !ctrue !cfalse !const

    let to_toml () =
      Toml.Min.of_key_values
        [
          (Toml.Min.key "sat", Toml.Types.TInt !ctrue);
          (Toml.Min.key "unsat", Toml.Types.TInt !cfalse);
          (Toml.Min.key "const", Toml.Types.TInt !const);
        ]
  end

  module Solver = struct
    let sat = ref 0
    let unsat = ref 0
    let err = ref 0
    let get_sat () = !sat
    let get_unsat () = !unsat
    let get_err () = !err
    let incr_sat () = incr sat
    let incr_unsat () = incr unsat
    let incr_err () = incr err
    let total_time = { sec = 0. }
    let timer = { sec = 0. }
    let running = ref false

    let reset () =
      sat := 0;
      unsat := 0;
      err := 0;
      total_time.sec <- 0.;
      running := false

    let start_timer () =
      timer.sec <- Unix.gettimeofday ();
      running := true

    let stop_timer () =
      running := false;
      let elapsed = Unix.gettimeofday () -. timer.sec in
      total_time.sec <- total_time.sec +. elapsed

    let total () = !sat + !unsat + !err

    let total_time () =
      if !running then
        let elapsed = Unix.gettimeofday () -. timer.sec in
        total_time.sec +. elapsed
      else total_time.sec

    let get_time = total_time

    let pp ppf () =
      let open Format in
      let time = total_time () and queries = total () in
      fprintf ppf
        "@[<v 2>@[<h>Satisfiability queries@]@,\
         @[<h>total          %d@]@,\
         @[<h>sat            %d@]@,\
         @[<h>unsat          %d@]@,\
         @[<h>unknown        %d@]@,\
         @[<h>time           %.2f@]@,\
         @[<h>average        %.2f@]@]" (total ()) !sat !unsat !err time
        (time /. float queries)

    let to_toml () =
      Toml.Min.of_key_values
        [
          (Toml.Min.key "sat", Toml.Types.TInt !sat);
          (Toml.Min.key "unsat", Toml.Types.TInt !unsat);
          (Toml.Min.key "err", Toml.Types.TInt !err);
          (Toml.Min.key "time", Toml.Types.TFloat (total_time ()));
        ]
  end

  let reset () =
    Preprocess.reset ();
    Solver.reset ()

  let pp ppf () =
    let open Format in
    fprintf ppf "@[<v 0>%a@,@,%a@,@]" Preprocess.pp () Solver.pp ()
end
