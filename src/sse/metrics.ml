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

type status =
  | Halt
  | Cut
  | Merged
  | Stashed
  | Unsatisfiable_assumption
  | Assertion_failure
  | Max_depth
  | Enumeration_limit
  | Unresolved_formula
  | Non_executable_code
  | Error of string

let status_to_string : status -> string = function
  | Halt -> "halt"
  | Cut -> "cut"
  | Merged -> "merged"
  | Stashed -> "stashed"
  | Unsatisfiable_assumption -> "unsatisfiable_assumption"
  | Assertion_failure -> "assertion_failure"
  | Max_depth -> "max_depth"
  | Enumeration_limit -> "enumeration_limit"
  | Unresolved_formula -> "unresolved_formula"
  | Non_executable_code -> "non_executable_code"
  | Error _ -> "error"

let pp_status : Format.formatter -> status -> unit =
 fun ppf status -> Format.pp_print_string ppf (status_to_string status)

module type TIMER = sig
  type t

  val get : t -> float
  val start : t -> unit
  val stop : t -> unit
end

module type ASPECT = sig
  type t

  val get : t -> int
  val incr : t -> unit

  module Timer : TIMER with type t := unit

  val pp : Format.formatter -> unit -> unit
  val to_toml : unit -> Toml.Types.table
end

module type QUERY = sig
  module Preprocess : ASPECT with type t := Binsec_smtlib.Solver.status
  module Solver : ASPECT with type t := Binsec_smtlib.Solver.status

  val reset : unit -> unit
  val pp : Format.formatter -> unit -> unit
  val to_toml : unit -> Toml.Types.table
end

module Timer () : sig
  include TIMER with type t := unit

  val reset : unit -> unit
end = struct
  type t = {
    mutable time : float;
    mutable active : bool;
    mutable since : float;
  }

  let timer = { time = 0.; active = false; since = 0. }

  let get () =
    if timer.active then timer.time +. (Unix.gettimeofday () -. timer.since)
    else timer.time

  let start () =
    timer.since <- Unix.gettimeofday ();
    timer.active <- true

  let stop () =
    timer.active <- false;
    timer.time <- timer.time +. (Unix.gettimeofday () -. timer.since)

  let reset () =
    timer.time <- 0.;
    timer.active <- false
end

module Query () : QUERY = struct
  type status = Binsec_smtlib.Solver.status

  let to_int : status -> int = function Sat -> 0 | Unsat -> 1 | Unknown -> 2

  module Common () = struct
    let counters = Array.make 3 0
    let get s = Array.unsafe_get counters (to_int s)

    let incr s =
      Array.unsafe_set counters (to_int s)
        (Array.unsafe_get counters (to_int s) + 1)

    module Timer = Timer ()

    let reset () =
      Array.fill counters 0 3 0;
      Timer.reset ()
  end

  module Preprocess = struct
    include Common ()

    let pp ppf () =
      let sat = get Sat and unsat = get Unsat in
      Format.fprintf ppf
        "@[<v 2>@[<h>Preprocessing simplifications@]@,\
         @[<h>total          %d@]@,\
         @[<h>sat            %d@]@,\
         @[<h>unsat          %d@]@,\
         @[<h>time           %.2f@]@]" (sat + unsat) sat unsat (Timer.get ())

    let to_toml () =
      Toml.Min.of_key_values
        [
          (Toml.Min.key "sat", Toml.Types.TInt (get Sat));
          (Toml.Min.key "unsat", Toml.Types.TInt (get Unsat));
          (Toml.Min.key "time", Toml.Types.TFloat (Timer.get ()));
        ]
  end

  module Solver = struct
    include Common ()

    let pp ppf () =
      let sat = get Sat
      and unsat = get Unsat
      and unknown = get Unknown
      and time = Timer.get () in
      let total = sat + unsat + unknown in
      Format.fprintf ppf
        "@[<v 2>@[<h>Satisfiability queries@]@,\
         @[<h>total          %d@]@,\
         @[<h>sat            %d@]@,\
         @[<h>unsat          %d@]@,\
         @[<h>unknown        %d@]@,\
         @[<h>time           %.2f@]@,\
         @[<h>average        %.2f@]@]" total sat unsat unknown time
        (time /. float total)

    let to_toml () =
      Toml.Min.of_key_values
        [
          (Toml.Min.key "sat", Toml.Types.TInt (get Sat));
          (Toml.Min.key "unsat", Toml.Types.TInt (get Unsat));
          (Toml.Min.key "unknown", Toml.Types.TInt (get Unknown));
          (Toml.Min.key "time", Toml.Types.TFloat (Timer.get ()));
        ]
  end

  let reset () =
    Preprocess.reset ();
    Solver.reset ()

  let pp ppf () =
    let open Format in
    fprintf ppf "@[<v 0>%a@,@,%a@,@]" Preprocess.pp () Solver.pp ()

  let to_toml () =
    Toml.Min.of_key_values
      [
        (Toml.Min.key "preprocess", Toml.Types.TTable (Preprocess.to_toml ()));
        (Toml.Min.key "solver", Toml.Types.TTable (Solver.to_toml ()));
      ]
end

module type EXPLORATION = sig
  module Paths : sig
    type t = Total | Pending | Completed | Discontinued

    val get : t -> int
    val status : status -> int
    val incr : unit -> unit
    val resume : unit -> unit
    val signal : status -> unit
  end

  module Topology : sig
    type t = Branch | Jump | Assert

    val get : t -> int
    val incr : t -> unit
  end

  module Max_depth : sig
    val get : unit -> int
    val update : int -> unit
  end

  module Addresses : sig
    val unique : unit -> int
    val register : Virtual_address.t -> unit
  end

  module Instructions : sig
    val get : unit -> int
    val incr : int -> unit
  end

  module Timer : TIMER with type t := unit

  val reset : unit -> unit
  val pp : Format.formatter -> unit -> unit
  val to_toml : unit -> Toml.Types.table
end

module Exploration () : EXPLORATION = struct
  module Paths = struct
    type t = Total | Pending | Completed | Discontinued

    let status_to_int : status -> int = function
      | Halt -> 0
      | Cut -> 1
      | Merged -> 2
      | Stashed -> 3
      | Unsatisfiable_assumption -> 4
      | Assertion_failure -> 5
      | Max_depth -> 6
      | Enumeration_limit -> 7
      | Unresolved_formula -> 8
      | Non_executable_code -> 9
      | Error _ -> 10

    let to_int : t -> int = function
      | Total -> 11
      | Pending -> 12
      | Completed -> 13
      | Discontinued -> 14

    let status_to_t : status -> t = function
      | Halt -> Completed
      | Cut -> Completed
      | Merged -> Completed
      | Stashed -> Pending
      | Unsatisfiable_assumption -> Completed
      | Assertion_failure -> Completed
      | Max_depth -> Discontinued
      | Enumeration_limit -> Discontinued
      | Unresolved_formula -> Discontinued
      | Non_executable_code -> Discontinued
      | Error _ -> Discontinued

    let counters = Array.make 15 0

    let incr x =
      Array.unsafe_set counters (to_int x)
        (Array.unsafe_get counters (to_int x) + 1)

    let get c = Array.unsafe_get counters (to_int c)
    let status s = Array.unsafe_get counters (status_to_int s)

    let signal s =
      Array.unsafe_set counters (status_to_int s)
        (Array.unsafe_get counters (status_to_int s) + 1);
      Array.unsafe_set counters (to_int Pending)
        (Array.unsafe_get counters (to_int Pending) - 1);
      incr (status_to_t s)

    let resume () =
      Array.unsafe_set counters (status_to_int Stashed)
        (Array.unsafe_get counters (status_to_int Stashed) - 1)

    let incr () =
      incr Total;
      incr Pending

    let reset () = Array.fill counters 0 15 0

    let to_toml () =
      let completed, discontinued =
        List.fold_left
          (fun (completed, discontinued) s ->
            let entry =
              (Toml.Min.key (status_to_string s), Toml.Types.TInt (status s))
            in
            match status_to_t s with
            | Total | Pending -> (completed, discontinued)
            | Completed -> (entry :: completed, discontinued)
            | Discontinued -> (completed, entry :: discontinued))
          ([], [])
          [
            Halt;
            Cut;
            Stashed;
            Unsatisfiable_assumption;
            Assertion_failure;
            Max_depth;
            Enumeration_limit;
            Unresolved_formula;
            Non_executable_code;
            Error "";
          ]
      in
      Toml.Min.of_key_values
        [
          (Toml.Min.key "total", Toml.Types.TInt (get Total));
          (Toml.Min.key "pending", Toml.Types.TInt (get Pending));
          ( Toml.Min.key "completed",
            Toml.Types.TTable (Toml.Min.of_key_values completed) );
          ( Toml.Min.key "discontinued",
            Toml.Types.TTable (Toml.Min.of_key_values discontinued) );
        ]
  end

  module Topology = struct
    type t = Branch | Jump | Assert

    let to_int : t -> int = function Branch -> 0 | Jump -> 1 | Assert -> 2
    let counters = Array.make 3 0
    let get e = Array.unsafe_get counters (to_int e)

    let incr e =
      Array.unsafe_set counters (to_int e)
        (Array.unsafe_get counters (to_int e) + 1)

    let reset () = Array.fill counters 0 3 0

    let to_toml () =
      Toml.Min.of_key_values
        [
          (Toml.Min.key "branches", Toml.Types.TInt (get Branch));
          (Toml.Min.key "jumps", Toml.Types.TInt (get Jump));
          (Toml.Min.key "assertions", Toml.Types.TInt (get Assert));
        ]
  end

  module Max_depth = struct
    let counter = ref 0
    let get () = !counter
    let update n = counter := max n !counter
    let reset () = counter := 0
  end

  module Addresses = struct
    let tbl = Virtual_address.Htbl.create 1024
    let unique () = Virtual_address.Htbl.length tbl
    let register addr = Virtual_address.Htbl.replace tbl addr ()
    let reset () = Virtual_address.Htbl.reset tbl
  end

  module Instructions = struct
    let counter = ref 0
    let get () = !counter
    let incr n = counter := !counter + n
    let reset () = counter := 0
  end

  module Timer = Timer ()

  let reset () =
    Paths.reset ();
    Topology.reset ();
    Max_depth.reset ();
    Addresses.reset ();
    Instructions.reset ()

  let pp ppf () =
    Format.fprintf ppf
      "@[<v 0>@[<h>total paths                      %d@]@,\
       @[<h>completed/cut paths              %d@]@,\
       @[<h>pending paths                    %d@]@,\
       @[<h>discontinued paths               %d@]@,\
       @[<h>failed assertions                %d@]@,\
       @[<h>branching points                 %d@]@,\
       @[<h>max path depth                   %d@]@,\
       @[<h>visited instructions (unrolled)  %d@]@,\
       @[<h>visited instructions (static)    %d@]@,\
       @]"
      (Paths.get Total) (Paths.get Completed) (Paths.get Pending)
      (Paths.get Discontinued)
      (Paths.status Assertion_failure)
      (Topology.get Branch) (Max_depth.get ()) (Instructions.get ())
      (Addresses.unique ())

  let to_toml () =
    Toml.Min.of_key_values
      [
        (Toml.Min.key "paths", Toml.Types.TTable (Paths.to_toml ()));
        (Toml.Min.key "topology", Toml.Types.TTable (Topology.to_toml ()));
        (Toml.Min.key "max_depth", Toml.Types.TInt (Max_depth.get ()));
        (Toml.Min.key "instructions", Toml.Types.TInt (Instructions.get ()));
        (Toml.Min.key "unique_insts", Toml.Types.TInt (Addresses.unique ()));
      ]
end
