(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

let nb_locs = ref 0
let set_location_count n = nb_locs := n

let nb_func = ref 0

let increase_function_count () = incr nb_func

let nb_evals = ref 0
and nb_evals_lhs = ref 0
and nb_evals_lhs_in_equalities = ref 0
and calls = ref ""
and dynamic_jumps = ref ""

let increase_evaluation_count () = incr nb_evals
let increase_lhseq_evaluation_count ()  = incr nb_evals_lhs_in_equalities
let increase_lhs_evaluation_count ()  = incr nb_evals_lhs

let save_evaluation_counts, restore_evaluation_counts =
  let e1, e2, e3 = ref 0, ref 0, ref 0 in
  (fun () ->
     e1 := !nb_evals;
     e2 := !nb_evals_lhs;
     e3 := !nb_evals_lhs_in_equalities
  ),
  (fun () ->
     nb_evals := !e1;
     nb_evals_lhs := !e2;
     nb_evals_lhs_in_equalities := !e3
  )

let equality_use, get_equality_uses =
  let v = ref 0 in
  (fun stack lvalue expr ->
      Logger.debug ~level:3 "using (%a = %a) %@ %a"
        Dba_printer.Ascii.pp_lhs lvalue
        Dba_printer.Ascii.pp_lhs expr
        Dba_types.AddressStack.pp stack;
      incr v)
  ,(fun () -> !v)

let time_without_equalities = ref 0.

let add_time full_time additional_time =
  full_time := !full_time +. additional_time

let _add_time_without_equalities =
  add_time time_without_equalities

type 'a getset =
  { set : 'a -> unit;
    get : unit -> 'a;
    add : 'a -> unit;
  }

let mk_obj default f =
  let v = ref default in
  { set = (fun w -> v := w);
    get = (fun () -> !v);
    add = (fun n -> v := f n !v);
  }

let mk_float () = mk_obj 0. (fun x y -> x +. y)

let analysis_time = mk_float ()

let _time_without_equalities = mk_float ()

let add_call , _get_calls =
  let l = ref [] in
  (fun (caller : Bigint.t) -> l := caller :: !l),
  (fun () -> !l)

type message =
  | Join of Dba.address * string * string * string
  | Widen of Dba.address * string * string * string
  | MError of Dba_types.AddressStack.Map.key * string
  | Info of Dba_types.AddressStack.Map.key * string
  | Elements of Region_bitvector.t list
  | DelayedWidenings of Dba_types.AddressStack.Map.key * int
  | Widenings of Dba_types.AddressStack.Map.key
  | Unrollings of Dba_types.AddressStack.Map.key
  | Joins of Dba_types.AddressStack.Map.key
  | Equalities of string
  | Assign of string * Dba.expr * string
  | Guard of Dba.cond * string * string
  | Call of Dba.address * Dba.address
  | RemoveEqualities of Dba.lhs * string
  | Cond of Dba.cond * string
  | CondNat of Dba.address * Dba.cond * string
  | CondReplacement of Dba.cond * Dba.expr * Dba.expr * Dba.expr * Dba.expr * string
  | Predicates of Dba.binary_op list
  | Djmps of Dba_types.Caddress.Set.t Dba_types.AddressStack.Map.t
  | Stats_flags
  | Stats_equalities


let get_priority msg =
  match msg with
  | Join _ -> 3
  | Widen _ -> 3
  | MError _ -> 0
  | Info _ -> 2
  | Elements _ -> 3
  | DelayedWidenings _ -> 3
  | Widenings _ -> 3
  | Unrollings _ -> 3
  | Joins _ -> 3
  | Equalities _ -> 3
  | Assign _ -> 3
  | Guard _ -> 3
  | Call _ -> 3
  | RemoveEqualities _ -> 3
  | Cond _ -> 3
  | CondNat _ -> 3
  | CondReplacement _ -> 3
  | Predicates _ -> 3
  | Djmps _ -> 0
  | Stats_flags -> 0
  | Stats_equalities -> 3


let widening_pts_string = ref ""
let unrolled_loops_string = ref ""
let joins_pts_string = ref ""
let _equality_uses = ref 0

let ai_stats () =
  Logger.debug ~level:3 "@[<v 0>%s@ %s@ %s@]@."
    !joins_pts_string
    !widening_pts_string
    !unrolled_loops_string

let error_buf = Buffer.create 2048

let display msg =
  let open Dba_printer.Ascii in
  let level = get_priority msg in
  match msg with
  | Join (addr, m', m, s) ->
    Logger.debug ~level " %a: join" pp_code_address addr;
    Logger.debug ~level "%s\n⊔\n%s\n=\n%s" m' m s


  | Widen (addr, m', m, s) ->
    Logger.debug ~level " %a: widening"
      Dba_printer.Ascii.pp_code_address addr;
    Logger.debug ~level "%s\n▽\n%s\n=\n%s" m' m s

  | MError (addrStack, message) ->
    Logger.error "%a : %s" Dba_types.AddressStack.pp addrStack  message;
    Buffer.add_string error_buf message

  | Info (addrStack, message)->
    Logger.info "%@ %a : %s" Dba_types.AddressStack.pp addrStack  message

  | Elements elements ->
    Logger.debug ~level "elements length = %d" (List.length elements) ;
    List.iter (fun e ->  Logger.debug ~level "e = %a" Region_bitvector.pp e) elements

  | DelayedWidenings (addrStack, iter) ->
    Logger.debug "%a: delayed widening (%d)"
      Dba_types.AddressStack.pp addrStack iter;
    (* widening_pts_string := !widening_pts_string ^ "\n" ^ message *)

  | Widenings (addrStack) ->
    Logger.debug ~level "%a: widening"  Dba_types.AddressStack.pp addrStack;
    (*    widening_pts_string := !widening_pts_string ^ "\n" ^ message *)

  | Unrollings (addrStack) ->
    Logger.debug ~level "%a: loop unrooling"
      Dba_types.AddressStack.pp addrStack;
(*    widening_pts_string := !widening_pts_string ^ "\n" ^ message *)

  | Joins addrStack ->
    Logger.debug ~level "%a : join" Dba_types.AddressStack.pp addrStack;
    (* joins_pts_string := !joins_pts_string ^ "\n" ^ message*)

  | Equalities equalities ->
    Logger.debug ~level "%s" equalities

  | RemoveEqualities (lhs, equalities) ->
    Logger.debug ~level "removing %a" Dba_printer.Ascii.pp_lhs lhs ;
    Logger.debug ~level "%s" equalities

  | Assign (lhs, e, v) ->
    Logger.debug ~level "%s := (%a = %s)" lhs pp_expr e v

  | Guard (cond, v1, v2) ->
    Logger.debug ~level "Guard (%a) = (%s, %s)"
      Dba_printer.Ascii.pp_cond cond v1 v2

  | Call (a, addr_ret) ->
    calls :=
    !calls ^
    (Format.asprintf "Call %a with ret %@ %a"
       pp_code_address a
       pp_code_address addr_ret)

  | Cond (cond, msg) ->
    Logger.debug ~level "%s: %a" msg pp_cond cond

  | CondNat (addr, cond, msg) ->
    Logger.debug ~level
      "%a: natural cond recovered : %a (%s)"
      pp_code_address addr
      pp_cond cond msg


  | CondReplacement (cond, op1, op2, v1, v2, msg) ->
    Logger.debug ~level
      "@[<v 0>%s :@ \
       @[cond = %a@]@ \
       @[op1 = %a@]@ \
       @[op2 = %a@]@ \
       @[v1 = %a@]@ \
       @[v2 = %a@]@]"
      msg
      Dba_printer.Ascii.pp_cond cond
      pp_expr op1
      pp_expr op2
      pp_expr v1
      pp_expr v2


  | Predicates predicates ->
    Logger.debug ~level "@[<hov 2>%a@]"
    (Print_utils.pp_list ~pre:"[" ~post:"]" ~sep:", "
       Dba_printer.Ascii.pp_binary_op) predicates

  | Djmps djmps ->
    let djmps =
      Dba_types.AddressStack.Map.fold (fun (addr, _, _loop) addrs acc ->
          let addrs' =
            try Dba_types.Caddress.Map.find addr acc
            with Not_found -> Dba_types.Caddress.Set.empty
          in
          Dba_types.Caddress.Map.add addr (Dba_types.Caddress.Set.union addrs addrs') acc
        ) djmps Dba_types.Caddress.Map.empty
    in
    let fprint addr _ =
      let message =
        Format.asprintf "%a: dynamic jump"
          Dba_printer.Ascii.pp_code_address addr
      in
      dynamic_jumps := !dynamic_jumps ^ message;
      Logger.debug ~level "%s" message
    in
    Dba_types.Caddress.Map.iter fprint djmps;

  | Stats_flags ->
    Logger.result "& %d & %d & %d & %d & %f\n%!"
      !Options.nb_nat_predicate_recovery_tries
      !Options.nb_recovered_nat_predicates
      !Options.nb_failed_nat_predicate_recoveries
      !Options.nb_conditional_cache_uses
      !Options.time_nat_flag_recovery ;


  | Stats_equalities ->
      Options.time_analysis := !Options.time_analysis -. !Options.time_redundant_evals;
      let time_without_equalities =
        !Options.time_analysis -. !Options.time_equalities in
      Logger.result
        "@[<v 0>\
        Locations: %d@ \
        Functions: %d@ \
        Time (no eq): %0.2f@ \
        Time : %.2f@ \
        Neq (names): %d@ \
        Neq (classes): %d@ \
        Evaluations: %d@ \
        Equality uses: %d@ \
        Equality refinments: %d@ \
        l-value evaluations %d@ \
        l-value in equalities: %d@ \
        Refined l-values: %d \
         @]"
        !nb_locs
        !nb_func
        time_without_equalities
        !Options.time_analysis
        !Options.nb_equalities_names
        !Options.nb_equalities_classes
        !nb_evals
        (get_equality_uses ())
        !Options.nb_equalities_refinement
        !nb_evals_lhs
        !nb_evals_lhs_in_equalities
        !Options.nb_refined_lhs



let pp_results () =
  if Buffer.length error_buf = 0 then
    Logger.result "Analysis completed"
  else
    Logger.error "@[<v 0>Verdict => @%s@]" (Buffer.contents error_buf);
  ai_stats ()
