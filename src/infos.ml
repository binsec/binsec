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


type instruction_kinds = Dba.instruction list

type widening_delay = int

module BoundThreshold = struct
  type t = {
    los : int array;
    his : int array;
  }

  let default = { los = [| |]; his = [| |]; }

  let mk_from_list (los: int list) (his : int list) : t =
    { los = Array.of_list los;
      his = Array.of_list his; }

  let is_unspecified bthreshold =
    Array.length bthreshold.los = 0 &&
    Array.length bthreshold.his = 0

end

module WideningThreshold = struct
  type t = {
    signed : BoundThreshold.t;
    unsigned : BoundThreshold.t;
  }


  let mk signed unsigned = { signed; unsigned; }

  let default = {
    signed = BoundThreshold.default;
    unsigned = BoundThreshold.default ;
  }

  let is_unspecified (wthreshold : t) =
    BoundThreshold.is_unspecified wthreshold.signed &&
    BoundThreshold.is_unspecified wthreshold.unsigned

  (* Translation function to legacy structure *)
  let flatten_to_arrays_tuple wthreshold =
    let open BoundThreshold in
    wthreshold.signed.his, wthreshold.signed.los,
    wthreshold.unsigned.his, wthreshold.unsigned.los

end


let default_global_widening_thresholds = WideningThreshold.default
and default_global_widening_delay = 0


type t = {
  entry_points :  Dba_types.Virtual_address.Set.t;
  jumps : Dba.addresses Dba_types.Caddress.Map.t ;
  allowed_jumpzones : (Dba.address * Dba.address) list ;
  stops : Dba_types.Caddress.Set.t ;
  prepend_stubs : instruction_kinds Dba_types.Caddress.Map.t ;
  substitute_stubs : Dba_types.instruction_sequence Dba_types.Caddress.Map.t ;
  linear_addresses : (Dba_types.Virtual_address.t * Dba_types.Virtual_address.t) list ;
  global_widening : WideningThreshold.t * widening_delay ;
  local_widening_thresholds : WideningThreshold.t Dba_types.Caddress.Map.t ;
  local_widening_delays : widening_delay Dba_types.Caddress.Map.t ;
}


let default =
  { entry_points = Dba_types.Virtual_address.Set.empty;
    jumps = Dba_types.Caddress.Map.empty;
    allowed_jumpzones = [];
    stops = Dba_types.Caddress.Set.empty;
    substitute_stubs = Dba_types.Caddress.Map.empty;
    prepend_stubs = Dba_types.Caddress.Map.empty;
    linear_addresses = [];
    global_widening = default_global_widening_thresholds,
                      default_global_widening_delay;
    local_widening_thresholds = Dba_types.Caddress.Map.empty;
    local_widening_delays = Dba_types.Caddress.Map.empty;
  }


let has_entry_points p = not (Dba_types.Virtual_address.Set.is_empty p.entry_points)

let has_stops p = not (Dba_types.Caddress.Set.is_empty p.stops)

let has_jumps p = not (Dba_types.Caddress.Map.is_empty p.jumps)

let has_allowed_jumpzones p = not (List_utils.is_empty p.allowed_jumpzones)

let has_prepend_stubs p = not (Dba_types.Caddress.Map.is_empty p.prepend_stubs)

let has_substitute_stubs p =
  not (Dba_types.Caddress.Map.is_empty p.substitute_stubs)

let has_linear_addresses p = not (List_utils.is_empty p.linear_addresses)

(* FIXME: in the next 2 functions
   * If those values are set to default values in the parameter file, the user
   * will be allowed to make another declaration in the same file *)
let has_global_widening_delay p =
  (snd p.global_widening) <> default_global_widening_delay

let has_global_widening_thresholds p =
  not (WideningThreshold.is_unspecified (fst p.global_widening))

let _has_local_widening_thresholds p =
  not (Dba_types.Caddress.Map.is_empty p.local_widening_thresholds)

let _has_local_widening_delays p =
  not (Dba_types.Caddress.Map.is_empty p.local_widening_delays)

let set_if_not err_p err_msg do_action parameters =
  if err_p parameters then failwith err_msg
  else do_action parameters

let set_entry_points addrs parameters =
  Logger.debug "@[Setting %d entry points...@]"
    (Dba_types.Virtual_address.Set.cardinal addrs);
  set_if_not has_entry_points
    "Entry points already set"
    (fun p -> { p with entry_points = addrs })
    parameters

let set_jumps addr_map parameters =
  set_if_not has_jumps
    "Jumps already set"
    (fun p -> { p with jumps = addr_map })
    parameters

let set_stops addresses parameters =
  set_if_not has_stops
    "Stops already set"
    (fun p -> { p with stops = addresses })
    parameters

let set_prepend_stubs addr_map parameters =
  set_if_not has_prepend_stubs
    "Prepend stubs already set"
    (fun p -> { p with prepend_stubs = addr_map })
    parameters

let set_substitute_stubs addr_map parameters =
  set_if_not has_substitute_stubs
    "Susbstitute stubs already set"
    (fun p -> { p with substitute_stubs = addr_map })
    parameters

let set_allowed_jumpzones addrs parameters =
  set_if_not has_allowed_jumpzones
    "Allowed jumps areas already set"
    (fun p -> { p with allowed_jumpzones = addrs })
    parameters

let set_linear_addresses addr_intervals parameters =
  let linear_addresses =
    List.map
      (fun (start, _end) ->
        Dba_types.Virtual_address.(of_code_address start, of_code_address _end))
      addr_intervals
  in
  set_if_not has_linear_addresses
    "Linear addresses already set"
    (fun p -> { p with linear_addresses })
    parameters

let set_global_widening_delay widening_delay parameters =
  set_if_not has_global_widening_delay
    "Global widening delay already set"
    (fun p ->
       { p with global_widening = (fst p.global_widening, widening_delay) })
    parameters

let set_global_widening_thresholds widening_thresholds parameters =
  set_if_not has_global_widening_thresholds
    "Global widening thresholds already set"
    (fun p ->
       { p with global_widening = (widening_thresholds, snd p.global_widening) })
    parameters
