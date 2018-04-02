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


open Display
open Dba_types

module type AbstractAnalysis =
sig
  type t
  type equalities
  type predsSet = Dba_types.AddressStack.Set.t
  type flagsType = High_level_predicate.t
  type statesMap = (t * flagsType * equalities * predsSet) AddressStack.Map.t
  type conditionsList = Smt_bitvectors.smtBvExprAlt list
  type djmpsMap = Caddress.Set.t AddressStack.Map.t
  type stopList = Caddress.Set.t
  type insert_instrs = Dba.instruction list Caddress.Map.t
  type replace_instrs = Dba_types.instruction_sequence Caddress.Map.t
  type localThresholdsType = (int array * int array * int array * int array)
  type globalThresholdsType =
    (int array * int array * int array * int array) Caddress.Map.t
  type thresholdsType = localThresholdsType * globalThresholdsType
  type wideningType = int * int Caddress.Map.t


  val analyse:
    Dba.address ->
    Dba.instruction list ->
    Disasm_types.pmap ->
    insert_instrs ->
    replace_instrs ->
    stopList ->
    thresholdsType ->
    wideningType ->
    statesMap * conditionsList * Disasm_types.pmap * djmpsMap * Caddress.Set.t

end


module Make (State: Ai_sigs.AbstractDomain) =
struct
  type t = State.t
  type equalities = State.equalities
  type predsSet = AddressStack.Set.t
  type flagsType = High_level_predicate.t
  type statesMap = (State.t * flagsType * equalities * predsSet) AddressStack.Map.t
  type conditionsList = Smt_bitvectors.smtBvExprAlt list
  type djmpsMap =  Caddress.Set.t AddressStack.Map.t
  type stopList = Caddress.Set.t
  type insert_instrs = Dba.instruction list Caddress.Map.t
  type replace_instrs = (Dba.address * Dba.instruction) list Caddress.Map.t
  type localThresholdsType = (int array * int array * int array * int array)
  type globalThresholdsType = (int array * int array * int array * int array) Caddress.Map.t
  type thresholdsType = localThresholdsType * globalThresholdsType
  type wideningType = int * int Caddress.Map.t

  let update_djmps djmps =
    AddressStack.Map.fold (fun (addr, _, _) new_set acc ->
        let old_set =
          try Caddress.Map.find addr acc with Not_found -> Caddress.Set.empty in
      Caddress.Map.add addr (Caddress.Set.union old_set new_set) acc
      ) djmps Caddress.Map.empty


  let log_exception exn instr (m, _equalities) =
    Logger.error
      "@[<v 0>%s:@ \
       - Instruction: %a@ \
       - Abstract state:@ %a@]"
      (Printexc.to_string exn)
      Dba_types.Statement.pp instr
      State.pp m;
    raise Not_found


  let replace_stub addr r_insts insts =
    try Disasm_types.add_chained_instr (Caddress.Map.find addr r_insts) insts
    with Not_found -> insts

  let _wp_analysis addrStack states insts w_pts m =
    Display.display (Info (addrStack, "recovering elements by backward analysis"));
    let elements, smt_env =
      Backward_analysis.backward_refine_elements
        addrStack states insts State.env_to_smt_list w_pts in
    Display.display (Elements elements);
    let m = State.refine_state m smt_env in
    elements, m


  let f_trans
      l0 addrStack insts _states assumes glbs config
      abs_vals w_delays cache loop_pts =
    let (r_insts, stops, djmps) = config in
    let (addr, _, _) = addrStack in
    let (w_pts, unrolled_loops) = loop_pts in
    let (m, flags, equalities) = abs_vals in
    let djmps' = update_djmps djmps in
    let insts = replace_stub addr r_insts insts in
    let abs_vals = (m, flags, equalities) in
    let (instr, _), insts, w_pts, unrolled_loops =
      try
        Static_utils.update_instr_map l0 addr insts
          stops w_pts w_delays djmps' unrolled_loops
      with _ -> failwith "Error raised by Disasm.update_instr_map!"
    in
    let aux m elements equalities =
      try
        Logger.debug ~level:3 "@[<hov 0>Current instruction:@ %a@]"
          Dba_types.Statement.pp
          (Dba_types.Statement.create addr instr)
        ;
        let l, cache, assumes, djmps =
          State.post abs_vals addrStack instr cache assumes
            glbs djmps unrolled_loops elements in
        l, insts, assumes, djmps, cache, (w_pts,unrolled_loops)
      with
      | Errors.Enumerate_Top ->
        (* let _elements, m = wp_analysis addrStack states insts w_pts m in *)
        log_exception
          Errors.Enumerate_Top
          (Dba_types.Statement.create addr instr) (m, equalities)
        (*        ([], insts, assumes, djmps, cache, (w_pts,unrolled_loops)) *)
      | exn ->
        log_exception exn (Dba_types.Statement.create addr instr) (m, equalities)
        (* [], insts, assumes, djmps, cache, (w_pts, unrolled_loops) *)
    in aux m [] equalities


  let find_state addrStack states =
    try AddressStack.Map.find addrStack states
    with Not_found ->
      let bottom_state, bottom_flgs, bottom_equalities = State.bottom in
      let addr, cstack, loop = addrStack in
      if loop = 0 then
        try AddressStack.Map.find (addr, cstack, 1) states
        with Not_found ->
           bottom_state, bottom_flgs, bottom_equalities, AddressStack.Set.empty
      else bottom_state, bottom_flgs, bottom_equalities, AddressStack.Set.empty

  let join addrStack addr preds m' m flgs' flgs w_pts equalities' equalities =
    if AddressStack.Set.cardinal preds = 1
    then (
      Logger.debug ~level:3
        "@[<v 0>@[%@%a: no join since@]@ %a,@ %a@ ⊑@ %a,@ %a@]"
        Dba_printer.Ascii.pp_code_address addr
        State.pp m'
        State.pp_equalities equalities'
        State.pp m
        State.pp_equalities equalities ;
      w_pts, m, flgs, equalities
    )
    else (
      let s, flgs, s_equalities =
        State.join (m', flgs', equalities') (m, flgs, equalities) in
      Display.display (
        Join(addr,
             State.to_string (m', equalities'),
             State.to_string (m, equalities),
             State.to_string (s, s_equalities)));
      Display.display (Joins addrStack);
      w_pts, s, flgs, s_equalities
    )


  let widen addr m' m flgs' flgs equalities' equalities thresholds w_pts =
    let w_pts = Caddress.Map.add addr (-1) w_pts in
    let global_thresholds, local_thresholds = thresholds in
    let thresholds =
      try Caddress.Map.find addr local_thresholds
      with Not_found -> global_thresholds in
    let s, flgs, s_equalities =
      State.widen (m', flgs', equalities') (m, flgs, equalities) thresholds in
    Display.display (Widen(addr,
                           State.to_string (m', equalities'),
                           State.to_string (m, equalities),
                           State.to_string (s, s_equalities)));
    w_pts, s, flgs, s_equalities


  let collect w_pts states m flgs equalities addrStack thresholds =
    let m', flgs', equalities', preds = find_state addrStack states in
    let addr, _, _ = addrStack in
    let w_pts, m, flgs, equalities =
      match Caddress.Map.find addr w_pts with
      | iter when iter > 0 ->
        Display.display (DelayedWidenings (addrStack, iter));
        let w_pts = Caddress.Map.add addr (iter - 1) w_pts in
        join addrStack addr preds m' m flgs' flgs w_pts equalities' equalities

      | _ -> (* <= 0 *)
        Display.display (Widenings (addrStack));
        widen addr m' m flgs flgs' equalities' equalities thresholds w_pts

      | exception Not_found ->
        join addrStack addr preds m' m flgs' flgs w_pts equalities' equalities
    in
    AddressStack.Map.add addrStack (m, flgs, equalities, preds) states, w_pts


  let update_states (states, todo, w_pts, addrStack) t thresholds =
    let aux (states, todo, w_pts, pred) (addrStack, m, flgs, equalities) =
      let m', flgs', equalities', preds = find_state addrStack states in
      let preds = AddressStack.Set.add pred preds in
      let states =
        AddressStack.Map.add addrStack (m', flgs', equalities', preds) states in
      if State.leq m m' && High_level_predicate.leq flgs flgs'
      then (
        Display.display (Info (addrStack, "fixpoint"));
        states, todo, w_pts, pred
      )
      else
        let todo = AddressStack.Set.add addrStack todo in
        let states, w_pts =
          collect w_pts states m flgs equalities addrStack thresholds in
        states, todo, w_pts, pred
    in
    List.fold_left aux (states, todo, w_pts, addrStack) t


  (** fixpoint computation *)
  let analyse l0 inits insts b_insts r_i stps thresholds w_delays =
    let rec fixpoint states conds glbs todo insts djmps visited cache loop_pts =
      try
        let _b_insts = b_insts in
        let addrStack = AddressStack.Set.choose todo in
        let todo = AddressStack.Set.remove addrStack todo in
        let m, flgs, equalities, _preds = find_state addrStack states in
        let abs_vals = m, flgs, equalities in
        let config = r_i, stps, djmps in
        let t, insts, conds, djmps, cache, loop_pts =
          f_trans l0 addrStack insts states conds glbs
            config abs_vals w_delays cache loop_pts in
        let w_pts = fst loop_pts in
        let states, todo, w_pts, _pred =
          update_states (states, todo, w_pts, addrStack) t thresholds in
        let loop_pts = w_pts, snd loop_pts in
        let visited =
          let addr, _, _ = addrStack in Caddress.Set.add addr visited in
        fixpoint states conds glbs todo insts djmps visited cache loop_pts
      with
      | Not_found ->
        Display.set_location_count (AddressStack.Map.cardinal states);
        Ai_results.pp_file
          states insts (snd cache) djmps State.regs_in_expr_to_string;
        states, conds, insts, djmps, visited
    in
    let cstack = [] in
    let loop = 0 in
    let m_init = State.get_initial_state inits in
    let _, flgs, eqs = State.top in
    let preds = AddressStack.Set.empty in
    let states =
      AddressStack.Map.singleton (l0, cstack, loop) (m_init, flgs, eqs, preds) in
    let conds = [] in
    let glbs = Dba_types.Caddress.Set.empty in
    let todo = AddressStack.Set.add (l0, [], 0) AddressStack.Set.empty in
    let djmps = AddressStack.Map.empty in
    let visited = Caddress.Set.empty in
    let cache = AddressStack.Map.empty, Caddress.Map.empty in
    let loop_pts = Caddress.Map.empty, Caddress.Map.empty in
    fixpoint states conds glbs todo insts djmps visited cache loop_pts


  let to_string states =
    let open Format in
    let ppf = str_formatter in
    if AddressStack.Map.is_empty states |> not then begin
      let pp_preds ppf preds =
        AddressStack.Set.iter
          (fun k-> fprintf ppf "%a" Dba_types.AddressStack.pp k) preds in
      fprintf ppf "@[<v 0>States@ ";
      AddressStack.Map.iter (
        fun (addr, stack, loop) (m, _flags, _equalities, preds)->
          match stack with
          | [] | [_] -> ()
          | stack ->
            fprintf ppf "@[* %a: preds={%a}@ %a@]@ "
              Dba_types.AddressStack.pp (addr, stack, loop)
              pp_preds preds
              State.pp m
      ) states;
      fprintf ppf "@]";
      flush_str_formatter ()
    end
    else ""


  let string_of_final_states states stops =
    let f elem acc = Format.asprintf "%a %s"
         Dba_types.AddressStack.pp elem acc in
    AddressStack.Map.fold (
      fun (a, _stack, _loop) (m, _flags, equalities, preds) acc ->
        if (Caddress.Set.mem a stops) then (
          Format.sprintf "%s\n⚑ Final state: pred={ %s }\n%s"
            acc
            (AddressStack.Set.fold f preds "")
            (State.to_string (m, equalities))
        )
        else acc
    ) states ""


  let set_analysis_outputs insts djmps l0 visited =
    let djmps =
      AddressStack.Map.fold (fun (addr, _stack, _loop) addrs acc ->
        let addrs' = try Caddress.Map.find addr acc with Not_found -> Caddress.Set.empty in
        Caddress.Map.add addr (Caddress.Set.union addrs addrs') acc
      ) djmps Caddress.Map.empty
    in
    let ast = Ast_builder.make insts djmps in
    let cs = Caddress.Map.empty in
    let l0 = Some l0 in
    let cfg_dba = Ast_builder.cfg_dba_of_ast ast l0 visited cs in
    let cfg_opcode = Ast_builder.cfg_opcode_of_ast ast l0 visited cs in
    let file_opcode = open_out_bin "cfg_opcode.dot" in
    let file_dba = open_out_bin "cfg_dba.dot" in
    Cfgraph.Dot.output_graph file_opcode cfg_opcode;
    Cfgraph.Dot.output_graph file_dba cfg_dba;
    close_out file_opcode;
    close_out file_dba;
    Display.pp_results ()


  let pp_stuff dynamic_jumps time =
    if Logger.get_debug_level () > 3 then begin
      Display.display (Display.Djmps dynamic_jumps);
      Display.display Display.Stats_flags;
      Display.display Display.Stats_equalities;
      Display.analysis_time.set time
    end

  let analyze start_address inits insts instsb instsr stops w d =
    Logger.info "Starting analysis ... @.";
    let time, (states, _, insts, djmps, visited) =
      Utils.time (fun () -> analyse start_address inits insts instsb instsr stops w d)
    in
    Logger.info "Analysis done";
    Logger.result "%s" (string_of_final_states states stops);
    Logger.result "%s" (to_string states);
    set_analysis_outputs insts djmps start_address visited;
    pp_stuff djmps time

end

let preprocess_and_analyze analyze program config =
  let open Infos in
  let stops = config.stops in
  let global_threshold, global_delay = config.global_widening in
  let local_thresholds = config.local_widening_thresholds in
  let local_delays = config.local_widening_delays in
  Concrete_eval.perm := fst program.permissions;
  Concrete_eval.permis := snd program.permissions;
  let global_thold =
    Infos.WideningThreshold.flatten_to_arrays_tuple global_threshold
  and local_thold =
    Dba_types.Caddress.Map.map
      Infos.WideningThreshold.flatten_to_arrays_tuple local_thresholds
  in
  let w = global_thold, local_thold in
  let d = global_delay, local_delays in
  analyze program.start_address program.initializations
    program.instructions config.prepend_stubs config.substitute_stubs
    stops w d


let run ?(dba_file=None) ~configuration_file () =
  let config = configuration_file |> Binsec_utils.read_optional_config_file in
  let program =
    match dba_file with
    | None -> Binsec_utils.load_dba_definition (Machine.ISA.get ())
    | Some filename -> Binsec_utils.read_dba_file filename
  in
  let (module State) =
    let open Ai_options in
    match Domain.get () with
    | Domain.TaintedKset ->
      (module Reduced_product.Make (Kset) (Taint): Ai_sigs.AbstractDomain)
    | Domain.Kset ->
      (module Nonrelational.Make(Kset): Ai_sigs.AbstractDomain)
    | Domain.Interval ->
      (module Nonrelational.Make(Range): Ai_sigs.AbstractDomain)
  in
  let module A = Make(State) in
  preprocess_and_analyze A.analyze program config
