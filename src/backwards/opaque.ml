(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

open Bw_options
open Format
;;

module IC = Instr_cfg ;;


module Opacity = struct
  type t =
    | Opaque
    | Clear
    | Unknown

  let pp ppf = function
    | Opaque  -> fprintf ppf "opaque"
    | Clear   -> fprintf ppf "clear"
    | Unknown -> fprintf ppf "unknown"

  let of_sat = function
    | Some Formula.SAT -> Clear
    | Some Formula.UNSAT -> Opaque
    | _ -> Unknown

  let is_clear o = o = Clear
  let is_opaque o = o = Opaque
end


module Opacity_status = struct

  type t = {
      c_op: Opacity.t; (* Opacity of consequent *)
      a_op: Opacity.t; (* Opacity of alternative *)
    }

  let default =
    { c_op = Opacity.Unknown;
      a_op = Opacity.Unknown; }

  let is_clear st = Opacity.is_clear st.c_op && Opacity.is_clear st.a_op ;;

  let may_clear st = Opacity.is_clear st.c_op || Opacity.is_clear st.a_op ;;

  let is_opaque st = Opacity.is_opaque st.c_op || Opacity.is_opaque st.a_op ;;

  let pp ppf st =
    fprintf ppf "@[<hov>then: %a;@ else: %a@]"
    Opacity.pp st.c_op Opacity.pp st.a_op
  ;;

  let pp_combined ppf st =
    let status =
      if is_opaque st then "opaque"
      else if may_clear st then "(may) clear"
      else "unknown" in
    pp_print_string ppf status
  ;;

  (* Use this function to combine two statuses on one predicate side
     from two different traces coming up to this predicate.
   *)
  let combine opa1 opa2 =
    let open Opacity in
    match opa1, opa2 with
    | _, Clear
    | Clear, _ -> Clear
    | Unknown, o
    | o, Unknown -> o
    | Opaque, Opaque -> Opaque

  let (<+>) st1 st2 =
    { c_op = combine st1.c_op st2.c_op;
      a_op = combine st1.a_op st2.a_op; }
  ;;

end ;;

module Trace = struct
  type t = Instruction.t list
  let empty = []
  let is_empty t = t = empty
  let cons i s = i :: s
  let _snoc i s = s @ [i]
  let _hd = List.hd
  let pop = function
    | x :: xs -> x, xs
    | [] -> failwith "empty trace"

  let peek = function
    | x :: _ -> x
    | [] -> failwith "empty trace"

  let iter = List.iter
  let _fold = List.fold_left
  let pp ppf t =
    pp_open_vbox ppf 0;
    iter
      (fun i ->
        fprintf ppf
          "%a: %a;@ "
          Virtual_address.pp i.Instruction.address
          Mnemonic.pp i.Instruction.mnemonic;
      ) t;
    pp_close_box ppf ()
end ;;

let assert_constraint st e =
  assert (Dba.Expr.size_of e = 1);
  let open Formula in
  let bv_term = Sse_smt.Translate.expr st e in
  let bv_true = mk_bv_cst Bitvector.one in
  mk_assert @@ mk_bv_comp BvEqual bv_term bv_true
;;

let add_assert_constraint st e =
  Sse_symbolic.State.add_entry (assert_constraint st e) st
;;

let add_deny_constraint st e = add_assert_constraint st (Dba.Expr.lognot e) ;;

let deny_constraint st e = assert_constraint st (Dba.Expr.lognot e) ;;

let with_solver k =
  let timeout = Formula_options.Solver.Timeout.get () in
  Sse_options.SMT_dir.set ".";
  let file = Sse_utils.temp_file () in
  let solver = Formula_options.Solver.get () in
  let session = Solver.Session.create ~file ~timeout solver in
  let res = k session in
  Solver.Session.destroy session;
  Some res

module Translator = struct
  type t = {
    hidx: Dhunk.Node.t; (* hunk index *)
    inst: Instruction.t;
    next : Trace.t;
    state : Sse_symbolic.State.t;
  }

  let create ?f tr =
    let open Sse_symbolic in
    let state = State.create () in
    let state =
      match f with
      | None -> state
      | Some g -> g state in

    let inst, next = Trace.pop tr in
    let hidx = Instruction.start inst in
    { hidx; inst; next; state; }

  let add_comment pp v t =
      let cmt_str =
        Formula_pp.pp_as_comment pp Format.str_formatter v;
        Format.flush_str_formatter ()
      in Sse_symbolic.State.comment cmt_str t.state

  let add_mnemonic inst t =
    add_comment Mnemonic.pp inst.Instruction.mnemonic t

  type result =
    | Script of Formula.bv_term option * Formula.formula
    | Continue of t

  let genr_formula t = Sse_symbolic.State.formula t.state

  let independant_formula t =
      Script (None, genr_formula t)

  let step t =
    if Trace.is_empty t.next then independant_formula t
    else begin
      let inst, next = Trace.pop t.next in
      Logger.debug ~level:2 "Continueing %@ %a" Instruction.pp inst;
      let hidx = Instruction.start inst in
      let state = add_mnemonic inst t in
      Continue { hidx; next; inst; state; }
      end

  let get_succs t =
    Dhunk.succ t.inst.Instruction.dba_block t.hidx

  let rec get_dba t =
    match Dhunk.Node.inst t.hidx with
    | Some dba -> dba
    | None -> (* Not sure this should happen *)
       match get_succs t with
       | [] -> Logger.fatal "No successors"
       | [hidx] ->
          get_dba {t with hidx;}
       | _ -> Logger.fatal "Too many successors"


  let pp ppf t =
    fprintf ppf
    "@[<v 0>Trace@ \
     @[<h>Node: %d@]@ \
     @[<hov 0>Instruction:@ %a@]@ \
     @[<hov 0>Next:@ %a@] \
     @]"
    (Dhunk.Node.id t.hidx)
    Instruction.pp t.inst
    Trace.pp t.next


  let next_is_addr addr t =
    not @@ Trace.is_empty t.next
    &&
    let vaddr = Dba_types.Caddress.to_virtual_address addr in
    let next_inst = Trace.peek t.next in
    Logger.debug "@[<v 0>Jumping to %a -> next in trace %a@ %a@]"
      Virtual_address.pp vaddr
      Virtual_address.pp next_inst.Instruction.address
      pp t;
    vaddr = next_inst.Instruction.address


  module Eval = struct

    let assign lval rval t =
      let state = Sse_smt.Translate.assign lval rval t.state in
      let nodes = get_succs t in
      match nodes with
      | [hidx] -> Continue { t with hidx; state }
      | [] -> assert false
      | _ -> assert false

    exception Broken_static_jump

    let sjump jt t =
      match jt with
      | Dba.JInner i ->
         begin
           match Dhunk.node t.inst.Instruction.dba_block i with
           | None -> assert false
           | Some hidx -> Continue { t with hidx; }
         end
      | Dba.JOuter addr ->
        if Trace.is_empty t.next || next_is_addr addr t then step t
        else raise Broken_static_jump

    let skip t =
      match get_succs t with
      | [] -> independant_formula t
      | [hidx] -> Continue { t with hidx; }
      | _ -> assert false

    let undef lv t =
      let open Dba.LValue in
      match lv with
      | Var { Dba.name; Dba.size = bitsize; _} ->
         begin
           let open Formula in
           let sort = bv_sort bitsize in
           let state = Sse_symbolic.State.declare name sort t.state in
           let nodes = get_succs t in
           match nodes with
           | [hidx] -> Continue { t with hidx; state; }
           | [] -> assert false
           | _ -> assert false
         end
      | Restrict _
      | Store _ -> Continue t

    let ite e jt _next_id t =
      (* Hypothesis here: all ites have an outside jump *)
      match jt with
      | Dba.JInner _ ->
         failwith
           "DBA expression has no outside jump. Hypothesis of ite is broken. \
            Aborting."
      | Dba.JOuter addr ->
         begin
           if next_is_addr addr t then add_assert_constraint t.state e
           else add_deny_constraint t.state e;
           step t
         end
  end

  let run t =
    let open Dba.Instr in

    let rec loop res =
      try
        match res with
        | Script (vset, fm) -> t, Some (vset, fm)
        | Continue t ->
           let dba = get_dba t in
           Logger.debug ~level:3 "Trace %@ %a" Dba_printer.Ascii.pp_instruction dba;
           do_dba t dba
      with e -> (* t, None *) raise e

    and do_dba t dba =
      let t =
        { t with state = add_comment Dba_printer.Ascii.pp_instruction dba t } in
      match dba with
      | Assign (lval, rval, _) -> Eval.assign lval rval t |> loop
      | SJump (jt, _) -> Eval.sjump jt t |> loop
      | If (e, jt, id) -> Eval.ite e jt id t |> loop
      | Undef (lv, _) -> Eval.undef lv t |> loop
      | DJump (_, Some Dba.Call addr) ->
         if Trace.is_empty t.next then independant_formula t |> loop
         else if next_is_addr addr t then Eval.skip t |> loop
         else begin
             Logger.warning "Aborting trace on dynamic call %a" pp t;
             t, None
           end
      | DJump (e, Some Dba.Return) ->
         assert (not @@ Trace.is_empty t.next);
         let bv_term = Sse_smt.Translate.expr t.state e in
         Script (Some bv_term, genr_formula t) |> loop

      | DJump (e, _) ->
         assert (not @@ Trace.is_empty t.next);
         let bv_term = Sse_smt.Translate.expr t.state e in
         Script (Some bv_term, genr_formula t) |> loop

      | dba ->
         Logger.fatal "Unhandled DBA %a" Dba_printer.Ascii.pp_instruction dba

    in loop (Continue t)
end


let init_mem ~filename symb_st =
  if not (Sys.file_exists filename) then symb_st
  else
    let initials =
      Logger.debug "Reading variable initialization from %s" filename;
      let parser = Parser.initialization
      and lexer = Lexer.token in
      Parse_utils.read_file ~parser ~lexer ~filename in
    let f syst init =
      let open Parse_helpers.Initialization in
      match init.operation with
      | Universal _ -> syst
      | Assignment(lval, Singleton rval, _) ->
        Sse_smt.Translate.assign lval rval syst
      | Assignment (lval, _, _) ->
         let msg =
           Format.asprintf "Unhandle deterministc assigment for %a"
             Dba_printer.EICAscii.pp_lhs lval in
         failwith msg
      | Mem_load(addr, size) ->
        Sse_symbolic.State.init_mem_at syst ~addr ~size
    in List.fold_left f symb_st initials
;;

let translate tr =
  let filename = Sse_options.MemoryFile.get () in
  let f = init_mem ~filename in
  let trsl = Translator.create ~f tr in
  Translator.run trsl
;;

let solve ~title f e t =
    with_solver
    (fun solver ->
      Solver.Session.put_entry solver (Formula.mk_comment title);
      (* This line below is way too hackish.
         There is something fishy going on in Sse_symbolic.
         My guess is that it's due to merging/path packing ....
         FIXME FIXME
       *)
      let state = Sse_symbolic.State.sync t.Translator.state in
      Logger.debug ~level:3 "@[<hov 0>Generating final assertion in state@ %a@]"
        Sse_symbolic.State.pp state;
      let assertion = f state e in

      let fml = Translator.genr_formula t in
      let append en = Solver.Session.put_entry solver en in
      Formula.iter_forward append fml;

      Solver.Session.put_entry solver
        (Formula.mk_comment "Is that an opaque predicate ?");
      Solver.Session.put_entry solver assertion;
      Logger.debug ~level:5
        "%a@\n%a@." Formula_pp.pp_formula fml Formula_pp.pp_entry assertion;
      Solver.Session.check_sat solver)
;;

let check_trace c tr =
  match translate tr with
  | _t, None -> Opacity_status.default
  | t, Some _ ->
     let e = c.Dhunk.condition in
     let c_sat = solve ~title:"consequent"  assert_constraint e t
     and a_sat = solve ~title:"alternative"  deny_constraint   e t in
     let open Opacity_status in
     let a_op = Opacity.of_sat a_sat
     and c_op = Opacity.of_sat c_sat in
     { a_op; c_op; }
  | exception Translator.Eval.Broken_static_jump ->
     Logger.warning "Could not determine status for instruction: defaulting ...";
     Opacity_status.default
;;


(* Does not work with loops let's assume they do not exist first*)
let backward_build_traces max_sz cfg v =
  let rec loop tr sz v =
    if sz >= max_sz then [tr]
    else
      let vs = IC.pred cfg v in
      let tr' =
        match IC.V.inst v with
        | None -> tr
        | Some i -> Trace.cons i tr in
      List_utils.flat_map (loop tr' (sz + 1)) vs
  in
  let vs = IC.pred cfg v in
  List_utils.flat_map (loop Trace.empty 0) vs
;;

let get_traces cfg v =
  let sz = Bw_options.K.get () in
  backward_build_traces sz cfg v
;;


module Check = struct
  type t = Opacity_status.t Virtual_address.Map.t ;;

  let predicate ~cfg c v =
    let traces = get_traces cfg v in
    let rec loop s = function
      | [] -> Ok s
      | t :: ts ->
         match check_trace c t with
         | s2 ->
            let s' = Opacity_status.(s <+> s2) in
            if Opacity_status.is_clear s' then Ok s' else loop s' ts
         | exception Failure _ -> Error v
    in loop Opacity_status.default traces
  ;;

  let instruction ~cfg inst v =
    let h = Instruction.hunk inst in
    match Dhunk.conditional h with
    | None -> Ok None
    | Some c ->
       match predicate ~cfg c v with
       | Ok s -> Ok (Some s)
       | Error e -> Error e
  ;;

  let vertex ~cfg v =
    match IC.V.inst v with
    | None -> None
    | Some inst ->
       Logger.debug ~level:4 "@[<h>%@%a@ :@ %a@]"
         Virtual_address.pp (Instruction.address inst)
         Mnemonic.pp (Instruction.mnemonic inst);
       match instruction ~cfg inst v with
       | Ok None -> None
       | Ok (Some ost as r) ->
          Logger.result "@[@[<h>Predicate %a@ %@ 0x%a is %a@]@ (%a)@]"
            Mnemonic.pp (Instruction.mnemonic inst)
            Virtual_address.pp (Instruction.address inst)
            Opacity_status.pp_combined ost
            Opacity_status.pp ost;
          r
       | Error _ ->
          Logger.warning "Could not evaluate predicate %a %@ 0x%a"
            Mnemonic.pp (Instruction.mnemonic inst)
            Virtual_address.pp (Instruction.address inst);
          None
  ;;

  let add_map map addr = function
    | None -> map
    | Some ost -> Virtual_address.Map.add addr ost map
  ;;

  let vertices ~cfg vs =
    List.fold_left
      (fun map v -> add_map map (IC.V.addr v) (vertex ~cfg v))
      Virtual_address.Map.empty
      vs
  ;;

  let addresses ~cfg vaddrs =
    List.fold_left
    (fun map va ->
      match IC.mem_vertex_a cfg va with
      | None -> map
      | Some v ->
         Logger.debug "@[<h>Checking opacity %@%a@]"
           Virtual_address.pp va;
         add_map map (IC.V.addr v) (vertex ~cfg v))
    Virtual_address.Map.empty
    vaddrs
  ;;

  let graph ~cfg =
    IC.fold_vertex
      (fun v map -> add_map map (IC.V.addr v) (vertex ~cfg v))
    cfg Virtual_address.Map.empty
  ;;

  let cfg_of_file ~filename =
    let open Disasm in
    let p = file ~filename in
    p.Program.instructions
  ;;

  let vaddrs () = Opaque_addresses.get () |> List.map Virtual_address.create ;;

  let cfg_of_sections sections =
    let img = Kernel_functions.get_img () in
    let p = Disasm.sections img sections in
    p.Disasm.Program.instructions
  ;;

  let sections_of_addresses img addrs =
    let section_name_of_addr address =
      let address = Virtual_address.to_int address in
      let s = Loader_utils.find_section_by_address_exn ~address img in
      Loader.Section.name s in
    List.fold_left
      (fun s a ->
        match section_name_of_addr a with
        | exception Not_found -> s
        | name -> Basic_types.String.Set.add name s
      ) Basic_types.String.Set.empty addrs
  ;;

  let subset () =
    let addrs = vaddrs () in
    let img = Kernel_functions.get_img () in
    let sections = sections_of_addresses img addrs in
    let cfg = cfg_of_sections sections in
    addresses ~cfg addrs
  ;;

  let sections secs =
    let cfg = cfg_of_sections secs in
    graph ~cfg
  ;;

  let file ~filename = graph ~cfg:(cfg_of_file ~filename) ;;

  let all () =
    if not @@ Kernel_options.ExecFile.is_set () then begin
        Logger.fatal "No file set for opaque predicates"
      end
    else
      let filename = Kernel_options.ExecFile.get () in
      Logger.info "Checking all predicates in %s for opacity" filename;
      file ~filename
  ;;

end
