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

(* Xtrasec main algorithm, which consists in analyzing the sequence
   of instructions given by the pintool, and decode it into either a
   formula or a LLVM module. *)

(* Note: this function is x86-specific. *)
let register_to_size = function
  | "CF" -> 1
  | "DF" -> 1
  | "ZF" -> 1
  | "OF" -> 1
  | "SF" -> 1
  | "AF" -> 1
  | "PF" -> 1
  | "eax" -> 32
  | "ecx" -> 32
  | "edx" -> 32
  | "ebx" -> 32
  | "esp" -> 32
  | "ebp" -> 32
  | "esi" -> 32
  | "edi" -> 32
  | "mm0" -> 64
  | "mm1" -> 64
  | "mm2" -> 64
  | "mm3" -> 64
  | "mm4" -> 64
  | "mm5" -> 64
  | "mm6" -> 64
  | "mm7" -> 64
  | "st0" -> 80
  | "st1" -> 80
  | "st2" -> 80
  | "st3" -> 80
  | "st4" -> 80
  | "st5" -> 80
  | "st6" -> 80
  | "st7" -> 80
  | "xmm0" -> 128
  | "xmm1" -> 128
  | "xmm2" -> 128
  | "xmm3" -> 128
  | "xmm4" -> 128
  | "xmm5" -> 128
  | "xmm6" -> 128
  | "xmm7" -> 128
  | "fs" -> 16
  | "gs" -> 16
  | reg -> failwith ("register_to_size: unknown register " ^ reg)

(******** The algorithm is parametrized by the wanted output. ********)

module Make (Param : sig
  include Generic_decoder_sig.Instr_Input

  (* Says that we do not know anything after memory, for instance
     after a system call. *)
  val clear_memory : State.t -> State.t
end) : sig
  val parse : Param.State.t -> string -> Param.State.t
end = struct
  (* Adds the concretization informations to an existing Param.  *)
  module Param2 : sig
    include
      Generic_decoder_sig.Instr_Input
        with module State = Param.State
         and type boolean = Param.boolean
         and type binary = Param.binary

    val load_addr_info : Virtual_address.t option ref
    val store_addr_info : Virtual_address.t option ref
  end = struct
    include Param

    let load_addr_info = ref None
    let store_addr_info = ref None

    let add_assertion state address v =
      match Xtrasec_options.Concretize_mem.get () with
      | `No -> state
      | `Exact ->
          let v, state =
            Param.Binary.biconst ~size:32 (Virtual_address.to_bigint v) state
          in
          let assertion, state = Param.Binary.beq ~size:32 address v state in
          snd @@ Param.assume assertion state
      | `Approximate i ->
          let assertion_inf, state =
            (* Saturation if underflow. *)
            let addr = Z.of_int @@ max 0 @@ (-i + Virtual_address.to_int v) in
            let addr, state = Param.Binary.biconst ~size:32 addr state in
            Param.Binary.biule ~size:32 addr address state
          in
          let assertion_sup, state =
            (* Saturation if overflow *)
            let addr =
              Z.of_int @@ min ((1 lsl 32) - 1) @@ (i + Virtual_address.to_int v)
            in
            let addr, state = Param.Binary.biconst ~size:32 addr state in
            Param.Binary.biule ~size:32 address addr state
          in
          let assertion, state =
            Param.Boolean.( && ) assertion_inf assertion_sup state
          in
          snd @@ Param.assume assertion state

    let store ~size endian addr value state =
      let state =
        match !store_addr_info with
        | None -> state
        | Some v -> add_assertion state addr v
      in
      Param.store ~size endian addr value state

    let load ~size endian addr state =
      let state =
        match !load_addr_info with
        | None -> state
        | Some v -> add_assertion state addr v
      in
      Param.load ~size endian addr state
  end

  module M = Generic_decoder.Decode_Instr (Param2)

  (* The algorithm explore traces non-deterministically, and
     back-up when it finds out that we did not followed the
     correct trace (e.g. when the next instruction in the trace is
     incompatible with the destination of a branch.

     Note: This algorithm is problematic in the case when several
     paths in the DBA can lead to the same next instruction in the
     trace, which does not seem to happen on x86. A better
     algorithm should be using path-merging for these cases. *)
  type trace = Final_State of Param.State.t | Wrong_Trace

  (* Unhandled corresponds to instructions that cannot be decoded into
     DBA. *)
  type dhunk_type = Unhandled | Dhunk of Dhunk.t

  type enriched_ins = {
    (* The parsed instruction given by xtrasec. *)
    ins : Parsepin.ins;
    (* Normally, we realize that we are on a wrong trace when the
       final jump of an instruction does not match the address of the
       next instruction in the trace. Some instructions are handled
       specially (e.g.  we unroll instructions that have a REP
       prefix); in this case we do not perform the check. *)
    check_outgoing_edge : bool;
    (* Dhunk if present. *)
    dhunk : dhunk_type;
    (* Mnemonic, for debugging. *)
    mnemonic : string;
    (* Size of the instruction in bytes. *)
    size : int;
  }

  exception LastInstruction

  (* Wrapper around Parsepin.pop_ins to add the additional
     informations. *)
  let pop_enriched_ins x =
    match Parsepin.pop_ins x with
    | None -> raise LastInstruction
    | Some (ins, lp) -> (
        let open Parsepin in
        let { address = addr; code; _ } = ins in

        (* Remove rep and repne prefix; the xtrasec instrumentation
           repeats the instruction in the trace, so we known how many of
           them to put (i.e. we can unroll the rep precisely). *)
        let rep, code =
          if code.[0] = 'f' && (code.[1] == '2' || code.[1] == '3') then
            (true, String.sub code 2 (String.length code - 2))
          else (false, code)
        in
        (* We remove the rep prefix, so the ougoing edge may not be the
           right one, as the instruction may appear again. *)
        let check_outgoing_edge = not rep in
        try
          let inst, dhunk =
            X86toDba.decode_binstream ~base_addr:addr
              (Binstream.of_nibbles code)
          in
          let size = Size.Byte.to_int inst.X86Instruction.size in
          let mnemonic = Format.asprintf "%a" X86Instruction.pp_mnemonic inst in
          (* Xtrasec_options.Logger.result "parsed ins %d %x %s res %s"
             count (Virtual_address.to_int addr) code mnemonic; *)
          ({ ins; check_outgoing_edge; dhunk = Dhunk dhunk; size; mnemonic }, lp)
        with X86toDba.InstructionUnhandled _ ->
          (* Note: we cannot assume that these instructions jump to
             the next in sequence; this is not the case e.g. for
             sysenter instructions on Linux. *)
          (* let next_addr = Bitvector.create (Bigint.big_int_of_int
             ((Virtual_address.to_int addr) + (String.length code/2))) 32 in
           * Xtrasec_options.Logger.result "next_addr: %a"
             Bitvector.pp_hex next_addr; *)
          let mnemonic = "binsec_unhandled" in
          let size = String.length code / 2 in
          ({ ins; check_outgoing_edge; dhunk = Unhandled; size; mnemonic }, lp))

  (* Handle the next instruction. It must have been already parsed
     because of the edge checking, so:
     - it is passed as an argument here
     - lp points to the further instruction.
     - it has not been handled by acc yet. *)
  let rec do_next acc ins lp =
    let open Parsepin in
    let { count; address; code; _ } = ins.ins in
    let comment =
      Format.asprintf "ins %08d @0x%x %s %s" count
        (Virtual_address.to_int address)
        code ins.mnemonic
    in
    let acc = Param.add_comment comment acc in
    match ins with
    | { dhunk = Unhandled; _ } ->
        let acc = Param.clear_memory acc in
        let ins, lp = pop_enriched_ins lp in
        do_next acc ins lp
    | { dhunk = Dhunk dhunk; _ } as ins ->
        (* Provide the concretization informations to the
           decoder. Note that when there are several loads, we don't
           know which address correspond to which load, so we cannot
           make use of the information. *)
        let () =
          Param2.load_addr_info :=
            let open Parsepin in
            match ins.ins.Parsepin.mem_read with
            | Zero | Several -> None
            | One x -> Some x
        in
        let () =
          Param2.store_addr_info :=
            let open Parsepin in
            match ins.ins.Parsepin.mem_written with
            | Zero | Several -> None
            | One x -> Some x
        in

        (* Process DBA instruction i in dhunk. *)
        let rec do_dba_instr acc i =
          let instr =
            match Dhunk.inst dhunk i with None -> assert false | Some x -> x
          in
          let jt, acc = M.instruction acc instr in
          let open Generic_decoder_sig in
          match jt with
          | JKStop -> Final_State acc
          | JKAssume _ -> assert false
          | JKJump jt -> do_edge acc jt
          | JKIf (cond, targ1, targ2) -> (
              (* TODO: Some instructions really have two
                 behaviours. This should be handled with path merging,
                 or using the concretization to re-compute cond. *)
              let res1 =
                let (), acc = Param.assume cond acc in
                do_edge acc targ1
              in
              match res1 with
              | Final_State x -> Final_State x
              | Wrong_Trace ->
                  let ncond, acc = Param.Boolean.not cond acc in
                  let (), acc = Param.assume ncond acc in
                  do_edge acc targ2)
        (* Perform the necessary actions according to whether we are
           leaving [ins]. *)
        and do_edge acc =
          let open Generic_decoder_sig in
          function
          | Static (Dba.JInner i') -> do_dba_instr acc i'
          | Static (Dba.JOuter a) ->
              let acc = do_instruction_end acc ins in
              do_outer a acc ins lp
          | Dynamic x ->
              let acc = do_instruction_end acc ins in
              do_dynamic x acc ins lp
        in

        do_dba_instr acc 0
  (* (Dhunk.start dhunk) *)

  (* Modify the state at the end of the instruction. This is mainly to
     add assertions regarding concretization. *)
  and do_instruction_end acc ins =
    let add_assertion reg value acc =
      let size = register_to_size reg in
      let reg, acc = Param.get_var ~size reg acc in
      let value, acc =
        Param.Binary.biconst ~size (Virtual_address.to_bigint value) acc
      in
      let assertion, acc = Param.Binary.beq ~size reg value acc in
      let (), acc = Param.assume assertion acc in
      acc
    in

    let regs_to_concretize = Xtrasec_options.Concretize_regs.get () in
    List.fold_left
      (fun acc (reg, value) ->
        match reg with
        | "eflags" -> acc (* Need to be handled specially, ignored for now. *)
        | ("esp" | "ebp") when List.mem `Stack regs_to_concretize ->
            add_assertion reg value acc
        | _ when List.mem `All regs_to_concretize -> add_assertion reg value acc
        | reg when List.mem (`Register reg) regs_to_concretize ->
            add_assertion reg value acc
        | _ -> acc)
      acc ins.ins.Parsepin.reg_values

  (* Handle leaving the instruction with a static jump. *)
  and do_outer a acc ins lp =
    assert (a.Dba.id == 0);
    match pop_enriched_ins lp with
    | exception LastInstruction -> Final_State acc
    | ins', lp ->
        if
          ins.check_outgoing_edge
          && not (Virtual_address.equal a.Dba.base ins'.ins.Parsepin.address)
        then (
          Xtrasec_options.Logger.result "Wrong trace a.base %a addr'%x"
            Virtual_address.pp a.Dba.base
            (Virtual_address.to_int ins'.ins.Parsepin.address);
          Wrong_Trace)
        else do_next acc ins' lp

  (* Handle leaving the instruction with a dynamic jump. *)
  and do_dynamic x acc ins lp =
    match pop_enriched_ins lp with
    | exception LastInstruction -> Final_State acc
    | ins', lp ->
        let addr' = ins'.ins.Parsepin.address in
        let acc =
          if ins.check_outgoing_edge then
            (* Asserts that this should be a feasible jump. *)
            let addr', acc =
              Param.Binary.biconst ~size:32
                (Virtual_address.to_bigint addr')
                acc
            in
            let cond, acc = Param.Binary.beq ~size:32 x addr' acc in
            snd @@ Param.assume cond acc
          else acc
        in
        do_next acc ins' lp

  (* Main algorithm. *)
  let parse initial_state file =
    let lp = Parsepin.from file in
    let ins, lp = pop_enriched_ins lp in
    match do_next initial_state ins lp with
    | Wrong_Trace -> assert false
    | Final_State state -> state
end

(**************** Instantiation on Formulas and LLVM ****************)

module With_Formula = Formula_decoder.Instr_to_Formula
module Parse_formula = Make (With_Formula)

let run_formula input_file output_file =
  let state = Parse_formula.parse With_Formula.initial_state input_file in
  let out = open_out output_file in
  Format.fprintf
    (Format.formatter_of_out_channel out)
    "%a%a@\n(check-sat)@." Formula_pp.pp_header () Formula_pp.pp_formula
  @@ With_Formula.get_formula state;
  close_out out

let run input_file =
  if Xtrasec_options.Output_smt.is_set () then
    run_formula input_file @@ Xtrasec_options.Output_smt.get ()

let main () =
  if Xtrasec_options.is_enabled () then
    if not @@ Xtrasec_options.Trace_file.is_set () then
      failwith "Trace file must be given"
    else run @@ Xtrasec_options.Trace_file.get ()

let _ = Cli.Boot.enlist ~name:"xtrasec run" ~f:main
