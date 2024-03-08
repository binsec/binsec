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

module L = Llvm

let ctx = L.global_context ()

type 'a llvm_state = {
  llcontext : L.llcontext;
  llmodule : L.llmodule;
  llbuilder : L.llbuilder;
  other : 'a;
}

module StringHashtbl = Hashtbl.Make (struct
  include String

  let equal s1 s2 = compare s1 s2 = 0
  let hash = Hashtbl.hash
end)

type llvm_registers = L.llvalue StringHashtbl.t
type other = { llregs : llvm_registers; entry_block : L.llbasicblock }

module Boolean = struct
  let ar2 f a b (llst : other llvm_state) = (f a b "" llst.llbuilder, llst)

  type boolean = L.llvalue

  let false_ llst = (L.const_int (L.integer_type llst.llcontext 1) 0, llst)
  let true_ llst = (L.const_int (L.integer_type llst.llcontext 1) 1, llst)
  let ( && ) = ar2 L.build_and
  let ( || ) = ar2 L.build_or
  let not a llst = (L.build_not a "" llst.llbuilder, llst)
end

module Binary = struct
  type binary = L.llvalue
  type boolean = L.llvalue

  let biconst ~size i llst =
    assert (size <= 64);
    ( L.const_of_int64
        (L.integer_type llst.llcontext size)
        (Transfer_functions.Integer.to_int64 i)
        false,
      llst )

  let ar2 f ~size a b (llst : other llvm_state) =
    let _ = size in
    (f a b "" llst.llbuilder, llst)

  let blshr = ar2 L.build_lshr
  let bashr = ar2 L.build_ashr
  let bshl = ar2 L.build_shl
  let biurem = ar2 L.build_urem
  let biudiv = ar2 L.build_udiv
  let bisrem = ar2 L.build_srem
  let bisdiv = ar2 L.build_sdiv

  let buext ~size ~oldsize a llst =
    let _ = oldsize in
    let newtype = L.integer_type llst.llcontext size in
    (L.build_zext a newtype "" llst.llbuilder, llst)

  let bsext ~size ~oldsize a llst =
    let _ = oldsize in
    let newtype = L.integer_type llst.llcontext size in
    (L.build_sext a newtype "" llst.llbuilder, llst)

  let bxor = ar2 L.build_xor
  let bor = ar2 L.build_or
  let band = ar2 L.build_and

  let bextract ~lo ~hi ~oldsize a llst =
    let index = lo in
    let size = hi - lo + 1 in
    assert (size > 0);
    let value =
      if index == 0 then a
      else
        let cst = L.const_int (L.integer_type llst.llcontext oldsize) index in
        L.build_lshr a cst "" llst.llbuilder
    in
    let new_type = L.integer_type llst.llcontext size in
    let value = L.build_trunc value new_type "" llst.llbuilder in
    (value, llst)

  let bconcat ~size1 ~size2 a b llst =
    let size = size1 + size2 in
    let llsize = L.integer_type llst.llcontext size in
    let exta = L.build_zext a llsize "" llst.llbuilder in
    let extb = L.build_zext b llsize "" llst.llbuilder in
    let shifta =
      L.build_shl exta (L.const_int llsize size2) "" llst.llbuilder
    in
    let res = L.build_or shifta extb "" llst.llbuilder in
    (res, llst)

  let bpred _x ~size a b llst =
    let _ = size in
    (L.build_icmp L.Icmp.Eq a b "" llst.llbuilder, llst)

  let biult = bpred L.Icmp.Ult
  let biule = bpred L.Icmp.Ule
  let bislt = bpred L.Icmp.Slt
  let bisle = bpred L.Icmp.Sle
  let beq = bpred L.Icmp.Eq
  let bimul = ar2 L.build_mul
  let bisub = ar2 L.build_sub
  let biadd = ar2 L.build_add

  let bv_right_rotate ~size _ =
    let _ = size in
    assert false

  let bv_left_rotate ~size _ =
    let _ = size in
    assert false
end

(* Note: the `state' type can be completed with additional information
   (e.g. from a trace), notably to really resolve dynamic jumps. *)
module Instr_to_LLVM = struct
  module Boolean = Boolean
  module Binary = Binary

  module State = struct
    type t = other llvm_state
  end

  module M = Generic_decoder_sig.State_Monad (State)

  (* TODO: Make sure that endianness matches the one of LLVM. *)
  let load ~size _endianness address llst =
    let to_type = L.pointer_type @@ L.integer_type llst.llcontext size in
    let address = L.build_inttoptr address to_type "" llst.llbuilder in
    (L.build_load address "" llst.llbuilder, llst)

  let store ~size _endianness address value llst =
    let to_type = L.pointer_type @@ L.integer_type llst.llcontext size in
    let address = L.build_inttoptr address to_type "" llst.llbuilder in
    ignore @@ L.build_store value address llst.llbuilder;
    llst

  (* We store the contents of registers in global variables. *)
  let var_to_address size name llst =
    let llregs = llst.other.llregs in
    try StringHashtbl.find llregs name
    with Not_found ->
      let lltype = L.integer_type llst.llcontext size in
      let value = L.declare_global lltype name llst.llmodule in
      StringHashtbl.replace llregs name value;
      value

  let get_var ~size name llst =
    let address = var_to_address size name llst in
    (L.build_load address name llst.llbuilder, llst)

  let set_var ~size name llvalue llst =
    let address = var_to_address size name llst in
    let _ = L.build_store llvalue address llst.llbuilder in
    llst

  let ite cond a b llst = (L.build_select cond a b "" llst.llbuilder, llst)
  let bool_of_bin x = M.return x
  let bin_of_bool x = M.return x

  type binary = L.llvalue
  type boolean = L.llvalue

  let unknown ~size llst = (L.undef (L.integer_type llst.llcontext size), llst)
  let undef = unknown

  let assume bool llst =
    let assume_type =
      L.function_type
        (L.void_type llst.llcontext)
        [| L.i1_type llst.llcontext |]
    in
    let assumef = L.declare_function "llvm.assume" assume_type llst.llmodule in
    (* It is important not to give any name here. *)
    let _ = L.build_call assumef [| bool |] "" llst.llbuilder in
    ((), llst)

  let add_comment _string llst =
    (* In theory, we could add string metadata nodes to the
       following LLVM instructions. But the support in the LLVM
       OCaml bindings is lacking. *)
    llst
end

module Decode_Instr = Generic_decoder.Decode_Instr (Instr_to_LLVM)

let initial_state ~modname ~funname =
  let llcontext = L.global_context () in
  let llmodule = L.create_module ctx modname in

  let fty = L.function_type (L.void_type ctx) [||] in
  let f = L.define_function funname fty llmodule in
  let entry_block = L.entry_block f in
  let llbuilder = L.builder_at_end ctx entry_block in

  let llregs = StringHashtbl.create 17 in
  let other = { llregs; entry_block } in
  { llcontext; llmodule; llbuilder; other }

(**************** Application: LLVM pretty-printing of DBA blocks. ****************)

module Jump_Target = struct
  type t = L.llvalue Generic_decoder_sig.jump_target

  open Generic_decoder_sig

  let equal a b =
    match (a, b) with
    | Static a, Static b -> a = b
    | Dynamic _, Dynamic _ -> true
    | Static _, Dynamic _ | Dynamic _, Static _ -> false

  let hash = function Dynamic _x -> 1 | Static x -> 2 + Hashtbl.hash x
end

module Jump_Target_Hash = Hashtbl.Make (Jump_Target)
open Generic_decoder_sig
open Dba

let get_jump_target jump_target_hash llst entry_block target =
  try Jump_Target_Hash.find jump_target_hash target
  with Not_found ->
    let name =
      match target with
      | Dynamic _ -> "dynamic"
      | Static (JInner id) -> "label" ^ string_of_int id
      | Static (JOuter address) ->
          Format.asprintf "address%a" Dba_types.Caddress.pp_base address
    in
    let block = L.insert_block llst.llcontext name entry_block in
    (match target with
    | Static (JInner _) -> ()
    | _ ->
        let builder = L.builder_at_end llst.llcontext block in
        let _ = L.build_ret_void builder in
        ());
    Jump_Target_Hash.replace jump_target_hash target block;
    block

let decode_dba block =
  let llst = initial_state ~modname:"mymodule" ~funname:"instr_decode" in

  let jump_target_hash = Jump_Target_Hash.create 3 in
  let get_jump_target =
    get_jump_target jump_target_hash llst llst.other.entry_block
  in

  let unreachable =
    lazy
      (let block =
         L.insert_block llst.llcontext "unreachable" llst.other.entry_block
       in
       let builder = L.builder_at_end ctx block in
       let _ = L.build_unreachable builder in
       block)
  in

  (* Entry jumps to label 0 *)
  (let llblock = get_jump_target (Static (JInner 0)) in
   let llbuilder = L.builder_at_end llst.llcontext llst.other.entry_block in
   let _ = L.build_br llblock llbuilder in
   ());

  Dhunk.iteri
    ~f:(fun id inst ->
      let llblock = get_jump_target (Static (JInner id)) in
      let llbuilder = L.builder_at_end llst.llcontext llblock in
      let llst = { llst with llbuilder } in
      let succ, _llst = Decode_Instr.instruction llst inst in
      match succ with
      | JKJump target ->
          let llblock = get_jump_target target in
          let _ = L.build_br llblock llbuilder in
          ()
      | JKStop ->
          let _ = L.build_ret_void llbuilder in
          ()
      | JKIf (cond, target1, target2) ->
          let llblock1 = get_jump_target target1 in
          let llblock2 = get_jump_target target2 in
          let _ = L.build_cond_br cond llblock1 llblock2 llbuilder in
          ()
      | JKAssume (cond, target) ->
          let llblock = get_jump_target target in
          let _ =
            L.build_cond_br cond llblock (Lazy.force unreachable) llbuilder
          in
          ())
    block;

  llst

let pretty fmt block =
  let env = decode_dba block in
  let output = L.string_of_llmodule env.llmodule in
  Format.fprintf fmt "LLVM: %s\n" output

let generate_in_function ~modname ~funname f =
  let state = initial_state ~modname ~funname in
  let return_state = f state in
  let _ = L.build_ret_void return_state.llbuilder in
  return_state.llmodule

(** Generic options for disassembly *)

include Cli.Make (struct
  let name = "llvm binding"
  let shortname = "llvm"
end)

module Decode = Builder.String_option (struct
  let name = "decode"
  let doc = "Decode hexadecimal opcode as LLVM"
end)

module Output = Builder.String_option (struct
  let name = "xtrasec-output"
  let doc = "If set, output a llvm function to this file"
end)

let check_hex_string s =
  let open String_utils in
  match lfindi s (fun c -> not (is_hex_char c)) with
  | Some i ->
      Logger.fatal "Invalid hexadecimal character '%c' in opcode %s" s.[i] s
  | None -> ()

let inst_of_raw ?base raw =
  check_hex_string raw;
  Binstream.of_nibbles raw |> Disasm_core.decode_binstream ?base |> fst

let decode_llvm raw =
  try
    let i = inst_of_raw raw in
    Logger.result "%a" pretty i.Instruction.dba_block
  with X86toDba.InstructionUnhandled s ->
    Logger.warning "Not decoded %s" s;
    exit 1

let run_decode_llvm () = if Decode.is_set () then decode_llvm (Decode.get ())
let () = Cli.Boot.enlist ~name:"decode hex as llvm" ~f:run_decode_llvm

module Parse_llvm = Xtrasec.Make (struct
  include Instr_to_LLVM

  let clear_memory _ =
    failwith
      "Clearing memory (which happens e.g. when callingunknown functions) is \
       not implemented for LLVM"
end)

let run_llvm input_file output_file =
  let llm =
    generate_in_function ~modname:"xtrasec" ~funname:"decode_trace"
      (fun state -> Parse_llvm.parse state input_file)
  in
  Llvm.print_module output_file llm

let run input_file =
  if Output.is_set () then run_llvm input_file @@ Output.get ()

let main () =
  if Xtrasec_options.is_enabled () then
    if not @@ Xtrasec_options.Trace_file.is_set () then
      failwith "Trace file must be given"
    else run @@ Xtrasec_options.Trace_file.get ()

let _ = Cli.Boot.enlist ~name:"xtrasec llvm run" ~f:main
