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

module L = Llvm
let ctx = L.global_context();;  

type 'a llvm_state = {
  llcontext: L.llcontext;
  llmodule: L.llmodule;
  llbuilder: L.llbuilder;
  other: 'a
};;


module StringHashtbl = Hashtbl.Make(struct
    include String;;
    let equal s1 s2 = compare s1 s2 = 0 ;;
    let hash = Hashtbl.hash
  end 
  )
type llvm_registers = L.llvalue StringHashtbl.t
type other = {
  llregs: llvm_registers
}

module Boolean = struct
  let ar2 f = fun  a b (llst:other llvm_state) -> (f a b "" llst.llbuilder, llst)
  type boolean = L.llvalue
  let false_ llst = L.const_int (L.integer_type llst.llcontext 1) 0, llst
  let true_ llst = L.const_int (L.integer_type llst.llcontext 1) 1, llst
  let (&&) = ar2 L.build_and
  let (||) = ar2 L.build_or
  let not a llst = (L.build_not a "" llst.llbuilder,llst)

end

module Binary = struct
  type binary = L.llvalue
  type boolean = L.llvalue

  let biconst ~size i llst =
    assert(size <= 64);
    (L.const_of_int64 (L.integer_type llst.llcontext size)
       (Transfer_functions.Integer.int64_of_big_int i) false, llst)

  let ar2 f = fun ~size a b (llst:other llvm_state) ->
    let _ = size in (f a b "" llst.llbuilder, llst)
  
  let blshr  = ar2 L.build_lshr
  let bashr  = ar2 L.build_ashr
  let bshl = ar2 L.build_shl
  let biumod = ar2 L.build_urem
  let biudiv = ar2 L.build_udiv
  let bismod = ar2 L.build_srem
  let bisdiv = ar2 L.build_sdiv

  let buext ~size a llst =
    let newtype = L.integer_type  llst.llcontext size in
    L.build_zext a newtype "" llst.llbuilder, llst

  let bsext ~size a llst =
    let newtype = L.integer_type  llst.llcontext size in
    L.build_sext a newtype "" llst.llbuilder, llst

  
  let bxor = ar2 L.build_xor
  let bor = ar2 L.build_or
  let band = ar2 L.build_and

  let bextract ~size ~index ~oldsize a llst =
    assert(size > 0);
    let value =
      if index == 0 then a
      else
        let cst = (L.const_int (L.integer_type llst.llcontext oldsize) index) in
        L.build_lshr a cst "" llst.llbuilder
    in
    let new_type = L.integer_type llst.llcontext size in
    let value = L.build_trunc value new_type  "" llst.llbuilder in
    (value,llst)
  ;;

  let bconcat ~size1 ~size2 a b llst =
    let size = size1 + size2 in
    let llsize = (L.integer_type llst.llcontext size) in
    let exta = L.build_zext a llsize "" llst.llbuilder in
    let extb = L.build_zext b llsize "" llst.llbuilder in
    let shifta = L.build_shl exta (L.const_int llsize size2) "" llst.llbuilder in
    let res = L.build_or shifta extb "" llst.llbuilder in
    res, llst
  ;;

  let bpred _x = fun ~size a b llst ->
    let _ = size in
    (L.build_icmp L.Icmp.Eq a b "" llst.llbuilder,llst)
  
  let biult = bpred L.Icmp.Ult
  let biule = bpred L.Icmp.Ule
  let bislt = bpred L.Icmp.Slt
  let bisle = bpred L.Icmp.Sle
  let beq = bpred L.Icmp.Eq

  let bimul = ar2 L.build_mul
  let bisub = ar2 L.build_sub
  let biadd = ar2 L.build_add

end


(* Note: the `state' type can be completed with additional information
   (e.g. from a trace), notably to really resolve dynamic jumps. *)
module LLVM_Decoder = struct

  module Arg = struct

    module Boolean = Boolean
    module Binary = Binary

    module State = struct type t = other llvm_state end
    module M = Generic_decoder_sig.State_Monad(State)

    (* TODO: Make sure that endianness matches the one of LLVM. *)
    let load ~size _endianness address llst =
      let to_type = L.pointer_type @@ L.integer_type llst.llcontext size in
      let address = L.build_pointercast address to_type "" llst.llbuilder in      
      L.build_load address "" llst.llbuilder, llst
    ;;

    let store ~size _endianness address value llst =
      let to_type = L.pointer_type @@ L.integer_type llst.llcontext size in
      let address = L.build_pointercast address to_type "" llst.llbuilder in      
      ignore @@ L.build_store address value llst.llbuilder;
      llst
    ;;

    let var_to_address size name llst =
      let llregs = llst.other.llregs in
      try StringHashtbl.find llregs name
      with Not_found ->
        let lltype = (L.integer_type llst.llcontext size) in
        let value = L.declare_global lltype name llst.llmodule in
        StringHashtbl.replace llregs name value;
        value
    ;;

    let get_var ~size name llst =
      let address = var_to_address size name llst in
      let to_type = L.pointer_type @@ L.integer_type llst.llcontext size in
      let address = L.build_pointercast address to_type (name ^ "ptr") llst.llbuilder in
      (* assert(size < 64); *)
      (L.build_load address name llst.llbuilder,llst)
    ;;

    let set_var ~size name llvalue llst =
      let address = var_to_address size name llst in
      let to_type = L.pointer_type @@ L.integer_type llst.llcontext size in
      let address = L.build_pointercast address to_type (name ^ "ptr") llst.llbuilder in
      (* assert(size < 64); *)
      let _ = L.build_store llvalue address llst.llbuilder in
      llst
    ;;

    let select cond a b llst = L.build_select cond a b "" llst.llbuilder, llst
    let bool_of_bin x = M.return x
    let bin_of_bool x = M.return x
    type binary = L.llvalue
    type boolean = L.llvalue

    let unknown ~size llst = L.undef (L.integer_type llst.llcontext size), llst
    let undef = unknown

  end
  
  module Decode_Instr = Generic_decoder.Decode_Instr(Arg)

end


(**************** Application: LLVM pretty-printing of DBA blocks. ****************)

module Jump_Target = struct
  type t = L.llvalue Generic_decoder_sig.jump_target

  open Generic_decoder_sig

  let equal a b = match (a,b) with
    | Static a, Static b -> Pervasives.(=) a b
    | Dynamic _, Dynamic _ -> true
    | Static _, Dynamic _ | Dynamic _, Static _ -> false

  let hash = function
    | Dynamic _x -> 1
    | Static x -> 2 + Hashtbl.hash x
  
end

module Jump_Target_Hash = Hashtbl.Make(Jump_Target)

open Generic_decoder_sig;;
open Dba;;

module IdHash = Hashtbl.Make(struct let hash = Hashtbl.hash let equal = (==) type t = int end)

let get_jump_target jump_target_hash llst entry_block target =
    try Jump_Target_Hash.find jump_target_hash target
    with Not_found ->
      let name =
        match target with
        | Dynamic _ -> "dynamic"
        | Static(JInner id) -> "label" ^ (string_of_int id)
        | Static(JOuter address) -> Format.asprintf "address%a" Dba_types.Caddress.pp_base address
      in
      let block = L.insert_block llst.llcontext name entry_block in
      begin match target with
        | Static(JInner _) -> ()
        | _ ->
          let builder = L.builder_at_end llst.llcontext block in
          let _ = L.build_ret_void builder in
          ()
      end;
      Jump_Target_Hash.replace jump_target_hash target block;
      block
;;
  
let decode_dba block =
  let llcontext = L.global_context() in
  let llmodule = L.create_module ctx "mymodule" in

  (* Create a dummy function. *)
  let fty = L.function_type (L.void_type ctx) [| |] in
  let f = L.define_function "instr_decode" fty llmodule in
  let llbuilder = L.builder_at_end ctx (L.entry_block f) in

  let llregs = StringHashtbl.create 17 in
  let other = {llregs} in
  let llst = {llcontext;llmodule;llbuilder;other} in

  let jump_target_hash = Jump_Target_Hash.create 3 in
  let get_jump_target = get_jump_target jump_target_hash llst (L.entry_block f) in

  let unreachable = lazy (
    let block = L.insert_block llcontext "unreachable" (L.entry_block f) in
    let builder = L.builder_at_end ctx block in
    let _ = L.build_unreachable builder in
    block
  ) in

  (* Entry jumps to label 0 *)
  begin
    let llblock = get_jump_target (Static(JInner 0)) in
    let llbuilder = L.builder_at_end llst.llcontext (L.entry_block f) in
    let _ = L.build_br llblock llbuilder in
    ()
  end;

  Dba_types.Block.iteri (fun id inst ->
      let llblock = get_jump_target (Static(JInner id)) in
      let llbuilder = L.builder_at_end llst.llcontext llblock in
      let llst = { llst with llbuilder } in
      let succ,_llst = LLVM_Decoder.Decode_Instr.instruction llst inst in
      match succ with
      | JKJump target ->
        let llblock = get_jump_target target in
        let _ = L.build_br llblock llbuilder in ()
      | JKStop -> let _ = L.build_ret_void llbuilder in ()
      | JKIf(cond,target1,target2) ->
        let llblock1 = get_jump_target target1 in
        let llblock2 = get_jump_target target2 in
        let _ = L.build_cond_br cond llblock1 llblock2 llbuilder in
        ()
      | JKAssume(cond,target) ->
        let llblock = get_jump_target target in
        let _ = L.build_cond_br cond llblock (Lazy.force unreachable) in
        ()
    ) block;

  llst
;;

let pretty fmt block =
  let env = decode_dba block in
  let output = L.string_of_llmodule env.llmodule in
  Format.fprintf fmt "LLVM: %s\n" output
;;
