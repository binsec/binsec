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

open Trace_type
open Common_piqi
open Decode_utils

let rec expand_flag (read_vars:Basic_types.String.Set.t) (write_vars:Basic_types.String.Set.t) (concret_info:trace_concrete_infos list): trace_concrete_infos list =
  let get_val fl value = X86Util.get_flag_value fl value in
  let flag_list = X86Types.([("OF",OF);("CF",CF);("ZF",ZF);("SF",SF);("DF",DF);("PF",PF)]) in
  match concret_info with
    [] -> concret_info
  | RegRead("eflags", value) :: q ->
    let flgs = List.fold_left (fun acc (name,v) ->
      if Basic_types.String.Set.mem name read_vars then RegRead(name,get_val v value)::acc else acc) [] flag_list in
    flgs@(expand_flag read_vars write_vars q)
  | RegWrite("eflags", value) :: q ->
    let flgs =
      List.fold_left
        (fun acc (name,v) ->
           if Basic_types.String.Set.mem name write_vars
           then RegWrite(name,get_val v value) :: acc else acc) [] flag_list in
    flgs@(expand_flag read_vars write_vars q)
  |  t::q -> t::(expand_flag read_vars write_vars q)


let the = Utils.unsafe_get_opt

let size_t_to_int64 (value:register_value_t): int64 =
  let open Register_value_t in
  match value.typeid with
  | `invalid_size -> Logger.warning "Invalid register size encountered in the trace, no sound analysis" ; 0x0L; 
  | `bit8 -> Int64.of_int32 (Int32.logand 0x000000FFl (the value.value_8))
  | `bit16 -> Int64.of_int32 (Int32.logand 0x0000FFFFl (the value.value_16))
  | `bit32 -> Int64.logand 0x00000000FFFFFFFFL (Int64.of_int32 (the value.value_32))
  | `bit64 -> the value.value_64
  | `bit80 -> Int64.of_string (the value.value_80)
  | `bit128 -> Int64.of_string (the value.value_128)
  | `bit256 -> Int64.of_string (the value.value_256)

let proto_size_t_to_int (typ:proto_size_t): int =
  match typ with
  | `invalid_size -> failwith "Invalid address encountered in the trace"
  | `bit8 -> 8
  | `bit16 -> 16
  | `bit32 -> 32
  | `bit64 -> 64
  | `bit80 -> 80
  | `bit128 -> 128
  | `bit256 -> 256

let read_header header: Trace_piqi.header_t_architecture_t * int =
  let arch = header.Trace_piqi.Header_t.architecture in
  let addr = proto_size_t_to_int (header.Trace_piqi.Header_t.address_size) in
  arch, addr

let read_metadata meta: metadata =
  let open Trace_piqi.Metadata_t in
  let open Trace_piqi.Metadata_t_exception_t in
  match meta.typeid with
  | `invalid_metadata -> failwith "Invalid metadata in trace"
  | `exception_type -> let excep = the meta.exception_metadata in
    Exception(excep.type_exception, excep.handler)
  | `module_type -> let modu = (the meta.module_metadata) in Module modu
  | `wave_type -> Layer (Int32.to_int (the meta.wave_metadata))

let read_register reg: string * int64 =
  let open Register_t in
  reg.name, size_t_to_int64 (reg.value)

let read_memory mem: int64 * string =
  let open Memory_t in
  mem.addr, mem.value

let read_concrete_infos info: trace_concrete_infos =
  let open Trace_piqi.Ins_con_info_t in
  match info.typeid with
  | `invalid -> Not_retrieved (* Not meant to occur *)
  | `regread -> let s,v = read_register (the info.read_register) in RegRead(s,v)
  | `regwrite -> let s,v = read_register (the info.write_register) in RegWrite(s,v)
  | `memload -> let a,v = read_memory (the info.load_memory) in MemLoad(a,v)
  | `memstore -> let a,v = read_memory (the info.store_memory) in MemStore(a,v)
  | `call -> Libcall (the info.call)
  | `syscall -> Syscall(the info.system_call)
  | `not_retrieved -> Not_retrieved
  | `comment -> Comment(the info.reserved_comment)
  | `next_address -> NextAddr(the info.next_address)
  | `wave -> Wave(the info.wave |> Int32.to_int)


let read_instr instr (_addr_size:int): trace_inst =
  let open Trace_piqi in
  let thread = instr.Instruction_t.thread_id in
  let location = instr.Instruction_t.address in
  let opcode = instr.Instruction_t.opcode in
  let concrete_infos =
    List.map read_concrete_infos instr.Instruction_t.concrete_infos in
  try
    let binstream = Basic_types.Binstream.of_bytes opcode in
    let binstr, dbainstrs = X86toDba.decode_string binstream location in
    let r_var, w_var = Dba_visitor.get_var_dbainstr dbainstrs in
    let dbainstrs = Dba_types.Block.to_dbainstrs dbainstrs (Dba_types.Virtual_address.of_int64 location) in
    let concrete_infos = expand_flag r_var w_var concrete_infos in
    let mnemonic =
      Format.asprintf "%a" X86Instruction.pp_mnemonic binstr in
    { thread; location; decoded=true;
      opcode=mnemonic; opcode_bytes=opcode;
      dbainstrs; concrete_infos}
  with X86toDba.InstructionUnhandled _ ->
    let s =
      String_utils.replace_chars
        (fun c -> Printf.sprintf "\\x%02x" (Char.code c )) opcode in
    Logger.warning "Not decoded %Lx: %s" location s;
    {thread; location; decoded=false;
     opcode=Printf.sprintf "[not decoded %s]" s; opcode_bytes=opcode;
     dbainstrs=[]; concrete_infos}


let _opts = Piqirun_ext.make_options () ~piq_frameless_input:true ~piq_frameless_output:true

let read_body_t (start_offset:int) body (addr_size:int): trace_inst_map * metadata_map =
  (* let offset = (List.length body)+start_offset-1 in *)
  let open Trace_piqi.Body_t in
  let insts,metas,_,_,_ =
    List.fold_left (fun (instrs,metas,off,next,is_libc) b ->
        match b.typeid with
        | `metadata ->
          let cur = if Basic_types.Int.Map.mem off metas then Basic_types.Int.Map.find off metas else [] in
          let newmetas = Basic_types.Int.Map.add off ((read_metadata (the b.metadata))::cur) metas in
          (instrs,newmetas,off, next,is_libc)
        | `instruction ->
          let instr = read_instr (the b.instruction) addr_size in
          if next <> 0L && next <> instr.location && not is_libc then
            Logger.warning "Break in instruction continuity (and not libcall):%d" off;
          let newinstrs = InstrMap.add off instr instrs in
          let is_libc = is_libcall instr.concrete_infos in
          let next =
            try get_next_address instr.concrete_infos
            with Not_found_in_concrete_infos _ -> next in
          (newinstrs, metas, off+1, next, is_libc)
      ) (InstrMap.empty,Basic_types.Int.Map.empty,start_offset, 0L,false) body
  in
  let insts =
  if !Options.natural_cond_enabled then
    Trace_postprocessing.merge_natural_conditions insts
  else insts in
  insts, metas

let read_i32 ic : int =
  let b = Bytes.create 4 in
  really_input ic b 0 4;
  Bigint.int_of_big_int (string_to_big_int (Bytes.to_string b))

let really_read ic size = Pervasives.really_input_string ic size

let sized_read ic : string =
  let sz = read_i32 ic in
  really_read ic sz

let parse_chunk ch =
  try
    let buf = Piqirun.init_from_string (sized_read ch) in
    Some (Trace_piqi.parse_chunk_t buf)
  with
  | End_of_file -> None
  | Piqirun.Error _ ->
    Logger.error "Broken chunk";
    None

let chunk_count = ref 0
let chunk_timestamp = ref (Unix.gettimeofday ())

let complete_partial_trace (trace:trace) chunk (keep_existing:bool) : trace =
  let open Trace_piqi.Chunk_t in
  let current_t = Unix.gettimeofday () in
  Logger.debug "New chunk.. %d (%.2f)" !chunk_count (current_t -. !chunk_timestamp);
  chunk_count := !chunk_count +1 ; chunk_timestamp := current_t;
  let max,_ =
    if InstrMap.is_empty trace.instrs then 0, empty_inst
    else InstrMap.max_binding trace.instrs in
  let new_instrs, new_metas = read_body_t (max+1) chunk.body trace.address_size in
  let merge_inst _ map1 map2 =
    match map1, map2 with
    | Some _, Some _ -> failwith "Overlapping map offset"
    | Some a, None | None, Some a -> Some a
    | None, None -> None in
  let merge_metas _ map1 map2 =
    match map1,map2 with
    | Some a, Some b -> Some (a @ b)
    | Some a, None | None, Some a -> Some a
    | None, None -> None
  in
  let new_instrs =
    if keep_existing then InstrMap.merge merge_inst trace.instrs new_instrs
    else new_instrs in
  let new_metas =
    if keep_existing then Basic_types.Int.Map.merge merge_metas trace.metadatas new_metas
    else new_metas in
  {trace with instrs=new_instrs; metadatas=new_metas}

let complete_all_partial_trace (trace:trace) ch : trace =
  let rec aux tr =
    if tr.complete then tr
    else
      match parse_chunk ch with
      | Some chk ->
        aux (complete_partial_trace tr chk true)
      | None -> {tr with complete=true}
  in
  aux trace

let load_partial_trace_from_file ?(load_all=false) ch: trace =
  let buf = Piqirun.init_from_string (sized_read ch) in
  let _arch, address_size = read_header (Trace_piqi.parse_header_t buf) in
  let chunk = parse_chunk ch in
  match chunk with
  | Some chunk ->
    let instrs, metas = read_body_t 0 chunk.Trace_piqi.Chunk_t.body address_size in
    let tr = {instrs; complete=false; metadatas=metas; address_size;} in
    if load_all then
      complete_all_partial_trace tr ch
    else
      tr
  | None -> failwith "Trace empty (no chunk found..)"

let load_partial_trace_from_string (header:string) (chunk:string) : trace =
  let buf = Piqirun.init_from_string header in
  let _arch, address_size = read_header (Trace_piqi.parse_header_t buf) in
  let buf = Piqirun.init_from_string chunk in
  let chunk = Trace_piqi.parse_chunk_t buf in
  let instrs, metas = read_body_t 0 chunk.Trace_piqi.Chunk_t.body address_size in
  {instrs; complete=false; metadatas=metas; address_size}





(* --------------- Printing functions ---------------- *)
let string_of_instr ?(with_conc=false) (instr:trace_inst) (addr_size:int): string =
  let addr64_to_32 (size:int) (addr:int64) =
    if size = 32 then Printf.sprintf "%lx" (Int64.to_int32 addr) else Printf.sprintf "%Lx" addr
  in
  let print_concrete acc x =
    match x with
    | NextAddr(addr) -> acc ^ (Printf.sprintf " next=%Lx" addr)
    | RegRead(name, value) -> acc ^ (Printf.sprintf " R[%s]=%Lx" name value)
    | RegWrite(name, value) -> acc ^ (Printf.sprintf " W[%s]=%Lx" name value)
    | Syscall _ ->
      begin "SYSCALL"
      (*         match sys with
                 | Stub -> acc ^ (Printf.sprintf " SYSCALL:Stub")
                 | Read(fd,addr,cnt,sz_read,data) -> acc ^ (Printf.sprintf " SYSCALL:Read(fd:%ld, @buff:%Lx, size:%d, size_read:%d, data:%S)" fd addr cnt sz_read data)
                 | Generic(addr,name) -> acc ^ (Printf.sprintf "SYSCALL:@[%ld]:%s" addr name)
      *)      end
    | MemLoad(addr, content)
    | MemStore(addr, content) ->
      let s = match x with | MemStore(_) -> "S" | MemLoad(_) -> "L" | _ -> "" in
      let data = if String.length content > 4 then Printf.sprintf "(raw)0x%s" (Decode_utils.string_to_hex content) else "0x"^(little_string_to_big_string content) in
      Printf.sprintf "%s %s@%s[%i]=%s" acc s (addr64_to_32 addr_size addr) (String.length content) data
    | Libcall(c) -> acc^" "^Libcall_stubs.libcall_to_string c
    | Not_retrieved -> acc ^ (Printf.sprintf " NONE")
    | Comment(s) -> acc ^ (Printf.sprintf "Comment:%s" s)
    | Wave n -> Printf.sprintf "%s Wave:%d" acc n
  in
  let conc_string = List.fold_left print_concrete "" instr.concrete_infos in
  let opcode = Decode_utils.string_to_hex ~with_space:true instr.opcode_bytes in
  let padding1 = String.make (let x = 15 - (String.length opcode) in if x < 0 then 5 else x) ' ' in
  let padding = String.make (let x = 35 - (String.length instr.opcode) in if x < 0 then 5 else x) ' ' in
  Printf.sprintf "%Lx t[%lx]    %s%s%s%s%s" instr.location instr.thread opcode padding1 instr.opcode padding (if with_conc then conc_string else "")

let exception_type_str s =
  match s with
  | 0x80000001l -> "EXCEPTION_GUARD_PAGE" | 0x80000002l -> "EXCEPTION_DATATYPE_MISALIGNMENT"
  | 0x80000003l -> "EXCEPTION_BREAKPOINT" | 0x80000004l -> "EXCEPTION_SINGLE_STEP"
  | 0xC0000009l -> "EXCEPTION_ACCESS_VIOLATION" | 0xC000008Cl -> "EXCEPTION_ARRAY_BOUNDS_EXCEEDED"
  | 0xC000008Dl -> "EXCEPTION_FLT_DENORMAL_OPERAND" | 0xC000008El -> "EXCEPTION_FLT_DIVIDE_BY_ZERO"
  | 0xC000008Fl -> "EXCEPTION_FLT_INEXACT_RESULT" | 0xC0000090l -> "EXCEPTION_FLT_INVALID_OPERATION"
  | 0xC0000005l -> "EXCEPTION_ACCESS_VIOLATION" | 0xC0000091l -> "EXCEPTION_FLT_OVERFLOW"
  | 0xC0000092l -> "EXCEPTION_FLT_STACK_CHECK" | 0xC0000093l -> "EXCEPTION_FLT_UNDERFLOW"
  | 0xC000001Dl -> "EXCEPTION_ILLEGAL_INSTRUCTION" | 0xC0000006l -> "EXCEPTION_IN_PAGE_ERROR"
  | 0xC0000094l -> "EXCEPTION_INT_DIVIDE_BY_ZERO" | 0xC0000095l -> "EXCEPTION_INT_OVERFLOW"
  | 0xC0000026l -> "EXCEPTION_INVALID_DISPOSITION" | 0xC0000025l -> "EXCEPTION_NONCONTINUABLE_EXCEPTION"
  | 0xC0000096l -> "EXCEPTION_PRIV_INSTRUCTION" | 0xc00000FDl -> "EXCEPTION_STACK_OVERFLOW"
  | 0x40010006l -> "DBG_PRINTEXCEPTION_C" | 0x40010007l -> "DBG_RIPEXCEPTION" | _ -> ""

let string_of_metadatas (meta: metadata): string =
  match meta with
  | Exception(typ, handler) -> Printf.sprintf "Exception %lx:%s at [%Lx]" typ (exception_type_str typ) handler
  | Module s -> Printf.sprintf "Module: %s" s
  | Layer i -> Printf.sprintf "============================= Wave %d ==============================" i


let print_instrs trace =
  InstrMap.iter
    (fun (key:int) (inst:trace_inst) ->
       if Basic_types.Int.Map.mem key trace.metadatas
       then
         List.iter
           (fun i -> Logger.debug "%s" (string_of_metadatas i))
           (Basic_types.Int.Map.find key trace.metadatas);
       Logger.result "%d %s" key (string_of_instr inst ~with_conc:true trace.address_size);
       List.iter(fun dbainst ->
        Logger.info ~level:1 "⇒ %a" Dba_types.Statement.pp dbainst) inst.dbainstrs
    ) trace.instrs

let print_trace trace_input =
  Logger.set_tagged_entry false;
  match trace_input with
  | Options.Chunked(chan,r_all) ->
    let rec print_recurs trace =
      print_instrs trace;
      if trace.complete then ()
      else begin
        Logger.debug ~level:1 "complete trace..";
        match parse_chunk chan with
        | Some chk ->
          print_recurs (complete_partial_trace trace chk false)
        | None -> ()
      end
    in
    let trace = load_partial_trace_from_file ~load_all:r_all chan in
    print_recurs trace;
  | Options.Stream _ -> Logger.error "Not implemented yet"


let rec get_load_content (address:int64) ?(esp_value=0L) (size:int) = function
  | [] ->
    Logger.error "Did not found address:%Lx in trace_concrete_infos" address;
    raise (Not_found_in_concrete_infos "Load addr content")
  | MemLoad(addr, data)::q ->
    let supp_bound = Int64.add address (Int64.of_int size) in
    let curr_bound = Int64.add addr (Int64.of_int (String.length data)) in
    if address = addr && supp_bound = curr_bound then
      data
    else if address >= addr && supp_bound <= curr_bound then
      let offset = Int64.to_int (Int64.sub address addr) in
      String.sub data offset size
    else
      get_load_content address ~esp_value size q
  | (RegRead("esp", value) :: _ ) as infos ->
    get_load_content address ~esp_value:value size infos
  | Libcall lib_t :: q ->
    let addr =
      if esp_value <> 0L then esp_value
      else
        try get_reg_value "esp" q
        with Not_found_in_concrete_infos _ ->
          Logger.warning "No esp found cannot serialize libcall params"; 0L
    in
    if addr = 0L then
      (* Just recurse do nothing with the libcall *)
      get_load_content address ~esp_value size q
    else (
      (* If we have a call, create a MemLoad (doing a lookup) and recurse *)
      let memload = MemLoad(addr, Libcall_stubs.serialize_stack_params lib_t) in
      Logger.debug ~level:1 "Create a fake MemLoad with address:%Lx" addr;
      get_load_content address ~esp_value size (memload :: q))

  | _ :: tl -> get_load_content address ~esp_value size tl
