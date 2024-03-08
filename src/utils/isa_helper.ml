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

module type ARCH = sig
  val get_defs : unit -> (string * Dba.LValue.t) list
  val get_return_address : unit -> Dba.Expr.t
  val get_arg : ?syscall:bool -> int -> Dba.Expr.t
  val get_ret : ?syscall:bool -> unit -> Dba.LValue.t
  val make_return : ?value:Dba.Expr.t -> unit -> Dhunk.t
  val get_stack_pointer : unit -> Dba.Var.t * Bitvector.t
  val get_shortlived_flags : unit -> Dba.Var.t list
  val get_dwarf_register : int -> Dba.Expr.t

  val core :
    Loader_elf.Img.t -> Virtual_address.t * (Dba.Var.t * Dba.Expr.t) list

  val max_instruction_len : Size.Byte.t
end

module X86 : ARCH = struct
  let info = Dba.Var.Tag.Register

  let eax = Dba.Var.create "eax" ~bitsize:Size.Bit.bits32 ~tag:info
  and ebx = Dba.Var.create "ebx" ~bitsize:Size.Bit.bits32 ~tag:info
  and ecx = Dba.Var.create "ecx" ~bitsize:Size.Bit.bits32 ~tag:info
  and edx = Dba.Var.create "edx" ~bitsize:Size.Bit.bits32 ~tag:info
  and edi = Dba.Var.create "edi" ~bitsize:Size.Bit.bits32 ~tag:info
  and esi = Dba.Var.create "esi" ~bitsize:Size.Bit.bits32 ~tag:info
  and esp = Dba.Var.create "esp" ~bitsize:Size.Bit.bits32 ~tag:info
  and ebp = Dba.Var.create "ebp" ~bitsize:Size.Bit.bits32 ~tag:info

  let cs = Dba.Var.create "cs" ~bitsize:Size.Bit.bits16 ~tag:info
  and ds = Dba.Var.create "ds" ~bitsize:Size.Bit.bits16 ~tag:info
  and es = Dba.Var.create "es" ~bitsize:Size.Bit.bits16 ~tag:info
  and fs = Dba.Var.create "fs" ~bitsize:Size.Bit.bits16 ~tag:info
  and gs = Dba.Var.create "gs" ~bitsize:Size.Bit.bits16 ~tag:info
  and gs_base = Dba.Var.create "gs_base" ~bitsize:Size.Bit.bits32 ~tag:info
  and ss = Dba.Var.create "ss" ~bitsize:Size.Bit.bits16 ~tag:info

  let stx =
    Array.init 8 (fun x ->
        Dba.Var.create (Format.sprintf "st%d" x) ~bitsize:Size.Bit.bits80
          ~tag:info)

  let mmx =
    Array.init 8 (fun x ->
        Dba.Var.create (Format.sprintf "mm%d" x) ~bitsize:Size.Bit.bits64
          ~tag:info)

  let xmmx =
    Array.init 8 (fun x ->
        Dba.Var.create (Format.sprintf "xmm%d" x) ~bitsize:Size.Bit.bits128
          ~tag:info)

  let xmm0 = Array.get xmmx 0
  and xmm1 = Array.get xmmx 1
  and xmm2 = Array.get xmmx 2
  and xmm3 = Array.get xmmx 3
  and xmm4 = Array.get xmmx 4
  and xmm5 = Array.get xmmx 5
  and xmm6 = Array.get xmmx 6
  and xmm7 = Array.get xmmx 7

  let info = Dba.Var.Tag.Flag

  let cf = Dba.Var.create "CF" ~bitsize:Size.Bit.bits1 ~tag:info
  and pf = Dba.Var.create "PF" ~bitsize:Size.Bit.bits1 ~tag:info
  and af = Dba.Var.create "AF" ~bitsize:Size.Bit.bits1 ~tag:info
  and zf = Dba.Var.create "ZF" ~bitsize:Size.Bit.bits1 ~tag:info
  and sf = Dba.Var.create "SF" ~bitsize:Size.Bit.bits1 ~tag:info
  and tf = Dba.Var.create "TF" ~bitsize:Size.Bit.bits1 ~tag:info
  and if' = Dba.Var.create "IF" ~bitsize:Size.Bit.bits1 ~tag:info
  and df = Dba.Var.create "DF" ~bitsize:Size.Bit.bits1 ~tag:info
  and of' = Dba.Var.create "OF" ~bitsize:Size.Bit.bits1 ~tag:info
  and iopl = Dba.Var.create "IOPL" ~bitsize:Size.Bit.bits2 ~tag:info
  and nt = Dba.Var.create "NT" ~bitsize:Size.Bit.bits1 ~tag:info
  and rf = Dba.Var.create "RF" ~bitsize:Size.Bit.bits1 ~tag:info
  and vm = Dba.Var.create "VM" ~bitsize:Size.Bit.bits1 ~tag:info
  and ac = Dba.Var.create "AC" ~bitsize:Size.Bit.bits1 ~tag:info
  and vif = Dba.Var.create "VIF" ~bitsize:Size.Bit.bits1 ~tag:info
  and vip = Dba.Var.create "VIP" ~bitsize:Size.Bit.bits1 ~tag:info
  and id = Dba.Var.create "ID" ~bitsize:Size.Bit.bits1 ~tag:info

  let defs =
    [
      ("eax", Dba.LValue.v eax);
      ("ebx", Dba.LValue.v ebx);
      ("ecx", Dba.LValue.v ecx);
      ("edx", Dba.LValue.v edx);
      ("edi", Dba.LValue.v edi);
      ("esi", Dba.LValue.v esi);
      ("esp", Dba.LValue.v esp);
      ("ebp", Dba.LValue.v ebp);
      ("al", Dba.LValue.restrict eax 0 7);
      ("ah", Dba.LValue.restrict eax 8 15);
      ("ax", Dba.LValue.restrict eax 0 15);
      ("bl", Dba.LValue.restrict ebx 0 7);
      ("bh", Dba.LValue.restrict ebx 8 15);
      ("bx", Dba.LValue.restrict ebx 0 15);
      ("cl", Dba.LValue.restrict ecx 0 7);
      ("ch", Dba.LValue.restrict ecx 8 15);
      ("cx", Dba.LValue.restrict ecx 0 15);
      ("dl", Dba.LValue.restrict ebx 0 7);
      ("dh", Dba.LValue.restrict ebx 8 15);
      ("dx", Dba.LValue.restrict ebx 0 15);
      ("di", Dba.LValue.restrict edi 0 15);
      ("si", Dba.LValue.restrict esi 0 15);
      ("sp", Dba.LValue.restrict esp 0 15);
      ("bp", Dba.LValue.restrict ebp 0 15);
      ("CF", Dba.LValue.v cf);
      ("PF", Dba.LValue.v pf);
      ("AF", Dba.LValue.v af);
      ("ZF", Dba.LValue.v zf);
      ("SF", Dba.LValue.v sf);
      ("TF", Dba.LValue.v tf);
      ("IF", Dba.LValue.v if');
      ("DF", Dba.LValue.v df);
      ("OF", Dba.LValue.v of');
      ("IOPL", Dba.LValue.v iopl);
      ("NT", Dba.LValue.v nt);
      ("RF", Dba.LValue.v rf);
      ("VM", Dba.LValue.v vm);
      ("AC", Dba.LValue.v ac);
      ("VIF", Dba.LValue.v vif);
      ("VIP", Dba.LValue.v vip);
      ("ID", Dba.LValue.v id);
      ("xmm0", Dba.LValue.restrict xmm0 0 127);
      ("xmm1", Dba.LValue.restrict xmm1 0 127);
      ("xmm2", Dba.LValue.restrict xmm2 0 127);
      ("xmm3", Dba.LValue.restrict xmm3 0 127);
      ("xmm4", Dba.LValue.restrict xmm4 0 127);
      ("xmm5", Dba.LValue.restrict xmm5 0 127);
      ("xmm6", Dba.LValue.restrict xmm6 0 127);
      ("xmm7", Dba.LValue.restrict xmm7 0 127);
    ]

  let shortlived_flags = [ cf; pf; af; zf; sf; of' ]
  let get_shortlived_flags () = shortlived_flags
  let get_defs () = defs

  let notes img =
    Array.fold_left
      (fun (entrypoint, defs) note ->
        match note with
        | { Loader_elf.Note.name = "CORE"; kind = 1; offset = at; _ } ->
            let cursor = Loader_elf.Img.cursor ~at img in
            Loader_buf.advance cursor 0x48;
            let rebx =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let recx =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let redx =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let resi =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let redi =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let rebp =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let reax =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let rds =
              Dba.Expr.constant
                (Bitvector.of_int ~size:16 (Loader_buf.Read.u32 cursor))
            in
            let res =
              Dba.Expr.constant
                (Bitvector.of_int ~size:16 (Loader_buf.Read.u32 cursor))
            in
            let rfs =
              Dba.Expr.constant
                (Bitvector.of_int ~size:16 (Loader_buf.Read.u32 cursor))
            in
            let rgs =
              Dba.Expr.constant
                (Bitvector.of_int ~size:16 (Loader_buf.Read.u32 cursor))
            in
            Loader_buf.advance cursor 4;
            let entrypoint =
              Virtual_address.create (Loader_buf.Read.u32 cursor)
            in
            let rcs =
              Dba.Expr.constant
                (Bitvector.of_int ~size:16 (Loader_buf.Read.u32 cursor))
            in
            let eflags = Loader_buf.Read.u32 cursor in
            let rcf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 0) land 0b1))
            and rpf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 2) land 0b1))
            and raf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 4) land 0b1))
            and rzf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 6) land 0b1))
            and rsf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 7) land 0b1))
            and rtf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 8) land 0b1))
            and rif =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 9) land 0b1))
            and rdf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 10) land 0b1))
            and rof =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 11) land 0b1))
            and riopl =
              Dba.Expr.constant
                (Bitvector.of_int ~size:2 ((eflags lsr 12) land 0b11))
            and rnt =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 14) land 0b1))
            and rrf =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 16) land 0b1))
            and rvm =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 17) land 0b1))
            and rac =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 18) land 0b1))
            and rvif =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 19) land 0b1))
            and rvip =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 20) land 0b1))
            and rid =
              Dba.Expr.constant
                (Bitvector.of_int ~size:1 ((eflags lsr 21) land 0b1))
            in
            let resp =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            let rss =
              Dba.Expr.constant
                (Bitvector.of_int ~size:16 (Loader_buf.Read.u32 cursor))
            in
            ( entrypoint,
              List.rev_append
                [
                  (ebx, rebx);
                  (ecx, recx);
                  (edx, redx);
                  (esi, resi);
                  (edi, redi);
                  (ebp, rebp);
                  (eax, reax);
                  (esp, resp);
                  (ds, rds);
                  (es, res);
                  (fs, rfs);
                  (gs, rgs);
                  (cs, rcs);
                  (ss, rss);
                  (cf, rcf);
                  (pf, rpf);
                  (af, raf);
                  (zf, rzf);
                  (sf, rsf);
                  (tf, rtf);
                  (if', rif);
                  (df, rdf);
                  (of', rof);
                  (iopl, riopl);
                  (nt, rnt);
                  (rf, rrf);
                  (vm, rvm);
                  (ac, rac);
                  (vif, rvif);
                  (vip, rvip);
                  (id, rid);
                ]
                defs )
        | { Loader_elf.Note.name = "CORE"; kind = 2; offset = at; _ } ->
            let cursor = Lreader.create ~at Loader_elf.read_offset img in
            Lreader.advance cursor 0xa0;
            ( entrypoint,
              List.fold_left
                (fun defs reg ->
                  (reg, Dba.Expr.constant (Lreader.Read.read cursor 16)) :: defs)
                defs
                [ xmm0; xmm1; xmm2; xmm3; xmm4; xmm5; xmm6; xmm7 ] )
        | { Loader_elf.Note.name = "PIN"; kind = 0; offset = at; _ } ->
            let cursor = Loader_elf.Img.cursor ~at img in
            let rgs_base =
              Dba.Expr.constant
                (Bitvector.of_int ~size:32 (Loader_buf.Read.u32 cursor))
            in
            (entrypoint, (gs_base, rgs_base) :: defs)
        | _ -> (entrypoint, defs))
      (Virtual_address.create 0, [])
      (Loader_elf.notes img)

  let core = notes
  let ret = Dba.LValue.v eax

  let esp_l = Dba.LValue.v esp
  and esp_r = Dba.Expr.v esp
  and eax_l = Dba.LValue.v eax
  and ebx_r = Dba.Expr.v ebx
  and ecx_r = Dba.Expr.v ecx
  and edx_r = Dba.Expr.v edx
  and esi_r = Dba.Expr.v esi
  and edi_r = Dba.Expr.v edi
  and ebp_r = Dba.Expr.v ebp
  and four = Dba.Expr.constant (Bitvector.of_int ~size:32 4)

  let return_address = Dba.Expr.load Size.Byte.four LittleEndian esp_r
  let get_return_address () = return_address

  let get_arg ?(syscall = false) i =
    if syscall then
      match i with
      | 0 -> ebx_r
      | 1 -> ecx_r
      | 2 -> edx_r
      | 3 -> esi_r
      | 4 -> edi_r
      | 5 -> ebp_r
      | _ -> raise (Invalid_argument "syscall")
    else
      Dba.Expr.load Size.Byte.four LittleEndian
        (Dba.Expr.add esp_r
           (Dba.Expr.constant (Bitvector.of_int ~size:32 (4 * (i + 1)))))

  let get_ret ?syscall:_ () = ret

  let void_return =
    Dhunk.init 2 (function
      | 0 -> Dba.Instr.assign esp_l (Dba.Expr.add esp_r four) 1
      | _ ->
          Dba.Instr.dynamic_jump ~tag:Return
            (Dba.Expr.load Size.Byte.four LittleEndian (Dba.Expr.sub esp_r four)))

  and val_return value =
    Dhunk.init 3 (function
      | 0 -> Dba.Instr.assign eax_l value 1
      | 1 -> Dba.Instr.assign esp_l (Dba.Expr.add esp_r four) 2
      | _ ->
          Dba.Instr.dynamic_jump ~tag:Return
            (Dba.Expr.load Size.Byte.four LittleEndian (Dba.Expr.sub esp_r four)))

  let make_return ?value () =
    match (value : Dba.Expr.t option) with
    | None -> void_return
    | Some (Var v) when Dba.Var.equal v eax -> void_return
    | Some value -> val_return value

  let get_stack_pointer () = (esp, Bitvector.of_int ~size:32 0xfff00000)
  let max_instruction_len = Size.Byte.fifteen

  let eax_r = Dba.Expr.v eax
  and eip_r = Dba.Expr.var "eip" 32
  (* note that eip is not used nor updated by the DBA semantics -- do not use it outside of debug information *)

  and es_r = Dba.Expr.v es
  and cs_r = Dba.Expr.v cs
  and ss_r = Dba.Expr.v ss
  and ds_r = Dba.Expr.v ds
  and fs_r = Dba.Expr.v fs
  and gs_r = Dba.Expr.v gs
  and stx_r = Array.map Dba.Expr.v stx
  and mmx_r = Array.map Dba.Expr.v mmx
  and xmmx_r = Array.map Dba.Expr.v xmmx

  let get_dwarf_register = function
    | 0 -> eax_r
    | 1 -> ecx_r
    | 2 -> edx_r
    | 3 -> ebx_r
    | 4 -> esp_r
    | 5 -> ebp_r
    | 6 -> esi_r
    | 7 -> edi_r
    | 8 -> eip_r
    | (11 | 12 | 13 | 14 | 15 | 16 | 17 | 18) as x -> Array.get stx_r (x - 11)
    | (21 | 22 | 23 | 24 | 25 | 26 | 27 | 28) as x -> Array.get xmmx_r (x - 21)
    | (29 | 30 | 31 | 32 | 33 | 34 | 35 | 36) as x -> Array.get mmx_r (x - 29)
    | 40 -> es_r
    | 41 -> cs_r
    | 42 -> ss_r
    | 43 -> ds_r
    | 44 -> fs_r
    | 45 -> gs_r
    | n ->
        Kernel_options.Logger.fatal
          "unable to map integer %d to a known expression" n
end

module AMD64 : ARCH = struct
  let info = Dba.Var.Tag.Register

  let rax = Dba.Var.create "rax" ~bitsize:Size.Bit.bits64 ~tag:info
  and rbx = Dba.Var.create "rbx" ~bitsize:Size.Bit.bits64 ~tag:info
  and rcx = Dba.Var.create "rcx" ~bitsize:Size.Bit.bits64 ~tag:info
  and rdx = Dba.Var.create "rdx" ~bitsize:Size.Bit.bits64 ~tag:info
  and rdi = Dba.Var.create "rdi" ~bitsize:Size.Bit.bits64 ~tag:info
  and rsi = Dba.Var.create "rsi" ~bitsize:Size.Bit.bits64 ~tag:info
  and rsp = Dba.Var.create "rsp" ~bitsize:Size.Bit.bits64 ~tag:info
  and rbp = Dba.Var.create "rbp" ~bitsize:Size.Bit.bits64 ~tag:info
  and r8 = Dba.Var.create "r8" ~bitsize:Size.Bit.bits64 ~tag:info
  and r9 = Dba.Var.create "r9" ~bitsize:Size.Bit.bits64 ~tag:info
  and r10 = Dba.Var.create "r10" ~bitsize:Size.Bit.bits64 ~tag:info
  and r11 = Dba.Var.create "r11" ~bitsize:Size.Bit.bits64 ~tag:info
  and r12 = Dba.Var.create "r12" ~bitsize:Size.Bit.bits64 ~tag:info
  and r13 = Dba.Var.create "r13" ~bitsize:Size.Bit.bits64 ~tag:info
  and r14 = Dba.Var.create "r14" ~bitsize:Size.Bit.bits64 ~tag:info
  and r15 = Dba.Var.create "r15" ~bitsize:Size.Bit.bits64 ~tag:info

  let cs = Dba.Var.create "cs" ~bitsize:Size.Bit.bits16 ~tag:info
  and ds = Dba.Var.create "ds" ~bitsize:Size.Bit.bits16 ~tag:info
  and es = Dba.Var.create "es" ~bitsize:Size.Bit.bits16 ~tag:info
  and fs = Dba.Var.create "fs" ~bitsize:Size.Bit.bits16 ~tag:info
  and gs = Dba.Var.create "gs" ~bitsize:Size.Bit.bits16 ~tag:info
  and ss = Dba.Var.create "ss" ~bitsize:Size.Bit.bits16 ~tag:info
  and fs_base = Dba.Var.create "fs_base" ~bitsize:Size.Bit.bits64 ~tag:info
  and gs_base = Dba.Var.create "gs_base" ~bitsize:Size.Bit.bits64 ~tag:info

  let ymm0 = Dba.Var.create "ymm0" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm1 = Dba.Var.create "ymm1" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm2 = Dba.Var.create "ymm2" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm3 = Dba.Var.create "ymm3" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm4 = Dba.Var.create "ymm4" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm5 = Dba.Var.create "ymm5" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm6 = Dba.Var.create "ymm6" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm7 = Dba.Var.create "ymm7" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm8 = Dba.Var.create "ymm8" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm9 = Dba.Var.create "ymm9" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm10 = Dba.Var.create "ymm10" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm11 = Dba.Var.create "ymm11" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm12 = Dba.Var.create "ymm12" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm13 = Dba.Var.create "ymm13" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm14 = Dba.Var.create "ymm14" ~bitsize:Size.Bit.bits256 ~tag:info
  and ymm15 = Dba.Var.create "ymm15" ~bitsize:Size.Bit.bits256 ~tag:info

  let info = Dba.Var.Tag.Flag

  let cf = Dba.Var.create "cf" ~bitsize:Size.Bit.bits1 ~tag:info
  and pf = Dba.Var.create "pf" ~bitsize:Size.Bit.bits1 ~tag:info
  and af = Dba.Var.create "af" ~bitsize:Size.Bit.bits1 ~tag:info
  and zf = Dba.Var.create "zf" ~bitsize:Size.Bit.bits1 ~tag:info
  and sf = Dba.Var.create "sf" ~bitsize:Size.Bit.bits1 ~tag:info
  and tf = Dba.Var.create "tf" ~bitsize:Size.Bit.bits1 ~tag:info
  and if' = Dba.Var.create "if" ~bitsize:Size.Bit.bits1 ~tag:info
  and df = Dba.Var.create "df" ~bitsize:Size.Bit.bits1 ~tag:info
  and of' = Dba.Var.create "of" ~bitsize:Size.Bit.bits1 ~tag:info
  and iopl = Dba.Var.create "iopl" ~bitsize:Size.Bit.bits2 ~tag:info
  and nt = Dba.Var.create "nt" ~bitsize:Size.Bit.bits1 ~tag:info
  and rf = Dba.Var.create "rf" ~bitsize:Size.Bit.bits1 ~tag:info
  and vm = Dba.Var.create "vm" ~bitsize:Size.Bit.bits1 ~tag:info
  and ac = Dba.Var.create "ac" ~bitsize:Size.Bit.bits1 ~tag:info
  and vif' = Dba.Var.create "vif" ~bitsize:Size.Bit.bits1 ~tag:info
  and vip = Dba.Var.create "vip" ~bitsize:Size.Bit.bits1 ~tag:info
  and id = Dba.Var.create "id" ~bitsize:Size.Bit.bits1 ~tag:info

  let defs =
    [
      ("rax", Dba.LValue.v rax);
      ("rbx", Dba.LValue.v rbx);
      ("rcx", Dba.LValue.v rcx);
      ("rdx", Dba.LValue.v rdx);
      ("rdi", Dba.LValue.v rdi);
      ("rsi", Dba.LValue.v rsi);
      ("rsp", Dba.LValue.v rsp);
      ("rbp", Dba.LValue.v rbp);
      ("r8", Dba.LValue.v r8);
      ("r9", Dba.LValue.v r9);
      ("r10", Dba.LValue.v r10);
      ("r11", Dba.LValue.v r11);
      ("r12", Dba.LValue.v r12);
      ("r13", Dba.LValue.v r13);
      ("r14", Dba.LValue.v r14);
      ("r15", Dba.LValue.v r15);
      ("eax", Dba.LValue.restrict rax 0 31);
      ("ebx", Dba.LValue.restrict rbx 0 31);
      ("ecx", Dba.LValue.restrict rcx 0 31);
      ("edx", Dba.LValue.restrict rdx 0 31);
      ("edi", Dba.LValue.restrict rdi 0 31);
      ("esi", Dba.LValue.restrict rsi 0 31);
      ("esp", Dba.LValue.restrict rsp 0 31);
      ("ebp", Dba.LValue.restrict rbp 0 31);
      ("r8d", Dba.LValue.restrict r8 0 31);
      ("r9d", Dba.LValue.restrict r9 0 31);
      ("r10d", Dba.LValue.restrict r10 0 31);
      ("r11d", Dba.LValue.restrict r11 0 31);
      ("r12d", Dba.LValue.restrict r12 0 31);
      ("r13d", Dba.LValue.restrict r13 0 31);
      ("r14d", Dba.LValue.restrict r14 0 31);
      ("r15d", Dba.LValue.restrict r15 0 31);
      ("al", Dba.LValue.restrict rax 0 7);
      ("ah", Dba.LValue.restrict rax 8 15);
      ("ax", Dba.LValue.restrict rax 0 15);
      ("bl", Dba.LValue.restrict rbx 0 7);
      ("bh", Dba.LValue.restrict rbx 8 15);
      ("bx", Dba.LValue.restrict rbx 0 15);
      ("cl", Dba.LValue.restrict rcx 0 7);
      ("ch", Dba.LValue.restrict rcx 8 15);
      ("cx", Dba.LValue.restrict rcx 0 15);
      ("dl", Dba.LValue.restrict rbx 0 7);
      ("dh", Dba.LValue.restrict rbx 8 15);
      ("dx", Dba.LValue.restrict rbx 0 15);
      ("dil", Dba.LValue.restrict rdi 0 7);
      ("di", Dba.LValue.restrict rdi 0 15);
      ("sil", Dba.LValue.restrict rsi 0 7);
      ("si", Dba.LValue.restrict rsi 0 15);
      ("spl", Dba.LValue.restrict rsp 0 7);
      ("sp", Dba.LValue.restrict rsp 0 15);
      ("bpl", Dba.LValue.restrict rbp 0 7);
      ("bp", Dba.LValue.restrict rbp 0 15);
      ("r8w", Dba.LValue.restrict r8 0 31);
      ("r9w", Dba.LValue.restrict r9 0 31);
      ("r10w", Dba.LValue.restrict r10 0 15);
      ("r11w", Dba.LValue.restrict r11 0 15);
      ("r12w", Dba.LValue.restrict r12 0 15);
      ("r13w", Dba.LValue.restrict r13 0 15);
      ("r14w", Dba.LValue.restrict r14 0 15);
      ("r15w", Dba.LValue.restrict r15 0 15);
      ("r8b", Dba.LValue.restrict r8 0 7);
      ("r9b", Dba.LValue.restrict r9 0 7);
      ("r10b", Dba.LValue.restrict r10 0 7);
      ("r11b", Dba.LValue.restrict r11 0 7);
      ("r12b", Dba.LValue.restrict r12 0 7);
      ("r13b", Dba.LValue.restrict r13 0 7);
      ("r14b", Dba.LValue.restrict r14 0 7);
      ("r15b", Dba.LValue.restrict r15 0 7);
      ("cs", Dba.LValue.v cs);
      ("ds", Dba.LValue.v ds);
      ("es", Dba.LValue.v es);
      ("fs", Dba.LValue.v fs);
      ("gs", Dba.LValue.v gs);
      ("ss", Dba.LValue.v ss);
      ("fs_base", Dba.LValue.v fs_base);
      ("gs_base", Dba.LValue.v gs_base);
      ("ymm0", Dba.LValue.v ymm0);
      ("ymm1", Dba.LValue.v ymm1);
      ("ymm2", Dba.LValue.v ymm2);
      ("ymm3", Dba.LValue.v ymm3);
      ("ymm4", Dba.LValue.v ymm4);
      ("ymm5", Dba.LValue.v ymm5);
      ("ymm6", Dba.LValue.v ymm6);
      ("ymm7", Dba.LValue.v ymm7);
      ("ymm8", Dba.LValue.v ymm8);
      ("ymm9", Dba.LValue.v ymm9);
      ("ymm10", Dba.LValue.v ymm10);
      ("ymm11", Dba.LValue.v ymm11);
      ("ymm12", Dba.LValue.v ymm12);
      ("ymm13", Dba.LValue.v ymm13);
      ("ymm14", Dba.LValue.v ymm14);
      ("ymm15", Dba.LValue.v ymm15);
      ("xmm0", Dba.LValue.restrict ymm0 0 127);
      ("xmm1", Dba.LValue.restrict ymm1 0 127);
      ("xmm2", Dba.LValue.restrict ymm2 0 127);
      ("xmm3", Dba.LValue.restrict ymm3 0 127);
      ("xmm4", Dba.LValue.restrict ymm4 0 127);
      ("xmm5", Dba.LValue.restrict ymm5 0 127);
      ("xmm6", Dba.LValue.restrict ymm6 0 127);
      ("xmm7", Dba.LValue.restrict ymm7 0 127);
      ("xmm8", Dba.LValue.restrict ymm8 0 127);
      ("xmm9", Dba.LValue.restrict ymm9 0 127);
      ("xmm10", Dba.LValue.restrict ymm10 0 127);
      ("xmm11", Dba.LValue.restrict ymm11 0 127);
      ("xmm12", Dba.LValue.restrict ymm12 0 127);
      ("xmm13", Dba.LValue.restrict ymm13 0 127);
      ("xmm14", Dba.LValue.restrict ymm14 0 127);
      ("xmm15", Dba.LValue.restrict ymm15 0 127);
      ("cf", Dba.LValue.v cf);
      ("pf", Dba.LValue.v pf);
      ("af", Dba.LValue.v af);
      ("zf", Dba.LValue.v zf);
      ("sf", Dba.LValue.v sf);
      ("tf", Dba.LValue.v tf);
      ("if", Dba.LValue.v if');
      ("df", Dba.LValue.v df);
      ("of", Dba.LValue.v of');
      ("iopl", Dba.LValue.v iopl);
      ("nt", Dba.LValue.v nt);
      ("rf", Dba.LValue.v rf);
      ("vm", Dba.LValue.v vm);
      ("ac", Dba.LValue.v ac);
      ("vif", Dba.LValue.v vif');
      ("vip", Dba.LValue.v vip);
      ("id", Dba.LValue.v id);
    ]

  let shortlived_flags = [ cf; pf; af; zf; sf; of' ]
  let get_shortlived_flags () = shortlived_flags
  let get_defs () = defs

  let ymmx =
    [
      ymm0;
      ymm1;
      ymm2;
      ymm3;
      ymm4;
      ymm5;
      ymm6;
      ymm7;
      ymm8;
      ymm9;
      ymm10;
      ymm11;
      ymm12;
      ymm13;
      ymm14;
      ymm15;
    ]

  let notes img =
    let entrypoint, defs, lymmx, hymmx =
      Array.fold_left
        (fun ((entrypoint, defs, lymmx, hymmx) as result) -> function
          | { Loader_elf.Note.name = "CORE"; kind = 1; offset = at; _ } ->
              let cursor = Lreader.create ~at Loader_elf.read_offset img in
              Lreader.advance cursor 0x70;
              let vr15 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr14 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr13 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr12 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrbp = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrbx = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr11 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr10 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr9 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vr8 = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrax = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrcx = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrdx = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrsi = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vrdi = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              Lreader.advance cursor 8;
              let vrip =
                Virtual_address.of_bitvector (Lreader.Read.bv64 cursor)
              in
              let vcs =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:16 (Lreader.Read.u16 cursor))
              in
              Lreader.advance cursor 6;
              let rflags_15_0 = Lreader.Read.u16 cursor in
              let rflags_31_16 = Lreader.Read.u16 cursor in
              Lreader.advance cursor 4;
              let vrsp = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vss =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:16 (Lreader.Read.u16 cursor))
              in
              Lreader.advance cursor 6;
              let vfs_base = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vgs_base = Dba.Expr.constant (Lreader.Read.bv64 cursor) in
              let vds =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:16 (Lreader.Read.u16 cursor))
              in
              Lreader.advance cursor 6;
              let ves =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:16 (Lreader.Read.u16 cursor))
              in
              Lreader.advance cursor 6;
              let vfs =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:16 (Lreader.Read.u16 cursor))
              in
              Lreader.advance cursor 6;
              let vgs =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:16 (Lreader.Read.u16 cursor))
              in
              Lreader.advance cursor 6;
              let vcf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 0) land 0b1))
              and vpf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 2) land 0b1))
              and vaf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 4) land 0b1))
              and vzf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 6) land 0b1))
              and vsf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 7) land 0b1))
              and vtf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 8) land 0b1))
              and vif =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 9) land 0b1))
              and vdf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 10) land 0b1))
              and vof =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 11) land 0b1))
              and viopl =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:2 ((rflags_15_0 lsr 12) land 0b11))
              and vnt =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 14) land 0b1))
              and vrf =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_31_16 lsr 0) land 0b1))
              and vvm =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_31_16 lsr 1) land 0b1))
              and vac =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_31_16 lsr 2) land 0b1))
              and vvif =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_31_16 lsr 3) land 0b1))
              and vvip =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_31_16 lsr 4) land 0b1))
              and vid =
                Dba.Expr.constant
                  (Bitvector.of_int ~size:1 ((rflags_31_16 lsr 5) land 0b1))
              in
              ( vrip,
                List.rev_append
                  [
                    (r15, vr15);
                    (r14, vr14);
                    (r13, vr13);
                    (r12, vr12);
                    (r11, vr11);
                    (r10, vr10);
                    (r9, vr9);
                    (r8, vr8);
                    (rbx, vrbx);
                    (rcx, vrcx);
                    (rdx, vrdx);
                    (rsi, vrsi);
                    (rdi, vrdi);
                    (rbp, vrbp);
                    (rax, vrax);
                    (rsp, vrsp);
                    (ds, vds);
                    (es, ves);
                    (fs, vfs);
                    (gs, vgs);
                    (cs, vcs);
                    (ss, vss);
                    (fs_base, vfs_base);
                    (gs_base, vgs_base);
                    (cf, vcf);
                    (pf, vpf);
                    (af, vaf);
                    (zf, vzf);
                    (sf, vsf);
                    (tf, vtf);
                    (if', vif);
                    (df, vdf);
                    (of', vof);
                    (iopl, viopl);
                    (nt, vnt);
                    (rf, vrf);
                    (vm, vvm);
                    (ac, vac);
                    (vif', vvif);
                    (vip, vvip);
                    (id, vid);
                  ]
                  defs,
                lymmx,
                hymmx )
          | { Loader_elf.Note.name = "CORE"; kind = 2; offset = at; _ } ->
              let cursor = Lreader.create ~at Loader_elf.read_offset img in
              Lreader.advance cursor 0xa0;
              ( entrypoint,
                defs,
                List.map
                  (fun _ -> Dba.Expr.constant (Lreader.Read.read cursor 16))
                  ymmx,
                hymmx )
          | { Loader_elf.Note.name = "LINUX"; kind = 0x202; offset = at; _ } ->
              let cursor = Lreader.create ~at Loader_elf.read_offset img in
              Lreader.advance cursor 0x200;
              let component = Lreader.Peek.u16 cursor in
              let hymmx =
                if component land 0b00000100 <> 0 then (
                  Lreader.advance cursor 0x40;
                  List.map
                    (fun _ -> Dba.Expr.constant (Lreader.Read.read cursor 16))
                    ymmx)
                else hymmx
              in
              let lymmx =
                if component land 0b00000010 <> 0 then (
                  let cursor = Lreader.create ~at Loader_elf.read_offset img in
                  Lreader.advance cursor 0xa0;
                  List.map
                    (fun _ -> Dba.Expr.constant (Lreader.Read.read cursor 16))
                    ymmx)
                else lymmx
              in
              (entrypoint, defs, lymmx, hymmx)
          | _ -> result)
        (Virtual_address.create 0, [], [], [])
        (Loader_elf.notes img)
    in
    let vymmx =
      match (hymmx, lymmx) with
      | [], [] -> []
      | _, [] -> List.map (fun h -> Dba.Expr.(append h (zeros 128))) hymmx
      | [], _ -> List.map (fun l -> Dba.Expr.uext 256 l) lymmx
      | _, _ -> List.map2 (fun h l -> Dba.Expr.append h l) hymmx lymmx
    in
    ( entrypoint,
      if vymmx <> [] then
        List.fold_left2 (fun defs r v -> (r, v) :: defs) defs ymmx vymmx
      else defs )

  let core = notes
  let ret = Dba.LValue.v rax

  let rsp_l = Dba.LValue.v rsp
  and rsp_r = Dba.Expr.v rsp
  and rax_l = Dba.LValue.v rax
  and rdi_r = Dba.Expr.v rdi
  and rsi_r = Dba.Expr.v rsi
  and rdx_r = Dba.Expr.v rdx
  and rcx_r = Dba.Expr.v rcx
  and r8_r = Dba.Expr.v r8
  and r9_r = Dba.Expr.v r9
  and r10_r = Dba.Expr.v r10
  and eight = Dba.Expr.constant (Bitvector.of_int ~size:64 8)

  let return_address = Dba.Expr.load Size.Byte.eight LittleEndian rsp_r
  let get_return_address () = return_address

  let get_arg ?(syscall = false) i =
    if syscall then
      match i with
      | 0 -> rdi_r
      | 1 -> rsi_r
      | 2 -> rdx_r
      | 3 -> r10_r
      | 4 -> r8_r
      | 5 -> r9_r
      | _ -> raise (Invalid_argument "syscall")
    else
      match i with
      | 0 -> rdi_r
      | 1 -> rsi_r
      | 2 -> rdx_r
      | 3 -> rcx_r
      | 4 -> r8_r
      | 5 -> r9_r
      | i ->
          Dba.Expr.load Size.Byte.eight LittleEndian
            (Dba.Expr.add rsp_r
               (Dba.Expr.constant (Bitvector.of_int ~size:64 (8 * (i - 5)))))

  let get_ret ?syscall:_ () = ret

  let void_return =
    Dhunk.init 2 (function
      | 0 -> Dba.Instr.assign rsp_l (Dba.Expr.add rsp_r eight) 1
      | _ ->
          Dba.Instr.dynamic_jump ~tag:Return
            (Dba.Expr.load Size.Byte.eight LittleEndian
               (Dba.Expr.sub rsp_r eight)))

  and val_return value =
    Dhunk.init 3 (function
      | 0 -> Dba.Instr.assign rax_l value 1
      | 1 -> Dba.Instr.assign rsp_l (Dba.Expr.add rsp_r eight) 2
      | _ ->
          Dba.Instr.dynamic_jump ~tag:Return
            (Dba.Expr.load Size.Byte.eight LittleEndian
               (Dba.Expr.sub rsp_r eight)))

  let make_return ?value () =
    match (value : Dba.Expr.t option) with
    | None -> void_return
    | Some (Var v) when Dba.Var.equal v rax -> void_return
    | Some value -> val_return value

  let get_stack_pointer () = (rsp, Bitvector.of_int64 0x7fff000000000000L)
  let get_dwarf_register _ = Errors.not_yet_implemented "AMD64 dwarf mapping"
  let max_instruction_len = Size.Byte.fifteen
end

module ARM : ARCH = struct
  let info = Dba.Var.Tag.Register

  let rx =
    Array.init 11 (fun i ->
        Dba.Var.create (Format.sprintf "r%d" i) ~bitsize:Size.Bit.bits32
          ~tag:info)

  let r0 = Array.get rx 0
  and r1 = Array.get rx 1
  and r2 = Array.get rx 2
  and r3 = Array.get rx 3
  and r4 = Array.get rx 4
  and r5 = Array.get rx 5
  and r6 = Array.get rx 6
  and r7 = Array.get rx 7
  and r8 = Array.get rx 8
  and r9 = Array.get rx 9
  and r10 = Array.get rx 10
  and r11 = Dba.Var.create "fp" ~bitsize:Size.Bit.bits32 ~tag:info
  and r12 = Dba.Var.create "ip" ~bitsize:Size.Bit.bits32 ~tag:info
  and r13 = Dba.Var.create "sp" ~bitsize:Size.Bit.bits32 ~tag:info
  and r14 = Dba.Var.create "lr" ~bitsize:Size.Bit.bits32 ~tag:info
  and r15 = Dba.Var.create "pc" ~bitsize:Size.Bit.bits32 ~tag:info
  (* note that pc is not used nor updated by the DBA semantics -- do not use it outside of debug information *)

  let info = Dba.Var.Tag.Flag

  let n = Dba.Var.create "n" ~bitsize:Size.Bit.bits1 ~tag:info
  and z = Dba.Var.create "z" ~bitsize:Size.Bit.bits1 ~tag:info
  and c = Dba.Var.create "c" ~bitsize:Size.Bit.bits1 ~tag:info
  and v = Dba.Var.create "v" ~bitsize:Size.Bit.bits1 ~tag:info
  and t = Dba.Var.create "t" ~bitsize:Size.Bit.bits1 ~tag:info

  let defs =
    [
      ("r0", Dba.LValue.v r0);
      ("r1", Dba.LValue.v r1);
      ("r2", Dba.LValue.v r2);
      ("r3", Dba.LValue.v r3);
      ("r4", Dba.LValue.v r4);
      ("r5", Dba.LValue.v r5);
      ("r6", Dba.LValue.v r6);
      ("r7", Dba.LValue.v r7);
      ("r8", Dba.LValue.v r8);
      ("r9", Dba.LValue.v r9);
      ("r10", Dba.LValue.v r10);
      ("r11", Dba.LValue.v r11);
      ("r12", Dba.LValue.v r12);
      ("r13", Dba.LValue.v r13);
      ("r14", Dba.LValue.v r14);
      ("r15", Dba.LValue.v r15);
      ("fp", Dba.LValue.v r11);
      ("ip", Dba.LValue.v r12);
      ("sp", Dba.LValue.v r13);
      ("lr", Dba.LValue.v r14);
      ("pc", Dba.LValue.v r15);
      ("n", Dba.LValue.v n);
      ("z", Dba.LValue.v z);
      ("c", Dba.LValue.v c);
      ("v", Dba.LValue.v v);
      ("t", Dba.LValue.v t);
    ]

  let get_shortlived_flags () = []
  let core _ = Errors.not_yet_implemented "arm core"
  let get_defs () = defs

  let r0_l = Dba.LValue.v r0
  and r0_r = Dba.Expr.v r0
  and r1_r = Dba.Expr.v r1
  and r2_r = Dba.Expr.v r2
  and r3_r = Dba.Expr.v r3
  and r13_r = Dba.Expr.v r13
  and r14_r = Dba.Expr.v r14
  and r15_r = Dba.Expr.v r15

  let ret = r0_l
  let get_return_address () = r14_r

  let get_arg ?(syscall = false) i =
    if syscall then Errors.not_yet_implemented "syscall";
    match i with
    | 0 -> r0_r
    | 1 -> r1_r
    | 2 -> r2_r
    | 3 -> r3_r
    | i ->
        Dba.Expr.load Size.Byte.four LittleEndian
          (Dba.Expr.add r13_r
             (Dba.Expr.constant (Bitvector.of_int ~size:32 (4 * (i - 4)))))

  let get_ret ?syscall:_ () = ret

  let void_return = Dhunk.singleton (Dba.Instr.dynamic_jump ~tag:Return r14_r)

  and val_return value =
    Dhunk.init 2 (function
      | 0 -> Dba.Instr.assign r0_l value 1
      | _ -> Dba.Instr.dynamic_jump ~tag:Return r14_r)

  let make_return ?value () =
    match (value : Dba.Expr.t option) with
    | None -> void_return
    | Some (Var v) when Dba.Var.equal v r0 -> void_return
    | Some value -> val_return value

  let get_stack_pointer () = (r13, Bitvector.of_int ~size:32 0xfff00000)

  let rx_r = Array.map Dba.Expr.v rx
  and sl_r = Dba.Expr.v r10
  and fp_r = Dba.Expr.v r11
  and ip_r = Dba.Expr.v r12

  let get_dwarf_register = function
    | (0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9) as n -> Array.get rx_r n
    | 10 -> sl_r
    | 11 -> fp_r
    | 12 -> ip_r
    | 13 -> r13_r
    | 14 -> r14_r
    | 15 -> r15_r
    | n ->
        Kernel_options.Logger.fatal
          "unable to map integer %d to a known expression" n

  let max_instruction_len = Size.Byte.four
end

module AARCH64 : ARCH = struct
  let info = Dba.Var.Tag.Register

  let rx =
    Array.init 31 (fun i ->
        Dba.Var.create (Format.sprintf "x%d" i) ~bitsize:Size.Bit.bits64
          ~tag:info)

  let x0 = Array.get rx 0
  and x1 = Array.get rx 1
  and x2 = Array.get rx 2
  and x3 = Array.get rx 3
  and x4 = Array.get rx 4
  and x5 = Array.get rx 5
  and x6 = Array.get rx 6
  and x7 = Array.get rx 7
  and x8 = Array.get rx 8
  and x9 = Array.get rx 9
  and x10 = Array.get rx 10
  and x11 = Array.get rx 11
  and x12 = Array.get rx 12
  and x13 = Array.get rx 13
  and x14 = Array.get rx 14
  and x15 = Array.get rx 15
  and x16 = Array.get rx 16
  and x17 = Array.get rx 17
  and x18 = Array.get rx 18
  and x19 = Array.get rx 19
  and x20 = Array.get rx 20
  and x21 = Array.get rx 21
  and x22 = Array.get rx 22
  and x23 = Array.get rx 23
  and x24 = Array.get rx 24
  and x25 = Array.get rx 25
  and x26 = Array.get rx 26
  and x27 = Array.get rx 27
  and x28 = Array.get rx 28
  and x29 = Array.get rx 29
  and x30 = Array.get rx 30
  and sp = Dba.Var.create "sp" ~bitsize:Size.Bit.bits64 ~tag:info

  let info = Dba.Var.Tag.Flag

  let n = Dba.Var.create "n" ~bitsize:Size.Bit.bits1 ~tag:info
  and z = Dba.Var.create "z" ~bitsize:Size.Bit.bits1 ~tag:info
  and c = Dba.Var.create "c" ~bitsize:Size.Bit.bits1 ~tag:info
  and v = Dba.Var.create "v" ~bitsize:Size.Bit.bits1 ~tag:info
  and t = Dba.Var.create "t" ~bitsize:Size.Bit.bits1 ~tag:info

  let defs =
    [
      ("r0", Dba.LValue.v x0);
      ("r1", Dba.LValue.v x1);
      ("r2", Dba.LValue.v x2);
      ("r3", Dba.LValue.v x3);
      ("r4", Dba.LValue.v x4);
      ("r5", Dba.LValue.v x5);
      ("r6", Dba.LValue.v x6);
      ("r7", Dba.LValue.v x7);
      ("r8", Dba.LValue.v x8);
      ("r9", Dba.LValue.v x9);
      ("r10", Dba.LValue.v x10);
      ("r11", Dba.LValue.v x11);
      ("r12", Dba.LValue.v x12);
      ("r13", Dba.LValue.v x13);
      ("r14", Dba.LValue.v x14);
      ("r15", Dba.LValue.v x15);
      ("r16", Dba.LValue.v x16);
      ("r17", Dba.LValue.v x17);
      ("r18", Dba.LValue.v x18);
      ("r19", Dba.LValue.v x19);
      ("r20", Dba.LValue.v x20);
      ("r21", Dba.LValue.v x21);
      ("r22", Dba.LValue.v x22);
      ("r23", Dba.LValue.v x23);
      ("r24", Dba.LValue.v x24);
      ("r25", Dba.LValue.v x25);
      ("r26", Dba.LValue.v x26);
      ("r27", Dba.LValue.v x27);
      ("r28", Dba.LValue.v x28);
      ("r29", Dba.LValue.v x29);
      ("r30", Dba.LValue.v x30);
      ("x0", Dba.LValue.v x0);
      ("x1", Dba.LValue.v x1);
      ("x2", Dba.LValue.v x2);
      ("x3", Dba.LValue.v x3);
      ("x4", Dba.LValue.v x4);
      ("x5", Dba.LValue.v x5);
      ("x6", Dba.LValue.v x6);
      ("x7", Dba.LValue.v x7);
      ("x8", Dba.LValue.v x8);
      ("x9", Dba.LValue.v x9);
      ("x10", Dba.LValue.v x10);
      ("x11", Dba.LValue.v x11);
      ("x12", Dba.LValue.v x12);
      ("x13", Dba.LValue.v x13);
      ("x14", Dba.LValue.v x14);
      ("x15", Dba.LValue.v x15);
      ("x16", Dba.LValue.v x16);
      ("x17", Dba.LValue.v x17);
      ("x18", Dba.LValue.v x18);
      ("x19", Dba.LValue.v x19);
      ("x20", Dba.LValue.v x20);
      ("x21", Dba.LValue.v x21);
      ("x22", Dba.LValue.v x22);
      ("x23", Dba.LValue.v x23);
      ("x24", Dba.LValue.v x24);
      ("x25", Dba.LValue.v x25);
      ("x26", Dba.LValue.v x26);
      ("x27", Dba.LValue.v x27);
      ("x28", Dba.LValue.v x28);
      ("x29", Dba.LValue.v x29);
      ("x30", Dba.LValue.v x30);
      ("sp", Dba.LValue.v sp);
      ("w0", Dba.LValue.restrict x0 0 31);
      ("w1", Dba.LValue.restrict x1 0 31);
      ("w2", Dba.LValue.restrict x2 0 31);
      ("w3", Dba.LValue.restrict x3 0 31);
      ("w4", Dba.LValue.restrict x4 0 31);
      ("w5", Dba.LValue.restrict x5 0 31);
      ("w6", Dba.LValue.restrict x6 0 31);
      ("w7", Dba.LValue.restrict x7 0 31);
      ("w8", Dba.LValue.restrict x8 0 31);
      ("w9", Dba.LValue.restrict x9 0 31);
      ("w10", Dba.LValue.restrict x10 0 31);
      ("w11", Dba.LValue.restrict x11 0 31);
      ("w12", Dba.LValue.restrict x12 0 31);
      ("w13", Dba.LValue.restrict x13 0 31);
      ("w14", Dba.LValue.restrict x14 0 31);
      ("w15", Dba.LValue.restrict x15 0 31);
      ("w16", Dba.LValue.restrict x16 0 31);
      ("w17", Dba.LValue.restrict x17 0 31);
      ("w18", Dba.LValue.restrict x18 0 31);
      ("w19", Dba.LValue.restrict x19 0 31);
      ("w20", Dba.LValue.restrict x20 0 31);
      ("w21", Dba.LValue.restrict x21 0 31);
      ("w22", Dba.LValue.restrict x22 0 31);
      ("w23", Dba.LValue.restrict x23 0 31);
      ("w24", Dba.LValue.restrict x24 0 31);
      ("w25", Dba.LValue.restrict x25 0 31);
      ("w26", Dba.LValue.restrict x26 0 31);
      ("w27", Dba.LValue.restrict x27 0 31);
      ("w28", Dba.LValue.restrict x28 0 31);
      ("w29", Dba.LValue.restrict x29 0 31);
      ("w30", Dba.LValue.restrict x30 0 31);
      ("n", Dba.LValue.v n);
      ("z", Dba.LValue.v z);
      ("c", Dba.LValue.v c);
      ("v", Dba.LValue.v v);
      ("t", Dba.LValue.v t);
    ]

  let get_shortlived_flags () = []
  let core _ = Errors.not_yet_implemented "arm core"
  let get_defs () = defs

  let x0_l = Dba.LValue.v x0
  and x0_r = Dba.Expr.v x0
  and x1_r = Dba.Expr.v x1
  and x2_r = Dba.Expr.v x2
  and x3_r = Dba.Expr.v x3
  and x4_r = Dba.Expr.v x4
  and x5_r = Dba.Expr.v x5
  and x6_r = Dba.Expr.v x6
  and x7_r = Dba.Expr.v x7
  and x30_r = Dba.Expr.v x30
  and sp_r = Dba.Expr.v sp

  let ret = x0_l
  let get_return_address () = x30_r

  let get_arg ?(syscall = false) i =
    if syscall then Errors.not_yet_implemented "syscall";
    match i with
    | 0 -> x0_r
    | 1 -> x1_r
    | 2 -> x2_r
    | 3 -> x3_r
    | 4 -> x4_r
    | 5 -> x5_r
    | 6 -> x6_r
    | 7 -> x7_r
    | i ->
        Dba.Expr.load Size.Byte.four LittleEndian
          (Dba.Expr.add sp_r
             (Dba.Expr.constant (Bitvector.of_int ~size:64 (8 * (i - 8)))))

  let get_ret ?syscall:_ () = ret

  let void_return = Dhunk.singleton (Dba.Instr.dynamic_jump ~tag:Return x30_r)

  and val_return value =
    Dhunk.init 2 (function
      | 0 -> Dba.Instr.assign x0_l value 1
      | _ -> Dba.Instr.dynamic_jump ~tag:Return x30_r)

  let make_return ?value () =
    match (value : Dba.Expr.t option) with
    | None -> void_return
    | Some (Var v) when Dba.Var.equal v x0 -> void_return
    | Some value -> val_return value

  let rx_r = Array.map Dba.Expr.v rx
  and pc_r = Dba.Expr.var "pc" 64

  let get_dwarf_register = function
    | ( 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16
      | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 ) as
      n ->
        Array.get rx_r n
    | 31 -> sp_r
    | 32 -> pc_r
    | n ->
        Kernel_options.Logger.fatal
          "unable to map integer %d to a known expression" n

  let get_stack_pointer () = (sp, Bitvector.of_int64 0x7fff000000000000L)
  let max_instruction_len = Size.Byte.four
end

module PPC64 : ARCH = struct
  let info = Dba.Var.Tag.Register

  let r0 = Dba.Var.create "r0" ~bitsize:Size.Bit.bits64 ~tag:info
  and r1 = Dba.Var.create "r1" ~bitsize:Size.Bit.bits64 ~tag:info
  and r2 = Dba.Var.create "r2" ~bitsize:Size.Bit.bits64 ~tag:info
  and r3 = Dba.Var.create "r3" ~bitsize:Size.Bit.bits64 ~tag:info
  and r4 = Dba.Var.create "r4" ~bitsize:Size.Bit.bits64 ~tag:info
  and r5 = Dba.Var.create "r5" ~bitsize:Size.Bit.bits64 ~tag:info
  and r6 = Dba.Var.create "r6" ~bitsize:Size.Bit.bits64 ~tag:info
  and r7 = Dba.Var.create "r7" ~bitsize:Size.Bit.bits64 ~tag:info
  and r8 = Dba.Var.create "r8" ~bitsize:Size.Bit.bits64 ~tag:info
  and r9 = Dba.Var.create "r9" ~bitsize:Size.Bit.bits64 ~tag:info
  and r10 = Dba.Var.create "r10" ~bitsize:Size.Bit.bits64 ~tag:info
  and r11 = Dba.Var.create "r11" ~bitsize:Size.Bit.bits64 ~tag:info
  and r12 = Dba.Var.create "r12" ~bitsize:Size.Bit.bits64 ~tag:info
  and r13 = Dba.Var.create "r13" ~bitsize:Size.Bit.bits64 ~tag:info
  and r14 = Dba.Var.create "r14" ~bitsize:Size.Bit.bits64 ~tag:info
  and r15 = Dba.Var.create "r15" ~bitsize:Size.Bit.bits64 ~tag:info
  and r16 = Dba.Var.create "r16" ~bitsize:Size.Bit.bits64 ~tag:info
  and r17 = Dba.Var.create "r17" ~bitsize:Size.Bit.bits64 ~tag:info
  and r18 = Dba.Var.create "r18" ~bitsize:Size.Bit.bits64 ~tag:info
  and r19 = Dba.Var.create "r19" ~bitsize:Size.Bit.bits64 ~tag:info
  and r20 = Dba.Var.create "r20" ~bitsize:Size.Bit.bits64 ~tag:info
  and r21 = Dba.Var.create "r21" ~bitsize:Size.Bit.bits64 ~tag:info
  and r22 = Dba.Var.create "r22" ~bitsize:Size.Bit.bits64 ~tag:info
  and r23 = Dba.Var.create "r23" ~bitsize:Size.Bit.bits64 ~tag:info
  and r24 = Dba.Var.create "r24" ~bitsize:Size.Bit.bits64 ~tag:info
  and r25 = Dba.Var.create "r25" ~bitsize:Size.Bit.bits64 ~tag:info
  and r26 = Dba.Var.create "r26" ~bitsize:Size.Bit.bits64 ~tag:info
  and r27 = Dba.Var.create "r27" ~bitsize:Size.Bit.bits64 ~tag:info
  and r28 = Dba.Var.create "r28" ~bitsize:Size.Bit.bits64 ~tag:info
  and r29 = Dba.Var.create "r29" ~bitsize:Size.Bit.bits64 ~tag:info
  and r30 = Dba.Var.create "r30" ~bitsize:Size.Bit.bits64 ~tag:info
  and r31 = Dba.Var.create "r31" ~bitsize:Size.Bit.bits64 ~tag:info
  and lr = Dba.Var.create "lr" ~bitsize:Size.Bit.bits64 ~tag:info

  let defs =
    [
      ("r0", Dba.LValue.v r0);
      ("r1", Dba.LValue.v r1);
      ("r2", Dba.LValue.v r2);
      ("r3", Dba.LValue.v r3);
      ("r4", Dba.LValue.v r4);
      ("r5", Dba.LValue.v r5);
      ("r6", Dba.LValue.v r6);
      ("r7", Dba.LValue.v r7);
      ("r8", Dba.LValue.v r8);
      ("r9", Dba.LValue.v r9);
      ("r10", Dba.LValue.v r10);
      ("r11", Dba.LValue.v r11);
      ("r12", Dba.LValue.v r12);
      ("r13", Dba.LValue.v r13);
      ("r14", Dba.LValue.v r14);
      ("r15", Dba.LValue.v r15);
      ("r16", Dba.LValue.v r16);
      ("r17", Dba.LValue.v r17);
      ("r18", Dba.LValue.v r18);
      ("r19", Dba.LValue.v r19);
      ("r20", Dba.LValue.v r20);
      ("r21", Dba.LValue.v r21);
      ("r22", Dba.LValue.v r22);
      ("r23", Dba.LValue.v r23);
      ("r24", Dba.LValue.v r24);
      ("r25", Dba.LValue.v r25);
      ("r26", Dba.LValue.v r26);
      ("r27", Dba.LValue.v r27);
      ("r28", Dba.LValue.v r28);
      ("r29", Dba.LValue.v r29);
      ("r30", Dba.LValue.v r30);
      ("r31", Dba.LValue.v r31);
      ("lr", Dba.LValue.v lr);
    ]

  let get_shortlived_flags () = []
  let core _ = Errors.not_yet_implemented "ppc core"
  let get_defs () = defs

  let r3_l = Dba.LValue.v r3
  and r3_r = Dba.Expr.v r3
  and r4_r = Dba.Expr.v r4
  and r5_r = Dba.Expr.v r5
  and r6_r = Dba.Expr.v r6
  and r7_r = Dba.Expr.v r7
  and r8_r = Dba.Expr.v r8
  and r9_r = Dba.Expr.v r9
  and r10_r = Dba.Expr.v r10
  and lr_r = Dba.Expr.v lr
  and r1_r = Dba.Expr.v r1

  let ret = r3_l
  let get_return_address () = lr_r

  let get_arg ?(syscall = false) i =
    if syscall then Errors.not_yet_implemented "syscall";
    match i with
    | 0 -> r3_r
    | 1 -> r4_r
    | 2 -> r5_r
    | 3 -> r6_r
    | 4 -> r7_r
    | 5 -> r8_r
    | 6 -> r9_r
    | 7 -> r10_r
    | i ->
        Dba.Expr.load Size.Byte.four LittleEndian
          (Dba.Expr.add r1_r
             (Dba.Expr.constant (Bitvector.of_int ~size:64 (8 * (i - 8)))))

  let get_ret ?syscall:_ () = ret

  let void_return = Dhunk.singleton (Dba.Instr.dynamic_jump ~tag:Return lr_r)

  and val_return value =
    Dhunk.init 2 (function
      | 0 -> Dba.Instr.assign r3_l value 1
      | _ -> Dba.Instr.dynamic_jump ~tag:Return lr_r)

  let make_return ?value () =
    match (value : Dba.Expr.t option) with
    | None -> void_return
    | Some (Var v) when Dba.Var.equal v r3 -> void_return
    | Some value -> val_return value

  let get_stack_pointer () = (r1, Bitvector.of_int64 0x7fff000000000000L)
  let get_dwarf_register _ = Errors.not_yet_implemented "PPC64 dwarf mapping"
  let max_instruction_len = Size.Byte.four
end

module RISCV (C : sig
  val size : int
end) : ARCH = struct
  let registers =
    Array.map
      (fun name ->
        Dba.Var.create name ~bitsize:(Size.Bit.create C.size)
          ~tag:Dba.Var.Tag.Register)
      [|
        "ra";
        "sp";
        "gp";
        "tp";
        "t0";
        "t1";
        "t2";
        "s0";
        "s1";
        "a0";
        "a1";
        "a2";
        "a3";
        "a4";
        "a5";
        "a6";
        "a7";
        "s2";
        "s3";
        "s4";
        "s5";
        "s6";
        "s7";
        "s8";
        "s9";
        "s10";
        "s11";
        "t3";
        "t4";
        "t5";
        "t6";
      |]

  let reg i = Array.get registers (i - 1)

  let defs =
    Array.to_list
      (Array.map
         (fun (var : Dba.Var.t) -> (var.name, Dba.LValue.v var))
         registers)

  let get_shortlived_flags () = []
  let get_defs () = defs
  let core _ = Errors.not_yet_implemented "RISC V core"
  let a0 = reg 10

  let a0_l = Dba.LValue.v a0
  and a0_r = Dba.Expr.v a0
  and a1_r = Dba.Expr.v (reg 11)
  and a2_r = Dba.Expr.v (reg 12)
  and a3_r = Dba.Expr.v (reg 13)
  and a4_r = Dba.Expr.v (reg 14)
  and a5_r = Dba.Expr.v (reg 15)
  and a6_r = Dba.Expr.v (reg 16)
  and a7_r = Dba.Expr.v (reg 17)
  and ra_r = Dba.Expr.v (reg 1)
  and sp_r = Dba.Expr.v (reg 2)

  let bytesize = Size.Byte.create (C.size / 8)
  let ret = a0_l
  let get_return_address () = ra_r

  let get_arg ?(syscall = false) i =
    if syscall then Errors.not_yet_implemented "syscall";
    match i with
    | 0 -> a0_r
    | 1 -> a1_r
    | 2 -> a2_r
    | 3 -> a3_r
    | 4 -> a4_r
    | 5 -> a5_r
    | 6 -> a6_r
    | 7 -> a7_r
    | i ->
        Dba.Expr.load bytesize LittleEndian
          (Dba.Expr.add sp_r
             (Dba.Expr.constant
                (Bitvector.of_int ~size:C.size (C.size / 8 * (i - 8)))))

  let get_ret ?syscall:_ () = ret

  let void_return = Dhunk.singleton (Dba.Instr.dynamic_jump ~tag:Return ra_r)

  and val_return value =
    Dhunk.init 2 (function
      | 0 -> Dba.Instr.assign a0_l value 1
      | _ -> Dba.Instr.dynamic_jump ~tag:Return ra_r)

  let make_return ?value () =
    match (value : Dba.Expr.t option) with
    | None -> void_return
    | Some (Var v) when Dba.Var.equal v a0 -> void_return
    | Some value -> val_return value

  let get_stack_pointer () =
    ( reg 2,
      match C.size with
      | 32 -> Bitvector.of_int ~size:32 0xfff00000
      | 64 -> Bitvector.of_int64 0x7fff000000000000L
      | _ ->
          raise
            (Errors.not_yet_implemented "incomplete architecture definition") )

  let get_dwarf_register _ = Errors.not_yet_implemented "RISCV dwarf mapping"
  let max_instruction_len = Size.Byte.four
end

module RISCV32 = RISCV (struct
  let size = 32
end)

module RISCV64 = RISCV (struct
  let size = 64
end)

module Z80 : ARCH = struct
  let defs =
    let add r l =
      let name = Z80_arch.name r in
      let lval = Z80_arch.lval r in
      (name, lval) :: (String.lowercase_ascii name, lval) :: l
    in
    Array.fold_right add Z80_arch.registers16
      (Array.fold_right add Z80_arch.registers8
         (Array.fold_right add Z80_arch.flags []))

  let get_shortlived_flags () = []
  let get_defs () = defs

  let get_return_address () =
    Errors.not_yet_implemented "Z80 calling convention"

  let get_arg ?syscall:_ _ = Errors.not_yet_implemented "Z80 calling convention"

  let get_ret ?syscall:_ () =
    Errors.not_yet_implemented "Z80 calling convention"

  let make_return ?value:_ () =
    Errors.not_yet_implemented "Z80 calling convention"

  let get_stack_pointer () = Errors.not_yet_implemented "Z80 definitions"
  let get_dwarf_register _ = Errors.not_yet_implemented "Z80 dwarf mapping"
  let core _ = Errors.not_yet_implemented "Z80 core"
  let max_instruction_len = Size.Byte.four
end

let get_arch () : (module ARCH) =
  match Kernel_options.Machine.isa () with
  | X86 { bits = `x32 } -> (module X86)
  | X86 { bits = `x64 } -> (module AMD64)
  | ARM { rev = `v7; endianness = LittleEndian } -> (module ARM)
  | ARM { rev = `v8; endianness = LittleEndian } -> (module AARCH64)
  | PPC { bits = `x64; _ } -> (module PPC64)
  | RISCV { bits = `x32 } -> (module RISCV32)
  | RISCV { bits = `x64 } -> (module RISCV64)
  | Z80 -> (module Z80)
  | _ ->
      (* TODO *)
      Errors.not_yet_implemented "incomplete architecture definition"

let get_defs () =
  let module A = (val get_arch ()) in
  A.get_defs ()

let get_return_address () =
  let module A = (val get_arch ()) in
  A.get_return_address ()

let get_arg ?syscall i =
  let module A = (val get_arch ()) in
  A.get_arg ?syscall i

let get_ret ?syscall () =
  let module A = (val get_arch ()) in
  A.get_ret ?syscall ()

let make_return ?value () =
  let module A = (val get_arch ()) in
  A.make_return ?value ()

let core img =
  let module A = (val get_arch ()) in
  A.core img

let get_stack_pointer () =
  let module A = (val get_arch ()) in
  A.get_stack_pointer ()

let get_shortlived_flags () =
  let module A = (val get_arch ()) in
  A.get_shortlived_flags ()

let get_dwarf_register n =
  let module A = (val get_arch ()) in
  A.get_dwarf_register n

let max_instruction_len () =
  let module A = (val get_arch ()) in
  A.max_instruction_len
