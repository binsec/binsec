(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2023                                               *)
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

module X86 = struct
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

  and ss = Dba.Var.create "ss" ~bitsize:Size.Bit.bits16 ~tag:info

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
    ]

  let notes img =
    Array.fold_left
      (fun result -> function
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
              ] )
        | _ -> result)
      (Virtual_address.create 0, [])
      (Loader_elf.notes img)
end

module AMD64 = struct
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

  let notes img =
    Array.fold_left
      (fun result -> function
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
              ] )
        | _ -> result)
      (Virtual_address.create 0, [])
      (Loader_elf.notes img)
end

module ARM = struct
  let defs =
    let info = Dba.Var.Tag.Register in
    let r0 = Dba.Var.create "r0" ~bitsize:Size.Bit.bits32 ~tag:info
    and r1 = Dba.Var.create "r1" ~bitsize:Size.Bit.bits32 ~tag:info
    and r2 = Dba.Var.create "r2" ~bitsize:Size.Bit.bits32 ~tag:info
    and r3 = Dba.Var.create "r3" ~bitsize:Size.Bit.bits32 ~tag:info
    and r4 = Dba.Var.create "r4" ~bitsize:Size.Bit.bits32 ~tag:info
    and r5 = Dba.Var.create "r5" ~bitsize:Size.Bit.bits32 ~tag:info
    and r6 = Dba.Var.create "r6" ~bitsize:Size.Bit.bits32 ~tag:info
    and r7 = Dba.Var.create "r7" ~bitsize:Size.Bit.bits32 ~tag:info
    and r8 = Dba.Var.create "r8" ~bitsize:Size.Bit.bits32 ~tag:info
    and r9 = Dba.Var.create "r9" ~bitsize:Size.Bit.bits32 ~tag:info
    and r10 = Dba.Var.create "r10" ~bitsize:Size.Bit.bits32 ~tag:info
    and r11 = Dba.Var.create "fp" ~bitsize:Size.Bit.bits32 ~tag:info
    and r12 = Dba.Var.create "ip" ~bitsize:Size.Bit.bits32 ~tag:info
    and r13 = Dba.Var.create "sp" ~bitsize:Size.Bit.bits32 ~tag:info
    and r14 = Dba.Var.create "lr" ~bitsize:Size.Bit.bits32 ~tag:info
    and r15 = Dba.Var.create "pc" ~bitsize:Size.Bit.bits32 ~tag:info in
    let info = Dba.Var.Tag.Flag in
    let n = Dba.Var.create "n" ~bitsize:Size.Bit.bits1 ~tag:info
    and z = Dba.Var.create "z" ~bitsize:Size.Bit.bits1 ~tag:info
    and c = Dba.Var.create "c" ~bitsize:Size.Bit.bits1 ~tag:info
    and v = Dba.Var.create "v" ~bitsize:Size.Bit.bits1 ~tag:info
    and t = Dba.Var.create "t" ~bitsize:Size.Bit.bits1 ~tag:info in
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
end

module AARCH64 = struct
  let defs =
    let info = Dba.Var.Tag.Register in
    let x0 = Dba.Var.create "x0" ~bitsize:Size.Bit.bits64 ~tag:info
    and x1 = Dba.Var.create "x1" ~bitsize:Size.Bit.bits64 ~tag:info
    and x2 = Dba.Var.create "x2" ~bitsize:Size.Bit.bits64 ~tag:info
    and x3 = Dba.Var.create "x3" ~bitsize:Size.Bit.bits64 ~tag:info
    and x4 = Dba.Var.create "x4" ~bitsize:Size.Bit.bits64 ~tag:info
    and x5 = Dba.Var.create "x5" ~bitsize:Size.Bit.bits64 ~tag:info
    and x6 = Dba.Var.create "x6" ~bitsize:Size.Bit.bits64 ~tag:info
    and x7 = Dba.Var.create "x7" ~bitsize:Size.Bit.bits64 ~tag:info
    and x8 = Dba.Var.create "x8" ~bitsize:Size.Bit.bits64 ~tag:info
    and x9 = Dba.Var.create "x9" ~bitsize:Size.Bit.bits64 ~tag:info
    and x10 = Dba.Var.create "x10" ~bitsize:Size.Bit.bits64 ~tag:info
    and x11 = Dba.Var.create "x11" ~bitsize:Size.Bit.bits64 ~tag:info
    and x12 = Dba.Var.create "x12" ~bitsize:Size.Bit.bits64 ~tag:info
    and x13 = Dba.Var.create "x13" ~bitsize:Size.Bit.bits64 ~tag:info
    and x14 = Dba.Var.create "x14" ~bitsize:Size.Bit.bits64 ~tag:info
    and x15 = Dba.Var.create "x15" ~bitsize:Size.Bit.bits64 ~tag:info
    and x16 = Dba.Var.create "x16" ~bitsize:Size.Bit.bits64 ~tag:info
    and x17 = Dba.Var.create "x17" ~bitsize:Size.Bit.bits64 ~tag:info
    and x18 = Dba.Var.create "x18" ~bitsize:Size.Bit.bits64 ~tag:info
    and x19 = Dba.Var.create "x19" ~bitsize:Size.Bit.bits64 ~tag:info
    and x20 = Dba.Var.create "x20" ~bitsize:Size.Bit.bits64 ~tag:info
    and x21 = Dba.Var.create "x21" ~bitsize:Size.Bit.bits64 ~tag:info
    and x22 = Dba.Var.create "x22" ~bitsize:Size.Bit.bits64 ~tag:info
    and x23 = Dba.Var.create "x23" ~bitsize:Size.Bit.bits64 ~tag:info
    and x24 = Dba.Var.create "x24" ~bitsize:Size.Bit.bits64 ~tag:info
    and x25 = Dba.Var.create "x25" ~bitsize:Size.Bit.bits64 ~tag:info
    and x26 = Dba.Var.create "x26" ~bitsize:Size.Bit.bits64 ~tag:info
    and x27 = Dba.Var.create "x27" ~bitsize:Size.Bit.bits64 ~tag:info
    and x28 = Dba.Var.create "x28" ~bitsize:Size.Bit.bits64 ~tag:info
    and x29 = Dba.Var.create "x29" ~bitsize:Size.Bit.bits64 ~tag:info
    and x30 = Dba.Var.create "x30" ~bitsize:Size.Bit.bits64 ~tag:info
    and sp = Dba.Var.create "sp" ~bitsize:Size.Bit.bits64 ~tag:info in
    let info = Dba.Var.Tag.Flag in
    let n = Dba.Var.create "n" ~bitsize:Size.Bit.bits1 ~tag:info
    and z = Dba.Var.create "z" ~bitsize:Size.Bit.bits1 ~tag:info
    and c = Dba.Var.create "c" ~bitsize:Size.Bit.bits1 ~tag:info
    and v = Dba.Var.create "v" ~bitsize:Size.Bit.bits1 ~tag:info
    and t = Dba.Var.create "t" ~bitsize:Size.Bit.bits1 ~tag:info in
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
      ("n", Dba.LValue.v n);
      ("z", Dba.LValue.v z);
      ("c", Dba.LValue.v c);
      ("v", Dba.LValue.v v);
      ("t", Dba.LValue.v t);
    ]
end

module RISCV = struct
  let defs size =
    let info = Dba.Var.Tag.Register in
    let bitsize = Size.Bit.create size in
    let reg name = (name, Dba.LValue.var name ~bitsize ~tag:info) in
    List.map reg
      [
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
        "s7";
        "s7";
        "s8";
        "s9";
        "s10";
        "s11";
        "t3";
        "t4";
        "t5";
        "t6";
      ]
end

module Z80 = struct
  let defs =
    let add r l =
      let name = Z80_arch.name r in
      let lval = Z80_arch.lval r in
      (name, lval) :: (String.lowercase_ascii name, lval) :: l
    in
    Array.fold_right add Z80_arch.registers16
      (Array.fold_right add Z80_arch.registers8
         (Array.fold_right add Z80_arch.flags []))
end

let get_defs () =
  match Kernel_options.Machine.isa () with
  | X86 { bits = `x32 } -> X86.defs
  | X86 { bits = `x64 } -> AMD64.defs
  | ARM { rev = `v7; _ } -> ARM.defs
  | ARM { rev = `v8; _ } -> AARCH64.defs
  | RISCV { bits = `x32 } -> RISCV.defs 32
  | RISCV { bits = `x64 } -> RISCV.defs 64
  | Z80 -> Z80.defs
  | _ ->
      (* TODO *)
      raise (Errors.not_yet_implemented "incomplete architecture definition")

let core img =
  match Kernel_options.Machine.isa () with
  | X86 { bits = `x32 } -> X86.notes img
  | X86 { bits = `x64 } -> AMD64.notes img
  | _ -> raise (Errors.not_yet_implemented "core dump")
(* TODO *)

let max_instruction_len () =
  let b4 = Size.Byte.create 4 and b15 = Size.Byte.create 15 in
  match Kernel_options.Machine.isa () with
  | X86 _ -> b15
  | ARM _ -> b4
  | RISCV _ -> b4
  | Z80 -> b4
  | _ ->
      (* TODO *)
      raise (Errors.not_yet_implemented "incomplete architecture definition")
