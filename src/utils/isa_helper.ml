(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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
  let info = Dba.VarTag.Register

  let eax = { Dba.name = "eax"; size = 32; info }

  and ebx = { Dba.name = "ebx"; size = 32; info }

  and ecx = { Dba.name = "ecx"; size = 32; info }

  and edx = { Dba.name = "edx"; size = 32; info }

  and edi = { Dba.name = "edi"; size = 32; info }

  and esi = { Dba.name = "esi"; size = 32; info }

  and esp = { Dba.name = "esp"; size = 32; info }

  and ebp = { Dba.name = "ebp"; size = 32; info }

  let cs = { Dba.name = "cs"; size = 16; info }

  and ds = { Dba.name = "ds"; size = 16; info }

  and es = { Dba.name = "es"; size = 16; info }

  and fs = { Dba.name = "fs"; size = 16; info }

  and gs = { Dba.name = "gs"; size = 16; info }

  and ss = { Dba.name = "ss"; size = 16; info }

  let info = Dba.VarTag.Flag

  let cf = { Dba.name = "CF"; size = 1; info }

  and pf = { Dba.name = "PF"; size = 1; info }

  and af = { Dba.name = "AF"; size = 1; info }

  and zf = { Dba.name = "ZF"; size = 1; info }

  and sf = { Dba.name = "SF"; size = 1; info }

  and tf = { Dba.name = "TF"; size = 1; info }

  and if' = { Dba.name = "IF"; size = 1; info }

  and df = { Dba.name = "DF"; size = 1; info }

  and of' = { Dba.name = "OF"; size = 1; info }

  and iopl = { Dba.name = "IOPL"; size = 2; info }

  and nt = { Dba.name = "NT"; size = 1; info }

  and rf = { Dba.name = "RF"; size = 1; info }

  and vm = { Dba.name = "VM"; size = 1; info }

  and ac = { Dba.name = "AC"; size = 1; info }

  and vif = { Dba.name = "VIF"; size = 1; info }

  and vip = { Dba.name = "VIP"; size = 1; info }

  and id = { Dba.name = "ID"; size = 1; info }

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
                (Bitvector.of_int ~size:1 ((eflags lsr 12) land 0b11))
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
  let info = Dba.VarTag.Register

  let rax = { Dba.name = "rax"; size = 64; info }

  and rbx = { Dba.name = "rbx"; size = 64; info }

  and rcx = { Dba.name = "rcx"; size = 64; info }

  and rdx = { Dba.name = "rdx"; size = 64; info }

  and rdi = { Dba.name = "rdi"; size = 64; info }

  and rsi = { Dba.name = "rsi"; size = 64; info }

  and rsp = { Dba.name = "rsp"; size = 64; info }

  and rbp = { Dba.name = "rbp"; size = 64; info }

  and r8 = { Dba.name = "r8"; size = 64; info }

  and r9 = { Dba.name = "r9"; size = 64; info }

  and r10 = { Dba.name = "r10"; size = 64; info }

  and r11 = { Dba.name = "r11"; size = 64; info }

  and r12 = { Dba.name = "r12"; size = 64; info }

  and r13 = { Dba.name = "r13"; size = 64; info }

  and r14 = { Dba.name = "r14"; size = 64; info }

  and r15 = { Dba.name = "r15"; size = 64; info }

  let cs = { Dba.name = "cs"; size = 16; info }

  and ds = { Dba.name = "ds"; size = 16; info }

  and es = { Dba.name = "es"; size = 16; info }

  and fs = { Dba.name = "fs"; size = 16; info }

  and gs = { Dba.name = "gs"; size = 16; info }

  and ss = { Dba.name = "ss"; size = 16; info }

  and fs_base = { Dba.name = "fs_base"; size = 64; info }

  and gs_base = { Dba.name = "gs_base"; size = 64; info }

  let ymm0 = { Dba.name = "ymm0"; size = 256; info }

  and ymm1 = { Dba.name = "ymm1"; size = 256; info }

  and ymm2 = { Dba.name = "ymm2"; size = 256; info }

  and ymm3 = { Dba.name = "ymm3"; size = 256; info }

  and ymm4 = { Dba.name = "ymm4"; size = 256; info }

  and ymm5 = { Dba.name = "ymm5"; size = 256; info }

  and ymm6 = { Dba.name = "ymm6"; size = 256; info }

  and ymm7 = { Dba.name = "ymm7"; size = 256; info }

  and ymm8 = { Dba.name = "ymm8"; size = 256; info }

  and ymm9 = { Dba.name = "ymm9"; size = 256; info }

  and ymm10 = { Dba.name = "ymm10"; size = 256; info }

  and ymm11 = { Dba.name = "ymm11"; size = 256; info }

  and ymm12 = { Dba.name = "ymm12"; size = 256; info }

  and ymm13 = { Dba.name = "ymm13"; size = 256; info }

  and ymm14 = { Dba.name = "ymm14"; size = 256; info }

  and ymm15 = { Dba.name = "ymm15"; size = 256; info }

  let info = Dba.VarTag.Flag

  let cf = { Dba.name = "CF"; size = 1; info }

  and pf = { Dba.name = "PF"; size = 1; info }

  and af = { Dba.name = "AF"; size = 1; info }

  and zf = { Dba.name = "ZF"; size = 1; info }

  and sf = { Dba.name = "SF"; size = 1; info }

  and tf = { Dba.name = "TF"; size = 1; info }

  and if' = { Dba.name = "IF"; size = 1; info }

  and df = { Dba.name = "DF"; size = 1; info }

  and of' = { Dba.name = "OF"; size = 1; info }

  and iopl = { Dba.name = "IOPL"; size = 2; info }

  and nt = { Dba.name = "NT"; size = 1; info }

  and rf = { Dba.name = "RF"; size = 1; info }

  and vm = { Dba.name = "VM"; size = 1; info }

  and ac = { Dba.name = "AC"; size = 1; info }

  and vif' = { Dba.name = "VIF"; size = 1; info }

  and vip = { Dba.name = "VIP"; size = 1; info }

  and id = { Dba.name = "ID"; size = 1; info }

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
      ("xmm0", Dba.LValue.restrict ymm0 0 128);
      ("xmm1", Dba.LValue.restrict ymm1 0 128);
      ("xmm2", Dba.LValue.restrict ymm2 0 128);
      ("xmm3", Dba.LValue.restrict ymm3 0 128);
      ("xmm4", Dba.LValue.restrict ymm4 0 128);
      ("xmm5", Dba.LValue.restrict ymm5 0 128);
      ("xmm6", Dba.LValue.restrict ymm6 0 128);
      ("xmm7", Dba.LValue.restrict ymm7 0 128);
      ("xmm8", Dba.LValue.restrict ymm8 0 128);
      ("xmm9", Dba.LValue.restrict ymm9 0 128);
      ("xmm10", Dba.LValue.restrict ymm10 0 128);
      ("xmm11", Dba.LValue.restrict ymm11 0 128);
      ("xmm12", Dba.LValue.restrict ymm12 0 128);
      ("xmm13", Dba.LValue.restrict ymm13 0 128);
      ("xmm14", Dba.LValue.restrict ymm14 0 128);
      ("xmm15", Dba.LValue.restrict ymm15 0 128);
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
      ("VIF", Dba.LValue.v vif');
      ("VIP", Dba.LValue.v vip);
      ("ID", Dba.LValue.v id);
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
                (Bitvector.of_int ~size:1 ((rflags_15_0 lsr 12) land 0b11))
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
    let info = Dba.VarTag.Register in
    let r0 = { Dba.name = "r0"; size = 32; info }
    and r1 = { Dba.name = "r1"; size = 32; info }
    and r2 = { Dba.name = "r2"; size = 32; info }
    and r3 = { Dba.name = "r3"; size = 32; info }
    and r4 = { Dba.name = "r4"; size = 32; info }
    and r5 = { Dba.name = "r5"; size = 32; info }
    and r6 = { Dba.name = "r6"; size = 32; info }
    and r7 = { Dba.name = "r7"; size = 32; info }
    and r8 = { Dba.name = "r8"; size = 32; info }
    and r9 = { Dba.name = "r9"; size = 32; info }
    and r10 = { Dba.name = "r10"; size = 32; info }
    and r11 = { Dba.name = "fp"; size = 32; info }
    and r12 = { Dba.name = "ip"; size = 32; info }
    and r13 = { Dba.name = "sp"; size = 32; info }
    and r14 = { Dba.name = "lr"; size = 32; info }
    and r15 = { Dba.name = "pc"; size = 32; info } in
    let info = Dba.VarTag.Flag in
    let n = { Dba.name = "n"; size = 1; info }
    and z = { Dba.name = "z"; size = 1; info }
    and c = { Dba.name = "c"; size = 1; info }
    and v = { Dba.name = "v"; size = 1; info }
    and t = { Dba.name = "t"; size = 1; info } in
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
    let info = Dba.VarTag.Register in
    let x0 = { Dba.name = "x0"; size = 64; info }
    and x1 = { Dba.name = "x1"; size = 64; info }
    and x2 = { Dba.name = "x2"; size = 64; info }
    and x3 = { Dba.name = "x3"; size = 64; info }
    and x4 = { Dba.name = "x4"; size = 64; info }
    and x5 = { Dba.name = "x5"; size = 64; info }
    and x6 = { Dba.name = "x6"; size = 64; info }
    and x7 = { Dba.name = "x7"; size = 64; info }
    and x8 = { Dba.name = "x8"; size = 64; info }
    and x9 = { Dba.name = "x9"; size = 64; info }
    and x10 = { Dba.name = "x10"; size = 64; info }
    and x11 = { Dba.name = "x11"; size = 64; info }
    and x12 = { Dba.name = "x12"; size = 64; info }
    and x13 = { Dba.name = "x13"; size = 64; info }
    and x14 = { Dba.name = "x14"; size = 64; info }
    and x15 = { Dba.name = "x15"; size = 64; info }
    and x16 = { Dba.name = "x16"; size = 64; info }
    and x17 = { Dba.name = "x17"; size = 64; info }
    and x18 = { Dba.name = "x18"; size = 64; info }
    and x19 = { Dba.name = "x19"; size = 64; info }
    and x20 = { Dba.name = "x20"; size = 64; info }
    and x21 = { Dba.name = "x21"; size = 64; info }
    and x22 = { Dba.name = "x22"; size = 64; info }
    and x23 = { Dba.name = "x23"; size = 64; info }
    and x24 = { Dba.name = "x24"; size = 64; info }
    and x25 = { Dba.name = "x25"; size = 64; info }
    and x26 = { Dba.name = "x26"; size = 64; info }
    and x27 = { Dba.name = "x27"; size = 64; info }
    and x28 = { Dba.name = "x28"; size = 64; info }
    and x29 = { Dba.name = "x29"; size = 64; info }
    and x30 = { Dba.name = "x30"; size = 64; info }
    and sp = { Dba.name = "sp"; size = 64; info } in
    let info = Dba.VarTag.Flag in
    let n = { Dba.name = "n"; size = 1; info }
    and z = { Dba.name = "z"; size = 1; info }
    and c = { Dba.name = "c"; size = 1; info }
    and v = { Dba.name = "v"; size = 1; info }
    and t = { Dba.name = "t"; size = 1; info } in
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

let get_defs () =
  match Kernel_options.Machine.isa () with
  | X86 { bits = `x32 } -> X86.defs
  | X86 { bits = `x64 } -> AMD64.defs
  | ARM { rev = `v7; _ } -> ARM.defs
  | ARM { rev = `v8; _ } -> AARCH64.defs
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
  | _ ->
      (* TODO *)
      raise (Errors.not_yet_implemented "incomplete architecture definition")
