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
  let defs =
    let info = Dba.VarTag.Register in
    let eax = { Dba.name = "eax"; size = 32; info }
    and ebx = { Dba.name = "ebx"; size = 32; info }
    and ecx = { Dba.name = "ecx"; size = 32; info }
    and edx = { Dba.name = "edx"; size = 32; info }
    and edi = { Dba.name = "edi"; size = 32; info }
    and esi = { Dba.name = "esi"; size = 32; info }
    and esp = { Dba.name = "esp"; size = 32; info }
    and ebp = { Dba.name = "ebp"; size = 32; info } in
    let info = Dba.VarTag.Flag in
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
    and id = { Dba.name = "ID"; size = 1; info } in
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
end

module AMD64 = struct
  let defs =
    let info = Dba.VarTag.Register in
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
    and r15 = { Dba.name = "r15"; size = 64; info } in
    let cs = { Dba.name = "cs"; size = 16; info }
    and ds = { Dba.name = "ds"; size = 16; info }
    and es = { Dba.name = "es"; size = 16; info }
    and fs = { Dba.name = "fs"; size = 16; info }
    and gs = { Dba.name = "gs"; size = 16; info }
    and ss = { Dba.name = "ss"; size = 16; info } in
    let info = Dba.VarTag.Flag in
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
    and id = { Dba.name = "ID"; size = 1; info } in
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
