(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

open Types

type 'a t =
  | Hook : {
      addr : Virtual_address.t;
      info : string;
      mutable succ : [ `All ] t;
    }
      -> [< `Label | `All ] t
  | Exec : {
      addr : Virtual_address.t;
      info : string;
      n : int;
      others : (Virtual_address.t * string) list;
      mutable succ : [ `All ] t;
    }
      -> [< `Label | `All ] t
  | Assign : {
      var : Var.t;
      rval : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `All ] t
  | Clobber : { var : Var.t; mutable succ : [ `All ] t } -> [< `All ] t
  | Load : {
      var : Var.t;
      base : A.t;
      dir : Machine.endianness;
      addr : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `All ] t
  | Store : {
      base : A.t;
      dir : Machine.endianness;
      addr : Expr.t;
      rval : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `All ] t
  | Symbolize : { var : Var.t; mutable succ : [ `All ] t } -> [< `All ] t
  | Assume : {
      test : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `Assume | `All ] t
  | Assert : {
      test : Expr.t;
      mutable succ : [ `All ] t;
    }
      -> [< `Assert | `All ] t
  | Branch : {
      test : Expr.t;
      mutable taken : [ `All ] t;
      mutable fallthrough : [ `All ] t;
    }
      -> [< `Branch | `All ] t
  | Goto : {
      addr : Virtual_address.t;
      mutable preds : [ `All ] t list;
    }
      -> [< `All ] t
  | Jump : Expr.t -> [< `Jump | `All ] t
  | Halt : [< `All ] t
  | Probe : {
      kind : Probe.t;
      mutable succ : [ `All ] t;
    }
      -> [< `Probe | `All ] t
  | Cut : [< `All ] t
  | Die : string -> [< `All ] t

let addr (t : [ `Label ] t) =
  match t with Hook { addr; _ } | Exec { addr; _ } -> addr

let extract_load =
  let rec fold m (e : Expr.t) =
    match e with
    | Cst _ -> (m, e)
    | Var _ -> (m, e)
    | Load (sz, dir, addr, base) ->
        let m', addr' = fold m addr in
        let k = (sz, dir, addr', base) in
        let v =
          try List.assoc k m'
          with Not_found ->
            Dba.Var.(
              create
                (Printf.sprintf "$$%d" (List.length m'))
                ~bitsize:(Size.Bit.create (8 * sz))
                ~tag:Tag.Temp)
        in
        ((k, v) :: m', Expr.v v)
    | Unary (o, x) ->
        let m', x' = fold m x in
        let e' = if x == x' then e else Expr.unary o x' in
        (m', e')
    | Binary (o, x, y) ->
        let m', x' = fold m x in
        let m', y' = fold m' y in
        let e' = if x == x' && y == y' then e else Expr.binary o x' y' in
        (m', e')
    | Ite (c, x, y) ->
        let m', c' = fold m c in
        let m', x' = fold m' x in
        let m', y' = fold m' y in
        let e' =
          if c == c' && x == x' && y == y' then e else Expr.ite c' x' y'
        in
        (m', e')
  in
  fold

let define_load m succ =
  List.fold_left
    (fun succ ((_, dir, addr, base), var) ->
      Load { var; base; dir; addr; succ })
    succ m

let assign (loc : Dba.LValue.t) (rval : Dba.Expr.t) succ =
  match (loc, rval) with
  | Var var, Load (_, dir, addr, base) ->
      let m, addr = extract_load [] addr in
      define_load m (Load { var; base; dir; addr; succ })
  | Var var, _ ->
      let m, rval = extract_load [] rval in
      define_load m (Assign { var; rval; succ })
  | Restrict (var, { lo; hi }), _ ->
      let m, rval = extract_load [] rval in
      let rval = Dba_utils.Expr.complement rval ~hi ~lo var in
      define_load m (Assign { var; rval; succ })
  | Store (_, dir, addr, base), _ ->
      let m, addr = extract_load [] addr in
      let m, rval = extract_load m rval in
      define_load m (Store { base; dir; addr; rval; succ })

let entropy = Printf.sprintf "%%entropy%%%d"

let nondet (loc : Dba.LValue.t) succ =
  match loc with
  | Var var -> Symbolize { var; succ }
  | Restrict (var, { lo; hi }) ->
      let size' = hi - lo + 1 in
      let name' = entropy size' in
      let var' = Dba.Var.temporary name' (Size.Bit.create size') in
      let rval = Dba_utils.Expr.complement (Expr.v var') ~lo ~hi var in
      Symbolize { var = var'; succ = Assign { var; rval; succ } }
  | Store (bytes, dir, addr, base) ->
      let size' = 8 * bytes in
      let name' = entropy size' in
      let var' = Dba.Var.temporary name' (Size.Bit.create size') in
      let rval = Expr.v var' in
      Symbolize { var = var'; succ = Store { base; dir; addr; rval; succ } }

let of_dhunk : Dhunk.t -> _ t =
  let rec lookup d a i =
    match Array.get a i with
    | Halt -> (
        match Dhunk.inst_exn d i with
        | SJump (JInner i, _) -> lookup d a i
        | _ -> Halt)
    | t -> t
  in
  let rec forward_load t =
    match t with
    | Hook _ | Exec _ | Assign _ | Clobber _
    | Load { succ = Halt; _ }
    | Store _ | Symbolize _ | Assume _ | Assert _ | Branch _ | Probe _ | Goto _
    | Jump _ | Cut | Halt | Die _ ->
        t
    | Load { succ; _ } -> forward_load succ
  in
  fun d ->
    let d = Dhunk.optimize ~inplace:true d in
    let n = Dhunk.length d in
    let a = Array.make n Halt in
    for i = 0 to n - 1 do
      Array.set a i
        (match Dhunk.inst_exn d i with
        | Assign (loc, value, _) -> assign loc value Halt
        | Undef (Var var, _) -> Clobber { var; succ = Halt }
        | Nondet (loc, _) -> nondet loc Halt
        | Assume (test, _) ->
            let m, test = extract_load [] test in
            define_load m (Assume { test; succ = Halt })
        | Assert (test, _) ->
            let m, test = extract_load [] test in
            define_load m (Assert { test; succ = Halt })
        | If (test, JInner _, _) ->
            let m, test = extract_load [] test in
            define_load m (Branch { test; taken = Halt; fallthrough = Halt })
        | If (test, JOuter { base; _ }, _) ->
            let m, test = extract_load [] test in
            define_load m
              (Branch
                 {
                   test;
                   taken = Goto { addr = base; preds = [] };
                   fallthrough = Halt;
                 })
        | DJump (target, _) ->
            let m, target = extract_load [] target in
            define_load m (Jump target)
        | SJump (JOuter { base; _ }, _) -> Goto { addr = base; preds = [] }
        | SJump (JInner _, _) -> Halt
        | Stop (None | Some OK) -> Halt
        | Stop (Some (Undecoded msg | Unsupported msg)) -> Die msg
        | (Undef _ | Stop (Some KO)) as ins ->
            Options.Logger.fatal "unexpected instruction kind %a"
              Dba_printer.Ascii.pp_instruction ins)
    done;
    for i = 0 to n - 1 do
      match (Dhunk.inst_exn d i, forward_load (Array.get a i)) with
      | Assign (Var _, _, i'), Load t -> t.succ <- lookup d a i'
      | Assign ((Var _ | Restrict _), _, i'), Assign t ->
          t.succ <- lookup d a i'
      | Assign (Store _, _, i'), Store t -> t.succ <- lookup d a i'
      | Undef (_, i'), Clobber t -> t.succ <- lookup d a i'
      | Nondet (Var _, i'), Symbolize t -> t.succ <- lookup d a i'
      | Nondet (Restrict _, i'), Symbolize { succ = Assign t; _ } ->
          t.succ <- lookup d a i'
      | Nondet (Store _, i'), Symbolize { succ = Store t; _ } ->
          t.succ <- lookup d a i'
      | Assume (_, i'), Assume t -> t.succ <- lookup d a i'
      | Assert (_, i'), Assert t -> t.succ <- lookup d a i'
      | If (_, JInner i', f'), Branch t ->
          t.taken <- lookup d a i';
          t.fallthrough <- lookup d a f'
      | If (_, JOuter _, i'), Branch t -> t.fallthrough <- lookup d a i'
      | SJump (JOuter _, _), Goto t ->
          t.preds <- List.map (Array.get a) (Dhunk.pred d i)
      | DJump _, _ | SJump _, _ | Stop _, _ -> ()
      | _ -> assert false
    done;
    lookup d a 0

let abort = Die "invalid fallthrough instruction"

let relink ?(taken = false) (t : [ `All ] t) (succ : [ `All ] t) =
  (match succ with Goto g when t <> Halt -> g.preds <- t :: g.preds | _ -> ());
  match t with
  | Hook t -> t.succ <- succ
  | Exec t -> t.succ <- succ
  | Assign t -> t.succ <- succ
  | Clobber t -> t.succ <- succ
  | Load t -> t.succ <- succ
  | Store t -> t.succ <- succ
  | Symbolize t -> t.succ <- succ
  | Assume t -> t.succ <- succ
  | Assert t -> t.succ <- succ
  | Branch t when taken -> t.taken <- succ
  | Branch t -> t.fallthrough <- succ
  | Probe t -> t.succ <- succ
  | Goto _ | Jump _ | Cut | Halt | Die _ -> ()

let rec iter continue entries reloc passthrough labels pred
    (stmts : Script.Instr.t list) =
  match stmts with
  | [] ->
      List.iter (fun name -> S.Htbl.add entries name continue) labels;
      relink pred continue;
      reloc
  | Label name :: stmts ->
      iter continue entries reloc passthrough (name :: labels) pred stmts
  | Assign (Var var, Load (_, dir, addr, base)) :: stmts ->
      let m, addr = extract_load [] addr in
      let last = Load { var; base; dir; addr; succ = Halt } in
      let step = define_load m last in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] last stmts
  | Assign (Var var, rval) :: stmts ->
      let m, rval = extract_load [] rval in
      let last = Assign { var; rval; succ = Halt } in
      let step = define_load m last in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] last stmts
  | Assign (Restrict (var, { hi; lo }), rval) :: stmts ->
      let m, rval = extract_load [] rval in
      let rval = Dba_utils.Expr.complement rval ~hi ~lo var in
      let last = Assign { var; rval; succ = Halt } in
      let step = define_load m last in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] last stmts
  | Assign (Store (_, dir, addr, base), rval) :: stmts ->
      let m, addr = extract_load [] addr in
      let m, rval = extract_load m rval in
      let last = Store { base; dir; addr; rval; succ = Halt } in
      let step = define_load m last in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] last stmts
  | Nondet (Var var) :: stmts ->
      let step = Symbolize { var; succ = Halt } in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] step stmts
  | Nondet (Restrict (var, { hi; lo })) :: stmts ->
      let size' = hi - lo + 1 in
      let name' = entropy size' in
      let var' = Dba.Var.temporary name' (Size.Bit.create size') in
      let rval = Dba_utils.Expr.complement (Expr.v var') ~hi ~lo var in
      let succ = Assign { var; rval; succ = Halt } in
      let step = Symbolize { var = var'; succ } in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] succ stmts
  | Nondet (Store (len, dir, addr, base)) :: stmts ->
      let m, addr = extract_load [] addr in
      let size' = 8 * len in
      let name' = entropy size' in
      let var' = Dba.Var.temporary name' (Size.Bit.create size') in
      let succ = Store { base; dir; addr; rval = Expr.v var'; succ = Halt } in
      let step = define_load m (Symbolize { var = var'; succ }) in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] succ stmts
  | Undef (Var var) :: stmts ->
      let step = Clobber { var; succ = Halt } in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] step stmts
  | Undef _ :: _ -> Options.Logger.fatal "only variables can be undefined"
  | Assume test :: stmts ->
      let m, test = extract_load [] test in
      let last = Assume { test; succ = Halt } in
      let step = define_load m last in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] last stmts
  | Assert test :: stmts ->
      let m, test = extract_load [] test in
      let last = Assert { test; succ = Halt } in
      let step = define_load m last in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] last stmts
  | It (test, target) :: stmts ->
      let m, test = extract_load [] test in
      let branch = Branch { test; taken = Halt; fallthrough = Halt } in
      let step = define_load m branch in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries
        ((branch, target, true) :: reloc)
        passthrough [] branch stmts
  | Goto target :: stmts ->
      List.iter (fun name -> S.Htbl.add passthrough name target) labels;
      iter continue entries
        ((pred, target, false) :: reloc)
        passthrough [] Halt stmts
  | Jump (Cst bv) :: stmts ->
      let goto = Goto { addr = Virtual_address.of_bitvector bv; preds = [] } in
      List.iter (fun name -> S.Htbl.add entries name goto) labels;
      relink pred goto;
      iter continue entries reloc passthrough [] goto stmts
  | Jump target :: stmts ->
      let m, target = extract_load [] target in
      let jump = Jump target in
      let step = define_load m jump in
      List.iter (fun name -> S.Htbl.add entries name step) labels;
      relink pred step;
      iter continue entries reloc passthrough [] jump stmts
  | Halt :: stmts ->
      List.iter (fun name -> S.Htbl.add entries name Halt) labels;
      (* not needed *)
      relink pred Halt;
      iter continue entries reloc passthrough [] Halt stmts

let rec lookup entries passthrough target =
  try S.Htbl.find entries target
  with Not_found -> (
    match S.Htbl.find passthrough target with
    | exception Not_found ->
        Options.Logger.fatal "label %S is not defined" target
    | target -> lookup entries passthrough target)

let of_script ?(continue = abort) stmts =
  let entries = S.Htbl.create 10 and passthrough = S.Htbl.create 10 in
  let reloc = iter continue entries [] passthrough [ "%start%" ] Halt stmts in
  List.iter
    (fun (pred, target, taken) ->
      relink ~taken pred (lookup entries passthrough target))
    reloc;
  S.Htbl.find entries "%start%"

let mk_cut (addr : Virtual_address.t) (saddr : string) (guard : Expr.t option)
    succ =
  match guard with
  | None -> Hook { addr; info = Printf.sprintf "cut at %s" saddr; succ = Cut }
  | Some guard ->
      let m, test = extract_load [] guard in
      Hook
        {
          addr;
          info =
            Format.asprintf "cut at %s if %a" saddr Dba_printer.Ascii.pp_bl_term
              guard;
          succ =
            define_load m (Branch { test; taken = Cut; fallthrough = succ });
        }

let mk_assume (addr : Virtual_address.t) (saddr : string) (guard : Expr.t) succ
    =
  let m, test = extract_load [] guard in
  Hook
    {
      addr;
      info =
        Format.asprintf "at %s assume %a" saddr Dba_printer.Ascii.pp_bl_term
          guard;
      succ = define_load m (Assume { test; succ });
    }

let mk_assert (addr : Virtual_address.t) (saddr : string) (guard : Expr.t) succ
    =
  let m, test = extract_load [] guard in
  Hook
    {
      addr;
      info =
        Format.asprintf "at %s assert %a" saddr Dba_printer.Ascii.pp_bl_term
          guard;
      succ = define_load m (Assert { test; succ });
    }

let mk_reach (addr : Virtual_address.t) (saddr : string) id
    (guard : Expr.t option) n rev_actions succ =
  let info, guard =
    match guard with
    | None -> (Printf.sprintf "reach %s" saddr, Expr.one)
    | Some test ->
        ( Format.asprintf "reach %s such that %a" saddr
            Dba_printer.Ascii.pp_bl_term test,
          test )
  in
  Hook
    {
      addr;
      info;
      succ =
        Probe
          {
            kind = Reach { id; n; guard; actions = List.rev rev_actions };
            succ;
          };
    }

let mk_enumerate (addr : Virtual_address.t) (saddr : string) id format
    (expr : Expr.t) n succ =
  Hook
    {
      addr;
      info =
        Format.asprintf "at %s enumerate %a" saddr Dba_printer.Ascii.pp_bl_term
          expr;
      succ =
        Probe
          {
            kind = Enumerate { enum = expr; id; format; n; k = 0; values = [] };
            succ;
          };
    }
