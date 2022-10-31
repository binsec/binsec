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

module Expr = struct
  include Term.Make (Dba.VarTag) (Basic_types.String)

  let var ?(tag = Dba.VarTag.Empty) name size = var name size tag

  let restrict lo hi e = restrict ~hi ~lo e

  let neg = uminus

  let size_of = sizeof

  let succ e = addi e 1

  let rec of_dba (e : Dba.Expr.t) =
    match e with
    | Cst bv -> constant bv
    | Var { name; size; info = tag } -> var ~tag name size
    | Load (len, dir, addr) -> load len dir (of_dba addr) "@"
    | Unary (op, e) -> unary (Fiber.Translate.unary e op) (of_dba e)
    | Binary (op, e0, e1) ->
        binary (Fiber.Translate.binary op) (of_dba e0) (of_dba e1)
    | Ite (c, t, e) -> ite (of_dba c) (of_dba t) (of_dba e)

  let pp_endianness ppf = function
    | LittleEndian -> ()
    | BigEndian -> Format.pp_print_string ppf "<-,"

  let rec pp ppf (t : t) =
    match t with
    | Var { name; label = Symbol (attr, _); _ } ->
        Format.fprintf ppf "<%s%a>" name Dba.VarTag.pp_attribute attr
    | Var { name; _ } -> Format.pp_print_string ppf name
    | Load { len; dir; addr; label; _ } ->
        Format.fprintf ppf "%s[%a,%a%d]" label pp addr pp_endianness dir len
    | Cst bv -> Bitvector.pp_hex_or_bin ppf bv
    | Unary { f = Uext n; x; _ } -> Format.fprintf ppf "@[(uext%d %a)@]" n pp x
    | Unary { f = Sext n; x; _ } -> Format.fprintf ppf "@[(sext%d %a)@]" n pp x
    | Unary { f = Restrict { hi; lo }; x; _ } when hi = lo ->
        Format.fprintf ppf "%a{%d}" pp x lo
    | Unary { f = Restrict { hi; lo }; x; _ } ->
        Format.fprintf ppf "%a{%d..%d}" pp x hi lo
    | Unary { f = Not; x; _ } -> Format.fprintf ppf "!%a" pp x
    | Unary { f = Minus; x; _ } -> Format.fprintf ppf "-%a" pp x
    | Binary { f; x; y; _ } ->
        Format.fprintf ppf "@[<hov 1>(%a@ %a@ %a)@]" pp x Term.pp_op f pp y
    | Ite { c; t; e; _ } ->
        Format.fprintf ppf "@[<hov 0>%a@ ? %a@ : %a@]" pp c pp t pp e
end

module LValue = struct
  type t =
    | Var of ([ `Var ], Dba.VarTag.t, string) Expr.term
    | Restrict of {
        var : ([ `Var ], Dba.VarTag.t, string) Expr.term;
        lo : int;
        hi : int;
      }
    | Store of ([ `Mem ], Dba.VarTag.t, string) Expr.term

  let size_of = function
    | Var (Var { size; _ }) -> size
    | Restrict { hi; lo; _ } -> hi - lo + 1
    | Store (Load { len; _ }) -> 8 * len

  let var name size = Var (Term.to_var_exn (Expr.var name size))

  let store len dir addr label =
    Store (Term.to_mem_exn (Expr.load len dir addr label))

  let to_expr = function
    | Var var -> Term.to_exp var
    | Restrict { hi; lo; var } -> Expr.restrict lo hi (Term.to_exp var)
    | Store load -> Term.to_exp load

  let pp ppf = function
    | Var var -> Expr.pp ppf (Term.to_exp var)
    | Restrict { hi; lo; var } when hi = lo ->
        Format.fprintf ppf "%a{%d}" Expr.pp (Term.to_exp var) lo
    | Restrict { hi; lo; var } ->
        Format.fprintf ppf "%a{%d..%d}" Expr.pp (Term.to_exp var) hi lo
    | Store load -> Expr.pp ppf (Term.to_exp load)

  let of_dba (lval : Dba.LValue.t) =
    match lval with
    | Var { name; size; info = tag } ->
        Var (Term.to_var_exn (Expr.var ~tag name size))
    | Restrict ({ name; size; info = tag }, { hi; lo }) ->
        Restrict { hi; lo; var = Term.to_var_exn (Expr.var ~tag name size) }
    | Store (len, dir, addr) ->
        Store (Term.to_mem_exn (Expr.load len dir (Expr.of_dba addr) "@"))
end

module Action = struct
  type format = Fiber.Action.format = Bin | Dec | Hex | Ascii

  type t =
    | Print_formula of (Expr.t * string) list option
    | Print_model
    | Print_value of format * Expr.t
    | Print_stream of string
    | Print_c_string of string
end

module Instr = struct
  type t =
    | Assign of LValue.t * Expr.t
    | Undef of LValue.t
    | Nondet of LValue.t
    | Assume of Expr.t
    | Assert of Expr.t
    | It of Expr.t * string
    | Goto of string
    | Jump of Expr.t
    | Label of string
    | Halt

  let assign loc value = Assign (loc, value)

  let undef loc = Undef loc

  let nondet loc = Nondet loc

  let assume test = Assume test

  let dynamic_assert test = Assert test

  let conditional_jump test target = It (test, target)

  let goto target = Goto target

  let dynamic_jump target = Jump target

  let label name = Label name

  let halt = Halt
end

module Hunk = struct
  type t = Instr.t list

  let pp ppf hunk =
    Format.pp_open_vbox ppf 0;
    Format.pp_open_vbox ppf 2;
    List.iter
      (fun (instr : Instr.t) ->
        match instr with
        | Assign (lval, rval) ->
            Format.fprintf ppf "@ @[<hov>%a := %a@]" LValue.pp lval Expr.pp rval
        | Undef lval ->
            Format.fprintf ppf "@ @[<hov>%a := \\undef@]" LValue.pp lval
        | Nondet lval ->
            Format.fprintf ppf "@ @[<hov>%a := \\nondet@]" LValue.pp lval
        | Assume test -> Format.fprintf ppf "@ @[<hov>assume %a@]" Expr.pp test
        | Assert test -> Format.fprintf ppf "@ @[<hov>assert %a]" Expr.pp test
        | It (test, target) ->
            Format.fprintf ppf "@ @[<hov>if %a@ goto %s@]" Expr.pp test target
        | Goto target -> Format.fprintf ppf "@ goto %s" target
        | Jump target -> Format.fprintf ppf "@ @[<hov>jump %a@]" Expr.pp target
        | Label name -> Format.fprintf ppf "@]@ @[<v 2>%s:" name
        | Halt -> Format.fprintf ppf "@ halt")
      hunk;
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ()

  let to_fiber =
    let open Instr in
    let open Fiber in
    let entropy = Printf.sprintf "%%entropy%%%d" in
    let abort =
      let rec step = Step { uop = Assert Expr.zero; succ = step } in
      step
    in
    let relink ?(taken = false) t succ =
      (match (t, succ) with
      | ((Hook _ | Exec _ | Step _ | Branch _) as t), Goto g ->
          g.preds <- t :: g.preds
      | _ -> ());
      match t with
      | Hook t -> t.succ <- succ
      | Exec t -> t.succ <- succ
      | Step t -> t.succ <- succ
      | Branch t when taken -> t.taken <- succ
      | Branch t -> t.fallthrough <- succ
      | Reach _ | Goto _ | Jump _ | Cut | Halt | Die _ -> ()
    in
    let rec iter continue ctx entries reloc passthrough labels pred stmts =
      match stmts with
      | [] ->
          List.iter (fun name -> S.Htbl.add entries name continue) labels;
          relink pred continue;
          reloc
      | Label name :: stmts ->
          iter continue ctx entries reloc passthrough (name :: labels) pred
            stmts
      | Assign (Var (Var { name; size; _ }), value) :: stmts ->
          let var = Translate.var ctx name size
          and rval = Translate.expr' ctx value in
          let step = Step { uop = Define { var; rval }; succ = Halt } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | Assign (Restrict { hi; lo; var = Var { name; size; _ } }, value)
        :: stmts ->
          let var = Translate.var ctx name size
          and rval =
            Translate.(
              complement (expr' ctx value) ~hi ~lo (evar ctx name size))
          in
          let step = Step { uop = Define { var; rval }; succ = Halt } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | Assign (Store (Load { dir; addr; label; _ }), value) :: stmts ->
          let base = Translate.array ctx label
          and addr = Translate.expr' ctx addr
          and rval = Translate.expr' ctx value in
          let step =
            Step { uop = Store { base; dir; addr; rval }; succ = Halt }
          in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | Nondet (Var (Var { name; size; _ })) :: stmts ->
          let var = Translate.var ctx name size in
          let step = Step { uop = Symbol var; succ = Halt } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | Nondet (Restrict { hi; lo; var = Var { name; size; _ } }) :: stmts ->
          let size' = hi - lo + 1 in
          let name' = entropy size' in
          let var' = Translate.var ctx name' size' in
          let var = Translate.var ctx name size in
          let rval =
            Translate.complement (Term.to_exp var') ~lo ~hi (Term.to_exp var)
          in
          let succ = Step { uop = Define { var; rval }; succ = Halt } in
          let step = Step { uop = Symbol var'; succ } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] succ stmts
      | Nondet (Store (Load { len; dir; addr; label; _ })) :: stmts ->
          let size' = 8 * len in
          let name' = entropy size' in
          let var' = Translate.var ctx name' size' in
          let rval = Term.to_exp var' in
          let base = Translate.array ctx label
          and addr = Translate.expr' ctx addr in
          let succ =
            Step { uop = Store { base; dir; addr; rval }; succ = Halt }
          in
          let step = Step { uop = Symbol var'; succ } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] succ stmts
      | Undef (Var (Var { name; size; _ })) :: stmts ->
          let var = Translate.(var ctx name size) in
          let step = Step { uop = Forget var; succ = Halt } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | Undef _ :: _ -> Options.Logger.fatal "only variables can be undefined"
      | Assume test :: stmts ->
          let test = Translate.expr' ctx test in
          let step = Step { uop = Assume test; succ = Halt } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | Assert test :: stmts ->
          let test = Translate.expr' ctx test in
          let step = Step { uop = Assert test; succ = Halt } in
          List.iter (fun name -> S.Htbl.add entries name step) labels;
          relink pred step;
          iter continue ctx entries reloc passthrough [] step stmts
      | It (test, target) :: stmts ->
          let test = Translate.expr' ctx test in
          let branch = Branch { test; taken = Halt; fallthrough = Halt } in
          List.iter (fun name -> S.Htbl.add entries name branch) labels;
          relink pred branch;
          iter continue ctx entries
            ((branch, target, true) :: reloc)
            passthrough [] branch stmts
      | Goto target :: stmts ->
          List.iter (fun name -> S.Htbl.add passthrough name target) labels;
          iter continue ctx entries
            (match pred with
            | Hook _ | Exec _ | Step _ | Branch _ ->
                (pred, target, false) :: reloc
            | Goto _ | Jump _ | Reach _ | Cut | Halt | Die _ -> reloc)
            passthrough [] Halt stmts
      | Jump (Cst bv) :: stmts ->
          let goto =
            Goto
              {
                addr = Virtual_address.of_bitvector bv;
                preds =
                  (match pred with
                  | (Hook _ | Exec _ | Step _ | Branch _) as p -> [ p ]
                  | Reach _ | Goto _ | Jump _ | Cut | Halt | Die _ -> []);
              }
          in
          List.iter (fun name -> S.Htbl.add entries name goto) labels;
          relink pred goto;
          iter continue ctx entries reloc passthrough [] goto stmts
      | Jump target :: stmts ->
          let target = Translate.expr' ctx target in
          let jump = Jump target in
          List.iter (fun name -> S.Htbl.add entries name jump) labels;
          relink pred jump;
          iter continue ctx entries reloc passthrough [] jump stmts
      | Halt :: stmts ->
          List.iter (fun name -> S.Htbl.add entries name Halt) labels;
          (* not needed *)
          relink pred Halt;
          iter continue ctx entries reloc passthrough [] Halt stmts
    in
    let rec lookup entries passthrough target =
      try S.Htbl.find entries target
      with Not_found -> (
        match S.Htbl.find passthrough target with
        | exception Not_found ->
            Options.Logger.fatal "label %S is not defined" target
        | target -> lookup entries passthrough target)
    in
    fun ?(continue = abort) ctx stmts ->
      let entries = S.Htbl.create 10 and passthrough = S.Htbl.create 10 in
      let reloc =
        iter continue ctx entries [] passthrough [ "%start%" ] Halt stmts
      in
      List.iter
        (fun (pred, target, taken) ->
          relink ~taken pred (lookup entries passthrough target))
        reloc;
      S.Htbl.find entries "%start%"
end

module Directive = struct
  type t =
    | Cut of Expr.t option
    | Assume of Expr.t
    | Assert of Expr.t
    | Reach of int * Expr.t option * Action.t list
    | Enumerate of int * Expr.t
end

type t =
  | Start_from of Expr.t * Instr.t list
  | Start_from_core of Instr.t list
  | Load_sections of string list
  | Load_data of Expr.t
  | Import_symbols of (string * Dba.VarTag.attribute) list * string
  | Stub of Expr.t list * Instr.t list
  | Init of Instr.t list
  | Directive of Expr.t * Directive.t
