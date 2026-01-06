(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module Logger = Dba_logger

let cur_address = ref 0
let incr_address addr = cur_address := addr.Dba.id + 1
let cur_address () = !cur_address

module Declarations = struct
  module SH = Basic_types.String.Htbl

  let declarations : (Dba.size * Dba.Var.Tag.t) SH.t = SH.create 16
  let add name size opttags = SH.add declarations name (size, opttags)
end

module Mk = struct
  open Dba

  module Predicates = struct
    let rec of_list l =
      match l with
      | [] -> assert false
      | [ (elmt, p) ] -> ([ elmt ], p)
      | (elmt, (p1, p2, p3)) :: l ->
          let elmts, (q1, q2, q3) = of_list l in
          ( elmt :: elmts,
            (Expr.logand p1 q1, Expr.logand p2 q2, Expr.logand p3 q3) )
  end
end

let expr_of_name name =
  let first_char = name.[0] in
  if first_char = '_' || first_char = '?' || first_char = '!' then
    let name = if first_char = '_' then "*" else name in
    Dba.Expr.var name 0
  else
    let open Dba.Expr in
    let reg name = var name 32 in
    let eax = reg "eax"
    and ebx = reg "ebx"
    and ecx = reg "ecx"
    and edx = reg "edx" in
    match name with
    | "al" -> restrict 0 7 eax
    | "ah" -> restrict 8 15 eax
    | "ax" -> restrict 0 15 eax
    | "eax" -> eax
    | "bl" -> restrict 0 7 ebx
    | "bh" -> restrict 8 15 ebx
    | "bx" -> restrict 0 15 ebx
    | "ebx" -> ebx
    | "cl" -> restrict 0 7 ecx
    | "ch" -> restrict 8 15 ecx
    | "cx" -> restrict 0 15 ecx
    | "ecx" -> ecx
    | "dl" -> restrict 0 7 edx
    | "dh" -> restrict 8 15 edx
    | "dx" -> restrict 0 15 edx
    | "edx" -> edx
    | "di" -> restrict 0 15 (reg "edi")
    | "edi" -> reg "edi"
    | "si" -> restrict 0 15 (reg "esi")
    | "esi" -> reg "esi"
    | "bp" -> restrict 0 15 (reg "ebp")
    | "ebp" -> reg "ebp"
    | "sp" -> restrict 0 15 (reg "esp")
    | "esp" -> reg "esp"
    | "btemp" -> temporary "btemp" ~size:8
    | "stemp" -> temporary "stemp" ~size:16
    | "temp" -> temporary "temp" ~size:32
    | "dtemp" -> temporary "dtemp" ~size:64
    | name ->
        Logger.error "Unknown variable name: %s" name;
        raise Parsing.Parse_error

let is_wildmetapld_expr = function
  | Dba.(Expr.Var { name; _ }) ->
      let c = name.[0] in
      c = '*' || c = '?' || c = '!'
  | _ -> false

let rec patch_expr_size e sz =
  let open Dba.Expr in
  Logger.debug ~level:2 "Will patch: %a with %d" Dba_printer.Ascii.pp_bl_term e
    sz;
  match e with
  | Dba.(Expr.Var { name; info = tag; _ }) ->
      if is_wildmetapld_expr e then e else var name sz ~tag
  | Dba.Expr.Cst bv -> constant (Bitvector.create (Bitvector.value_of bv) sz)
  | Dba.Expr.Load (_old_sz, en, e, array) ->
      let bysz = Size.(Bit.create sz |> Byte.of_bitsize) in
      load bysz en e ?array
  | Dba.Expr.Unary (Dba.Unary_op.Uext size, e) ->
      uext size (patch_expr_size e (sz - size))
  | Dba.Expr.Unary (Dba.Unary_op.Sext size, e) ->
      sext size (patch_expr_size e (sz - size))
  | Dba.Expr.Unary (op, e) -> unary op (patch_expr_size e sz)
  | _ -> e

module Message = struct
  module Value = struct
    type t = Int of Z.t | Str of string

    let vstr v = Str v
    let vint v = Int (Z.of_string v)
  end

  module Instruction = struct
    type t =
      | Undefined
      | Unimplemented
      | Unsupported of {
          read : Dba.LValue.t list;
          write : Dba.LValue.t list;
          goto : Virtual_address.t;
        }
      | Precise of (Dba.address * Dba.Instr.t) list
  end

  type t = (string * Value.t) list * Instruction.t
end

module Initialization = struct
  open Dba

  type rvalue =
    | Nondet
    | Signed_interval of Dba.Expr.t * Dba.Expr.t
    | Unsigned_interval of Dba.Expr.t * Dba.Expr.t
    | Set of Dba.Expr.t list
    | Singleton of Dba.Expr.t

  type identifier = string

  type operation =
    | Assignment of Dba.LValue.t * rvalue * identifier option
    | Mem_load of Dba.Expr.t * int
    | Assumption of Dba.Expr.t
    | Universal of Dba.LValue.t

  type t = { controlled : bool; operation : operation }

  let create ~controlled ~operation = { controlled; operation }
  let assume e = create ~controlled:false ~operation:(Assumption e)

  let assign ?identifier ?(controlled = true) lval rval =
    let operation = Assignment (lval, rval, identifier) in
    create ~controlled ~operation

  let universal lval = create ~controlled:false ~operation:(Universal lval)

  let from_assignment ?identifier ?(controlled = true) = function
    | Dba.Instr.Assign (lval, rval, _) ->
        assign ?identifier ~controlled lval (Singleton rval)
    | Dba.Instr.Nondet (lval, _) -> assign ?identifier ~controlled lval Nondet
    | _ -> failwith "initialization with non assignment"

  let from_store ?(controlled = true) lv =
    Logger.debug "Init from store %a" Dba_printer.Ascii.pp_lhs lv;
    match lv with
    | LValue.Store (size, _, addr, None) ->
        (* assert (Dba.Expr.size_of addr = Kernel_options.Machine.word_size ()); *)
        let operation = Mem_load (addr, size) in
        create ~controlled ~operation
    | _ -> failwith "initialization from file with non store"

  let set_control controlled t =
    match t.operation with Universal _ -> t | _ -> { t with controlled }
end

let mk_patches l =
  List.fold_left
    (fun vmap (vaddr, opcode) ->
      Virtual_address.Map.add (Virtual_address.create vaddr) opcode vmap)
    Virtual_address.Map.empty l
