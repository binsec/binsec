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

open Format

module type Renderer = sig
  val binary_ops : (Dba.Binary_op.t * string) list
  val unary_ops : (Dba.Unary_op.t * string) list
  val endiannesses : (Machine.endianness * string) list
  val string_of_digit_char : char -> string
  val left_right_parentheses : string * string
end

module AsciiRenderer = struct
  let binary_ops =
    [
      (Dba.Binary_op.Plus, "+");
      (Dba.Binary_op.Minus, "-");
      (Dba.Binary_op.Mult, "*");
      (Dba.Binary_op.DivU, "/u");
      (Dba.Binary_op.DivS, "/s");
      (Dba.Binary_op.ModU, "%u");
      (Dba.Binary_op.ModS, "%s");
      (Dba.Binary_op.Or, "|");
      (Dba.Binary_op.And, "&");
      (Dba.Binary_op.Xor, "^");
      (Dba.Binary_op.Concat, "::");
      (Dba.Binary_op.LShift, "<<");
      (Dba.Binary_op.RShiftU, ">>u");
      (Dba.Binary_op.RShiftS, ">>s");
      (Dba.Binary_op.LeftRotate, "lrot");
      (Dba.Binary_op.RightRotate, "rrot");
      (Dba.Binary_op.Eq, "=");
      (Dba.Binary_op.Diff, "<>");
      (Dba.Binary_op.LeqU, "<=u");
      (Dba.Binary_op.LtU, "<u");
      (Dba.Binary_op.GeqU, ">=u");
      (Dba.Binary_op.GtU, ">u");
      (Dba.Binary_op.LeqS, "<=s");
      (Dba.Binary_op.LtS, "<s");
      (Dba.Binary_op.GeqS, ">=s");
      (Dba.Binary_op.GtS, ">s");
    ]

  let unary_ops = [ (Dba.Unary_op.UMinus, "-"); (Dba.Unary_op.Not, "!") ]
  let endiannesses = [ (Machine.BigEndian, "B"); (Machine.LittleEndian, "L") ]
  let string_of_digit_char c = Format.sprintf "%c" c
  let left_right_parentheses = ("(", ")")
end

module UnicodeRenderer : Renderer = struct
  let binary_ops =
    [
      (Dba.Binary_op.Plus, "+");
      (Dba.Binary_op.Minus, "-");
      (Dba.Binary_op.Mult, "*");
      (Dba.Binary_op.DivU, "/");
      (Dba.Binary_op.DivS, "/𝒔");
      (Dba.Binary_op.ModU, "mod𝒖");
      (Dba.Binary_op.ModS, "mod𝒔");
      (Dba.Binary_op.Or, "||");
      (Dba.Binary_op.And, "&&");
      (Dba.Binary_op.Xor, "⨁");
      (Dba.Binary_op.Concat, "::");
      (Dba.Binary_op.LShift, "≪");
      (Dba.Binary_op.RShiftU, "≫𝒖");
      (Dba.Binary_op.RShiftS, "≫𝒔");
      (Dba.Binary_op.LeftRotate, "lrot");
      (Dba.Binary_op.RightRotate, "rrot");
      (Dba.Binary_op.Eq, "=");
      (Dba.Binary_op.Diff, "≠");
      (Dba.Binary_op.LeqU, "≤𝒖");
      (Dba.Binary_op.LtU, "<𝒖");
      (Dba.Binary_op.GeqU, "≥𝒖");
      (Dba.Binary_op.GtU, ">𝒖");
      (Dba.Binary_op.LeqS, "≤𝒔");
      (Dba.Binary_op.LtS, "<𝒔");
      (Dba.Binary_op.GeqS, "≥𝒔");
      (Dba.Binary_op.GtS, ">𝒔");
    ]

  let unary_ops = [ (Dba.Unary_op.UMinus, "-"); (Dba.Unary_op.Not, "¬") ]

  let endiannesses = [ (Machine.LittleEndian, "𝐿"); (Machine.BigEndian, "𝐵") ]

  let string_of_digit_char = function
    (* Unicode lowercase digits starts at 0x2080 *)
    | '0' -> "₀"
    | '1' -> "₁"
    | '2' -> "₂"
    | '3' -> "₃"
    | '4' -> "₄"
    | '5' -> "₅"
    | '6' -> "₆"
    | '7' -> "₇"
    | '8' -> "₈"
    | '9' -> "₉"
    | _ -> assert false

  let left_right_parentheses = ("₍", "₎")
end

module EIC (R : Renderer) : Renderer = struct
  (* Endian Independent Code: Consider that the code has no endianness *)
  include R

  (* On purpose: this will not print any endianness information *)
  let endiannesses = []
end

module type DbaPrinter = sig
  val pp_code_address : Format.formatter -> Dba.address -> unit
  val pp_tag : Format.formatter -> Dba.tag -> unit
  val pp_binary_op : Format.formatter -> Dba.Binary_op.t -> unit
  val pp_unary_op : Format.formatter -> Dba.Unary_op.t -> unit
  val pp_bl_term : Format.formatter -> Dba.Expr.t -> unit
  val pp_expr : Format.formatter -> Dba.Expr.t -> unit
  val pp_instruction : Format.formatter -> Dba.Instr.t -> unit
  val pp_lhs : Format.formatter -> Dba.LValue.t -> unit

  val pp_instruction_maybe_goto :
    current_id:int -> Format.formatter -> Dba.Instr.t -> unit
end

module Make (R : Renderer) : DbaPrinter = struct
  let mk_tbl assocs =
    let h = Hashtbl.create (List.length assocs) in
    List.iter (fun (key, value) -> Hashtbl.add h key value) assocs;
    h

  let binary_op_tbl = mk_tbl R.binary_ops
  and unary_op_tbl = mk_tbl R.unary_ops
  and endianness_tbl = mk_tbl R.endiannesses

  let find_or_default h key default =
    try Hashtbl.find h key with Not_found -> default

  let pp_binary_op ppf bop =
    fprintf ppf "%s" (find_or_default binary_op_tbl bop "?bop?")

  let pp_unary_op ppf uop =
    fprintf ppf "%s" (find_or_default unary_op_tbl uop "?uop?")

  let _pp_endianness ppf endianness =
    fprintf ppf "%s" (find_or_default endianness_tbl endianness "")

  let pp_size ppf size =
    let is_digit c =
      try
        let ccode = Char.code c in
        ccode >= 48 && ccode <= 57
      with Invalid_argument _ -> false
    in
    let encode_char c = if is_digit c then R.string_of_digit_char c else "X" in
    let b = Buffer.create 16 in
    (* Wild guess: how many bytes do we need for the size ?*)
    String.iter
      (fun c -> Buffer.add_string b (encode_char c))
      (string_of_int size);
    fprintf ppf "%s" (Buffer.contents b)

  let pp_code_address ppf addr =
    fprintf ppf "(%a, %d)" Virtual_address.pp addr.Dba.base addr.Dba.id

  let pp_opt pp_value ppf = function
    | None -> ()
    | Some value -> fprintf ppf "%a" pp_value value

  let pp_tag ppf = function
    | Dba.Default -> ()
    | Dba.Call caddr ->
        fprintf ppf "#call with return address %@ %a" pp_code_address caddr
    | Dba.Return -> fprintf ppf "#return"

  (* Arbitrarily set value limits displayed as integer *)
  let max_display_int = Z.of_int 4096
  let min_display_int = Z.of_int (-4096)

  let pp_constant ppf bv =
    let n = Bitvector.signed_of bv in
    if n < min_display_int || n > max_display_int then
      fprintf ppf "%a" Bitvector.pp_hex_or_bin bv
    else
      let size = Bitvector.size_of bv in
      fprintf ppf "%s<%a>" (Z.to_string n) pp_size size

  let pp_array =
    Format.pp_print_option
      ~none:(fun ppf () -> Format.pp_print_char ppf '@')
      Format.pp_print_string

  let rec pp_bl_term ppf = function
    | Dba.Expr.Var { name; size; _ } -> fprintf ppf "%s<%a>" name pp_size size
    | Dba.Expr.Load (size, _endian, expr, array) ->
        fprintf ppf "%a[%a,%a]" pp_array array pp_bl_term expr pp_size size
        (* pp_endianness endian *)
    | Dba.Expr.Cst bv -> pp_constant ppf bv
    | Dba.Expr.Unary (Dba.Unary_op.Uext n, expr) ->
        fprintf ppf "@[(extu %a %d)@]" pp_bl_term expr n
    | Dba.Expr.Unary (Dba.Unary_op.Sext n, expr) ->
        fprintf ppf "@[(exts %a %d)@]" pp_bl_term expr n
    | Dba.Expr.Unary
        (Dba.Unary_op.Restrict { Interval.lo = i; Interval.hi = j }, expr) ->
        if i = j then fprintf ppf "%a{%d}" pp_bl_term expr i
        else fprintf ppf "%a{%d,%d}" pp_bl_term expr i j
    | Dba.Expr.Unary (unary_op, expr) ->
        fprintf ppf "@[%a@ (%a)@]" pp_unary_op unary_op pp_bl_term expr
    | Dba.Expr.Binary (binary_op, lexpr, rexpr) ->
        fprintf ppf "@[<hov 1>(%a@ %a@ %a)@]" pp_bl_term lexpr pp_binary_op
          binary_op pp_bl_term rexpr
    | Dba.Expr.Ite (cond, then_expr, else_expr) ->
        fprintf ppf "@[<hov 0>%a@ ? %a@ : %a@]" pp_bl_term cond pp_bl_term
          then_expr pp_bl_term else_expr

  let pp_lhs ppf = function
    | Dba.(LValue.Var { name; size; _ }) -> fprintf ppf "%s<%d>" name size
    | Dba.(LValue.Restrict ({ name; size; _ }, { Interval.lo; Interval.hi })) ->
        if lo <> hi then fprintf ppf "%s<%d>{%d, %d}" name size lo hi
        else fprintf ppf "%s<%d>{%d}" name size lo
    | Dba.LValue.Store (size, _endian, expr, array) ->
        fprintf ppf "%a[%a,%a]" pp_array array pp_bl_term expr pp_size size
  (* pp_endianness endian *)

  let pp_address ppf = function
    | Dba.JInner id -> fprintf ppf "%d" id
    | Dba.JOuter caddr -> fprintf ppf "%a" pp_code_address caddr

  let pp_state ppf = function
    | Dba.OK -> fprintf ppf "OK"
    | Dba.KO -> fprintf ppf "KO"
    | Dba.Undecoded s -> fprintf ppf "#undecoded %s" s
    | Dba.Unsupported s -> fprintf ppf "#unsupported %s" s

  let pp_instruction n ppf instruction =
    let suffix ppf id =
      match n with
      | None -> fprintf ppf ""
      | Some value ->
          if id = value + 1 then fprintf ppf ";" else fprintf ppf "; goto %d" id
    in
    match instruction with
    | Dba.Instr.Assign (lhs, expr, id) ->
        fprintf ppf "@[<hov 1>%a :=@ %a@,%a@]" pp_lhs lhs pp_bl_term expr suffix
          id
    | Dba.Instr.SJump (addr, tag) ->
        fprintf ppf "goto %a %a" pp_address addr pp_tag tag
    | Dba.Instr.DJump (e_addr, tag) ->
        fprintf ppf "goto %a %a" pp_bl_term e_addr pp_tag tag
    | Dba.Instr.If (e, addr, int_addr) ->
        fprintf ppf "@[<hov 2>if %a@ @[<hv 0>goto %a@ else goto %d@]@]"
          pp_bl_term e pp_address addr int_addr
    | Dba.Instr.Stop state_opt -> fprintf ppf "%a" (pp_opt pp_state) state_opt
    | Dba.Instr.Assume (cond, id) ->
        fprintf ppf "%@assume (%a)%a" pp_bl_term cond suffix id
    | Dba.Instr.Assert (cond, id) ->
        fprintf ppf "%@assert (%a)%a" pp_bl_term cond suffix id
    | Dba.Instr.Undef (lhs, id) ->
        fprintf ppf "%a := \\undef%a" pp_lhs lhs suffix id
    | Dba.Instr.Nondet (lhs, id) ->
        fprintf ppf "%a := nondet%a" pp_lhs lhs suffix id

  let pp_expr = pp_bl_term
  let old_pp = pp_instruction
  let pp_instruction ppf instruction = old_pp None ppf instruction

  let pp_instruction_maybe_goto ~current_id ppf instruction =
    old_pp (Some current_id) ppf instruction
end

module Ascii = Make (AsciiRenderer)
module EICAscii = Make (EIC (AsciiRenderer))
module Unicode = Make (UnicodeRenderer)
module EICUnicode = Make (EIC (UnicodeRenderer))
