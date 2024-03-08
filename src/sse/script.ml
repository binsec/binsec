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

open Libparser
module S = Basic_types.String

type env = {
  wordsize : int;
  endianness : Machine.endianness;
  define : Dba.Var.t -> Lexing.position -> unit;
  origin : string -> Lexing.position option;
  lookup : string -> Dba.LValue.t;
  lookup_symbol : string -> Dba.Var.Tag.attribute -> Dba.Expr.t;
}

type 'a loc = 'a Ast.loc

module Symbol = Ast.Symbol
module Loc = Ast.Loc
module Expr = Ast.Expr
module Instr = Ast.Instr

exception Inference_failure of Expr.t loc
exception Invalid_size of Expr.t loc * int * int
exception Invalid_operation of Expr.t loc
exception Invalid_annotation of (Loc.t loc * int * Lexing.position option)

let _ =
  Printexc.register_printer (function
    | Inference_failure (e, pos) ->
        Some
          (Format.asprintf "@[<v>Unable to infer the size of %a %a@]" Expr.pp e
             Parse_utils.pp_pos pos)
    | Invalid_size ((e, pos), size, expect) ->
        Some
          (Format.asprintf "@[<v>Invalid size for %a (expected %d, got %d) %a@]"
             Expr.pp e expect size Parse_utils.pp_pos pos)
    | Invalid_operation (e, pos) ->
        Some
          (Format.asprintf "@[<v>Invalid operation in %a %a@]" Expr.pp e
             Parse_utils.pp_pos pos)
    | Invalid_annotation ((var, pos), _, Some pos') ->
        Some
          (Format.asprintf
             "@[<v>Conflicting size annotation in %a %a@ Previous definition \
              %a@]"
             Loc.pp var Parse_utils.pp_pos pos Parse_utils.pp_pos pos')
    | Invalid_annotation ((var, pos), size, None) ->
        Some
          (Format.asprintf
             "@[<v>Invalid size annotation in %a (expected <%d>) %a@]" Loc.pp
             var size Parse_utils.pp_pos pos)
    | _ -> None)

let rec eval_expr ?size ((e, p) as t : Expr.t loc) env =
  let e =
    match e with
    | Int z -> (
        match size with
        | None -> raise (Inference_failure t)
        | Some size ->
            (if Z.numbits z > size then
               let line = p.pos_lnum and column = p.pos_cnum - p.pos_bol - 1 in
               Options.Logger.warning
                 "integer %a (line %d, column %d) does not fit in a bitvector \
                  of %d bit%s"
                 Z.pp_print z line column size
                 (if size > 1 then "s" else ""));
            Dba.Expr.constant (Bitvector.create z size))
    | Bv bv -> Dba.Expr.constant bv
    | Symbol ((name, attr), _) -> env.lookup_symbol name attr
    | Loc (Sub ({ hi; lo }, loc), _) ->
        Dba.Expr.restrict lo hi
          (Dba.LValue.to_expr (eval_loc ?size:None loc env))
    | Loc loc -> Dba.LValue.to_expr (eval_loc ?size loc env)
    | Unary (op, x) ->
        let size =
          match op with Restrict _ | Uext _ | Sext _ -> None | _ -> size
        in
        Dba.Expr.unary op (eval_expr ?size x env)
    | Binary (op, x, y) ->
        let x, y = eval_binary ?size ~op x y env in
        Dba.Expr.binary op x y
    | Ite (q, x, y) ->
        let q = eval_expr ~size:1 q env in
        let x, y = eval_binary ?size x y env in
        Dba.Expr.ite q x y
  in
  Option.iter
    (fun size ->
      let size' = Dba.Expr.size_of e in
      if size' <> size then raise (Invalid_size (t, size', size)))
    size;
  e

and eval_binary ?(first = true) ?size ?op x y env =
  match
    eval_expr
      ?size:
        (match op with
        | None -> size
        | Some
            ( Plus | Minus | Mult | DivU | DivS | ModU | ModS | Or | And | Xor
            | LShift | RShiftU | RShiftS | LeftRotate | RightRotate ) ->
            size
        | Some _ -> None)
      x env
  with
  | x ->
      ( x,
        eval_expr
          ?size:
            (match op with
            | Some Concat -> (
                match size with
                | None -> None
                | Some size -> Some (size - Dba.Expr.size_of x))
            | None | Some _ -> Some (Dba.Expr.size_of x))
          y env )
  | exception Inference_failure _ when first ->
      let y, x = eval_binary ~first:false ?size ?op y x env in
      (x, y)

and eval_int ((e, _) as t : Expr.t loc) env =
  match e with
  | Int z ->
      if not (Z.fits_int z) then raise (Invalid_operation t);
      Z.to_int z
  | Bv bv ->
      if not (Z.fits_int (Bitvector.signed_of bv)) then
        raise (Invalid_operation t);
      Bitvector.to_int bv
  | Symbol ((name, attr), _) -> (
      match env.lookup_symbol name attr with
      | Var { info = Symbol (_, (lazy bv)); _ } ->
          if not (Z.fits_int (Bitvector.value_of bv)) then
            raise (Invalid_operation t);
          Bitvector.to_uint bv
      | _ -> raise (Invalid_operation t))
  | Unary (UMinus, x) -> -eval_int x env
  | Binary (Plus, x, y) -> eval_int x env + eval_int y env
  | Binary (Minus, x, y) -> eval_int x env - eval_int y env
  | Binary (Mult, x, y) -> eval_int x env * eval_int y env
  | Binary (DivS, x, y) -> eval_int x env / eval_int y env
  | Loc _ | Unary _ | Binary _ | Ite _ -> raise (Invalid_operation t)

and declare_var name size pos env =
  let var = Dba.Var.create name ~bitsize:(Size.Bit.create size) ~tag:Empty in
  env.define var pos;
  Dba.LValue.v var

and eval_var ?size ((_, p) as t) name (annot : Ast.Size.t) env =
  let lval =
    match env.lookup name with
    | lval ->
        let size' =
          match annot with
          | Explicit size -> size
          | Sizeof lval ->
              let lval = eval_loc lval env in
              Dba.LValue.size_of lval
          | Eval expr ->
              let size = eval_int expr env in
              if size < 0 then raise (Invalid_operation expr);
              size
          | Implicit -> Dba.LValue.size_of lval
        and size = Dba.LValue.size_of lval in
        if size <> size' then
          raise (Invalid_annotation (t, size, env.origin name));
        lval
    | exception Not_found -> (
        match annot with
        | Explicit size -> declare_var name size p env
        | Sizeof lval ->
            let lval = eval_loc lval env in
            declare_var name (Dba.LValue.size_of lval) p env
        | Eval expr ->
            let size = eval_int expr env in
            if size < 0 then raise (Invalid_operation expr);
            declare_var name size p env
        | Implicit -> (
            match size with
            | None -> raise (Inference_failure (Expr.loc t, p))
            | Some size -> declare_var name size p env))
  in
  Option.iter
    (fun size ->
      let size' = Dba.LValue.size_of lval in
      if size' <> size then raise (Invalid_size ((Expr.loc t, p), size', size)))
    size;
  lval

and eval_loc ?size ((l, p) as t : Loc.t loc) env =
  let lval =
    match l with
    | Var (name, annot) -> eval_var ?size t name annot env
    | Load (len, endianness, addr, array) ->
        let endianness =
          Option.fold ~none:env.endianness ~some:Fun.id endianness
        in
        let addr = eval_expr ~size:env.wordsize addr env in
        Dba.LValue.store (Size.Byte.create len) endianness addr ?array
    | Sub ({ hi; lo }, ((Var (name, annot), _) as t')) -> (
        match eval_var ?size t' name annot env with
        | Var var -> Dba.LValue.restrict var lo hi
        | Restrict (var, { hi = hi'; lo = lo' }) ->
            if hi' > hi + lo' then raise (Inference_failure (Expr.loc t, p));
            Dba.LValue.restrict var (lo + lo') (hi + lo')
        | _ -> raise (Invalid_operation (Expr.loc t, p)))
    | Sub _ -> raise (Invalid_operation (Expr.loc t, p))
  in
  Option.iter
    (fun size ->
      let size' = Dba.LValue.size_of lval in
      if size' <> size then raise (Invalid_size ((Expr.loc t, p), size', size)))
    size;
  lval

module Output = struct
  type format = Types.Output.format = Bin | Dec | Hex | Ascii

  type t =
    | Model
    | Formula
    | Slice of (Expr.t loc * string) list
    | Value of format * Expr.t loc
    | Stream of string
    | String of string

  let eval env (t : t) : Types.Output.t =
    match t with
    | Model -> Model
    | Formula -> Formula
    | Slice values ->
        Slice (List.map (fun (e, name) -> (eval_expr e env, name)) values)
    | Value (fmt, e) -> Value (fmt, eval_expr e env)
    | Stream name -> Stream name
    | String name -> String name

  let format_str = function
    | Bin -> "bin"
    | Dec -> "dec"
    | Hex -> "hexa"
    | Ascii -> "ascii"

  let pp ppf = function
    | Model -> Format.pp_print_string ppf "model"
    | Formula -> Format.pp_print_string ppf "formula"
    | Slice defs ->
        Format.fprintf ppf "formula for %a"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
             (fun ppf ((expr, _), name) ->
               Format.fprintf ppf "%a as %s" Expr.pp expr name))
          defs
    | Value (fmt, (e, _)) ->
        Format.fprintf ppf "%s %a" (format_str fmt) Expr.pp e
    | Stream name -> Format.fprintf ppf "ascii stream %s" name
    | String name -> Format.fprintf ppf "c string %s" name
end

type Ast.Obj.t +=
  | Int of int
  | Int_list of int list
  | Format of Output.format
  | Output of Output.t
  | Output_list of Output.t list
  | String_list of string list
  | Key_val of (string * string)
  | Key_val_list of (string * string) list
  | Symbol_list of Symbol.t loc list
  | Loc_opt of Loc.t loc option
  | Loc_opt_list of Loc.t loc option list
  | Expr_opt of Expr.t loc option
  | Expr_list of Expr.t loc list
  | Named of (Expr.t loc * string)
  | Named_list of (Expr.t loc * string) list

type Ast.Instr.t +=
  | Argument of Loc.t loc * int  (** [lval] := arg([i]) *)
  | Return of Expr.t loc option  (** return [rval] *)
  | Cut of Expr.t loc option
  | Print of Output.t
  | Reach of int * Expr.t loc option * Output.t list
  | Enumerate of int * Expr.t loc

type Ast.t +=
  | Starting_from of Expr.t loc * Instr.t list
  | Starting_from_core of Instr.t list
  | Load_sections of string list
  | Load_data of Loc.t loc
  | Concretize_stack_pointer
  | Import_symbols of Symbol.t loc list * string
  | Hook of Expr.t loc list * Instr.t list * bool
  | Decode of Binstream.t * Instr.t list
  | Init of Instr.t list
  | Explore_all

let pp_comma_list pp =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
    pp

let pp_exprs = pp_comma_list (fun ppf (e, _) -> Expr.pp ppf e)

let pp_outputs =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf " and ")
    (fun ppf output -> Format.fprintf ppf "print %a" Output.pp output)

let () =
  Ast.Instr.register_pp (fun ppf inst ->
      match inst with
      | Argument ((lval, _), n) ->
          Format.fprintf ppf "%a := arg(%d)" Loc.pp lval n;
          true
      | Return None ->
          Format.pp_print_string ppf "return";
          true
      | Return (Some (rval, _)) ->
          Format.fprintf ppf "return %a" Expr.pp rval;
          true
      | Cut guard ->
          Format.fprintf ppf "cut %a"
            (fun ppf guard ->
              Option.iter
                (fun (test, _) -> Format.fprintf ppf " if %a" Expr.pp test)
                guard)
            guard;
          true
      | Print output ->
          Format.fprintf ppf "print %a" Output.pp output;
          true
      | Enumerate (n, (rval, _)) ->
          Format.fprintf ppf "enumerate%s %a%a"
            (if n = max_int then "*" else "")
            Expr.pp rval
            (fun ppf n ->
              if n <> 1 && n <> max_int then Format.fprintf ppf " (%d)" n)
            n;
          true
      | Reach (n, guard, outputs) ->
          Format.fprintf ppf "reach%s%a%a%a"
            (if n < 0 then "*" else "")
            (fun ppf n -> if 1 < n then Format.fprintf ppf " %d times" n)
            n
            (fun ppf guard ->
              Option.iter
                (fun (test, _) ->
                  Format.fprintf ppf " such that %a" Expr.pp test)
                guard)
            guard
            (fun ppf outputs ->
              if outputs <> [] then
                Format.fprintf ppf " then %a" pp_outputs outputs)
            outputs;
          true
      | _ -> false)

let pp_stmts = Format.pp_print_list ~pp_sep:Format.pp_print_space Instr.pp

let pp_with_stmts ppf stmts =
  match stmts with
  | [] -> Format.pp_close_box ppf ()
  | stmts -> Format.fprintf ppf " with@ %a@]@ end" pp_stmts stmts

let decl_printers = ref []
let register_pp pp = decl_printers := pp :: !decl_printers

let rec resolve_pp ppf decl = function
  | [] -> Format.pp_print_string ppf "unknown"
  | pp :: printers -> if not (pp ppf decl) then resolve_pp ppf decl printers

let pp_options =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ',')
    (fun ppf (k, v) ->
      Format.fprintf ppf "%S" k;
      if v <> "" then Format.fprintf ppf "=%S" v)

let pp ppf decl =
  match decl with
  | Starting_from ((addr, _), stmts) ->
      Format.fprintf ppf "@[<v>@[<v 2>starting from %a%a@]" Expr.pp addr
        pp_with_stmts stmts
  | Starting_from_core stmts ->
      Format.fprintf ppf "@[<v>@[<v 2>starting from core%a@]" pp_with_stmts
        stmts
  | Load_sections [ name ] ->
      Format.fprintf ppf "load section %s from file" name
  | Load_sections names ->
      Format.fprintf ppf "load sections %a from file"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           Format.pp_print_string)
        names
  | Load_data (store, _) -> Format.fprintf ppf "%a from file" Loc.pp store
  | Concretize_stack_pointer ->
      Format.pp_print_string ppf "with concrete stack pointer"
  | Import_symbols (symbols, file) ->
      Format.fprintf ppf "import %a from %s"
        (pp_comma_list (fun ppf (sym, _) -> Symbol.pp ppf sym))
        symbols file
  | Hook (addr, stmts, _) ->
      Format.fprintf ppf "@[<v>@[<v 2>hook %a by@ %a@]@ end@]" pp_exprs addr
        pp_stmts stmts
  | Decode (opcode, stmts) ->
      Format.fprintf ppf "@[<v>@[<v 2>hook opcode %a by@ %a@]@ end@]"
        Binstream.pp opcode pp_stmts stmts
  | Init stmts -> Format.fprintf ppf "@[<v>%a@]" pp_stmts stmts
  | Explore_all -> Format.pp_print_string ppf "explore all"
  | _ -> resolve_pp ppf decl !decl_printers

let _pp_global ppf global_data =
  Format.fprintf ppf "@[<v>%a@]"
    (fun ppf map ->
      S.Map.iter
        (fun name values ->
          Format.fprintf ppf "@[<h>%s: %a@]@ " name
            (fun ppf set ->
              S.Set.iter (fun value -> Format.fprintf ppf "%s@ " value) set)
            values)
        map)
    global_data

let grammar :
    ( unit,
      Libparser.obj,
      unit,
      unit,
      Libparser.obj Dyp.dyplexbuf )
    Dyp.dyp_action
    list =
  [
    Dyp.Bind_to_cons
      [
        ("byte", "Obj");
        ("byte_rev_list", "Obj");
        ("key_value", "Obj");
        ("comma_separated_key_value_rev_list", "Obj");
        ("options", "Obj");
        ("format", "Obj");
        ("named", "Obj");
        ("comma_separated_named_rev_list", "Obj");
        ("output", "Obj");
        ("and_separated_output_rev_list", "Obj");
        ("outputs", "Obj");
        ("times", "Obj");
        ("such_that", "Obj");
        ("guard", "Obj");
        ("num", "Obj");
        ("qident", "String");
        ("section", "String");
        ("comma_separated_section_rev_list", "Obj");
        ("comma_separated_symbol_rev_list", "Obj");
        ("arg", "Obj");
        ("comma_separated_arg_rev_list", "Obj");
        ("arguments", "Stmt");
        ("expr_opt", "Obj");
        ("comma_separated_expr_rev_list", "Obj");
        ("with_stmts_end", "Stmt");
        ("directive", "Stmt");
        ("decl", "Decl");
        ("rev_program", "Program");
        ("program", "Program");
      ];
    Dyp.Add_rules
      [
        ( ( "byte",
            [
              Dyp.Regexp
                (Dyp.RE_Seq [ Dyp.RE_Name "hexdigit"; Dyp.RE_Name "hexdigit" ]);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Lexeme_matched byte ] ->
                (Syntax.Obj (Int (Z.to_int (Z.of_string_base 16 byte))), [])
            | _ -> assert false );
        ( ("byte_rev_list", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Int_list []), []) );
        ( ( "byte_rev_list",
            [
              Dyp.Non_ter ("byte_rev_list", No_priority);
              Dyp.Non_ter ("byte", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Int_list bytes); Syntax.Obj (Int byte) ] ->
                (Syntax.Obj (Int_list (byte :: bytes)), [])
            | _ -> assert false );
        ( ( "key_value",
            [ Dyp.Non_ter ("qident", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.String key ] -> (Syntax.Obj (Key_val (key, "")), [])
            | _ -> assert false );
        ( ( "key_value",
            [
              Dyp.Non_ter ("qident", No_priority);
              Dyp.Regexp (RE_Char '=');
              Dyp.Non_ter ("qident", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.String key; _; Syntax.String value ] ->
                (Syntax.Obj (Key_val (key, value)), [])
            | _ -> assert false );
        ( ( "comma_separated_key_value_rev_list",
            [ Dyp.Non_ter ("key_value", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Key_val pair) ] ->
                (Syntax.Obj (Key_val_list [ pair ]), [])
            | _ -> assert false );
        ( ( "comma_separated_key_value_rev_list",
            [
              Dyp.Non_ter ("comma_separated_key_value_rev_list", No_priority);
              Dyp.Regexp (RE_Char ',');
              Dyp.Non_ter ("key_value", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Key_val_list l); _; Syntax.Obj (Key_val pair) ] ->
                (Syntax.Obj (Key_val_list (pair :: l)), [])
            | _ -> assert false );
        ( ("options", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Key_val_list []), []) );
        ( ( "options",
            [
              Dyp.Regexp (RE_Char '[');
              Dyp.Non_ter ("comma_separated_key_value_rev_list", No_priority);
              Dyp.Regexp (RE_Char ']');
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj (Key_val_list l); _ ] ->
                (Syntax.Obj (Key_val_list (List.rev l)), [])
            | _ -> assert false );
        ( ("num", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Int 1), []) );
        ( ( "num",
            [
              Dyp.Regexp (RE_Char '('); Dyp.Ter "INT"; Dyp.Regexp (RE_Char ')');
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj_INT n; _ ] -> (Syntax.Obj (Int (Z.to_int n)), [])
            | _ -> assert false );
        ( ("times", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Int 1), []) );
        ( ( "times",
            [ Dyp.Ter "INT"; Dyp.Regexp (RE_String "times") ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj_INT n; _ ] -> (Syntax.Obj (Int (Z.to_int n)), [])
            | _ -> assert false );
        ( ("format", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Format Output.Hex), []) );
        ( ("format", [ Dyp.Regexp (RE_String "ascii") ], "default_priority", []),
          fun _ -> function _ -> (Syntax.Obj (Format Output.Ascii), []) );
        ( ("format", [ Dyp.Regexp (RE_String "hexa") ], "default_priority", []),
          fun _ -> function _ -> (Syntax.Obj (Format Output.Hex), []) );
        ( ("format", [ Dyp.Regexp (RE_String "bin") ], "default_priority", []),
          fun _ -> function _ -> (Syntax.Obj (Format Output.Bin), []) );
        ( ("format", [ Dyp.Regexp (RE_String "dec") ], "default_priority", []),
          fun _ -> function _ -> (Syntax.Obj (Format Output.Dec), []) );
        ( ("named", [ Dyp.Non_ter ("var", No_priority) ], "default_priority", []),
          fun _ -> function
            | [ Syntax.Loc ((Loc.Var (name, _), pos) as var) ] ->
                (Syntax.Obj (Named ((Expr.loc var, pos), name)), [])
            | _ -> assert false );
        ( ( "named",
            [
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Regexp (RE_String "as");
              Dyp.Non_ter ("ident", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Expr expr; _; Syntax.String alias ] ->
                (Syntax.Obj (Named (expr, alias)), [])
            | _ -> assert false );
        ( ( "comma_separated_named_rev_list",
            [ Dyp.Non_ter ("named", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Named term) ] ->
                (Syntax.Obj (Named_list [ term ]), [])
            | _ -> assert false );
        ( ( "comma_separated_named_rev_list",
            [
              Dyp.Non_ter ("comma_separated_named_rev_list", No_priority);
              Dyp.Regexp (RE_Char ',');
              Dyp.Non_ter ("named", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Named_list l); _; Syntax.Obj (Named term) ] ->
                (Syntax.Obj (Named_list (term :: l)), [])
            | _ -> assert false );
        ( ( "output",
            [ Dyp.Regexp (RE_String "print"); Dyp.Regexp (RE_String "formula") ],
            "default_priority",
            [] ),
          fun _ -> function _ -> (Syntax.Obj (Output Output.Formula), []) );
        ( ( "output",
            [
              Dyp.Regexp (RE_String "print");
              Dyp.Regexp (RE_String "formula");
              Dyp.Regexp (RE_String "for");
              Dyp.Non_ter ("comma_separated_named_rev_list", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _; Syntax.Obj (Named_list terms) ] ->
                (Syntax.Obj (Output (Output.Slice (List.rev terms))), [])
            | _ -> assert false );
        ( ( "output",
            [ Dyp.Regexp (RE_String "print"); Dyp.Regexp (RE_String "model") ],
            "default_priority",
            [] ),
          fun _ _ -> (Syntax.Obj (Output Output.Model), []) );
        ( ( "output",
            [
              Dyp.Regexp (RE_String "print");
              Dyp.Non_ter ("format", No_priority);
              Dyp.Non_ter ("expr", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj (Format format); Syntax.Expr term ] ->
                (Syntax.Obj (Output (Output.Value (format, term))), [])
            | _ -> assert false );
        ( ( "output",
            [
              Dyp.Regexp (RE_String "print");
              Dyp.Regexp (RE_String "ascii");
              Dyp.Regexp (RE_String "stream");
              Dyp.Non_ter ("ident", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _; Syntax.String name ] ->
                (Syntax.Obj (Output (Output.Stream name)), [])
            | _ -> assert false );
        ( ( "output",
            [
              Dyp.Regexp (RE_String "print");
              Dyp.Regexp (RE_String "c");
              Dyp.Regexp (RE_String "string");
              Dyp.Non_ter ("ident", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _; Syntax.String name ] ->
                (Syntax.Obj (Output (Output.String name)), [])
            | _ -> assert false );
        ( ( "and_separated_output_rev_list",
            [ Dyp.Non_ter ("output", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Output output) ] ->
                (Syntax.Obj (Output_list [ output ]), [])
            | _ -> assert false );
        ( ( "and_separated_output_rev_list",
            [
              Dyp.Non_ter ("and_separated_output_rev_list", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Regexp (RE_String "and");
              Dyp.Non_ter ("output", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Output_list l); _; _; Syntax.Obj (Output output) ]
              ->
                (Syntax.Obj (Output_list (output :: l)), [])
            | _ -> assert false );
        ( ("outputs", [], "default_priority", []),
          fun _ -> function _ -> (Syntax.Obj (Output_list []), []) );
        ( ( "outputs",
            [
              Dyp.Regexp (RE_String "then");
              Dyp.Non_ter ("and_separated_output_rev_list", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj (Output_list l) ] ->
                (Syntax.Obj (Output_list (List.rev l)), [])
            | _ -> assert false );
        ( ( "qident",
            [ Dyp.Non_ter ("ident", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.String id ] -> (Syntax.String id, [])
            | _ -> assert false );
        ( ("qident", [ Dyp.Ter "CONST" ], "default_priority", []),
          fun _ -> function
            | [ Syntax.Obj_CONST bv ] ->
                (Syntax.String (Bitvector.to_asciistring bv), [])
            | _ -> assert false );
        ( ( "section",
            [ Dyp.Non_ter ("qident", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.String id ] -> (Syntax.String id, [])
            | _ -> assert false );
        ( ( "section",
            [ Dyp.Non_ter ("label", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.String id ] -> (Syntax.String ("." ^ id), [])
            | _ -> assert false );
        ( ( "comma_separated_section_rev_list",
            [ Dyp.Non_ter ("section", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.String section ] ->
                (Syntax.Obj (String_list [ section ]), [])
            | _ -> assert false );
        ( ( "comma_separated_section_rev_list",
            [
              Dyp.Non_ter ("comma_separated_section_rev_list", No_priority);
              Dyp.Regexp (RE_Char ',');
              Dyp.Non_ter ("section", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (String_list l); _; Syntax.String section ] ->
                (Syntax.Obj (String_list (section :: l)), [])
            | _ -> assert false );
        ( ( "comma_separated_symbol_rev_list",
            [ Dyp.Non_ter ("symbol", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Symbol sym ] -> (Syntax.Obj (Symbol_list [ sym ]), [])
            | _ -> assert false );
        ( ( "comma_separated_symbol_rev_list",
            [
              Dyp.Non_ter ("comma_separated_symbol_rev_list", No_priority);
              Dyp.Regexp (RE_Char ',');
              Dyp.Non_ter ("symbol", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Symbol_list l); _; Syntax.Symbol sym ] ->
                (Syntax.Obj (Symbol_list (sym :: l)), [])
            | _ -> assert false );
        ( ("arg", [ Dyp.Non_ter ("var", No_priority) ], "default_priority", []),
          fun _ -> function
            | [ Syntax.Loc var ] -> (Syntax.Obj (Loc_opt (Some var)), [])
            | _ -> assert false );
        ( ("arg", [ Dyp.Regexp (RE_Char '_') ], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Loc_opt None), []) );
        ( ( "comma_separated_arg_rev_list",
            [ Dyp.Non_ter ("arg", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Loc_opt arg) ] ->
                (Syntax.Obj (Loc_opt_list [ arg ]), [])
            | _ -> assert false );
        ( ( "comma_separated_arg_rev_list",
            [
              Dyp.Non_ter ("comma_separated_arg_rev_list", No_priority);
              Dyp.Regexp (RE_Char ',');
              Dyp.Non_ter ("arg", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Loc_opt_list l); _; Syntax.Obj (Loc_opt arg) ] ->
                (Syntax.Obj (Loc_opt_list (arg :: l)), [])
            | _ -> assert false );
        ( ("arguments", [], "default_priority", []),
          fun _ _ -> (Syntax.Stmt [], []) );
        ( ( "arguments",
            [ Dyp.Regexp (RE_Char '('); Dyp.Regexp (RE_Char ')') ],
            "default_priority",
            [] ),
          fun _ _ -> (Syntax.Stmt [], []) );
        ( ( "arguments",
            [
              Dyp.Regexp (RE_Char '(');
              Dyp.Non_ter ("comma_separated_arg_rev_list", No_priority);
              Dyp.Regexp (RE_Char ')');
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj (Loc_opt_list args); _ ] ->
                ( Syntax.Stmt
                    (List.mapi
                       (fun i arg ->
                         Option.fold ~none:Instr.Nop
                           ~some:(fun name -> Argument (name, i))
                           arg)
                       (List.rev args)),
                  [] )
            | _ -> assert false );
        ( ("expr_opt", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Expr_opt None), []) );
        ( ( "expr_opt",
            [ Dyp.Non_ter ("expr", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Expr expr ] -> (Syntax.Obj (Expr_opt (Some expr)), [])
            | _ -> assert false );
        ( ( "control",
            [
              Dyp.Regexp (RE_String "return");
              Dyp.Non_ter ("expr_opt", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj (Expr_opt expr) ] ->
                (Syntax.Stmt [ Return expr ], [])
            | _ -> assert false );
        ( ( "comma_separated_expr_rev_list",
            [ Dyp.Non_ter ("expr", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Expr expr ] -> (Syntax.Obj (Expr_list [ expr ]), [])
            | _ -> assert false );
        ( ( "comma_separated_expr_rev_list",
            [
              Dyp.Non_ter ("comma_separated_expr_rev_list", No_priority);
              Dyp.Regexp (RE_Char ',');
              Dyp.Non_ter ("expr", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Expr_list l); _; Syntax.Expr expr ] ->
                (Syntax.Obj (Expr_list (expr :: l)), [])
            | _ -> assert false );
        ( ("with_stmts_end", [], "default_priority", []),
          fun _ _ -> (Syntax.Stmt [], []) );
        ( ( "with_stmts_end",
            [
              Dyp.Regexp (RE_String "with");
              Dyp.Non_ter ("stmts", No_priority);
              Dyp.Regexp (RE_String "end");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Stmt stmts; _ ] -> (Syntax.Stmt stmts, [])
            | _ -> assert false );
        ( ("such_that", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Expr_opt None), []) );
        ( ( "such_that",
            [
              Dyp.Regexp (RE_String "such");
              Dyp.Regexp (RE_String "that");
              Dyp.Non_ter ("expr", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Expr guard ] ->
                (Syntax.Obj (Expr_opt (Some guard)), [])
            | _ -> assert false );
        ( ("guard", [], "default_priority", []),
          fun _ _ -> (Syntax.Obj (Expr_opt None), []) );
        ( ( "guard",
            [ Dyp.Regexp (RE_String "if"); Dyp.Non_ter ("expr", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Expr guard ] ->
                (Syntax.Obj (Expr_opt (Some guard)), [])
            | _ -> assert false );
        ( ( "directive",
            [
              Dyp.Regexp (RE_String "enumerate");
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("num", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Expr expr; Syntax.Obj (Int n) ] ->
                (Syntax.Stmt [ Enumerate (n, expr) ], [])
            | _ -> assert false );
        ( ( "directive",
            [
              Dyp.Regexp (RE_String "enumerate");
              Dyp.Regexp (RE_Char '*');
              Dyp.Non_ter ("expr", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Expr expr ] ->
                (Syntax.Stmt [ Enumerate (max_int, expr) ], [])
            | _ -> assert false );
        ( ( "directive",
            [
              Dyp.Regexp (RE_String "assume"); Dyp.Non_ter ("expr", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Expr expr ] -> (Syntax.Stmt [ Instr.Assume expr ], [])
            | _ -> assert false );
        ( ( "directive",
            [
              Dyp.Regexp (RE_String "assert"); Dyp.Non_ter ("expr", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Expr expr ] -> (Syntax.Stmt [ Instr.Assert expr ], [])
            | _ -> assert false );
        ( ( "fallthrough",
            [ Dyp.Non_ter ("output", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Obj (Output output) ] -> (Syntax.Instr (Print output), [])
            | _ -> assert false );
        ( ( "fallthrough",
            [
              Dyp.Regexp (RE_String "enumerate");
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("num", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Expr expr; Syntax.Obj (Int n) ] ->
                (Syntax.Instr (Enumerate (n, expr)), [])
            | _ -> assert false );
        ( ( "fallthrough",
            [
              Dyp.Regexp (RE_String "reach");
              Dyp.Non_ter ("times", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("such_that", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("outputs", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                _;
                Syntax.Obj (Int n);
                _;
                Syntax.Obj (Expr_opt guard);
                _;
                Syntax.Obj (Output_list outputs);
              ] ->
                (Syntax.Instr (Reach (n, guard, outputs)), [])
            | _ -> assert false );
        ( ( "fallthrough",
            [
              Dyp.Regexp (RE_String "cut");
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("guard", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Obj (Expr_opt guard) ] ->
                (Syntax.Instr (Cut guard), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "starting");
              Dyp.Regexp (RE_String "from");
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("with_stmts_end", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Expr addr; Syntax.Stmt stmts ] ->
                (Syntax.Decl (Starting_from (addr, stmts)), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "starting");
              Dyp.Regexp (RE_String "from");
              Dyp.Regexp (RE_String "core");
              Dyp.Non_ter ("with_stmts_end", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _; Syntax.Stmt stmts ] ->
                (Syntax.Decl (Starting_from_core stmts), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "load");
              Dyp.Regexp (RE_String "section");
              Dyp.Non_ter ("section", No_priority);
              Dyp.Regexp (RE_String "from");
              Dyp.Regexp (RE_String "file");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.String section; _; _ ] ->
                (Syntax.Decl (Load_sections [ section ]), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "load");
              Dyp.Regexp (RE_String "sections");
              Dyp.Non_ter ("comma_separated_section_rev_list", No_priority);
              Dyp.Regexp (RE_String "from");
              Dyp.Regexp (RE_String "file");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Obj (String_list sections); _; _ ] ->
                (Syntax.Decl (Load_sections sections), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Non_ter ("load", No_priority);
              Dyp.Regexp (RE_String "from");
              Dyp.Regexp (RE_String "file");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Loc load; _; _ ] -> (Syntax.Decl (Load_data load), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "import");
              Dyp.Non_ter ("comma_separated_symbol_rev_list", No_priority);
              Dyp.Regexp (RE_String "from");
              Dyp.Non_ter ("qident", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Obj (Symbol_list syms); _; Syntax.String file ] ->
                (Syntax.Decl (Import_symbols (syms, file)), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "concretize");
              Dyp.Regexp (RE_String "stack");
            ],
            "default_priority",
            [] ),
          fun _ _ ->
            Options.Logger.warning
              "\"concretize stack\" is deprecated, use \"with concrete stack \
               pointer\" instead";
            (Syntax.Decl Concretize_stack_pointer, []) );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "with");
              Dyp.Regexp (RE_String "concrete");
              Dyp.Regexp (RE_String "stack");
              Dyp.Regexp (RE_String "pointer");
            ],
            "default_priority",
            [] ),
          fun _ _ -> (Syntax.Decl Concretize_stack_pointer, []) );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "replace");
              Dyp.Regexp (RE_String "opcode");
              Dyp.Non_ter ("byte_rev_list", No_priority);
              Dyp.Regexp (RE_String "by");
              Dyp.Non_ter ("stmts", No_priority);
              Dyp.Regexp (RE_String "end");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Obj (Int_list bytes); _; Syntax.Stmt stmts; _ ] ->
                ( Syntax.Decl
                    (Decode (Binstream.of_list (List.rev bytes), stmts)),
                  [] )
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "hook");
              Dyp.Non_ter ("comma_separated_expr_rev_list", No_priority);
              Dyp.Non_ter ("arguments", No_priority);
              Dyp.Regexp (RE_String "with");
              Dyp.Non_ter ("stmts", No_priority);
              Dyp.Regexp (RE_String "end");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                _;
                Syntax.Obj (Expr_list addr);
                Syntax.Stmt args;
                _;
                Syntax.Stmt stmts;
                _;
              ] ->
                (Syntax.Decl (Hook (addr, List.rev_append args stmts, true)), [])
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "replace");
              Dyp.Non_ter ("comma_separated_expr_rev_list", No_priority);
              Dyp.Non_ter ("arguments", No_priority);
              Dyp.Regexp (RE_String "by");
              Dyp.Non_ter ("stmts", No_priority);
              Dyp.Regexp (RE_String "end");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                _;
                Syntax.Obj (Expr_list addr);
                Syntax.Stmt args;
                _;
                Syntax.Stmt stmts;
                _;
              ] ->
                ( Syntax.Decl (Hook (addr, List.rev_append args stmts, false)),
                  [] )
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "abort");
              Dyp.Regexp (RE_String "at");
              Dyp.Non_ter ("comma_separated_expr_rev_list", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Obj (Expr_list addr) ] ->
                ( Syntax.Decl
                    (Hook
                       ( addr,
                         [
                           Instr.dynamic_assert (Expr.zero, Lexing.dummy_pos);
                           Instr.halt;
                         ],
                         false )),
                  [] )
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "halt");
              Dyp.Regexp (RE_String "at");
              Dyp.Non_ter ("comma_separated_expr_rev_list", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; Syntax.Obj (Expr_list addr) ] ->
                (Syntax.Decl (Hook (addr, [ Instr.halt ], false)), [])
            | _ -> assert false );
        ( ( "decl",
            [ Dyp.Non_ter ("instr", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Stmt instr ] -> (Syntax.Decl (Init instr), [])
            | _ -> assert false );
        ( ( "decl",
            [ Dyp.Regexp (RE_String "reach"); Dyp.Regexp (RE_String "all") ],
            "default_priority",
            [] ),
          fun _ _ ->
            Options.Logger.warning
              "\"reach all\" is deprecated, use \"explore all\" instead";
            (Syntax.Decl Explore_all, []) );
        ( ( "decl",
            [ Dyp.Regexp (RE_String "explore"); Dyp.Regexp (RE_String "all") ],
            "default_priority",
            [] ),
          fun _ _ -> (Syntax.Decl Explore_all, []) );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "reach");
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("arguments", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("times", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("such_that", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("outputs", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                _;
                Syntax.Expr addr;
                Syntax.Stmt args;
                _;
                Syntax.Obj (Int n);
                _;
                Syntax.Obj (Expr_opt guard);
                _;
                Syntax.Obj (Output_list outputs);
              ] ->
                ( Syntax.Decl
                    (Hook
                       ( [ addr ],
                         List.rev_append args [ Reach (n, guard, outputs) ],
                         true )),
                  [] )
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "reach");
              Dyp.Regexp (RE_Char '*');
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("arguments", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("such_that", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("outputs", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                _;
                _;
                Syntax.Expr addr;
                Syntax.Stmt args;
                _;
                Syntax.Obj (Expr_opt guard);
                _;
                Syntax.Obj (Output_list outputs);
              ] ->
                ( Syntax.Decl
                    (Hook
                       ( [ addr ],
                         List.rev_append args [ Reach (-1, guard, outputs) ],
                         true )),
                  [] )
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "cut");
              Dyp.Regexp (RE_String "at");
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("arguments", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Non_ter ("guard", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                _;
                _;
                Syntax.Expr addr;
                Syntax.Stmt args;
                _;
                Syntax.Obj (Expr_opt guard);
              ] ->
                ( Syntax.Decl
                    (Hook ([ addr ], List.rev_append args [ Cut guard ], true)),
                  [] )
            | _ -> assert false );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "at");
              Dyp.Non_ter ("expr", No_priority);
              Dyp.Non_ter ("arguments", No_priority);
              Dyp.Non_ter ("directive", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; Syntax.Expr addr; Syntax.Stmt args; Syntax.Stmt stmts ] ->
                ( Syntax.Decl (Hook ([ addr ], List.rev_append args stmts, true)),
                  [] )
            | _ -> assert false );
        ( ("rev_program", [], "default_priority", []),
          fun _ _ -> (Syntax.Program [], []) );
        ( ( "rev_program",
            [
              Dyp.Non_ter ("rev_program", No_priority);
              Dyp.Non_ter ("decl", No_priority);
              Dyp.Non_ter ("separator", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Program rev_program; Syntax.Decl decl; _ ] ->
                (Syntax.Program (decl :: rev_program), [ Dyp.Keep_grammar ])
            | _ -> assert false );
        ( ( "program",
            [ Dyp.Non_ter ("rev_program", No_priority); Dyp.Regexp RE_Eof_char ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Syntax.Program rev_program; _ ] ->
                (Syntax.Program (List.rev rev_program), [])
            | _ -> assert false );
      ];
  ]

let try_and_parse grammar_extensions lexbuf =
  let pilot =
    Dyp.update_pp (Syntax.pp ()) (List.concat (grammar :: grammar_extensions))
  in
  match Dyp.lexparse pilot "program" lexbuf with
  | [ (Syntax.Program ast, _) ] -> ast
  | exception Failure _ ->
      let s =
        Format.asprintf "@[<v>Probable lexing error %a@]" Parse_utils.pp_pos
          (Dyp.lexeme_end_p lexbuf)
      in
      raise (Parse_utils.UserFriendlyParseError s)
  | _ | (exception Dyp.Syntax_error) ->
      let s =
        Format.asprintf "@[<v>Parse error at word `%s' %a@]" (Dyp.lexeme lexbuf)
          Parse_utils.pp_pos (Dyp.lexeme_end_p lexbuf)
      in
      raise (Parse_utils.UserFriendlyParseError s)

type file_stream = Opened of in_channel * string list | Pending of string list

let rec refill_buf lexbuf file_stream buf len =
  match !file_stream with
  | Pending [] -> 0
  | Pending (filename :: files) -> (
      match open_in filename with
      | exception Sys_error _ ->
          Options.Logger.fatal "Cannot open file %s" filename
      | ic ->
          file_stream := Opened (ic, files);
          Dyp.set_fname (Option.get !lexbuf) filename;
          let std_lexbuf = Dyp.std_lexbuf (Option.get !lexbuf) in
          Lexing.set_position std_lexbuf
            { pos_fname = filename; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 };
          refill_buf lexbuf file_stream buf len)
  | Opened (ic, files) -> (
      match input ic buf 0 len with
      | 0 ->
          close_in ic;
          file_stream := Pending files;
          Bytes.set buf 0 '\n';
          1
      | n -> n)

let read_files gramma_extensions files =
  let file_stream = ref (Pending files) and lexbuf = ref None in
  lexbuf :=
    Some
      (Dyp.from_function (Libparser.Syntax.pp ())
         (refill_buf lexbuf file_stream));
  Fun.protect
    ~finally:(fun () ->
      match !file_stream with Opened (ic, _) -> close_in ic | _ -> ())
    (fun () -> try_and_parse gramma_extensions (Option.get !lexbuf))

(* include Cli.Make (struct *)
(*   let shortname = "ast" *)

(*   let name = "AST" *)
(* end) *)

(* module Parse = Builder.String_option (struct *)
(*   let name = "parse" *)

(*   let doc = "Parse AST node" *)
(* end) *)

(* let run () = *)
(*   Option.iter *)
(*     (fun node -> *)
(*       let lexbuf = Dyp.from_string (Syntax.pp ()) node in *)
(*       Logger.result "@[<v>%a@]" *)
(*         (Format.pp_print_list ~pp_sep:Format.pp_print_space pp) *)
(*         (try_and_parse lexbuf)) *)
(*     (Parse.get_opt ()) *)

(* let _ = Cli.Boot.enlist ~name:"AST" ~f:run *)
