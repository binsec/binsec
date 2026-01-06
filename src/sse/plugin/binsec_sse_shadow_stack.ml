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

open Types
open Ir

let expr_of_addr addr size =
  Dba.Expr.constant (Bitvector.create (Virtual_address.to_bigint addr) size)

let name = "shadow-stack"

module Logger = Logger.Sub (struct
  let name = name
end)

type Script.Ast.t += Initial_stack of Script.Ast.Expr.t Script.Ast.loc list

type ('value, 'model, 'state, 'path, 'a) field_id +=
  | Stack : ('value, 'model, 'state, 'path, 'value list) field_id

let grammar_extension =
  [
    Dyp.Add_rules
      [
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "initial");
              Dyp.Regexp (RE_String "call");
              Dyp.Regexp (RE_String "stack");
              Dyp.Non_ter ("comma_separated_expr_rev_list", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _; Binsec_script.Syntax.Obj (Script.Expr_list values) ] ->
                (Binsec_script.Syntax.Decl (Initial_stack (List.rev values)), [])
            | _ -> assert false );
      ];
  ]

let command_printer ppf = function
  | Initial_stack values ->
      Format.fprintf ppf "initial call stack %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           (fun ppf (addr, _) -> Script.Expr.pp ppf addr))
        values;
      true
  | _ -> false

module Inline_plugin = struct
  let name = name
  let fields : (module PATH) -> field list = fun _ -> []

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module E = (val engine) in
    let module P = E.Path in
    let pointer_size = Machine.ISA.word_size E.isa in
    let array = Some "shadow_stack" in
    let ptr =
      Dba.Var.create "shadow_stack_pointer"
        ~bitsize:(Size.Bit.create pointer_size)
        ~tag:Temp
    and witness =
      Dba.Var.create "shadow_stack_witness"
        ~bitsize:(Size.Bit.create pointer_size)
        ~tag:Temp
    in
    let ptr_r = Dba.Expr.v ptr and witness_r = Dba.Expr.v witness in
    let zero = Dba.Expr.constant (Bitvector.zeros pointer_size)
    and offset =
      Dba.Expr.constant (Bitvector.of_int ~size:pointer_size (pointer_size / 8))
    in
    let insert_return_check graph vertex target =
      Revision.insert_list_before graph vertex
        [
          Assert (Dba.Expr.ugt ptr_r zero);
          Assign { var = ptr; rval = Dba.Expr.sub ptr_r offset };
          Load { var = witness; base = array; dir = LittleEndian; addr = ptr_r };
          Assert (Dba.Expr.equal witness_r target);
        ]
    in
    [
      Grammar_extension grammar_extension;
      Initialization_callback (fun path -> P.assign path ptr zero);
      Command_handler
        (fun decl (env : Script.env) path ->
          match decl with
          | Initial_stack values ->
              let ptr_v =
                List.fold_left
                  (fun ptr_v addr ->
                    let addr = Script.eval_expr ~size:env.wordsize addr env in
                    P.store path array ~addr:ptr_v addr LittleEndian;
                    Dba.Expr.add ptr_v offset)
                  zero values
              in
              P.assign path ptr ptr_v;
              true
          | _ -> false);
      Command_printer command_printer;
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              | Fallthrough { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Jump { tag = Call { base; _ }; _ }; _ } ->
                  Revision.insert_list_before graph vertex
                    [
                      Store
                        {
                          base = array;
                          dir = LittleEndian;
                          addr = ptr_r;
                          rval = expr_of_addr base pointer_size;
                        };
                      Assign { var = ptr; rval = Dba.Expr.add ptr_r offset };
                    ]
              | Terminator { kind = Goto { target; tag = Return; _ }; _ } ->
                  insert_return_check graph vertex
                    (expr_of_addr target pointer_size)
              | Terminator { kind = Jump { target; tag = Return; _ }; _ } ->
                  insert_return_check graph vertex target
              | _ -> ())
            graph);
    ]
end

module Builtin (E : ENGINE) : EXTENSIONS with type path = E.Path.t = struct
  module P = E.Path
  module V = P.Value
  module M = P.Model

  type path = P.t

  let key = E.lookup Stack
  let pointer_size = Machine.ISA.word_size E.isa

  type Ir.builtin += Push of Dba.Expr.t | Pop of Dba.Expr.t

  let push target path =
    Logger.debug ~level:2 "%a: push(%a)" Virtual_address.pp (P.pc path)
      Dba_printer.Ascii.pp_bl_term target;
    P.set path key (P.get_value path target :: P.get path key)

  let report addr target witness model =
    Logger.error "@[<v>%a: Stack tampering@ (goto %a, expected %a)@ %a@]"
      Virtual_address.pp addr Bitvector.pp_hex_or_bin (M.eval target model)
      Bitvector.pp_hex_or_bin (M.eval witness model) M.pp
      model (* TODO pp_with_section ?*)

  let pop target path =
    let addr = P.pc path in
    Logger.debug ~level:2 "%a: pop(%a)" Virtual_address.pp addr
      Dba_printer.Ascii.pp_bl_term target;
    match P.get path key with
    | [] ->
        Logger.error "%a: return does not match any call" Virtual_address.pp
          addr;
        Signal Assertion_failure
    | witness_v :: tail -> (
        P.set path key tail;
        let target_v = P.get_value path target in
        (match
           P.check_sat_assuming_v ~retain:false path
             (V.binary Diff target_v witness_v)
         with
        | None -> ()
        | Some model -> report addr target_v witness_v model);
        match P.assume_v path (V.binary Eq target_v witness_v) with
        | None -> Signal Assertion_failure
        | Some _ -> Return)

  let list : P.t extension list =
    [
      Grammar_extension grammar_extension;
      Command_handler
        (fun decl (env : Script.env) path ->
          match decl with
          | Initial_stack values ->
              P.set path key
                (List.rev_map
                   (fun addr ->
                     P.get_value path
                       (Script.eval_expr ~size:env.wordsize addr env))
                   values);
              true
          | _ -> false);
      Command_printer command_printer;
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              | Fallthrough { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Jump { tag = Call { base; _ }; _ }; _ } ->
                  Revision.insert_before graph vertex
                    (Builtin (Push (expr_of_addr base pointer_size)))
              | Terminator { kind = Goto { target; tag = Return; _ }; _ } ->
                  Revision.insert_before graph vertex
                    (Builtin (Pop (expr_of_addr target pointer_size)))
              | Terminator { kind = Jump { target; tag = Return; _ }; _ } ->
                  Revision.insert_before graph vertex (Builtin (Pop target))
              | _ -> ())
            graph);
      Builtin_resolver
        (function
        | Push target -> Apply (push target)
        | Pop target -> Call (pop target)
        | _ -> Unknown);
      Builtin_printer
        (fun ppf -> function
          | Push target ->
              Format.fprintf ppf "shadow push %a" Dba_printer.Ascii.pp_bl_term
                target;
              true
          | Pop target ->
              Format.fprintf ppf "shadow pop %a" Dba_printer.Ascii.pp_bl_term
                target;
              true
          | _ -> false);
    ]
end

module Builtin_plugin = struct
  let name = name

  let fields : (module PATH) -> field list =
   fun _ -> [ Field { id = Stack; default = []; copy = None; merge = None } ]

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Extensions = Builtin ((val engine)) in
    Extensions.list
end
