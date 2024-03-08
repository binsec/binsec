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

open Types
open Ir

include Cli.Make (struct
  let name = "Shadow stack protection"
  let shortname = "shadow-stack"
end)

type mode =
  | Inline  (** Use standard DBA assertions *)
  | Builtin  (** Use new push and pop builtins *)

module Mode = Builder.Variant_choice_assoc (struct
  type t = mode

  let name = "mode"

  let doc =
    "Use standard DBA assertions [Inline] or new push and pop builtins \
     [Builtin]"

  let default = Inline
  let assoc_map = [ ("inline", Inline); ("builtin", Builtin) ]
end)

type Ast.t += Initial_stack of Ast.Expr.t Ast.loc list

module Inline (P : Path.S) (S : STATE) :
  Exec.EXTENSION with type path = P.t and type state = S.t = struct
  type path = P.t
  and state = S.t

  module Eval = Eval.Make (P) (S)

  let pointer_size = Kernel_options.Machine.word_size ()
  let array = "shadow_stack"

  let ptr =
    Dba.Var.create "shadow_stack_pointer"
      ~bitsize:(Size.Bit.create pointer_size)
      ~tag:Temp

  and witness =
    Dba.Var.create "shadow_stack_witness"
      ~bitsize:(Size.Bit.create pointer_size)
      ~tag:Temp

  let ptr_r = Dba.Expr.v ptr
  and witness_r = Dba.Expr.v witness

  let zero = Dba.Expr.constant (Bitvector.zeros pointer_size)

  and offset =
    Dba.Expr.constant (Bitvector.of_int ~size:pointer_size (pointer_size / 8))

  let initialization_callback =
    Some
      (fun _ state ->
        let state = S.alloc ~array state in
        S.assign ptr (S.Value.constant (Bitvector.zeros pointer_size)) state)

  let declaration_callback =
    Some
      (fun decl (env : Script.env) path state ->
        match decl with
        | Initial_stack values ->
            let offset =
              S.Value.constant
                (Bitvector.of_int ~size:pointer_size (pointer_size / 8))
            in
            let ptr_v, state =
              List.fold_left
                (fun (ptr_v, state) addr ->
                  let addr = Script.eval_expr ~size:env.wordsize addr env in
                  let addr, state = Eval.safe_eval addr state path in
                  ( S.Value.binary Plus ptr_v offset,
                    S.store array ~addr:ptr_v addr LittleEndian state ))
                (S.Value.constant (Bitvector.zeros pointer_size), state)
                values
            in
            Some (S.assign ptr ptr_v state)
        | _ -> None)

  let instruction_callback = None

  let process_handler : type a. (module Ir.GRAPH with type t = a) -> a -> unit =
   fun graph ->
    let module G = (val graph) in
    let insert_return_check graph vertex target =
      ignore
        (G.insert_list_before graph vertex
           [
             Assert (Dba.Expr.ugt ptr_r zero);
             Assign { var = ptr; rval = Dba.Expr.sub ptr_r offset };
             Load
               {
                 var = witness;
                 base = Some array;
                 dir = LittleEndian;
                 addr = ptr_r;
               };
             Assert (Dba.Expr.equal witness_r target);
           ])
    in
    fun graph ->
      G.iter_new_vertex
        (fun vertex ->
          match G.node graph vertex with
          | Goto { tag = Call { base; _ }; _ }
          | Terminator (Jump { tag = Call { base; _ }; _ }) ->
              ignore
                (G.insert_list_before graph vertex
                   [
                     Store
                       {
                         base = Some array;
                         dir = LittleEndian;
                         addr = ptr_r;
                         rval = Dba_utils.Expr.of_vaddr base;
                       };
                     Assign { var = ptr; rval = Dba.Expr.add ptr_r offset };
                   ])
          | Goto { target; tag = Return; _ } ->
              insert_return_check graph vertex (Dba_utils.Expr.of_vaddr target)
          | Terminator (Jump { target; tag = Return; _ }) ->
              insert_return_check graph vertex target
          | _ -> ())
        graph

  let process_callback = Some process_handler
  let builtin_callback = None
  let builtin_printer = None
  let at_exit_callback = None
end

module Builtin (P : Path.S) (S : STATE) :
  Exec.EXTENSION with type path = P.t and type state = S.t = struct
  type path = P.t
  and state = S.t

  let key = P.register_key []

  module Eval = Eval.Make (P) (S)

  let initialization_callback = None
  let instruction_callback = None

  let declaration_callback =
    Some
      (fun decl (env : Script.env) path state ->
        match decl with
        | Initial_stack values ->
            P.set key
              (List.rev_map
                 (fun addr -> Script.eval_expr ~size:env.wordsize addr env)
                 values)
              path;
            Some state
        | _ -> None)

  type Ir.builtin += Push of Dba.Expr.t | Pop of Dba.Expr.t

  let process_handler : type a. (module Ir.GRAPH with type t = a) -> a -> unit =
   fun graph ->
    let module G = (val graph) in
    fun graph ->
      G.iter_new_vertex
        (fun vertex ->
          match G.node graph vertex with
          | Goto { tag = Call { base; _ }; _ }
          | Terminator (Jump { tag = Call { base; _ }; _ }) ->
              ignore
                (G.insert_before graph vertex
                   (Builtin (Push (Dba_utils.Expr.of_vaddr base))))
          | Goto { target; tag = Return; _ } ->
              ignore
                (G.insert_before graph vertex
                   (Builtin (Pop (Dba_utils.Expr.of_vaddr target))))
          | Terminator (Jump { target; tag = Return; _ }) ->
              ignore (G.insert_before graph vertex (Builtin (Pop target)))
          | _ -> ())
        graph

  let process_callback = Some process_handler

  let push target addr path _ state =
    Logger.debug ~level:2 "%a: push(%a)" Virtual_address.pp addr
      Dba_printer.Ascii.pp_bl_term target;
    P.set key (target :: P.get key path) path;
    Ok state

  let report addr target witness state =
    Logger.error "@[<v>%a: Stack tampering@ (goto %a, expected %a)@ %a@]"
      Virtual_address.pp addr Bitvector.pp_hex_or_bin
      (S.get_a_value target state)
      Bitvector.pp_hex_or_bin
      (S.get_a_value witness state)
      S.pp state

  let pop target addr path _ state =
    Logger.debug ~level:2 "%a: pop(%a)" Virtual_address.pp addr
      Dba_printer.Ascii.pp_bl_term target;
    match P.get key path with
    | [] ->
        Logger.error "%a: return does not match any call" Virtual_address.pp
          addr;
        Error Assertion_failed
    | witness :: tail -> (
        P.set key tail path;
        let target_v = Eval.eval target state
        and witness_v = Eval.eval witness state in
        match S.test (S.Value.binary Eq target_v witness_v) state with
        | True state -> Ok state
        | False state ->
            report addr target_v witness_v state;
            Error Assertion_failed
        | Both { t; f } ->
            report addr target_v witness_v f;
            Ok t)

  let builtin_callback =
    Some
      (function
      | Push target -> Some (push target)
      | Pop target -> Some (pop target)
      | _ -> None)

  let builtin_printer =
    Some
      (fun ppf -> function
        | Push target ->
            Format.fprintf ppf "shadow push %a" Dba_printer.Ascii.pp_bl_term
              target;
            true
        | Pop target ->
            Format.fprintf ppf "shadow pop %a" Dba_printer.Ascii.pp_bl_term
              target;
            true
        | _ -> false)

  let at_exit_callback = None
end

let () =
  Exec.register_plugin
    (module struct
      let name = "shadow_stack"

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
                  | [ _; _; _; Libparser.Syntax.Obj (Script.Expr_list values) ]
                    ->
                      ( Libparser.Syntax.Decl (Initial_stack (List.rev values)),
                        [] )
                  | _ -> assert false );
            ];
        ]

      let instruction_printer = None

      let declaration_printer =
        Some
          (fun ppf -> function
            | Initial_stack values ->
                Format.fprintf ppf "initial call stack %a"
                  (Format.pp_print_list
                     ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                     (fun ppf (addr, _) -> Script.Expr.pp ppf addr))
                  values;
                true
            | _ -> false)

      let extension :
          type a b.
          (module EXPLORATION_STATISTICS) ->
          (module Path.S with type t = a) ->
          (module STATE with type t = b) ->
          (module Exec.EXTENSION with type path = a and type state = b) option =
       fun _ path state ->
        if is_enabled () then (
          if Options.Logger.is_debug_enabled () then
            Logger.set_log_level "debug";
          Logger.set_debug_level (Options.Logger.get_debug_level ());
          match Mode.get () with
          | Inline -> Some (module Inline ((val path)) ((val state)))
          | Builtin -> Some (module Builtin ((val path)) ((val state))))
        else None
    end : Exec.PLUGIN)
