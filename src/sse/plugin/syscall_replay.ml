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

open Types
open Ir

include Cli.Make (struct
  let name = "Syscall trace replay"

  let shortname = "syscall-replay"
end)

module Trace = Builder.String_option (struct
  let name = "trace"

  let doc = "Recorded syscall trace file"
end)

type Ast.Instr.t +=
  | Syscall
  | Clock_gettime of Ast.Expr.t Ast.loc * Ast.Expr.t Ast.loc

let () =
  Exec.register_plugin
    (module struct
      let name = "systrace"

      let grammar_extension =
        [
          Dyp.Add_rules
            [
              ( ( "fallthrough",
                  [ Dyp.Regexp (RE_String "syscall") ],
                  "default_priority",
                  [] ),
                fun _ _ -> (Libparser.Syntax.Instr Syscall, []) );
              ( ( "fallthrough",
                  [
                    Dyp.Regexp (RE_String "clock_gettime");
                    Dyp.Regexp (RE_Char '(');
                    Dyp.Non_ter ("expr", No_priority);
                    Dyp.Regexp (RE_Char ',');
                    Dyp.Non_ter ("expr", No_priority);
                    Dyp.Regexp (RE_Char ')');
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [
                      _;
                      _;
                      Libparser.Syntax.Expr clk_id;
                      _;
                      Libparser.Syntax.Expr tp;
                      _;
                    ] ->
                      (Libparser.Syntax.Instr (Clock_gettime (clk_id, tp)), [])
                  | _ -> assert false );
            ];
        ]

      let instruction_printer =
        Some
          (fun ppf -> function
            | Syscall ->
                Format.pp_print_string ppf "syscall";
                true
            | Clock_gettime ((clk_id, _), (tp, _)) ->
                Format.fprintf ppf "clock_gettime(%a, %a)" Ast.Expr.pp clk_id
                  Ast.Expr.pp tp;
                true
            | _ -> false)

      let declaration_printer = None

      let extension :
          type a b.
          (module EXPLORATION_STATISTICS) ->
          (module Path.S with type t = a) ->
          (module STATE with type t = b) ->
          (module Exec.EXTENSION with type path = a and type state = b) option =
       fun _ path state ->
        match Trace.get_opt () with
        | Some filename ->
            Some
              (module struct
                module P = (val path)

                module S = (val state)

                module E = Eval.Make (P) (S)

                type path = P.t

                and state = S.t

                type Ir.builtin +=
                  | Syscall
                  | Clock_gettime of { clk_id : Expr.t; tp : Expr.t }

                let trace = P.register_key (Syscall.import filename)

                let return =
                  match Isa_helper.get_ret ~syscall:true () with
                  | Var v -> S.assign v
                  | Store (_, dir, addr, base) ->
                      let write =
                        Option.fold ~none:S.write ~some:S.store base
                      in
                      fun value state ->
                        write ~addr:(E.eval addr state) value dir state
                  | Restrict _ -> assert false

                let initialization_callback = None

                let declaration_callback = None

                let instruction_callback =
                  Some
                    (fun (inst : Ast.Instr.t) (env : Script.env) ->
                      match inst with
                      | Syscall -> [ Builtin Syscall ]
                      | Clock_gettime (clk_id, tp) ->
                          [
                            Builtin
                              (Clock_gettime
                                 {
                                   clk_id =
                                     Script.eval_expr ~size:env.wordsize clk_id
                                       env;
                                   tp =
                                     Script.eval_expr ~size:env.wordsize tp env;
                                 });
                          ]
                      | _ -> [])

                (* let dbg ptr rval addr path _ state : (state, status) result = *)
                (*   let x = E.get_value ptr state path in *)
                (*   if *)
                (*     Bitvector.equal x *)
                (*       (Bitvector.of_int ~size:64 0x00007ffdc774e220) *)
                (*   then ( *)
                (*     (\* if *\) *)
                (*     (\*   Bitvector.ult *\) *)
                (*     (\*     (Bitvector.sub x *\) *)
                (*     (\*        (Bitvector.of_int ~size:64 0x00007ffdc774e220)) *\) *)
                (*     (\*     (Bitvector.of_int ~size:64 0x36) *\) *)
                (*     (\* then ( *\) *)
                (*     Logger.result "%a: %a <- %a %a" Virtual_address.pp addr *)
                (*       Bitvector.pp_hex_or_bin x Bitvector.pp_hex_or_bin *)
                (*       (E.get_value *)
                (*          (Expr.load *)
                (*             (Size.Byte.create (Expr.size_of rval / 8)) *)
                (*             LittleEndian (Expr.constant x)) *)
                (*          state path) *)
                (*       Bitvector.pp_hex_or_bin *)
                (*       (E.get_value rval state path); *)
                (*     Ok state) *)
                (*   else Ok state *)

                (* type Ir.builtin += Concretize of Expr.t *)

                (* let process_handler : *)
                (*     type a. (module Ir.GRAPH with type t = a) -> a -> unit = *)
                (*  fun graph -> *)
                (*   let module G = (val graph) in *)
                (*   fun graph -> *)
                (*     G.iter_new_vertex *)
                (*       (fun vertex -> *)
                (*         match G.node graph vertex with *)
                (*         | Fallthrough *)
                (*             { kind = Load { addr; _ } | Store { addr; _ }; _ } *)
                (*           -> *)
                (*             ignore *)
                (*               (G.insert_before graph vertex *)
                (*                  (Builtin (Concretize addr))) *)
                (*         | _ -> ()) *)
                (*       graph *)

                (* let process_callback = Some process_handler *)
                let process_callback = None

                let open_file pathname state =
                  let rec read_str buf addr state =
                    match
                      Bitvector.to_char
                        (S.get_a_value
                           (fst
                              (S.read ~addr:(S.Value.constant addr) 1
                                 LittleEndian state))
                           state)
                    with
                    | '\x00' -> Buffer.contents buf
                    | c ->
                        Buffer.add_char buf c;
                        read_str buf (Bitvector.add_int addr 1) state
                  in
                  match read_str (Buffer.create 32) pathname state with
                  | "" -> ()
                  | pathname -> Logger.debug "pathname %S" pathname

                (* let fd = ref Bitvector.zero *)

                exception Mismatch

                let syscall addr path _ state : (state, status) result =
                  match P.get trace path with
                  | [] -> Logger.fatal "Reach the end of the syscall record"
                  | syscall :: records -> (
                      P.set trace records path;
                      Logger.debug "%a" Syscall.pp syscall;
                      match syscall with
                      | Unknown (n, _) ->
                          Logger.fatal "Unknown syscall number %d %@ %a" n
                            Virtual_address.pp addr
                      | Incomplete (s, _, _) ->
                          Logger.fatal "Unsupported syscall %s %@ %a"
                            (Syscall.name s) Virtual_address.pp addr
                      | Exit _ -> Error Halt
                      (* | Full (Read, args, ret, writes) *)
                      (*   when Bitvector.is_zero !fd *)
                      (*        || Bitvector.equal (Array.get args 0) !fd -> *)
                      (*     if Bitvector.is_zero !fd then fd := Array.get args 0; *)
                      (*     let i, bytes = Array.get writes 0 in *)
                      (*     let size = String.length bytes in *)
                      (*     let state = *)
                      (*       if size <> 0 then ( *)
                      (*         let state, id, _ = *)
                      (*           String.fold_left *)
                      (*             (fun (state, id, ptr) value -> *)
                      (*               let var = S.Value.var id "read" 8 in *)
                      (*               let state = *)
                      (*                 Option.get *)
                      (*                   (S.expect var (Bitvector.of_char value) *)
                      (*                      state) *)
                      (*               in *)
                      (*               let state = *)
                      (*                 S.write ~addr:(S.Value.constant ptr) var *)
                      (*                   LittleEndian state *)
                      (*               in *)
                      (*               (state, S.Uid.succ id, Bitvector.succ ptr)) *)
                      (*             (state, P.get S.id path, Array.get args i) *)
                      (*             bytes *)
                      (*         in *)
                      (*         P.set S.id id path; *)
                      (*         state *)
                      (*         (\* let id = P.get S.id path in *\) *)
                      (*         (\* P.set S.id (S.Uid.succ id) path; *\) *)

                      (*         (\* let read = S.Value.var id "read" (8 * size) *\) *)
                      (*         (\* and value = Bitvector.of_bits bytes in *\) *)
                      (*         (\* let buf = Array.get args i in *\) *)
                      (*         (\* S.write ~addr:(S.Value.constant buf) read *\) *)
                      (*         (\*   LittleEndian *\) *)
                      (*         (\*   (Option.get (S.expect read value state))) *\)) *)
                      (*       else state *)
                      (*     in *)
                      (*     Ok (return (S.Value.constant ret) state) *)
                      | Full (s, args, ret, writes) -> (
                          (match s with
                          | Open -> open_file (Array.get args 0) state
                          | Openat | Openat2 | Newfstatat ->
                              open_file (Array.get args 1) state
                          | _ -> ());
                          try
                            let state =
                              Array_utils.fold_lefti
                                (fun i state arg ->
                                  let arg', state =
                                    E.safe_eval
                                      (Isa_helper.get_arg ~syscall:true i)
                                      state path
                                  in
                                  match
                                    S.assume
                                      S.Value.(binary Eq arg' (constant arg))
                                      state
                                  with
                                  | None -> raise_notrace Mismatch
                                  | Some state -> state)
                                state args
                            in
                            let state =
                              Array.fold_left
                                (fun state (i, data) ->
                                  let size = Bigarray.Array1.dim data in
                                  if size = 0 then state
                                  else
                                    let addr =
                                      if i = -1 then ret else Array.get args i
                                    in
                                    (* Logger.info "write %@[%a, %d]" *)
                                    (*   Bitvector.pp_hex_or_bin addr *)
                                    (*   (String.length data); *)
                                    S.memcpy ~addr size data state)
                                state writes
                            in
                            Ok (return (S.Value.constant ret) state)
                          with Mismatch -> Error Assertion_failed)
                      | Related _ ->
                          Logger.fatal
                            "@[<v>Current record for %a is not a syscall@ %a@]"
                            Virtual_address.pp addr Syscall.pp syscall)

                let ret =
                  match Isa_helper.get_ret () with
                  | Var v ->
                      fun bv state -> S.assign v (S.Value.constant bv) state
                  | Restrict _ | Store _ -> assert false
                (* todo ? *)

                let clock_gettime clk_id' tp' addr path _ state :
                    (state, status) result =
                  match P.get trace path with
                  | [] -> Logger.fatal "Reach the end of the syscall record"
                  | Related (Clock_gettime { clk_id; tp; res; tv_sec; tv_nsec })
                    :: records -> (
                      (* Logger.info "edx::eax := %a" Bitvector.pp_hex_or_bin *)
                      (*   (Bitvector.append hi lo); *)
                      P.set trace records path;
                      let clk_id', state = E.safe_eval clk_id' state path in
                      let tp', state = E.safe_eval tp' state path in
                      let pre =
                        S.Value.(
                          binary And
                            (binary Eq clk_id' (constant clk_id))
                            (binary Eq tp' (constant tp)))
                      in
                      match S.assume pre state with
                      | None -> Error Assertion_failed
                      | Some state ->
                          Ok
                            (ret res
                               (S.write ~addr:(S.Value.constant tp)
                                  (S.Value.constant tv_sec) LittleEndian
                                  (S.write
                                     ~addr:
                                       (S.Value.constant
                                          (Bitvector.add_int tp
                                             (Bitvector.size_of tv_sec lsr 3)))
                                     (S.Value.constant tv_nsec) LittleEndian
                                     state))))
                  | syscall :: _ ->
                      Logger.fatal
                        "@[<v>Current record for %a is not \"clock_gettime\"@ \
                         %a@]"
                        Virtual_address.pp addr Syscall.pp syscall

                (* let concretize e _ path _ state : (state, status) result = *)
                (*   let e, state = E.safe_eval e state path in *)
                (*   Ok *)
                (*     (Option.get *)
                (*        (S.assume *)
                (*           S.Value.( *)
                (*             binary Eq e (constant (S.get_a_value e state))) *)
                (*           state)) *)

                let builtin_callback =
                  Some
                    (function
                    | Syscall -> Some syscall
                    | Clock_gettime { clk_id; tp } ->
                        Some (clock_gettime clk_id tp)
                    (* | Concretize addr -> Some (concretize addr) *)
                    | _ -> None)

                let builtin_printer =
                  Some
                    (fun ppf -> function
                      | Syscall ->
                          Format.pp_print_string ppf "syscall";
                          true
                      | Clock_gettime { clk_id; tp } ->
                          Format.fprintf ppf "clock_gettime(%a, %a)"
                            Dba_printer.Ascii.pp_bl_term clk_id
                            Dba_printer.Ascii.pp_bl_term tp;
                          true
                      | _ -> false)

                let at_exit_callback = None
              end)
        | None -> None
    end : Exec.PLUGIN)
