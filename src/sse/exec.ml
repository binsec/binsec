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

open Options
open Types

module type EXTENSION = sig
  type path
  and state

  val initialization_callback : (path -> state -> state) option

  val declaration_callback :
    (Ast.t -> Script.env -> path -> state -> state option) option

  val instruction_callback :
    (Ast.Instr.t -> Script.env -> Ir.fallthrough list) option

  val process_callback :
    ((module Ir.GRAPH with type t = 'a) -> 'a -> unit) option

  val builtin_callback :
    (Ir.builtin ->
    (Virtual_address.t ->
    path ->
    int ->
    state ->
    (state, Types.status) Result.t)
    option)
    option

  val builtin_printer : (Format.formatter -> Ir.builtin -> bool) option
  val at_exit_callback : (unit -> unit) option
end

module type PLUGIN = sig
  val name : string

  val grammar_extension :
    ( unit,
      Libparser.obj,
      unit,
      unit,
      Libparser.obj Dyp.dyplexbuf )
    Dyp.dyp_action
    list

  val instruction_printer : (Format.formatter -> Ast.Instr.t -> bool) option
  val declaration_printer : (Format.formatter -> Ast.t -> bool) option

  val extension :
    (module Types.EXPLORATION_STATISTICS) ->
    (module Path.S with type t = 'a) ->
    (module Types.STATE with type t = 'b) ->
    (module EXTENSION with type path = 'a and type state = 'b) option
end

let plugins = S.Htbl.create 8

let register_plugin (plugin : (module PLUGIN)) =
  let module P = (val plugin) in
  if S.Htbl.mem plugins P.name then
    Options.Logger.fatal "plugin name %s has already been registered" P.name;
  S.Htbl.add plugins P.name plugin

exception Halt

module OMap = Map.Make (struct
  type t = string

  let compare =
    let rec iter s s' i =
      let d =
        Char.code (String.unsafe_get s i) - Char.code (String.unsafe_get s' i)
      in
      if d = 0 && i > 0 then iter s s' (i - 1) else d
    in

    fun s s' -> iter s s' (min (String.length s) (String.length s') - 1)
end)

let pp_value_as (format : Output.format) ppf bv =
  match format with
  | Bin -> Format.pp_print_string ppf @@ Bitvector.to_bitstring bv
  | Dec -> Z.pp_print ppf @@ Bitvector.signed_of bv
  | Hex -> Format.pp_print_string ppf @@ Bitvector.to_hexstring bv
  | Ascii -> Format.fprintf ppf "%S" @@ Bitvector.to_asciistring bv

let same_symbol name attr ((expr : Ast.Expr.t), _) =
  match expr with
  | Symbol ((name', attr'), _) -> name = name' && attr = attr'
  | _ -> false

module Run (SF : STATE_FACTORY) (W : WORKLIST) () = struct
  module Exploration_stats = Stats.Exploration ()
  module Query_stats = Stats.Query ()
  module Screen = Screen.Make (Exploration_stats) (Query_stats)
  module Path = Path.Make ()

  module State : STATE = struct
    include SF (Query_stats)

    let id =
      Path.register_key Uid.zero ~merge:(fun id id' ->
          if Uid.compare id id' >= 0 then Some id else Some id')

    let symbols = Path.register_key S.Map.empty
  end

  module Dcode = Dcode.Make (Exploration_stats) (Path) (State)
  module Fiber = Dcode.Fiber
  module Eval = Eval.Make (Path) (State)

  type fiber = [ `All ] Fiber.t

  and thunk = {
    path : Path.t;
    depth : int;
    ip : Virtual_address.t;
    fiber : fiber;
    state : State.t;
  }

  and 'a mode =
    | Default : unit mode
    | Linear : State.t mode
    | Merge : thunk mode

  type section =
    | RX of { base : Virtual_address.t; size : int; content : Loader_buf.t }
    | RWX

  type t = {
    mutable code : section Imap.t;  (** set of executable sections *)
    rocache : fiber Virtual_address.Htbl.t;
        (** instruction cache for RX sections *)
    cache : fiber OMap.t Virtual_address.Htbl.t;
        (** instruction cache for RWX sections *)
    mutable worklist : thunk W.t;  (** worklist of pending path *)
    mutable tid : int;  (** the next unique task identifier *)
    tasks : unit I.Htbl.t;  (** set of tasks to perform *)
    mutable dcode : Dcode.t option Imap.t;
    mutable whooks :
      ((string * Script.Instr.t list) list * Script.env) Virtual_address.Map.t;
    endianness : Machine.endianness;
  }

  let env : t =
    let img = Kernel_functions.get_img () in
    let transient = TransientEnum.get () > 0 in
    let rocache_size, _cache_size, code =
      Array.fold_left
        (fun (rocache, cache, code) s ->
          if Loader.Section.has_flag Loader_types.Exec s then
            let { Loader_types.virt = pos; _ } = Loader.Section.pos s in
            let { Loader_types.virt = size; _ } = Loader.Section.size s in
            let rodelta, delta, section =
              if Loader.Section.has_flag Loader_types.Write s then
                if transient then (
                  Logger.debug ~level:4
                    "Section %S [%a, 0x%x] has both Write and Execute flags."
                    (Loader.Section.name s) Virtual_address.pp
                    (Virtual_address.create pos)
                    size;
                  (0, size, RWX))
                else (
                  Logger.warning
                    "Section %S [%a, 0x%x] has both Write and Execute flags.@ \
                     Self-modifying code is disabled and writes will be \
                     ignored.@ Use '-sse-self-written-enum N' to enable \
                     symbolic reasoning up to 'N - 1' forks."
                    (Loader.Section.name s) Virtual_address.pp
                    (Virtual_address.create pos)
                    size;
                  ( size,
                    0,
                    RX
                      {
                        base = Virtual_address.create pos;
                        size;
                        content = Loader.Img.content img s;
                      } ))
              else
                ( size,
                  0,
                  RX
                    {
                      base = Virtual_address.create pos;
                      size;
                      content = Loader.Img.content img s;
                    } )
            in
            ( rocache + rodelta,
              cache + delta,
              Imap.add ~base:(Z.of_int pos) size section code )
          else (rocache, cache, code))
        (0, 0, Imap.empty) (Loader.Img.sections img)
    in
    let arrays = S.Htbl.create 7 in
    S.Htbl.add arrays "@" A.default;
    {
      code;
      rocache = Virtual_address.Htbl.create rocache_size;
      cache = Virtual_address.Htbl.create 0 (* cache_size *);
      worklist = W.empty;
      tid = 0;
      tasks = I.Htbl.create 7;
      dcode = Imap.empty;
      whooks = Virtual_address.Map.empty;
      endianness = Kernel_options.Machine.endianness ();
    }

  let choose () =
    try
      let thunk, worklist = W.pop env.worklist in
      Logger.debug ~level:3 "Selecting path #%d (among %d)" (Path.id thunk.path)
        (Exploration_stats.get_pending_paths ());
      env.worklist <- worklist;
      thunk
    with Not_found ->
      Logger.info "Empty path worklist: halting ...";
      raise_notrace Halt

  let add path = env.worklist <- W.push path env.worklist
  let at_exit_callbacks = Queue.create ()

  let threat_to_completeness () =
    let max_depth = Exploration_stats.get_status Max_depth in
    let incomplete_enum = Exploration_stats.get_status Enumeration_limit in
    let unknown = Exploration_stats.get_status Unresolved_formula in
    if max_depth + incomplete_enum + unknown > 0 then
      Logger.warning "@[<v>Threat to completeness :%a@]"
        (fun ppf (max_depth, incomplete_enum, unknown) ->
          if max_depth > 0 then
            Format.fprintf ppf
              "@ - %d paths have reached the maximal depth and have been cut \
               (-sse-depth)"
              max_depth;
          if incomplete_enum > 0 then
            Format.fprintf ppf
              "@ - some jump targets may have been omitted (-sse-jump-enum)";
          if unknown > 0 then
            Format.fprintf ppf
              "@ - %d SMT solver queries remain unsolved (-fml-solver-timeout)"
              unknown)
        (max_depth, incomplete_enum, unknown)

  let halt () =
    Screen.release ();
    Logger.info "@[<v 0>@[<v 2>SMT queries@,%a@]@,@[<v 2>Exploration@,%a@]@,@]"
      Query_stats.pp () Exploration_stats.pp ();
    threat_to_completeness ();
    Queue.iter (fun callback -> callback ()) at_exit_callbacks

  let ascii_stream name path state =
    let buf = Buffer.create 16 in
    List.iter (fun elt ->
        let rec iter bv =
          let size = Bitvector.size_of bv in
          assert (size mod 8 = 0);
          if size = 8 then Buffer.add_char buf (Bitvector.to_char bv)
          else
            let byte = Bitvector.extract bv { Interval.lo = 0; hi = 7 } in
            Buffer.add_char buf (Bitvector.to_char byte);
            iter (Bitvector.extract bv { Interval.lo = 8; hi = size - 1 })
        in
        iter (State.get_a_value elt state))
    @@ List.rev
    @@ S.Map.find name (Path.get State.symbols path);
    Buffer.contents buf

  let c_string array state =
    try
      let buf = Buffer.create 16 in
      let rec iter addr =
        let byte =
          State.get_a_value
            (Eval.eval
               (Expr.load ~array Size.Byte.one Machine.LittleEndian
                  (Expr.constant addr))
               state)
            state
        in
        if Bitvector.is_zeros byte then Buffer.contents buf
        else (
          Buffer.add_char buf (Bitvector.to_char byte);
          iter (Bitvector.succ addr))
      in
      iter (Bitvector.zeros (Kernel_options.Machine.word_size ()))
    with Uninterp _ -> ""

  let print addr path state (output : Output.t) =
    match output with
    | Model ->
        Logger.result "@[<v 0>Model %@ %a@ %a@]" Virtual_address.pp addr
          State.pp state
    | Formula ->
        Logger.result "Formula %@ %a@\n%a" Virtual_address.pp addr
          (State.pp_smt None) state
    | Slice slice ->
        let slice =
          let rec proceed slice state =
            try
              List.map (fun (expr, name) -> (Eval.eval expr state, name)) slice
            with
            | Undef var -> proceed slice (Eval.fresh var state path)
            | Uninterp array -> proceed slice (State.alloc ~array state)
          in
          proceed slice state
        in
        Logger.result "Formula for %a %@ %a@\n%a" Virtual_address.pp addr
          (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (_, n) ->
               Format.pp_print_string ppf n))
          slice
          (State.pp_smt (Some slice))
          state
    | Value (format, e) ->
        Logger.result "@[<v 0>Value %a : %a@]" Dba_printer.Ascii.pp_bl_term e
          (pp_value_as format)
          (fst (List.hd (Eval.split_on ~n:1 e state path)))
    | Stream name ->
        Logger.result "@[<v 0>Ascii stream %s : %S@]" name
          (ascii_stream name path state)
    | String name ->
        Logger.result "@[<v 0>C string %s : %S@]" name (c_string name state)

  let rec exec :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      fiber ->
      a =
   fun mode path depth ~max_depth state ip fiber ->
    match fiber with
    | Debug { msg; succ } ->
        Logger.debug "%s" msg;
        exec mode path depth ~max_depth state ip succ
    | Print { output; succ } ->
        print ip path state output;
        exec mode path depth ~max_depth state ip succ
    | Step { addr; n; succ } -> (
        let depth = depth + n in
        Exploration_stats.add_instructions n;
        Exploration_stats.update_depth depth;
        if depth <= max_depth then
          exec mode path depth ~max_depth state addr succ
        else
          match mode with
          | Default ->
              Logger.warning "@[<hov>Cut path %d (max depth) %@ %a@]"
                (Path.id path) Virtual_address.pp addr;
              Exploration_stats.terminate_path Max_depth;
              Path.terminate path Max_depth;
              yield Default ~max_depth
          | Linear -> state
          | Merge -> { path; depth; ip; state; fiber })
    | Symbolize { var; succ } ->
        exec mode path depth ~max_depth (Eval.fresh var state path) ip succ
    | Assign { var; rval; succ } ->
        exec mode path depth ~max_depth
          (Eval.assign var rval state path)
          ip succ
    | Clobber { var = { name; size; _ } as var; succ } ->
        Logger.warning ~level:3
          "path %d undefined variable %S arbitrarily set to zeros %@ %a"
          (Path.id path) name Virtual_address.pp ip;
        exec mode path depth ~max_depth
          (Eval.assign var (Dba.Expr.zeros size) state path)
          ip succ
    | Load { var; base = None; dir; addr; succ } ->
        let rval, state = Eval.read ~addr (var.size / 8) dir state path in
        exec mode path depth ~max_depth (State.assign var rval state) ip succ
    | Store { base = None; dir; addr; rval; succ } ->
        exec mode path depth ~max_depth
          (Eval.write ~addr rval dir state path)
          ip succ
    | Load { var; base = Some name; dir; addr; succ } ->
        let rval, state =
          Eval.select name ~addr (var.size / 8) dir state path
        in
        exec mode path depth ~max_depth (State.assign var rval state) ip succ
    | Store { base = Some name; dir; addr; rval; succ } ->
        exec mode path depth ~max_depth
          (Eval.store name ~addr rval dir state path)
          ip succ
    | Assume _ as fiber -> assume mode path depth ~max_depth state ip fiber
    | Assert _ as fiber -> check mode path depth ~max_depth state ip fiber
    | Branch _ as fiber -> ite mode path depth ~max_depth state ip fiber
    | Goto addr -> goto mode path depth ~max_depth state ip addr fiber
    | Jump _ as fiber -> dynamic_jump mode path depth ~max_depth state ip fiber
    | Probe _ as fiber -> probe mode path depth ~max_depth state ip fiber
    | Builtin { f; succ } -> (
        match f ip path depth state with
        | Error status ->
            Exploration_stats.terminate_path status;
            Path.terminate path status;
            if status = Halt then raise Halt;
            yield mode ~max_depth
        | Ok state -> exec mode path depth ~max_depth state ip succ)
    | Cut ->
        Exploration_stats.terminate_path Cut;
        Path.terminate path Cut;
        Logger.debug ~level:0 "@[<hov>Cut path %d (directive) %@ %a@]"
          (Path.id path) Virtual_address.pp ip;
        yield mode ~max_depth
    | Halt -> (
        match mode with
        | Default ->
            Exploration_stats.terminate_path Halt;
            Path.terminate path Halt;
            Logger.debug ~level:0 "@[<hov>End of path %d %@ %a@]" (Path.id path)
              Virtual_address.pp ip;
            yield mode ~max_depth
        | Linear -> state
        | Merge -> { path; depth; ip; state; fiber })
    | Die msg ->
        Exploration_stats.terminate_path Die;
        Path.terminate path Die;
        Logger.error "@[<hov>Cut path %d (uninterpreted %S) %@ %a@]"
          (Path.id path) msg Virtual_address.pp ip;
        yield mode ~max_depth

  and assume :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      [ `Assume ] Fiber.t ->
      a =
   fun mode path depth ~max_depth state ip (Assume { test; succ } as fiber) ->
    match mode with
    | Merge -> (
        match Eval.get_value test state path with
        | exception Non_unique -> { path; depth; ip; state; fiber }
        | x ->
            if Bitvector.is_one x then
              exec mode path depth ~max_depth state ip succ
            else { path; depth; ip; state; fiber = Cut })
    | Linear | Default -> (
        match Eval.assume test state path with
        | exception Unknown ->
            Exploration_stats.terminate_path Unresolved_formula;
            Path.terminate path Unresolved_formula;
            yield mode ~max_depth
        | None ->
            Logger.warning
              "@[<hov>Cut path %d (unsatisfiable assumption) %@ %a@]"
              (Path.id path) Virtual_address.pp ip;
            Exploration_stats.terminate_path Unsatisfiable_assumption;
            Path.terminate path Unsatisfiable_assumption;
            yield mode ~max_depth
        | Some state -> exec mode path depth ~max_depth state ip succ)

  and check :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      [ `Assert ] Fiber.t ->
      a =
   fun mode path depth ~max_depth state ip (Assert { test; succ } as fiber) ->
    match mode with
    | Linear -> assert false
    | Merge -> (
        match Eval.get_value test state path with
        | exception Non_unique -> { path; depth; ip; state; fiber }
        | x ->
            Exploration_stats.add_assert ();
            if Bitvector.is_one x then
              exec mode path depth ~max_depth state ip succ
            else (
              Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
                Virtual_address.pp ip State.pp state;
              Exploration_stats.add_failed_assert ();
              { path; depth; ip; state; fiber = Cut }))
    | Default -> (
        Exploration_stats.add_assert ();
        match Eval.test test state path with
        | exception Unknown ->
            Exploration_stats.terminate_path Unresolved_formula;
            Path.terminate path Unresolved_formula;
            yield mode ~max_depth
        | True state -> exec mode path depth ~max_depth state ip succ
        | False state ->
            Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
              Virtual_address.pp ip State.pp state;
            Exploration_stats.add_failed_assert ();
            Exploration_stats.terminate_path Assertion_failed;
            Path.terminate path Assertion_failed;
            yield mode ~max_depth
        | Both { t = state; f } ->
            Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
              Virtual_address.pp ip State.pp f;
            Exploration_stats.add_failed_assert ();
            exec mode path depth ~max_depth state ip succ)

  and ite :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      [ `Branch ] Fiber.t ->
      a =
   fun mode path depth ~max_depth state ip
       (Branch { test; taken; fallthrough; _ } as fiber) ->
    Exploration_stats.add_branch ();
    match mode with
    | Linear -> assert false
    | Merge -> (
        match Eval.get_value test state path with
        | exception Non_unique -> { path; depth; ip; state; fiber }
        | x ->
            exec Merge path depth ~max_depth state ip
              (if Bitvector.is_one x then taken else fallthrough))
    | Default -> (
        let parent = state in
        match Eval.test test state path with
        | exception Unknown ->
            Exploration_stats.terminate_path Unresolved_formula;
            Path.terminate path Unresolved_formula;
            yield mode ~max_depth
        | True state ->
            add { path; depth; ip; state; fiber = taken };
            yield mode ~max_depth
        | False state ->
            add { path; depth; ip; state; fiber = fallthrough };
            yield mode ~max_depth
        | Both { t = state; f = state' } ->
            let k = QMerge.get () in
            if k > 0 then (
              let path' = Path.fork path in
              let taken =
                exec Merge path depth ~max_depth:(depth + k) state ip taken
              in
              Path.set State.id (Path.get State.id path) path';
              let fallthrough =
                exec Merge path' depth ~max_depth:(depth + k) state' ip
                  fallthrough
              in
              if taken.ip == fallthrough.ip && taken.fiber == fallthrough.fiber
              then
                match Path.merge path path' with
                | None ->
                    add taken;
                    Exploration_stats.add_path ();
                    add fallthrough;
                    yield mode ~max_depth
                | Some path'' -> (
                    match State.merge ~parent taken.state fallthrough.state with
                    | exception Non_mergeable ->
                        add taken;
                        Exploration_stats.add_path ();
                        add fallthrough;
                        yield mode ~max_depth
                    | state ->
                        exec mode path''
                          (max taken.depth fallthrough.depth)
                          ~max_depth state taken.ip taken.fiber)
              else (
                add taken;
                Exploration_stats.add_path ();
                add fallthrough;
                yield mode ~max_depth))
            else (
              add { path; depth; ip; state; fiber = taken };
              Exploration_stats.add_path ();
              add
                {
                  path = Path.fork path;
                  depth;
                  ip;
                  state = state';
                  fiber = fallthrough;
                };
              yield mode ~max_depth))

  and dynamic_jump :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      [ `Jump ] Fiber.t ->
      a =
   fun mode path depth ~max_depth state ip (Jump target as fiber) ->
    let n = JumpEnumDepth.get () in

    let handle path depth ip addr (bv, state) =
      try
        Logger.debug ~level:4 "@[<hov>Dynamic jump@ %@%a@ could lead to@ %a@]"
          Virtual_address.pp addr Bitvector.pp_hex_or_bin bv;
        add
          {
            path;
            depth;
            ip;
            fiber = Goto (Virtual_address.of_bitvector bv);
            state;
          }
      with Virtual_address.Non_canonical_form ->
        Logger.warning
          "@[<hov>Dynamic jump %@ %a@ could have led to invalid address %a;@ \
           skipping@]"
          Virtual_address.pp addr Bitvector.pp_hex_or_bin bv;
        Exploration_stats.terminate_path Die;
        Path.terminate path Die
    in
    Exploration_stats.add_branch ();
    match mode with
    | Linear -> assert false
    | Merge -> (
        match Eval.get_value target state path with
        | exception Non_unique -> { path; depth; ip; state; fiber }
        | x ->
            goto mode path depth ~max_depth state ip
              (Virtual_address.of_bitvector x)
              fiber)
    | Default -> (
        match Eval.split_on target ~n state path with
        | [] | (exception Unknown) ->
            Exploration_stats.terminate_path Unresolved_formula;
            Path.terminate path Unresolved_formula;
            yield mode ~max_depth
        | [ (x, state) ] ->
            goto mode path depth ~max_depth state ip
              (Virtual_address.of_bitvector x)
              fiber
        | x :: bx ->
            let old_paths = Exploration_stats.get_paths () in
            let addr = ip in
            handle path depth ip addr x;
            List.iter
              (fun x ->
                Exploration_stats.add_path ();
                handle (Path.fork path) depth ip addr x)
              bx;
            if Exploration_stats.get_paths () - old_paths = n - 1 then (
              Logger.warning
                "Enumeration of jump targets %@ %a hit the limit %d and may be \
                 incomplete"
                Virtual_address.pp addr n;
              Exploration_stats.add_path ();
              Exploration_stats.terminate_path Enumeration_limit);
            yield mode ~max_depth)

  and goto :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      Virtual_address.t ->
      fiber ->
      a =
   fun mode path depth ~max_depth state ip addr rollback ->
    match Virtual_address.Htbl.find env.rocache addr with
    | target -> exec mode path depth ~max_depth state ip target
    | exception Not_found -> (
        match Imap.find (Virtual_address.to_bigint addr) env.dcode with
        | exception Not_found ->
            Logger.warning "@[<hov>Cut path %d (non executable) %@ %a@]"
              (Path.id path) Virtual_address.pp addr;
            Exploration_stats.terminate_path Non_executable_code;
            Path.terminate path Non_executable_code;
            yield mode ~max_depth
        | Some code ->
            let fiber = Dcode.get code addr in
            Virtual_address.Htbl.add env.rocache addr fiber;
            exec mode path depth ~max_depth state ip fiber
        | None -> (
            match mode with
            | Linear -> assert false
            | Merge -> { path; depth; ip; state; fiber = rollback }
            | Default ->
                transient_instruction mode path depth ~max_depth state addr))

  and transient_instruction :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      a =
   fun mode path depth ~max_depth state addr ->
    let n = TransientEnum.get () in
    let hooks = Virtual_address.Map.find_opt addr env.whooks in
    let handle path depth (omap : fiber OMap.t) addr (bv, state) =
      let opcode = Bitvector.to_asciistring bv in
      let omap, fiber =
        try (omap, OMap.find opcode omap)
        with Not_found ->
          let reader = Lreader.of_bytes ~endianness:env.endianness opcode in
          let scope, inst =
            Dcode.single ~task:env.tasks ?hooks addr reader
              (Bitvector.size_of bv / 8)
          in
          let omap =
            match inst with
            | None -> OMap.add "" scope omap
            | Some inst ->
                Logger.debug ~level:4
                  "@[<hov>Self-written instruction @@ %a could be %a [ %a ]@]"
                  Virtual_address.pp addr Mnemonic.pp
                  (Instruction.mnemonic inst)
                  String_utils.pp_hex opcode;
                let s = (Instruction.size inst :> int) in
                if s = 0 then omap
                else OMap.add (String.sub opcode 0 s) scope omap
          in
          (omap, scope)
      in
      add { path; depth; ip = addr; state; fiber };
      omap
    in
    let omap =
      try Virtual_address.Htbl.find env.cache addr
      with Not_found -> OMap.empty
    in
    let opcode =
      Expr.load
        (Isa_helper.max_instruction_len ())
        Machine.LittleEndian
        (Expr.constant
           (Bitvector.of_int
              ~size:(Kernel_options.Machine.word_size ())
              (addr :> int)))
    in

    match Eval.split_on opcode ~n state path with
    | [] | (exception Unknown) ->
        Exploration_stats.terminate_path Unresolved_formula;
        Path.terminate path Unresolved_formula;
        yield mode ~max_depth
    | x :: bx ->
        let omap =
          List.fold_left
            (fun omap x ->
              Exploration_stats.add_path ();
              let omap = handle (Path.fork path) depth omap addr x in

              omap)
            omap bx
        in
        let omap = handle path depth omap addr x in
        Virtual_address.Htbl.replace env.cache addr omap;
        yield mode ~max_depth

  and yield : type a. a mode -> max_depth:int -> a =
   fun mode ~max_depth ->
    let { path; depth; ip; fiber; state } = choose () in
    exec mode path depth ~max_depth state ip fiber

  and probe :
      type a.
      a mode ->
      Path.t ->
      int ->
      max_depth:int ->
      State.t ->
      Virtual_address.t ->
      [ `Probe ] Fiber.t ->
      a =
   fun mode path depth ~max_depth state ip (Probe { kind; succ } as fiber) ->
    match kind with
    | Enumerate ({ enum; id = tid; format; n; k; values } as e) ->
        if n > k then (
          let values' = Eval.split_on enum ~n ~except:values state path in
          let values =
            List.fold_left (fun values (bv, _) -> bv :: values) values values'
          in
          let k = List.length values in
          Logger.result
            "@[<hov 0>Directive :: enumerate@ possible values (%d) for %a %@ \
             %a:@ @[<hov 0>%a@]@]"
            k Dba_printer.Ascii.pp_bl_term enum Virtual_address.pp ip
            (Print_utils.pp_list ~sep:",@ " (pp_value_as format))
            values;
          e.k <- k;
          e.values <- values;
          if n = k then (
            I.Htbl.remove env.tasks tid;
            if I.Htbl.length env.tasks = 0 then raise_notrace Halt));
        exec mode path depth ~max_depth state ip succ
    | Reach { n = 0; _ } -> exec mode path depth ~max_depth state ip succ
    | Reach ({ id = tid; n; guard; actions } as r) -> (
        match mode with
        | Linear -> assert false
        | Merge -> { path; depth; ip; state; fiber }
        | Default -> (
            match Eval.assume guard state path with
            | None -> exec mode path depth ~max_depth state ip succ
            | Some state' ->
                let addr = ip in
                Logger.result "@[<h>Path %d reached address %a (%a to go)@]"
                  (Path.id path) Virtual_address.pp addr
                  (fun ppf n ->
                    if n = -1 then Format.pp_print_char ppf '*'
                    else Format.pp_print_int ppf (n - 1))
                  n;
                List.iter (fun output -> print addr path state' output) actions;
                if n > 0 then (
                  r.n <- n - 1;
                  if n = 1 then (
                    I.Htbl.remove env.tasks tid;
                    if I.Htbl.length env.tasks = 0 then raise_notrace Halt));
                exec Default path depth ~max_depth state ip succ))

  let initialize_state () =
    let state = State.empty () in
    let addr_size = Kernel_options.Machine.word_size ()
    and img = Kernel_functions.get_img () in
    let entry =
      match Kernel_functions.get_ep () with
      | Some addr -> addr
      | None -> Virtual_address.create (Loader.Img.entry img)
    in
    let start = ref (Fiber.Goto entry) in
    let prehooks = ref Virtual_address.Map.empty
    and hooks = ref Virtual_address.Map.empty in
    let symbols : (Dba.Var.Tag.attribute * Bitvector.t) S.Htbl.t =
      S.Htbl.create 100
    and shadowing_symbols : (string * string option) S.Htbl.t =
      S.Htbl.create 100
    and core_symbols : (Dba.Var.Tag.attribute * Bitvector.t) S.Htbl.t S.Htbl.t =
      S.Htbl.create 1
    in
    let parser_env =
      let wordsize = Kernel_options.Machine.word_size () in
      let tbl = S.Htbl.create 128 and ori = S.Htbl.create 128 in
      List.iter
        (fun (name, var) -> S.Htbl.add tbl (String.lowercase_ascii name) var)
        (Isa_helper.get_defs ());
      let define (var : Dba.Var.t) pos =
        let name = String.lowercase_ascii var.name in
        S.Htbl.add tbl name (Dba.LValue.v var);
        if pos <> Lexing.dummy_pos then S.Htbl.add ori name pos
      in
      let origin name = S.Htbl.find_opt ori name in
      let lookup name = S.Htbl.find tbl (String.lowercase_ascii name) in
      let tbl = S.Htbl.create 128 in

      let lookup_symbol name (attr : Dba.Var.Tag.attribute) =
        try List.assoc attr (S.Htbl.find_all tbl name)
        with Not_found ->
          let value =
            lazy
              (try List.assoc attr (S.Htbl.find_all symbols name)
               with Not_found -> raise (Unresolved (name, attr)))
          in
          (if S.Htbl.mem shadowing_symbols name then
             let file, sec = S.Htbl.find shadowing_symbols name in
             Logger.warning
               "@[<v>Symbol %s comes from%a the file %s and shadows other \
                definitions@ Use \"import <%s> from FILE\" to disambiguate"
               name
               (Format.pp_print_option (fun ppf sec ->
                    Format.fprintf ppf " a relocation in section %s of" sec))
               sec file name);
          let tag = Dba.Var.Tag.Symbol (attr, value) in
          let sym = Dba.Expr.var ~tag name wordsize in
          S.Htbl.add tbl name (attr, sym);
          sym
      in
      Script.
        {
          wordsize;
          endianness = Kernel_options.Machine.endianness ();
          define;
          origin;
          lookup;
          lookup_symbol;
        }
    in
    (match img with
    | ELF img -> (
        let open Loader_elf in
        Array.iter
          (fun sym ->
            match Symbol.header sym with
            | { kind = SECTION; sh = SEC { name; addr; size; _ }; _ }
            | { sh = SEC _; name; value = addr; size; _ }
              when not (Utils.is_ifunc img sym) ->
                S.Htbl.add symbols name
                  (Value, Bitvector.of_int ~size:addr_size addr);
                S.Htbl.add symbols name
                  (Size, Bitvector.of_int ~size:addr_size size);
                S.Htbl.add symbols name
                  (Last, Bitvector.of_int ~size:addr_size (addr + size - 1))
            | _ -> ())
          (Img.symbols img);
        Array.iter
          (fun sec ->
            let { Shdr.name; flags; addr; size; _ } = Section.header sec in
            if Shdr.SHF.is flags ALLOC && not (S.Htbl.mem symbols name) then (
              S.Htbl.add symbols name
                (Value, Bitvector.of_int ~size:addr_size addr);
              S.Htbl.add symbols name
                (Size, Bitvector.of_int ~size:addr_size size);
              S.Htbl.add symbols name
                (Last, Bitvector.of_int ~size:addr_size (addr + size - 1))))
          (Img.sections img);
        match Img.header img with
        | { Ehdr.kind = DYN; machine = X86 _; _ } ->
            let rela_plt, base =
              Array.fold_left
                (fun ((rela_opt, base) as r) sec ->
                  let ({ Shdr.name; addr; _ } as header) = Section.header sec in
                  match name with
                  | ".rela.plt" -> (Some header, base)
                  | ".plt" when base = -1 -> (rela_opt, addr + 16)
                  | ".plt.sec" -> (rela_opt, addr)
                  | _ -> r)
                (None, -1) (Img.sections img)
            in
            Option.iter
              (fun rela_plt ->
                Array.iteri
                  (fun i { Rel.symbol = { name; _ }; _ } ->
                    S.Htbl.add symbols name
                      (Plt, Bitvector.of_int ~size:addr_size (base + (16 * i))))
                  (Rel.read img rela_plt))
              rela_plt
        | _ -> ())
    | _ ->
        let open Loader in
        Array.iter
          (fun sym ->
            S.Htbl.add symbols (Symbol.name sym)
              (Value, Bitvector.of_int ~size:addr_size (Symbol.value sym)))
          (Img.symbols img);
        Array.iter
          (fun sec ->
            let name = Section.name sec in
            let { Loader_types.virt = addr; _ } = Section.pos sec in
            S.Htbl.add symbols name
              (Value, Bitvector.of_int ~size:addr_size addr);
            let { Loader_types.virt = size; _ } = Section.size sec in
            S.Htbl.add symbols name (Size, Bitvector.of_int ~size:addr_size size);
            S.Htbl.add symbols name
              (Last, Bitvector.of_int ~size:addr_size (addr + size - 1)))
          (Img.sections img));
    let rec copy_from addr size state =
      let section =
        match
          Loader_utils.find_section_by_address ~address:(Bitvector.to_int addr)
            img
        with
        | None ->
            Logger.fatal "address %a does not belong to file" Virtual_address.pp
              (Virtual_address.of_bitvector addr)
        | Some section -> section
      in
      let { Loader_types.virt; _ } = Loader.Section.size section in
      if virt >= size then
        State.memcpy ~addr size (Loader.Img.content img section) state
      else
        copy_from
          (Bitvector.add_int addr virt)
          (size - virt)
          (State.memcpy ~addr virt (Loader.Img.content img section) state)
    in
    let set path state init =
      let init =
        Dcode.script ~task:env.tasks entry ~fallthrough:false init parser_env
      in
      exec Linear path 0 ~max_depth:max_int state entry init
    in
    let from_core path prehook state =
      match Kernel_functions.get_img () with
      | Loader.Raw _ | Loader.PE _ | Loader.TI83 _ ->
          Logger.fatal "Binary is not an ELF file."
      | Loader.ELF img' ->
          let hdr = Loader_elf.Img.header img' in
          if hdr.Loader_elf.Ehdr.kind <> Loader_elf.Ehdr.ET.CORE then
            Logger.fatal "Binary is not a core file";
          let vmap, state =
            Array.fold_left
              (fun (vmap, state) section ->
                let open Loader_elf in
                let hdr = Section.header section in
                if Section.has_flag Loader_types.Read section then
                  let addr =
                    Bitvector.of_int
                      ~size:(Kernel_options.Machine.word_size ())
                      hdr.Shdr.addr
                  in
                  ( Imap.add ~base:(Bitvector.value_of addr) hdr.Shdr.size true
                      vmap,
                    State.memcpy ~addr hdr.Shdr.size (Img.content img' section)
                      state )
                else (vmap, state))
              (Imap.empty, state)
              (Loader_elf.Img.sections img')
          in
          let transient = TransientEnum.get () > 0 in
          let dyn_symbols = Queue.create () in
          let _, _, xcode, state =
            Array.fold_left
              (fun (vmap, fmap, xcode, state)
                   { Loader_elf.addresses = { lo; hi }; offset; name = fname } ->
                if S.Set.mem fname fmap then (vmap, fmap, xcode, state)
                else
                  let img = Loader_elf.load_file fname in
                  let size = Virtual_address.diff hi lo + 1 in
                  let section =
                    Option.get
                      (Loader_utils.find_section
                         ~p:(fun s ->
                           let { Loader_types.raw; _ } = Loader.Section.pos s in
                           Loader.Section.has_flag Loader_types.Read s
                           && offset <= raw
                           && raw < offset + size)
                         (Loader.ELF img))
                  in
                  let { Loader_types.raw; virt } = Loader.Section.pos section in
                  let base =
                    Virtual_address.diff
                      (Virtual_address.add_int (raw - offset) lo)
                      (Virtual_address.create virt)
                  in
                  Logger.debug "%08x :: %a-%a %08x %s" base Virtual_address.pp
                    lo Virtual_address.pp hi offset fname;
                  let private_symbols :
                      (Dba.Var.Tag.attribute * Bitvector.t) S.Htbl.t =
                    S.Htbl.create 100
                  in
                  Array.iter
                    (fun sym ->
                      match Loader_elf.Symbol.header sym with
                      | { kind = SECTION; sh = SEC { name; addr; size; _ }; _ }
                      | { sh = SEC _; name; value = addr; size; _ }
                        when not (Loader_elf.Utils.is_ifunc img sym) ->
                          if S.Htbl.mem symbols name then (
                            S.Htbl.remove symbols name;
                            S.Htbl.replace shadowing_symbols name (fname, None));
                          let value =
                            ( Dba.Var.Tag.Value,
                              Bitvector.of_int ~size:addr_size (base + addr) )
                          in
                          S.Htbl.add private_symbols name value;
                          S.Htbl.add symbols name value;
                          let last =
                            ( Dba.Var.Tag.Last,
                              Bitvector.of_int ~size:addr_size
                                (base + addr + size - 1) )
                          in
                          S.Htbl.add private_symbols name last;
                          S.Htbl.add symbols name last;
                          let size =
                            ( Dba.Var.Tag.Size,
                              Bitvector.of_int ~size:addr_size size )
                          in
                          S.Htbl.add private_symbols name size;
                          S.Htbl.add symbols name size
                      | _ -> ())
                    (Loader_elf.Img.symbols img);
                  Array.iter
                    (fun section ->
                      let name = Loader_elf.Section.name section
                      and { Loader_types.virt = addr; _ } =
                        Loader_elf.Section.pos section
                      and { Loader_types.virt = size; _ } =
                        Loader_elf.Section.size section
                      in
                      S.Htbl.add private_symbols name
                        (Value, Bitvector.of_int ~size:addr_size (base + addr));
                      S.Htbl.add private_symbols name
                        (Size, Bitvector.of_int ~size:addr_size size);
                      S.Htbl.add private_symbols name
                        ( Last,
                          Bitvector.of_int ~size:addr_size
                            (base + addr + size - 1) ))
                    (Loader_elf.Img.sections img);
                  S.Htbl.add core_symbols (Filename.basename fname)
                    private_symbols;
                  let sections = Loader_elf.Img.sections img in
                  let vmap, xcode, state =
                    Array.fold_left
                      (fun (vmap, xcode, state) s ->
                        let open Loader_elf in
                        let hdr = Section.header s in
                        let pos = hdr.Shdr.addr and size = hdr.Shdr.size in
                        let addr =
                          Bitvector.of_int ~size:addr_size (pos + base)
                        in
                        let base = Bitvector.value_of addr in
                        (if hdr.kind = RELA || hdr.kind = REL then
                           let lname =
                             Section.name (Array.get sections hdr.Shdr.info)
                           in
                           if
                             String_utils.start_with ~prefix:".got" lname
                             || String_utils.start_with ~prefix:".plt" lname
                           then
                             Array.iter
                               (fun Rel.
                                      {
                                        offset;
                                        kind = _;
                                        symbol = { name; _ };
                                        addend;
                                      } ->
                                 if name <> "" then
                                   let addend =
                                     Option.value ~default:0 addend
                                   in
                                   let reader =
                                     Lreader.create ~endianness:env.endianness
                                       ~at:(Z.to_int base + offset - pos)
                                       Loader_elf.read_address img'
                                   in
                                   let value =
                                     Bitvector.add_int
                                       (Lreader.Read.read reader
                                          (addr_size lsr 3))
                                       (-addend)
                                   in
                                   Queue.add
                                     (name, value, lname, fname)
                                     dyn_symbols)
                               (Rel.read img hdr));
                        if
                          (not (Section.has_flag Loader_types.Read s))
                          || Imap.mem base vmap
                        then (vmap, xcode, state)
                        else
                          let vmap = Imap.add ~base size true vmap in
                          let xcode =
                            if Section.has_flag Loader_types.Exec s then
                              let p =
                                if Section.has_flag Loader_types.Write s then
                                  if transient then RWX
                                  else (
                                    Logger.warning
                                      "Section %S [%a, 0x%x] has both Write \
                                       and Execute flags.@ Self-modifying code \
                                       is disabled and writes will be \
                                       ignored.@ Use '-sse-self-written-enum \
                                       N' to enable symbolic reasoning up to \
                                       'N - 1' forks."
                                      (Section.name s) Virtual_address.pp
                                      (Virtual_address.create pos)
                                      size;
                                    RX
                                      {
                                        base = Virtual_address.create pos;
                                        size;
                                        content = Img.content img s;
                                      })
                                else
                                  RX
                                    {
                                      base = Virtual_address.of_bitvector addr;
                                      size;
                                      content = Img.content img s;
                                    }
                              in
                              Imap.add ~base size p xcode
                            else xcode
                          in
                          let state =
                            State.memcpy ~addr size (Img.content img s) state
                          in
                          (vmap, xcode, state))
                      (vmap, xcode, state) sections
                  in
                  (vmap, S.Set.add fname fmap, xcode, state))
              (vmap, S.Set.empty, env.code, state)
              (Loader_elf.files img')
          in
          Queue.iter
            (fun (name, value, lname, fname) ->
              try
                if
                  Bitvector.diff value
                    (List.assoc Dba.Var.Tag.Value
                       (S.Htbl.find_all symbols name))
                then (
                  S.Htbl.replace shadowing_symbols name (fname, Some lname);
                  S.Htbl.replace symbols name (Value, value))
              with Not_found ->
                Logger.debug ~level:4 "symbol %S resolved to %a from GOT" name
                  Virtual_address.pp
                  (Virtual_address.of_bitvector value);
                S.Htbl.add symbols name (Value, value))
            dyn_symbols;
          let entrypoint, initializations = Isa_helper.core img' in
          start :=
            Dcode.script ~task:env.tasks entrypoint ~fallthrough:true prehook
              parser_env;
          env.code <- xcode;
          let state =
            List.fold_left
              (fun state (var, value) -> Eval.assign var value state path)
              state initializations
          in
          state
    in
    let register_hook hooks addr hook =
      let others =
        try Virtual_address.Map.find addr !hooks with Not_found -> []
      in
      hooks := Virtual_address.Map.add addr (hook :: others) !hooks
    in
    let path, state =
      match ScriptFiles.get () with
      | [] -> (Path.empty (), state)
      | files ->
          let initialization_callbacks = Queue.create ()
          and grammar_extentions = ref []
          and declaration_callbacks = ref [] in
          let rec resolve_decl decl env path state = function
            | [] -> Logger.fatal "Unhandled declaration %a" Script.pp decl
            | app :: handlers -> (
                match app decl env path state with
                | None -> resolve_decl decl env path state handlers
                | Some state -> state)
          in
          S.Htbl.iter
            (fun _ plugin ->
              let module P = (val plugin : PLUGIN) in
              match
                P.extension
                  (module Exploration_stats)
                  (module Path)
                  (module State)
              with
              | None -> ()
              | Some ext ->
                  Option.iter Ast.Instr.register_pp P.instruction_printer;
                  Option.iter Script.register_pp P.declaration_printer;
                  grammar_extentions :=
                    P.grammar_extension :: !grammar_extentions;
                  let module E = (val ext) in
                  Dcode.register_callback (module E);
                  Option.iter
                    (fun init_callback ->
                      Queue.add init_callback initialization_callbacks)
                    E.initialization_callback;
                  Option.iter
                    (fun decl_callback ->
                      declaration_callbacks :=
                        decl_callback :: !declaration_callbacks)
                    E.declaration_callback;
                  Option.iter Ir.register_builtin_pp E.builtin_printer;
                  Option.iter
                    (fun at_exit_callback ->
                      Queue.add at_exit_callback at_exit_callbacks)
                    E.at_exit_callback)
            plugins;
          let script = Script.read_files !grammar_extentions files in
          Logger.debug "@[<v>%a@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space Script.pp)
            script;
          let path = Path.empty () in
          let state =
            Queue.fold
              (fun state callback -> callback path state)
              state initialization_callbacks
          in
          ( path,
            List.fold_left
              (fun state -> function
                | Script.Starting_from (addr, prehook) -> (
                    try
                      let addr =
                        Script.eval_expr ~size:parser_env.wordsize addr
                          parser_env
                      in
                      let bv = Eval.get_value addr state path in
                      Logger.debug ~level:40
                        "the entrypoint address %a resolves to %a"
                        Dba_printer.Ascii.pp_bl_term addr
                        Bitvector.pp_hex_or_bin bv;
                      start :=
                        Dcode.script ~task:env.tasks
                          (Virtual_address.of_bitvector bv)
                          ~fallthrough:true prehook parser_env;
                      state
                    with Non_unique ->
                      Logger.fatal
                        "the entrypoint address %a does not resolve to a \
                         unique value"
                        Script.Expr.pp (fst addr))
                | Script.Starting_from_core prehook ->
                    from_core path prehook state
                | Script.Load_sections names ->
                    List.fold_left
                      (fun ss name ->
                        let section =
                          Loader_utils.find_section_by_name name img
                        in
                        let addr =
                          Bitvector.of_int ~size:addr_size
                            (Loader.Section.pos section).virt
                        and size = (Loader.Section.size section).virt in
                        Logger.info "Load section %s (%a, %#x)" name
                          Bitvector.pp_hex_or_bin addr size;
                        State.memcpy ~addr size
                          (Loader.Img.content img section)
                          ss)
                      state names
                | Script.Load_data (load, _) -> (
                    match load with
                    | Load (len, _, addr, None) -> (
                        try
                          let addr =
                            Script.eval_expr ~size:parser_env.wordsize addr
                              parser_env
                          in
                          let bv = Eval.get_value addr state path in
                          Logger.debug ~level:40
                            "the memory initializer address %a resolves to %a"
                            Dba_printer.Ascii.pp_bl_term addr Bitvector.pp bv;
                          copy_from bv len state
                        with Non_unique ->
                          Logger.fatal
                            "the memory initializer address %a does not \
                             resolve to a unique value"
                            Script.Expr.pp (fst addr))
                    | _ -> assert false)
                | Script.Concretize_stack_pointer ->
                    let sp, value = Isa_helper.get_stack_pointer () in
                    State.assign sp (State.Value.constant value) state
                | Script.Import_symbols (names, file) ->
                    List.iter
                      (fun ((name, attr), _) ->
                        try
                          S.Htbl.add symbols name
                            (List.find
                               (fun (attr', _) -> attr = attr')
                               (S.Htbl.find_all
                                  (S.Htbl.find core_symbols file)
                                  name));
                          S.Htbl.remove shadowing_symbols name
                        with Not_found ->
                          Logger.fatal "unable to import <%s%a> from %s" name
                            Dba.Var.Tag.pp_attribute attr file)
                      names;
                    state
                | Script.Hook (addresses, stmts, pre) ->
                    List.iter
                      (fun addr ->
                        let anchor =
                          Format.asprintf "%a" Script.Expr.pp (fst addr)
                        in
                        Logger.debug ~level:10
                          "@[<v 2> replace address %s by@ %a@]" anchor
                          Script.pp_stmts stmts;
                        try
                          let addr =
                            Script.eval_expr ~size:parser_env.wordsize addr
                              parser_env
                          in
                          let bv = Eval.get_value addr state path in
                          Logger.debug ~level:40
                            "the stub address %a resolves to %a"
                            Dba_printer.Ascii.pp_bl_term addr
                            Bitvector.pp_hex_or_bin bv;
                          let addr = Virtual_address.of_bitvector bv in
                          if pre then register_hook prehooks addr (anchor, stmts)
                          else register_hook hooks addr (anchor, stmts)
                        with
                        | Non_unique ->
                            Logger.fatal
                              "the stub address %s does not resolve to a \
                               unique value"
                              anchor
                        | Unresolved (name, attr) as exn
                          when same_symbol name attr addr -> (
                            match MissingSymbol.get () with
                            | Error -> raise exn
                            | Warn ->
                                Logger.warning
                                  "@[<v>Can not resolve symbol <%s%a>.@ Will \
                                   ignore the following %a@]"
                                  name Dba.Var.Tag.pp_attribute attr
                                  Parse_utils.pp_pos (snd addr)
                            | Quiet -> ()))
                      addresses;
                    state
                | Script.Decode (opcode, stmts) ->
                    Dcode.register_opcode_hook (fun reader ->
                        if
                          Binstream.fold
                            (fun byte eq -> eq && Lreader.Read.u8 reader = byte)
                            opcode true
                        then Some (stmts, parser_env)
                        else None);
                    state
                | Script.Init i -> set path state i
                | Script.Explore_all ->
                    I.Htbl.add env.tasks (I.Htbl.length env.tasks) ();
                    state
                | decl ->
                    resolve_decl decl parser_env path state
                      !declaration_callbacks)
              state script )
    in
    let in_section lo hi addr _ =
      let z = Virtual_address.to_bigint addr in
      Z.leq lo z && Z.leq z hi
    in
    let hooks =
      Virtual_address.Map.merge
        (fun _ prehooks hooks ->
          match (prehooks, hooks) with
          | None, None -> assert false
          | Some prehooks, None -> Some (List.rev prehooks)
          | None, Some hooks -> Some (List.rev hooks)
          | Some prehooks, Some hooks ->
              Some (List.rev_append prehooks (List.rev hooks)))
        !prehooks !hooks
    in
    let dcode, whooks, hooks =
      Imap.fold
        (fun (lo, hi) section (dcode, whooks, others) ->
          match section with
          | RWX ->
              let whooks', others =
                Virtual_address.Map.partition (in_section lo hi) others
              in
              ( Imap.add ~base:lo (Z.to_int (Z.sub hi lo) + 1) None dcode,
                Virtual_address.Map.fold
                  (fun addr hooks whooks ->
                    Virtual_address.Map.add addr (hooks, parser_env) whooks)
                  whooks' whooks,
                others )
          | RX { base; size; content; _ } ->
              let hooks, others =
                Virtual_address.Map.partition (in_section lo hi) others
              in
              ( Imap.add ~base:lo
                  (Z.to_int (Z.sub hi lo) + 1)
                  (Some
                     (Dcode.create ~task:env.tasks ~hooks:(hooks, parser_env)
                        base
                        (Lreader.of_zero_extend_buffer
                           ~endianness:env.endianness content)
                        size))
                  dcode,
                whooks,
                others ))
        env.code
        (Imap.empty, Virtual_address.Map.empty, hooks)
    in
    Virtual_address.Map.iter
      (fun addr hooks ->
        let scope, _ =
          Dcode.single ~task:env.tasks ~hooks:(hooks, parser_env) addr
            (Lreader.of_bytes "") 0
        in
        Virtual_address.Htbl.add env.rocache addr scope)
      hooks;
    env.dcode <- dcode;
    env.whooks <- whooks;
    (!start, path, state)

  let start () =
    let filename = Kernel_options.ExecFile.get () in
    Logger.debug "Running SSE on %s" filename;
    let entry, path, state =
      try initialize_state ()
      with Halt -> Logger.fatal "Unable to resolves the initial state"
    in
    if I.Htbl.length env.tasks = 0 then
      Logger.warning "Nothing to reach: halting..."
    else
      try
        Sys.catch_break true;
        Option.iter
          (fun timeout ->
            Sys.set_signal Sys.sigalrm
              (Sys.Signal_handle
                 (fun s ->
                   assert (s = Sys.sigalrm);
                   raise_notrace Halt));
            ignore (Unix.alarm timeout))
          (Timeout.get_opt ());
        Screen.init ();
        exec Default path 0 ~max_depth:(MaxDepth.get ()) state
          Virtual_address.zero entry
      with
      | Halt | Sys.Break -> halt ()
      | err ->
          Screen.release ();
          raise err

  let unit =
    try start ()
    with err -> (
      match Printexc.use_printers err with
      | None -> raise err
      | Some msg -> Logger.fatal "%s" msg)
end
