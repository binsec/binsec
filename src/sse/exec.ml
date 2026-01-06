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

open Basic_types.Integers
open Types
module IntTbl = Basic_types.Integers.Int.Htbl
module StrTbl = Basic_types.String.Htbl
module IntSet = Basic_types.Integers.Int.Set
module StrSet = Basic_types.String.Set
module StrMap = Basic_types.String.Map
module AttrMap = Dba.Var.Tag.Attribute.Map

exception Halt
exception Unresolved of string * Dba.Var.Tag.attribute
exception Unknown = Symbolic.State.Unknown
exception Initialization_failure

let () =
  Printexc.register_printer (function
    | Unresolved (name, attr) ->
        Some
          (Format.asprintf "Can not resolve symbol <%s%a>" name
             Dba.Var.Tag.Attribute.pp attr)
    | _ -> None)

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

let same_symbol name attr ((expr : Script.Ast.Expr.t), _) =
  match expr with
  | Symbol ((name', attr'), _) -> name = name' && attr = attr'
  | _ -> false

type output = (Dba.Var.t, Dba.Expr.t) Output.t

let pp_output : Format.formatter -> output -> unit =
  Output.pp
    (fun ppf ({ name; _ } : Dba.Var.t) -> Format.pp_print_string ppf name)
    Dba_printer.Ascii.pp_bl_term

type Ir.builtin += Print of output | Signal of status

type Ir.builtin +=
  | Enumerate of {
      id : int;
      n : int;
      enum : Dba.Expr.t;
      format : Output.format;
    }
  | Reach of { id : int; n : int; guard : Dba.Expr.t; actions : output list }

let () =
  Ir.register_builtin_printer (fun ppf -> function
    | Print output ->
        Format.fprintf ppf "print %a" pp_output output;
        true
    | Enumerate _ ->
        Format.pp_print_string ppf "enumerate";
        true
    | Reach _ ->
        Format.pp_print_string ppf "reach";
        true
    | Signal status ->
        Format.fprintf ppf "signal %a" Metrics.pp_status status;
        true
    | _ -> false)

type warnerror = Error | Warn | Quiet

module type CONFIG = sig
  val filename : string
  val isa : Machine.isa option
  val img : Loader.Img.t
  val fs : string -> Loader_types.buffer
  val assembler : (module Compiler.ASSEMBLER)
  val trace : Compiler.trace
  val transient_enum : int
  val max_depth : int
  val enumeration_limit : int
  val smt_backend : Symbolic.Smtlib.Solver.backend
  val smt_timeout : float option
  val smt_multichecks : bool
  val smt_dumpdir : string option
  val missing_symbols : warnerror
  val timeout : int option
  val entry : Virtual_address.t option

  val script :
    ( unit,
      Binsec_script.obj,
      unit,
      unit,
      Binsec_script.obj Dyp.dyplexbuf )
    Dyp.dyp_action
    list
    list ->
    Script.Ast.t list

  val plugins : (module PLUGIN) list
end

module Run (Config : CONFIG) (State : STATE) (W : Worklist.S) () = struct
  module Exploration_metrics = Metrics.Exploration ()
  module Query_metrics = Metrics.Query ()
  module Path = Symbolic.Path.Make (Query_metrics) (State)

  type enum = {
    id : int;
    n : int;
    enum : Dba.Expr.t;
    format : Output.format;
    mutable k : int;
    mutable values : State.Model.t Bitvector.Map.t;
  }

  type reach = {
    id : int;
    mutable n : int;
    guard : Dba.Expr.t;
    actions : output list;
  }

  let isa =
    match (Config.isa, Loader.Img.arch Config.img) with
    | None, arch -> arch
    | Some isa, Unknown -> isa
    | Some isa, arch ->
        if isa <> arch then
          Logger.warning "overwriting image isa (%a) with %a" Machine.pp arch
            Machine.pp isa;
        isa

  let decoder =
    try Decoder.get isa
    with Not_found ->
      Logger.fatal "Can not resolve decoder for %a" Machine.pp isa

  let image = Image.load ~fs:Config.fs Config.filename Config.img

  let (rev_section, rev_symbol) :
      (Virtual_address.t -> Image.section) * (Virtual_address.t -> Image.symbol)
      =
    Image.layout_with_cache image

  let pp_model : Format.formatter -> Path.Model.t -> unit =
    let section : Virtual_address.t -> string option =
     fun addr ->
      match rev_section addr with
      | exception Not_found -> None
      | { name; _ } -> Some name
    in
    Path.Model.pp_with_sections section

  let endianness = Machine.ISA.endianness isa

  let wordsize =
    Size.Bit.to_int (Machine.Bitwidth.bitsize (Machine.ISA.bits isa))

  let depth = Path.declare_field 0 ~merge:(fun x y -> Some (max x y))

  let step : Path.t -> Virtual_address.t -> int -> int =
   fun path addr n ->
    Path.set_pc path addr;
    let d = Path.get path depth + n in
    Path.set path depth d;
    Exploration_metrics.Instructions.incr n;
    Exploration_metrics.Max_depth.update d;
    d

  let config =
    Compiler.make_config ~debug:Config.trace
      ~echo:(fun _ msg -> Logger.result "%s" msg)
      ~step:(fun path addr n -> ignore (step path addr n))
      (module Path)
      Config.assembler

  let callback = ref Disassembly.Callback.empty

  let (full_range, full_mask) : Virtual_address.t Interval.t * unit Zmap.t =
    let lower_bound = Z.zero
    and upper_bound = Z.extract Z.minus_one 0 wordsize in
    ( {
        lo = Virtual_address.of_bigint lower_bound;
        hi = Virtual_address.of_bigint upper_bound;
      },
      Zmap.singleton ~lo:lower_bound ~hi:upper_bound () )

  let () =
    callback :=
      Disassembly.Callback.register_hook !callback full_range Fetch (fun addr ->
          Exploration_metrics.Addresses.register addr;
          None)

  module Isa = (val Isa_helper.get isa)

  type nonrec fiber = ([ `All ], Path.t) fiber
  type thunk = { path : Path.t; k : Path.t continuation }

  type t = {
    rocache : fiber Virtual_address.Htbl.t;
        (** instruction cache for RX sections *)
    cache : fiber OMap.t Virtual_address.Htbl.t;
        (** instruction cache for RWX sections *)
    mutable code : Path.t Disassembly.t option Zmap.t;
  }

  (** set of tasks to perform *)
  let tasks : unit IntTbl.t = IntTbl.create 7

  (** worklist of pending path *)
  let worklist : thunk W.t ref = ref W.empty

  let env : t =
    {
      rocache = Virtual_address.Htbl.create 2048;
      cache = Virtual_address.Htbl.create 0 (* cache_size *);
      code = Zmap.empty;
    }

  let choose () =
    try
      let thunk, worklist' = W.pop !worklist in
      Logger.debug ~level:3 "Selecting path #%d (among %d)" (Path.id thunk.path)
        (Exploration_metrics.Paths.get Pending);
      worklist := worklist';
      thunk
    with Not_found ->
      Logger.info "Empty path worklist: halting ...";
      raise_notrace Halt

  let add path = worklist := W.push path !worklist
  let at_fork_callbacks = Queue.create ()
  let at_signal_callbacks = Queue.create ()
  let at_exit_callbacks = Queue.create ()

  let threat_to_completeness () =
    let max_depth = Exploration_metrics.Paths.status Max_depth in
    let incomplete_enum = Exploration_metrics.Paths.status Enumeration_limit in
    let unknown = Exploration_metrics.Paths.status Unresolved_formula in
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
              "@ - %d SMT solver queries remain unsolved (-smt-timeout)" unknown)
        (max_depth, incomplete_enum, unknown)

  let halt () =
    Exploration_metrics.Timer.stop ();
    Logger.info "@[<v 0>@[<v 2>SMT queries@,%a@]@,@[<v 2>Exploration@,%a@]@,@]"
      Query_metrics.pp () Exploration_metrics.pp ();
    threat_to_completeness ();
    Queue.iter (fun callback -> callback ()) at_exit_callbacks

  let hook_anchors = Virtual_address.Htbl.create 4

  let pp_virtual_address ppf addr =
    Virtual_address.pp ppf addr;
    try
      let anchor = Virtual_address.Htbl.find hook_anchors addr in
      Format.fprintf ppf " (%s)" anchor
    with Not_found -> ()

  let c_string :
      Path.t -> string option -> Dba.Expr.t -> Dba.Expr.t option -> string =
   fun path array offset size ->
    try
      let rec iter path buf addr limit =
        if Bitvector.equal addr limit then Buffer.contents buf
        else
          let byte =
            Path.eval path
              (Dba.Expr.load ?array Size.Byte.one Machine.LittleEndian
                 (Dba.Expr.constant addr))
          in
          if Bitvector.is_zeros byte then Buffer.contents buf
          else
            (Buffer.add_char buf (Bitvector.to_char byte);
             iter path buf (Bitvector.succ addr))
              limit
      in
      let offset = Path.eval path offset in
      let limit =
        match size with
        | None -> offset
        | Some size -> Bitvector.add offset (Path.eval path size)
      in
      iter path (Buffer.create 16) offset (Bitvector.pred limit)
    with Symbolic.State.Undeclared _ -> ""

  let print (output : output) path model =
    match output with
    | Model ->
        Logger.result "@[<v 0>Model %@ %a@ %a@]" pp_virtual_address
          (Path.pc path) pp_model model
    | Formula ->
        Logger.result "Formula %@ %a@\n%a" pp_virtual_address (Path.pc path)
          (State.print_smtlib ?slice:None)
          (Path.state path)
    | Slice slice ->
        let slice =
          List.map (fun (expr, name) -> (Path.get_value path expr, name)) slice
        in
        Logger.result "Formula for %a %@ %a@\n%a" pp_virtual_address
          (Path.pc path)
          (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (_, n) ->
               Format.pp_print_string ppf n))
          slice
          (State.print_smtlib ~slice)
          (Path.state path)
    | Value (format, e) ->
        Logger.result "@[<v 0>Value %a : %a@]" Dba_printer.Ascii.pp_bl_term e
          (pp_value_as format) (Path.eval path e)
    | Stream var ->
        Logger.result "@[<v 0>Ascii stream %s : %a@]" var.name
          (fun ppf (model, rev_history) ->
            Format.pp_print_char ppf '"';
            (List.iter (fun elt ->
                 let rec iter bv =
                   let size = Bitvector.size_of bv in
                   assert (size mod 8 = 0);
                   if size = 8 then
                     Format.pp_print_char ppf (Bitvector.to_char bv)
                   else
                     let byte = Bitvector.extract ~hi:7 ~lo:0 bv in
                     Format.pp_print_char ppf (Bitvector.to_char byte);
                     iter (Bitvector.extract ~hi:(size - 1) ~lo:8 bv)
                 in
                 iter (State.Model.eval elt model)))
              (List.rev rev_history);
            Format.pp_print_char ppf '"')
          ( model,
            try Dba_types.Var.Map.find var (Path.symbols path)
            with Not_found -> [] )
    | String { array; offset; size } ->
        Logger.result "@[<v 0>%a : %S@]" pp_output output
          (c_string path array offset size)

  let enumerate ({ enum; id = tid; format; n; k; values } as e) path =
    (if n > k then
       let values' = Path.enumerate path enum ~n:(n - k) ~accumulator:values in
       if values != values' then (
         let k' = Bitvector.Map.cardinal values' in
         Logger.result
           "@[<hov 0>Directive :: enumerate@ possible values (%d) for %a %@ \
            %a:@ @[<hov 0>%a@]@]"
           k' Dba_printer.Ascii.pp_bl_term enum pp_virtual_address
           (Path.pc path)
           (fun ppf map ->
             ignore
               (Bitvector.Map.fold
                  (fun value _ f ->
                    f ppf ();
                    pp_value_as format ppf value;
                    fun ppf () ->
                      Format.pp_print_char ppf ',';
                      Format.pp_print_space ppf ())
                  map
                  (fun _ _ -> ())
                 : Format.formatter -> unit -> unit))
           values';
         e.k <- k';
         e.values <- values';
         if n = k' then (
           IntTbl.remove tasks tid;
           if IntTbl.length tasks = 0 then raise_notrace Halt)));
    Return

  let decr ({ id; n; _ } as r) =
    n <= 0
    ||
    (r.n <- n - 1;
     n <> 1
     ||
     (IntTbl.remove tasks id;
      IntTbl.length tasks <> 0))

  let reach ({ n; guard; actions; _ } as r) path : Path.t continuation =
    if n = 0 then Return
    else
      match Path.check_sat_assuming path ~retain:true guard with
      | None -> Return
      | Some model ->
          Logger.result "@[<h>Path %d reached address %a (%a to go)@]"
            (Path.id path) pp_virtual_address (Path.pc path)
            (fun ppf n ->
              if n = -1 then Format.pp_print_char ppf '*'
              else Format.pp_print_int ppf (n - 1))
            n;
          List.iter (fun output -> print output path model) actions;
          if decr r then Return else raise Halt

  let print output path = print output path (List.hd (Path.models path))

  let non_executable_code : Path.t -> Path.t continuation =
   fun path ->
    Logger.warning "@[<hov>Cut path %d (non executable) %@ %a@]" (Path.id path)
      pp_virtual_address (Path.pc path);
    Signal Non_executable_code

  let () =
    Compiler.register_builtin_callback config (function
      | Print output -> Apply (print output)
      | Enumerate { id; n; enum; format } ->
          Call
            (enumerate
               { id; n; enum; format; k = 0; values = Bitvector.Map.empty })
      | Reach { id; n; guard; actions } ->
          Call (reach { id; n; guard; actions })
      | Signal Non_executable_code -> Call non_executable_code
      | _ -> Unknown);
    let deps : output -> Dba_types.Var.Set.t option = function
      | Model -> Some Dba_types.Var.Set.empty
      | Slice list ->
          Some
            (List.fold_left
               (fun deps (expr, _) ->
                 Dba_types.Expr.collect_variables expr deps)
               Dba_types.Var.Set.empty list)
      | Value (_, expr) ->
          Some (Dba_types.Expr.collect_variables expr Dba_types.Var.Set.empty)
      | Formula | Stream _ | String _ -> None
    in
    let may_read : Ir.builtin -> Dba_types.Var.Set.t option option = function
      | Print output -> Some (deps output)
      | Enumerate { enum; _ } ->
          Some
            (Some
               (Dba_types.Expr.collect_variables enum Dba_types.Var.Set.empty))
      | Reach _ | Signal _ -> Some None
      | _ -> None
    in
    callback :=
      Disassembly.Callback.register_knowledge !callback May_read may_read;
    Compiler.register_knowledge config May_read may_read;
    Compiler.register_knowledge config May_write (function
      | Print _ | Enumerate _ | Reach _ | Signal _ ->
          Some (Some Dba_types.Var.Set.empty)
      | _ -> None);
    callback :=
      Disassembly.Callback.register_knowledge !callback Must_write (function
        | Print _ | Enumerate _ | Reach _ | Signal _ ->
            Some Dba_types.Var.Set.empty
        | _ -> None)

  let non_executable_code : Virtual_address.t -> Ir.Graph.t option =
   fun addr ->
    Some
      (let ir = Ir.Graph.empty () in
       ignore
         (Ir.Graph.append_node ir
            (Terminator
               {
                 label = Hook { addr; info = "segmentation fault" };
                 kind = Builtin (Signal Non_executable_code);
               }));
       ir)

  let status =
    Path.declare_field
      ~merge:(fun x y ->
        match (x, y) with
        | None, (None | Some Stashed) -> Some None
        | _, _ -> None)
      None

  let call_stack =
    Path.declare_field [] ~merge:(fun cs0 cs1 ->
        match (cs0, cs1) with
        | Continue f0 :: cs0', Continue f1 :: cs1' when f0 == f1 && cs0' == cs1'
          ->
            Some cs0
        | _, _ -> None)

  let fork : Path.t -> Path.t =
   fun path ->
    Exploration_metrics.Paths.incr ();
    let path' = Path.fork path in
    Queue.iter (fun f -> f path path') at_fork_callbacks;
    path'

  let report : Path.t -> status -> unit =
   fun path s ->
    Exploration_metrics.Paths.signal s;
    Path.set path status (Some s);
    (match s with
    | Error msg ->
        Logger.error "Cut path %d (uninterpreted %S) %@ %a" (Path.id path) msg
          Virtual_address.pp (Path.pc path)
    | _ -> ());
    Queue.iter (fun f -> f path s) at_signal_callbacks

  type test = False | True | Both of Path.t * Path.t

  let of_bool : bool -> test = function false -> False | true -> True

  let split : Path.t -> Dba.Expr.t -> test =
    let resolve : Path.t -> Path.State.t -> bool -> test =
     fun path candidate trueish ->
      match Path.State.check_sat (Path.cookie path) candidate with
      | exception Unknown ->
          let other = fork path in
          Path.set_state other candidate;
          report other Unresolved_formula;
          of_bool trueish
      | None -> of_bool trueish
      | Some model ->
          let other = fork path in
          Path.set_state other candidate;
          Path.set_models other [ model ];
          let path, other = if trueish then (path, other) else (other, path) in
          Both (path, other)
    in
    fun path test ->
      match Path.partition path test with
      | True -> True
      | False -> False
      | Trueish candidate -> resolve path candidate true
      | Falsish candidate -> resolve path candidate false
      | Split (state, models) ->
          let other = fork path in
          Path.set_state other state;
          Path.set_models other models;
          Both (path, other)

  module Generic
      (D : Interpreter.DRIVER with type path = Path.t and type outcome = unit) :
    Interpreter.DRIVER with type path = Path.t and type outcome = unit = struct
    type path = Path.t
    type outcome = D.outcome

    let[@tail_mod_cons] rec merge :
        Path.t -> Path.t list -> Path.t list -> Path.t list =
     fun main others rejected ->
      match others with
      | other :: others -> (
          match Path.get other status with
          | Some Stashed -> (
              match Path.merge main other with
              | None -> merge main others (other :: rejected)
              | Some main ->
                  report other Merged;
                  merge main others rejected)
          | None | Some _ -> raise (Invalid_argument "merge"))
      | [] -> (
          match List.rev rejected with
          | [] -> [ main ]
          | other :: others -> main :: (merge [@tailcall]) other others [])

    let rec resume : Path.t -> Path.t continuation -> outcome =
     fun path k ->
      match k with
      | Continue fiber ->
          (Interpreter.dispatch [@tailcall]) path fiber (module D)
      | Call (f, k) ->
          Path.set path call_stack (k :: Path.get path call_stack);
          resume path (f path)
      | Tail_call f -> resume path (f path)
      | Return -> (
          match Path.get path call_stack with
          | [] -> signal path (Error "invalid return")
          | k :: stack ->
              Path.set path call_stack stack;
              resume path k)
      | Return_to fiber -> (
          match Path.get path call_stack with
          | [] ->
              signal path (Error "invalid return");
              yield ()
          | _ :: stack ->
              Path.set path call_stack stack;
              (Interpreter.dispatch [@tailcall]) path fiber (module D))
      | Signal status -> signal path status
      | Fork (k, k') ->
          let path' = fork path in
          add { path; k };
          add { path = path'; k = k' };
          yield ()
      | Merge (others, k) ->
          let paths = merge path others [] in
          List.iter (fun path -> add { path; k }) (List.tl paths);
          resume (List.hd paths) k

    and signal : Path.t -> status -> outcome =
     fun path s ->
      report path s;
      yield ()

    and yield : unit -> outcome =
     fun () ->
      let { path; k } = choose () in
      resume path k

    let rec goto path addr =
      match Virtual_address.Htbl.find env.rocache addr with
      | target ->
          Path.set_pc path addr;
          (Interpreter.dispatch [@tailcall]) path target (module D)
      | exception Not_found -> (
          match Zmap.find (Virtual_address.to_bigint addr) env.code with
          | exception Not_found ->
              let fiber =
                Disassembly.disassemble_from
                  (Disassembly.create_small
                     (Disassembly.Callback.register_hook !callback
                        { lo = addr; hi = addr } ~stage:Late Fetch
                        non_executable_code)
                     ~decoder addr (Reader.of_bytes "") 1 config)
                  addr
              in
              Virtual_address.Htbl.add env.rocache addr fiber;
              Path.set_pc path addr;
              (Interpreter.dispatch [@tailcall]) path fiber (module D)
          | Item { elt = Some code; _ } ->
              let fiber = Disassembly.disassemble_from code addr in
              Virtual_address.Htbl.add env.rocache addr fiber;
              Path.set_pc path addr;
              (Interpreter.dispatch [@tailcall]) path fiber (module D)
          | Item { elt = None; _ } -> transient_instruction path addr)

    and transient_instruction path addr =
      let n = Config.transient_enum in
      let handle path omap addr opcode bv =
        ignore (Path.assume path (Dba.Expr.equal opcode (Dba.Expr.constant bv)));
        let opcode = Bitvector.to_asciistring bv in
        let omap, fiber =
          try (omap, OMap.find opcode omap)
          with Not_found ->
            let reader = Reader.of_bytes ~endianness opcode in
            let code =
              Disassembly.create_small !callback ~decoder addr reader
                (String.length opcode) config
            in
            let fiber = Disassembly.fetch_no_link code addr in
            let omap =
              match Ir.label_of (Ir.View.node (Disassembly.graph code) 0) with
              | Hook _ -> OMap.add "" fiber omap
              | Instruction inst ->
                  Logger.debug ~level:4
                    "@[<hov>Self-written instruction @@ %a could be %a [ %a ]@]"
                    Virtual_address.pp addr Mnemonic.pp
                    (Instruction.mnemonic inst)
                    String_utils.pp_hex opcode;
                  let s = (Instruction.size inst :> int) in
                  if s = 0 then omap
                  else OMap.add (String.sub opcode 0 s) fiber omap
            in
            (omap, fiber)
        in
        add { path; k = Continue fiber };
        omap
      in
      let omap =
        try Virtual_address.Htbl.find env.cache addr
        with Not_found -> OMap.empty
      in
      let opcode =
        Dba.Expr.load Isa.max_instruction_len endianness
          (Dba.Expr.constant
             (Bitvector.create (Virtual_address.to_bigint addr) wordsize))
      in
      match Path.enumerate path ~retain:true ~n opcode with
      | exception Unknown -> signal path Unresolved_formula
      | values ->
          let (value, _), values = Bitvector.Map.pop values in
          let omap =
            handle path
              (Bitvector.Map.fold
                 (fun value _ omap ->
                   Exploration_metrics.Paths.incr ();
                   handle (fork path) omap addr opcode value)
                 values omap)
              addr opcode value
          in
          Virtual_address.Htbl.replace env.cache addr omap;
          yield ()

    type ('a, 'b) fiber = ('a, 'b) Types.fiber

    let step : path -> ([ `Step ], path) fiber -> outcome =
     fun path (Step { addr; n; succ }) ->
      if step path addr n <= Config.max_depth then
        (Interpreter.dispatch [@tailcall]) path succ (module D)
      else (
        Logger.warning "@[<hov>Cut path %d (max depth) %@ %a@]" (Path.id path)
          Virtual_address.pp addr;
        signal path Max_depth)

    let assign : path -> ([ `Assign ], path) fiber -> outcome =
     fun path (Assign { var; rval; succ }) ->
      Path.assign path var rval;
      (Interpreter.dispatch [@tailcall]) path succ (module D)

    let clobber : path -> ([ `Clobber ], path) fiber -> outcome =
     fun path (Clobber { var; succ }) ->
      Path.clobber path var;
      (Interpreter.dispatch [@tailcall]) path succ (module D)

    let load : path -> ([ `Load ], path) fiber -> outcome =
     fun path (Load { var; base; addr; dir; succ }) ->
      Path.load path var base ~addr dir;
      (Interpreter.dispatch [@tailcall]) path succ (module D)

    let store : path -> ([ `Store ], path) fiber -> outcome =
     fun path (Store { base; addr; dir; rval; succ }) ->
      Path.store path base ~addr rval dir;
      (Interpreter.dispatch [@tailcall]) path succ (module D)

    let symbolize : path -> ([ `Symbolize ], path) fiber -> outcome =
     fun path (Symbolize { var; succ }) ->
      Path.symbolize path var;
      (Interpreter.dispatch [@tailcall]) path succ (module D)

    let apply : path -> ([ `Apply ], path) fiber -> outcome =
     fun path (Apply { f; succ }) ->
      f path;
      (Interpreter.dispatch [@tailcall]) path succ (module D)

    let assume : path -> ([ `Assume ], path) fiber -> outcome =
     fun path (Assume { test; succ }) ->
      match Path.assume path test with
      | exception Unknown -> signal path Unresolved_formula
      | None ->
          Logger.warning "@[<hov>Cut path %d (unsatisfiable assumption) %@ %a@]"
            (Path.id path) pp_virtual_address (Path.pc path);
          signal path Unsatisfiable_assumption
      | Some _ -> (Interpreter.dispatch [@tailcall]) path succ (module D)

    let check : path -> ([ `Assert ], path) fiber -> outcome =
     fun path (Assert { test; succ }) ->
      match split path test with
      | True -> (Interpreter.dispatch [@tailcall]) path succ (module D)
      | False ->
          Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]" pp_virtual_address
            (Path.pc path) pp_model
            (List.hd (Path.models path));
          signal path Assertion_failure
      | Both (path, other) ->
          report other Assertion_failure;
          Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]" pp_virtual_address
            (Path.pc path) pp_model
            (List.hd (Path.models other));
          (Interpreter.dispatch [@tailcall]) path succ (module D)

    let ite : path -> ([ `Branch ], path) fiber -> outcome =
     fun path (Branch { test; taken; fallthrough }) ->
      Exploration_metrics.Topology.incr Branch;
      match split path test with
      | True -> (Interpreter.dispatch [@tailcall]) path taken (module D)
      | False -> (Interpreter.dispatch [@tailcall]) path fallthrough (module D)
      | Both (path, other) ->
          add { path; k = Continue taken };
          add { path = other; k = Continue fallthrough };
          yield ()

    let jump : path -> ([ `Jump ], path) fiber -> outcome =
     fun path (Jump target) ->
      Exploration_metrics.Topology.incr Jump;
      let handle path target bv =
        Logger.debug ~level:4 "@[<hov>Dynamic jump@ %@%a@ could lead to@ %a@]"
          Virtual_address.pp (Path.pc path) Bitvector.pp_hex_or_bin bv;
        ignore (Path.assume path (Dba.Expr.equal target (Dba.Expr.constant bv)));
        add { path; k = Continue (Goto (Virtual_address.of_bitvector bv)) }
      in
      match
        Path.enumerate path ~retain:true ~n:Config.enumeration_limit target
      with
      | exception Unknown -> signal path Unresolved_formula
      | values ->
          let (value, _), values = Bitvector.Map.pop values in
          if Bitvector.Map.is_empty values then
            goto path (Virtual_address.of_bitvector value)
          else
            let old_paths = Exploration_metrics.Paths.get Total in
            Bitvector.Map.iter
              (fun value _ -> handle (fork path) target value)
              values;
            if
              Exploration_metrics.Paths.get Total - old_paths
              = Config.enumeration_limit - 1
            then (
              Logger.warning
                "Enumeration of jump targets %@ %a hit the limit %d and may be \
                 incomplete"
                Virtual_address.pp (Path.pc path) Config.enumeration_limit;
              let other = fork path in
              report other Enumeration_limit);
            handle path target value;
            yield ()

    let goto : path -> ([ `Goto ], path) fiber -> outcome =
     fun path (Goto target) -> goto path target

    let call : path -> ([ `Call ], path) fiber -> outcome =
     fun path (Call { f; succ }) ->
      Path.set path call_stack (Continue succ :: Path.get path call_stack);
      resume path (f path)

    let tail_call : path -> ([ `Tail_call ], path) fiber -> outcome =
     fun path (Tail_call f) -> resume path (f path)
  end

  module rec Default :
    (Interpreter.DRIVER with type path = Path.t and type outcome = unit) =
    Generic (Default)

  let initialize_state () =
    let entry =
      ref
        (match Config.entry with
        | Some addr -> addr
        | None -> Loader.Img.entry Config.img)
    in
    let start = ref [] in
    let symbols = StrTbl.create 128 in
    let parser_env : Script.env =
      let tbl = StrTbl.create 128 and ori = StrTbl.create 128 in
      List.iter
        (fun (name, var) -> StrTbl.add tbl (String.lowercase_ascii name) var)
        (Isa.get_defs ());
      let define (var : Dba.Var.t) pos =
        let name = String.lowercase_ascii var.name in
        StrTbl.add tbl name (Dba.LValue.v var);
        if pos <> Lexing.dummy_pos then StrTbl.add ori name pos
      in
      let origin name = StrTbl.find_opt ori name in
      let lookup name = StrTbl.find tbl (String.lowercase_ascii name) in

      let lookup_symbol name (attr : Dba.Var.Tag.attribute) =
        try List.assoc attr (StrTbl.find_all symbols name)
        with Not_found -> (
          match AttrMap.find attr (StrMap.find name image.symbols) with
          | [] | (exception Not_found) -> raise (Unresolved (name, attr))
          | (value, origin) :: others ->
              let others =
                List.filter (fun (other, _) -> not (Z.equal value other)) others
              in
              if others <> [] then
                Logger.warning
                  "@[<v>Symbol %s comes from the file %s and shadows other \
                   definitions@ Use \"import <%s> from FILE\" to disambiguate"
                  name origin name;
              let tag =
                Dba.Var.Tag.Symbol
                  (attr, Lazy.from_val (Bitvector.create value wordsize))
              in
              let sym = Dba.Expr.var ~tag name wordsize in
              StrTbl.add symbols name (attr, sym);
              sym)
      in
      { wordsize; endianness; define; origin; lookup; lookup_symbol }
    in
    let query_symbol : string -> Dba.Var.Tag.attribute -> Z.t =
     fun name attr ->
      match parser_env.lookup_symbol name attr with
      | Var { info = Symbol (_, (lazy bv)); _ } -> Bitvector.value_of bv
      | _ -> raise (Unresolved (name, attr))
    in
    let path =
      let instruction_callbacks = Queue.create ()
      and initialization_callbacks = Queue.create ()
      and grammar_extentions = ref []
      and declaration_callbacks = ref [] in
      Queue.add
        (fun inst parse_env : Ir.fallthrough list ->
          match inst with
          | Script.Print output ->
              [ Builtin (Print (Script.Output.eval parse_env output)) ]
          | Script.Enumerate (n, enum) ->
              let id = IntTbl.length tasks in
              IntTbl.add tasks id ();
              let enum = Script.eval_expr enum parse_env in
              [ Builtin (Enumerate { id; enum; format = Hex; n }) ]
          | Script.Reach (n, guard, actions) ->
              let id = IntTbl.length tasks in
              IntTbl.add tasks id ();
              let guard =
                Option.fold ~none:Dba.Expr.one
                  ~some:(fun test -> Script.eval_expr ~size:1 test parse_env)
                  guard
              in
              let actions = List.map (Script.Output.eval parse_env) actions in
              [ Builtin (Reach { id; n; guard; actions }) ]
          | _ -> [])
        instruction_callbacks;
      let rec resolve_decl decl env path = function
        | [] -> Logger.fatal "Unhandled declaration %a" Script.pp decl
        | app :: handlers ->
            if not (app decl env path) then resolve_decl decl env path handlers
      in
      let module Engine = struct
        let isa = isa
        let image = image
        let fs = Config.fs

        module Path = Path

        module Metrics = struct
          module Exploration = Exploration_metrics
          module Queries = Query_metrics
        end

        module Debug = struct
          let reverse_section : Virtual_address.t -> (string * Z.t) option =
           fun addr ->
            match rev_section addr with
            | exception Not_found -> None
            | { base; name; _ } ->
                Some
                  ( name,
                    Z.sub
                      (Virtual_address.to_bigint addr)
                      (Virtual_address.to_bigint base) )

          let reverse_symbol : Virtual_address.t -> (string * Z.t) option =
           fun addr ->
            match rev_symbol addr with
            | exception Not_found -> None
            | { base; name; _ } ->
                Some
                  ( name,
                    Z.sub
                      (Virtual_address.to_bigint addr)
                      (Virtual_address.to_bigint base) )
        end

        type ('a, 'b) eq = False | True : ('a, 'a) eq

        let equal :
            type a b.
            ('value0, 'model0, 'state0, 'path0, a) field_id ->
            ('value1, 'model1, 'state1, 'path1, b) field_id ->
            (a, b) eq =
         fun id id' ->
          if Obj.repr id = Obj.repr id' then Obj.magic True else False

        type entry =
          | Entry :
              ('value, 'model, 'state, 'path, 'a) field_id * 'a Path.key
              -> entry

        let fields : entry list ref = ref []

        let register :
            type a.
            ('value, 'model, 'state, 'path, a) field_id ->
            ?copy:(a -> a) ->
            ?merge:(a -> a -> a option) ->
            a ->
            unit =
         fun id ?copy ?merge default ->
          fields :=
            Entry (id, Path.declare_field default ?copy ?merge) :: !fields

        let rec lookup :
            type a.
            (Path.value, Path.model, Path.state, Path.t, a) field_id ->
            entry list ->
            a Path.key =
         fun target entries ->
          match entries with
          | [] -> raise Not_found
          | Entry (id, key) :: entries -> (
              match equal target id with
              | True -> key
              | False -> lookup target entries)

        let lookup : ('value, 'model, 'state, 'path, 'a) field_id -> 'a Path.key
            =
         fun id -> lookup id !fields

        let resume path k =
          match Path.get path status with
          | Some Stashed ->
              Path.set path status None;
              add { path; k }
          | None | Some _ -> raise (Invalid_argument "resume")
      end in
      List.iter
        (fun plugin ->
          let module P = (val plugin : PLUGIN) in
          Logger.debug "registering plugin %S fields" P.name;
          List.iter
            (fun (Field { id; default; copy; merge }) ->
              Engine.register id default ?copy ?merge)
            (P.fields (module Path)))
        Config.plugins;
      List.iter
        (fun plugin ->
          let module P = (val plugin : PLUGIN) in
          Logger.debug "registering plugin %S extensions" P.name;
          List.iter
            (function
              | Initialization_callback f ->
                  Queue.add f initialization_callbacks
              | Fetch_hook { scope; stage; callback = f } ->
                  callback :=
                    Disassembly.Callback.register_hook !callback
                      (Option.value ~default:full_range scope)
                      ~stage Fetch f
              | Decode_hook { scope; stage; callback = f } ->
                  callback :=
                    Disassembly.Callback.register_hook !callback
                      (Option.value ~default:full_range scope)
                      ~stage Decode f
              | Disasm_hook { scope; stage; callback = f } ->
                  callback :=
                    Disassembly.Callback.register_hook !callback
                      (Option.value ~default:full_range scope)
                      ~stage Disasm f
              | Rewrite_hook { scope; stage; callback = f } ->
                  callback :=
                    Disassembly.Callback.register_hook !callback
                      (Option.value ~default:full_range scope)
                      ~stage Rewrite f
              | Instrumentation_routine f ->
                  callback :=
                    Disassembly.Callback.register_instrumentation !callback f
              | Grammar_extension g ->
                  grammar_extentions := g :: !grammar_extentions
              | Instruction_resolver f -> Queue.add f instruction_callbacks
              | Instruction_printer f -> Script.Ast.Instr.register_pp f
              | Command_handler f ->
                  declaration_callbacks := f :: !declaration_callbacks
              | Command_printer f -> Script.register_pp f
              | Builtin_resolver f ->
                  Compiler.register_builtin_callback config f
              | Builtin_may_read f ->
                  callback :=
                    Disassembly.Callback.register_knowledge !callback May_read f;
                  Compiler.register_knowledge config May_read f
              | Builtin_may_write f ->
                  Compiler.register_knowledge config May_write f
              | Builtin_must_write f ->
                  callback :=
                    Disassembly.Callback.register_knowledge !callback Must_write
                      f
              | Builtin_printer f -> Ir.register_builtin_printer f
              | Fork_callback f -> Queue.add f at_fork_callbacks
              | Signal_callback f -> Queue.add f at_signal_callbacks
              | Exit_callback f -> Queue.add f at_exit_callbacks)
            (P.extensions (module Engine)))
        Config.plugins;
      let script = Config.script !grammar_extentions in
      Logger.debug "@[<v>%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Script.pp)
        script;
      let path = Path.create () in
      Exploration_metrics.Paths.incr ();
      let cookie = Path.cookie path in
      Option.iter
        (fun f -> f cookie Config.smt_backend)
        (Path.State.more Symbolic.State.SetSMTSolver);
      Option.iter
        (fun t ->
          Option.iter
            (fun f -> f cookie t)
            (Path.State.more Symbolic.State.SetSMTSolverTimeout))
        Config.smt_timeout;
      Option.iter
        (fun p ->
          Option.iter
            (fun f -> f cookie p)
            (Path.State.more Symbolic.State.SetSMTDumpDir))
        Config.smt_dumpdir;
      Option.iter
        (fun f -> f cookie Config.smt_multichecks)
        (Path.State.more Symbolic.State.SetSMTSolverMultiChecks);
      Queue.iter (fun callback -> callback path) initialization_callbacks;
      let rec resolve_instruction :
          (Script.Instr.t -> Script.env -> Ir.fallthrough list) Seq.t ->
          Script.Instr.t ->
          Script.env ->
          Ir.fallthrough list =
       fun handlers inst env ->
        match handlers () with
        | Nil -> raise (Invalid_argument "missing instruction callback")
        | Cons (h, handlers) -> (
            match h inst env with
            | [] -> resolve_instruction handlers inst env
            | list -> list)
      in
      let eval : Path.t -> Ir.fallthrough -> unit =
       fun path -> function
        | Nop | Instruction _ | Hook _ | Forget _ | Goto _ -> ()
        | Assign { var; rval } -> Path.assign path var rval
        | Clobber var -> Path.clobber path var
        | Load { var; base; dir; addr } -> Path.load path var base dir ~addr
        | Store { base; dir; addr; rval } -> Path.store path base rval dir ~addr
        | Symbolize var -> Path.symbolize path var
        | Assume test -> (
            match Path.assume path test with
            | exception Unknown ->
                report path Unresolved_formula;
                raise Initialization_failure
            | None ->
                Logger.warning
                  "@[<hov>Cut path %d (unsatisfiable assumption) %@ %a@]"
                  (Path.id path) pp_virtual_address (Path.pc path);
                report path Unsatisfiable_assumption;
                raise Initialization_failure
            | Some _ -> ())
        | Assert test -> (
            match split path test with
            | True -> ()
            | False ->
                Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
                  pp_virtual_address (Path.pc path) pp_model
                  (List.hd (Path.models path));
                report path Assertion_failure;
                raise Initialization_failure
            | Both (path, other) ->
                report other Assertion_failure;
                Logger.error "@[<v 2> Assertion failed %@ %a@ %a@]"
                  pp_virtual_address (Path.pc path) pp_model
                  (List.hd (Path.models other)))
        | Builtin builtin -> (
            match Compiler.resolve_builtin config builtin with
            | Unknown -> raise Initialization_failure
            | Apply f -> f path
            | Call f -> (
                match f path with
                | Return -> ()
                | Return_to _ | Call _ | Tail_call _ | Continue _ | Signal _
                | Fork _ | Merge _ ->
                    raise Initialization_failure))
      in
      let eval : Path.t -> Ir.stmt -> unit =
       fun path -> function
        | Nop -> ()
        | Opcode op -> eval path op
        | Label _ | If _ | Goto _ | End _ ->
            raise (Invalid_argument "initialization")
      in
      let blit : Path.t -> Image.buffer Zmap.t -> unit =
       fun path content ->
        Zmap.iter
          (fun (Item { lo; hi; elt }) ->
            let addr = Path.Value.constant (Bitvector.create lo wordsize) in
            match (elt : Image.buffer) with
            | Zero ->
                Logger.debug ~level:5 "zero [%a .. %a]" Virtual_address.pp
                  (Virtual_address.of_bigint lo)
                  Virtual_address.pp
                  (Virtual_address.of_bigint hi);
                Path.memcpy_v path None ~addr
                  (Z.to_int (Z.sub hi lo) + 1)
                  (* TODO fragile may raise Z.Overflow *)
                  (Bigarray.Array1.create Bigarray.int8_unsigned C_layout 0)
            | Data { offset; len; value } ->
                Logger.debug ~level:5 "blit [%a .. %a]" Virtual_address.pp
                  (Virtual_address.of_bigint lo)
                  Virtual_address.pp
                  (Virtual_address.of_bigint hi);
                Path.memcpy_v path None ~addr len
                  (Bigarray.Array1.sub value offset len))
          content
      in
      let memcpy : Path.t -> lo:Z.t -> hi:Z.t -> Image.buffer Zmap.t -> unit =
       fun path ~lo ~hi content ->
        let view = Zmap.singleton ~lo ~hi () in
        Logger.debug "memcpy [%a .. %a]" Virtual_address.pp
          (Virtual_address.of_bigint lo)
          Virtual_address.pp
          (Virtual_address.of_bigint hi);
        let data =
          Zmap.substract ~crop:Image.crop_buffer content
            (Zmap.substract full_mask view)
        in
        if not (Zmap.is_empty (Zmap.substract view data)) then
          Logger.fatal "some addresses in [%a .. %a] do not belong to file."
            Virtual_address.pp
            (Virtual_address.of_bigint lo)
            Virtual_address.pp
            (Virtual_address.of_bigint hi);
        blit path data
      in
      List.iter
        (function
          | Script.Starting_from (saddr, prehook) ->
              let addr =
                Script.eval_expr ~size:parser_env.wordsize saddr parser_env
              in
              if Path.is_symbolic path addr then
                Logger.fatal
                  "the entrypoint address %a does not resolve to a unique value"
                  Script.Expr.pp (fst saddr)
              else
                let bv = Path.eval path addr in
                Logger.debug ~level:40
                  "the entrypoint address %a resolves to %a"
                  Dba_printer.Ascii.pp_bl_term addr Bitvector.pp_hex_or_bin bv;
                start :=
                  Script.eval_block prehook parser_env
                    (module Isa)
                    (resolve_instruction (Queue.to_seq instruction_callbacks));
                entry := Virtual_address.of_bitvector bv
          | Script.Starting_from_core prehook ->
              let entrypoint, initializations =
                Isa.core
                  (match Config.img with
                  | ELF img -> img
                  | _ -> Logger.fatal "Binary is not an ELF file.")
              in
              Logger.debug "Core entrypoint: %a" Virtual_address.pp entrypoint;
              List.iter
                (fun (var, value) ->
                  Logger.debug "Core value %s: %a" var.Dba.Var.name
                    Dba_printer.Ascii.pp_expr value;
                  Path.assign path var value)
                initializations;
              blit path image.content;
              start :=
                Script.eval_block prehook parser_env
                  (module Isa)
                  (resolve_instruction (Queue.to_seq instruction_callbacks));
              entry := entrypoint
          | Script.Load_sections names ->
              List.iter
                (fun name ->
                  try
                    let lo = query_symbol name Value
                    and size = query_symbol name Size in
                    Logger.info "Load section %s (%a, %a)" name
                      Bitvector.pp_hex_or_bin
                      (Bitvector.create lo wordsize)
                      Z.pp_print size;
                    memcpy path ~lo ~hi:(Z.add lo (Z.pred size)) image.content
                  with
                  | Unresolved (name', _) as exn when String.equal name name' ->
                    (match Config.missing_symbols with
                    | Error -> Logger.fatal ~e:exn
                    | Warn -> Logger.warning ?level:None
                    | Quiet -> fun _ _ -> ())
                      "Can not load the section %S from the file." name)
                names
          | Script.Load_data (load, _) -> (
              match load with
              | Load (len, _, saddr, None) ->
                  let addr =
                    Script.eval_expr ~size:parser_env.wordsize saddr parser_env
                  in
                  if Path.is_symbolic path addr then
                    Logger.fatal
                      "the memory initializer address %a does not resolve to a \
                       unique value"
                      Script.Expr.pp (fst saddr)
                  else
                    let bv = Path.eval path addr in
                    Logger.debug ~level:40
                      "the memory initializer address %a resolves to %a"
                      Dba_printer.Ascii.pp_bl_term addr Bitvector.pp bv;
                    let lo = Bitvector.value_of bv in
                    let hi = Z.add lo (Z.of_int (len - 1)) in
                    memcpy path ~lo ~hi image.content
              | _ -> assert false)
          | Script.Concretize_stack_pointer ->
              let sp, value = Isa.get_stack_pointer () in
              Path.assign path sp (Dba.Expr.constant value)
          | Script.Import_symbols (names, file) ->
              List.iter
                (fun ((name, attr), _) ->
                  match
                    List.find_map
                      (fun (value, source) ->
                        if String.ends_with ~suffix:file source then
                          let tag =
                            Dba.Var.Tag.Symbol
                              ( attr,
                                Lazy.from_val (Bitvector.create value wordsize)
                              )
                          in
                          Some (attr, Dba.Expr.var ~tag name wordsize)
                        else None)
                      (AttrMap.find attr (StrMap.find name image.symbols))
                  with
                  | (exception Not_found) | None ->
                      Logger.fatal "unable to import <%s%a> from %s" name
                        Dba.Var.Tag.Attribute.pp attr file
                  | Some value -> StrTbl.add symbols name value)
                names
          | Script.Hook (addresses, stmts, pre) ->
              List.iter
                (fun saddr ->
                  let anchor =
                    Format.asprintf "%a" Script.Expr.pp (fst saddr)
                  in
                  Logger.debug ~level:10 "@[<v 2> replace address %s by@ %a@]"
                    anchor Script.pp_stmts stmts;
                  try
                    let addr =
                      Script.eval_expr ~size:parser_env.wordsize saddr
                        parser_env
                    in
                    if Path.is_symbolic path addr then
                      Logger.fatal
                        "the stub address %s does not resolve to a unique value"
                        anchor
                    else
                      let bv = Path.eval path addr in
                      Logger.debug ~level:40
                        "the stub address %a resolves to %a"
                        Dba_printer.Ascii.pp_bl_term addr
                        Bitvector.pp_hex_or_bin bv;
                      let addr = Virtual_address.of_bitvector bv in
                      Virtual_address.Htbl.add hook_anchors addr anchor;
                      let stmts =
                        Script.eval_block stmts parser_env
                          (module Isa)
                          (resolve_instruction
                             (Queue.to_seq instruction_callbacks))
                      in
                      let eoh : Ir.terminator option =
                        if pre then None
                        else Some (Die "Invalid replacement fallthrough")
                      in
                      let ir =
                        Ir.Graph.of_script addr ("hook at " ^ anchor) stmts ?eoh
                      in
                      callback :=
                        Disassembly.Callback.register_hook !callback
                          { lo = addr; hi = addr }
                          ~stage:(if pre then Early else Late)
                          Fetch (Fun.const (Some ir))
                  with
                  | Unresolved (name, attr) as exn
                  when same_symbol name attr saddr
                  -> (
                    match Config.missing_symbols with
                    | Error -> raise exn
                    | Warn ->
                        Logger.warning
                          "@[<v>Can not resolve symbol <%s%a>.@ Will ignore \
                           the following %a@]"
                          name Dba.Var.Tag.Attribute.pp attr Parse_utils.pp_pos
                          (snd saddr)
                    | Quiet -> ()))
                addresses
          | Script.Return_hook (((name, _), pos), stmts) -> (
              try
                let addr = query_symbol name Value
                and size = query_symbol name Size in
                let last = Z.pred (Z.add addr size) in
                let lo = Virtual_address.of_bigint addr
                and hi = Virtual_address.of_bigint last in
                Logger.debug ~level:5 "Return hook for <%s> [%a..%a]" name
                  Virtual_address.pp lo Virtual_address.pp hi;
                let stmts =
                  Script.eval_block stmts parser_env
                    (module Isa)
                    (resolve_instruction (Queue.to_seq instruction_callbacks))
                in
                let anchor = Format.sprintf "<%s> return" name in
                callback :=
                  Disassembly.Callback.register_hook !callback { lo; hi } Disasm
                    (fun inst ->
                      let addr = Instruction.address inst
                      and hunk = Instruction.hunk inst in
                      if
                        List.exists
                          (fun i ->
                            match Dhunk.inst_exn hunk i with
                            | DJump (_, Return) | SJump (_, Return) -> true
                            | Assign _ | SJump _ | DJump _ | If _ | Stop _
                            | Assert _ | Assume _ | Nondet _ | Undef _ ->
                                false)
                          (Dhunk.exits hunk)
                      then (
                        Virtual_address.Htbl.add hook_anchors addr anchor;
                        Some
                          (Ir.Graph.of_script addr ("hook for " ^ anchor) stmts))
                      else None)
              with Unresolved _ ->
                Logger.fatal
                  "Can not resolve symbol return instruction(s) for <%s> %a."
                  name Parse_utils.pp_pos pos)
          | Script.Decode (opcode, stmts) ->
              let stmts =
                Script.eval_block stmts parser_env
                  (module Isa)
                  (resolve_instruction (Queue.to_seq instruction_callbacks))
              in
              callback :=
                Disassembly.Callback.register_hook !callback full_range Decode
                  (fun addr reader ->
                    if
                      Binstream.fold
                        (fun byte eq ->
                          eq && Uint8.to_int (Reader.Read.u8 reader) = byte)
                        opcode true
                    then
                      let ir =
                        Ir.Graph.of_script addr
                          (Format.asprintf "hook for opcode %a" Binstream.pp
                             opcode)
                          stmts
                          ~eoh:
                            (Goto
                               {
                                 target =
                                   Virtual_address.add_int
                                     (Binstream.length opcode) addr;
                                 tag = Default;
                               })
                      in
                      Some ir
                    else None)
          | Script.Init init ->
              List.iter (eval path)
                (Script.eval_block init parser_env
                   (module Isa)
                   (resolve_instruction (Queue.to_seq instruction_callbacks)))
          | Script.Explore_all -> IntTbl.add tasks (IntTbl.length tasks) ()
          | decl -> resolve_decl decl parser_env path !declaration_callbacks)
        script;
      path
    in
    Compiler.set_annotation_printer config
      (Some
         (fun ppf vaddr ->
           match rev_symbol vaddr with
           | exception Not_found -> ()
           | { base; name; _ } ->
               Format.pp_print_string ppf "\t# <";
               Format.pp_print_string ppf name;
               Format.pp_print_char ppf '>';
               let offset = Virtual_address.diff vaddr base in
               if offset > 0 then Format.fprintf ppf " + %#x" offset));
    let map :
        lo:Z.t ->
        hi:Z.t ->
        Image.t ->
        Path.t Disassembly.t option Zmap.t ->
        Path.t Disassembly.t option Zmap.t =
     fun ~lo ~hi image code ->
      let base = Virtual_address.of_bigint lo and size = Z.succ (Z.sub hi lo) in
      Zmap.union_left
        (Zmap.singleton ~lo ~hi
           (Some
              (Disassembly.create !callback ~decoder base
                 (Image.content_reader base size ~endianness image.content)
                 size config)))
        code
    in
    env.code <-
      Zmap.fold
        (fun (Item { lo; hi; elt } : Image.protection Zmap.item) code ->
          match elt with
          | RX -> map ~lo ~hi image code
          | RWX when Config.transient_enum = 0 ->
              Logger.warning
                "Addresses [%a..%a] have both Write and Execute flags.@ \
                 Self-modifying code is disabled and writes will be ignored.@ \
                 Use '-sse-self-written-enum N' to enable symbolic reasoning \
                 up to 'N - 1' forks."
                Virtual_address.pp
                (Virtual_address.of_bigint lo)
                Virtual_address.pp
                (Virtual_address.of_bigint hi);
              map ~lo ~hi image code
          | RWX -> Zmap.union_left (Zmap.singleton ~lo ~hi None) code
          | R | RW -> code)
        Zmap.empty image.protection;
    (!entry, !start, path)

  let start () =
    let filename = Config.filename in
    Logger.debug "Running SSE on %s" filename;
    Exploration_metrics.Timer.start ();
    let entry, stmts, path =
      try initialize_state ()
      with e ->
        halt ();
        Logger.fatal ~e "Unable to resolve the initial state"
    in
    if IntTbl.length tasks = 0 then
      Logger.warning "Nothing to reach: halting..."
    else
      let fiber =
        match stmts with
        | [] -> Goto entry
        | stmts ->
            let ir =
              Ir.Graph.of_script entry "Initialization" stmts
                ~eoh:(Goto { target = entry; tag = Default })
            in
            let code = Compiler.create config (ir :> Ir.View.t) in
            Compiler.get code 0
      in
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
          Config.timeout;
        Interpreter.dispatch path fiber (module Default)
      with
      | Halt | Sys.Break -> halt ()
      | err ->
          halt ();
          raise err

  let unit =
    try start ()
    with err -> (
      match Printexc.use_printers err with
      | None -> raise err
      | Some msg -> Logger.fatal "%s" msg)
end
