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
open Script
module S = Basic_types.String
module B = Bitvector.Collection

include Cli.Make (struct
  let name = "Constant time checker"
  let shortname = "checkct"
end)

type leak_info =
  | HaltLeak  (** Halts at first leak *)
  | InstrLeak
      (** Reports leaky instructions (each one is reported only once) *)

module LeakInfo = Builder.Variant_choice_assoc (struct
  type t = leak_info

  let name = "leak-info"

  let doc =
    "Select the information that is reported about leakage.\n"
    ^ "\t\t- halt: halts at first leak\n"
    ^ "\t\t- instr: reports leaky instructions (instructions are reported only \
       once)\n"

  let default = InstrLeak
  let assoc_map = [ ("halt", HaltLeak); ("instr", InstrLeak) ]
end)

module Taint = Builder.No (struct
  let name = "taint"
  let doc = "Disable taint analysis (prove that instruction can not leak)"
end)

module ChosenValues = Builder.No (struct
  let name = "cv"
  let doc = "Disable chosen value sampling (prove that instruction may leak)"
end)

module Relse = Builder.No (struct
  let name = "relse"
  let doc = "Disable relational symbolic engine to answer security queries"
end)

module StatsFile = Builder.String_option (struct
  let name = "stats-file"
  let doc = "set file for dumping staistics"
end)

module Kind = struct
  type t = Control_flow | Memory_access

  let to_string = function
    | Control_flow -> "control flow"
    | Memory_access -> "memory access"

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Report = struct
  type t = {
    memory : Bitvector.t B.Map.t S.Map.t;
    public : Bitvector.t list S.Map.t;
    secret1 : Bitvector.t list S.Map.t;
    secret2 : Bitvector.t list S.Map.t;
  }

  let toml t =
    let open Toml in
    let toml_variable_vals m =
      Min.of_key_values
        (S.Map.fold
           (fun name vals res ->
             let toml_vals =
               Types.NodeString (List.map Bitvector.to_string vals)
             in
             (Min.key name, Types.TArray toml_vals) :: res)
           m [])
    and toml_array_vals m =
      Min.of_key_values
        (B.Map.fold
           (fun addr value res ->
             ( Min.key (Bitvector.to_string addr),
               Types.TString (Bitvector.to_string value) )
             :: res)
           m [])
    in
    let toml_memory_vals m =
      Min.of_key_values
        (S.Map.fold
           (fun name vals res ->
             (Min.key name, Types.TTable (toml_array_vals vals)) :: res)
           m [])
    in
    Min.of_key_values
      [
        (Min.key "memory", Types.TTable (toml_memory_vals t.memory));
        (Min.key "public", Types.TTable (toml_variable_vals t.public));
        (Min.key "secret1", Types.TTable (toml_variable_vals t.secret1));
        (Min.key "secret2", Types.TTable (toml_variable_vals t.secret2));
      ]

  let pp ppf t =
    let pp_category ppf vars =
      S.Map.iter
        (fun name list ->
          Format.fprintf ppf "@[<h>  %s : @[<hov>%a@]@]@," name
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Bitvector.pp_hex_or_bin)
            list)
        vars
    and pp_array ppf array =
      B.Map.iter
        (fun addr value ->
          Format.fprintf ppf "@[<h>  %a : %a@]@," Bitvector.pp_hex_or_bin addr
            Bitvector.pp_hex_or_bin value)
        array
    in
    let pp_memory ppf memory =
      S.Map.iter
        (fun name values ->
          Format.fprintf ppf "@[<v 1>%s :@,%a@]@," name pp_array values)
        memory
    in
    Format.fprintf ppf
      "@[<v 0>%a@[<v 1>public :@,\
       %a@]@,\
       @[<v 1>secret1 :@,\
       %a@]@,\
       @[<v 1>secret2 :@,\
       %a@]@]"
      pp_memory t.memory pp_category t.public pp_category t.secret1 pp_category
      t.secret2
end

module Status = struct
  type t = Secure | Insecure of Report.t | Unknown

  let to_string = function
    | Secure -> "secure"
    | Insecure _ -> "insecure"
    | Unknown -> "unknown"

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type Ast.Obj.t += Bool of bool
type Ast.Instr.t += Secret of Ast.Loc.t Ast.loc
type Ast.t += Globals of bool * string list
type builtin += Mirror of Dba.Var.t | Check of Dba.Expr.t * Kind.t

module type OPTIONS = sig
  val leak_info : leak_info
  val taint : bool
  val cv : bool
  val relse : bool
  val stats_file : string option
end

let make_options : unit -> (module OPTIONS) =
 fun () : (module OPTIONS) ->
  (module struct
    let leak_info = LeakInfo.get ()
    let taint = Taint.get ()
    let cv = ChosenValues.get ()
    let relse = Relse.get ()
    let stats_file = StatsFile.get_opt ()
  end)

module Ct_state = struct
  open Sexpr

  type load = ([ `Mem ], string, Memory.t) Term.t

  type t = {
    mutable constraints : Expr.t list;
    mutable conjunction : Expr.t;
    secrets : Expr.t BvTbl.t;
    mirror_e : Expr.t BvTbl.t;
    mirror_m : Memory.t AxTbl.t;
    mutable loads : load list;
    roots : Memory.t AxTbl.t;
    mutable models : (int * Model.t) list;
  }

  let empty () =
    let addr_space = Kernel_options.Machine.word_size () in
    {
      constraints = [];
      conjunction = Expr.one;
      secrets = BvTbl.create 8;
      mirror_e = BvTbl.create 64;
      mirror_m = AxTbl.create 16;
      loads = [];
      roots = AxTbl.create 16;
      models = List.init 4 (fun i -> (i, Model.empty addr_space));
    }

  let fork t =
    {
      t with
      mirror_e = BvTbl.copy t.mirror_e;
      mirror_m = AxTbl.copy t.mirror_m;
      secrets = BvTbl.copy t.secrets;
      roots = AxTbl.copy t.roots;
      models =
        List.map
          (fun (i, (_, _, _, _, addr_space)) -> (i, Model.empty addr_space))
          t.models;
    }

  let mirror (e : Expr.t) t =
    match e with
    | Var { name; size; label; _ } ->
        let e' = Expr.var ("mirror_" ^ name) size label in
        BvTbl.add t.mirror_e e e';
        BvTbl.add t.secrets e' e
    | _ -> raise_notrace (Invalid_argument "mirror")

  let rec is_tainted (e : Expr.t) t =
    try e != BvTbl.find t.mirror_e e
    with Not_found ->
      (match e with
      | Cst _ -> false
      | Var _ -> false
      | Load { label; _ } ->
          t.loads <- Term.to_mem_exn e :: t.loads;
          is_tainted_memory label t
      | Unary { x; _ } -> is_tainted x t
      | Binary { x; y; _ } -> is_tainted x t || is_tainted y t
      | Ite { c; t = x; e = y; _ } ->
          is_tainted c t || is_tainted x t || is_tainted y t)
      ||
      (BvTbl.add t.mirror_e e e;
       false)

  and is_tainted_memory (m : Memory.t) t =
    try not (Memory.equal m (AxTbl.find t.mirror_m m))
    with Not_found -> (
      match m with Root | Symbol _ -> false | Layer _ -> true)

  let rec make_mirror_e (e : Expr.t) t =
    try BvTbl.find t.mirror_e e
    with Not_found ->
      let e' =
        match e with
        | Cst _ -> e
        | Var _ -> e
        | Load { label; len; dir; addr; _ } ->
            t.loads <- Term.to_mem_exn e :: t.loads;
            let label' = make_mirror_m label t in
            let addr' = make_mirror_e addr t in
            if (not (Memory.equal label label')) || addr != addr' then
              Expr.load len dir addr' label'
            else e
        | Unary { x; f; _ } ->
            let x' = make_mirror_e x t in
            if x != x' then Expr.unary f x' else e
        | Binary { x; y; f; _ } ->
            let x' = make_mirror_e x t and y' = make_mirror_e y t in
            if x != x' || y != y' then Expr.binary f x' y' else e
        | Expr.Ite { c; t = x; e = y; _ } ->
            let c' = make_mirror_e c t
            and x' = make_mirror_e x t
            and y' = make_mirror_e y t in
            if c != c' || x != x' || y != y' then Expr.ite c' x' y' else e
      in
      BvTbl.add t.mirror_e e e';
      e'

  and make_mirror_m (m : Memory.t) t =
    try AxTbl.find t.mirror_m m
    with Not_found ->
      let m' =
        match m with
        | Root | Symbol _ ->
            AxTbl.add t.roots m m;
            m
        | Layer { over; addr; store; _ } ->
            let size = Expr.sizeof addr
            and addr' = make_mirror_e addr t
            and over' = make_mirror_m over t in
            AxTbl.add t.roots m (AxTbl.find t.roots over);
            let store', dirty =
              Store.fold
                (fun offset chunk (store', dirty) ->
                  let offset' = Bitvector.create offset size in
                  if Chunk.is_hunk chunk then
                    (Store.store offset' chunk store', dirty)
                  else
                    let value = Chunk.to_term chunk in
                    let value' = make_mirror_e value t in
                    ( Store.store offset' (Chunk.of_term value') store',
                      dirty || value != value' ))
                (Store.empty, false) store
            in
            if addr != addr' || dirty || not (Memory.equal over over') then
              Memory.layer addr' store' over'
            else m
      in
      AxTbl.add t.mirror_m m m';
      m'

  let make_context constraints t =
    let rec visit constraints t conjunction =
      if constraints == t.constraints then conjunction
      else
        match constraints with
        | [] -> assert false
        | e :: constraints ->
            let e' = make_mirror_e e t in
            visit constraints t
              (if e != e' then Expr.logand e' conjunction else conjunction)
    in
    t.conjunction <- visit constraints t t.conjunction;
    t.constraints <- constraints

  let rec find_root array t =
    try AxTbl.find t.roots array
    with Not_found ->
      let root =
        match array with
        | Root | Symbol _ -> array
        | Layer { over; _ } -> find_root over t
      in
      AxTbl.add t.roots array root;
      root
end

module Make
    (Options : OPTIONS)
    (Stats : EXPLORATION_STATISTICS)
    (Path : Path.S)
    (State : STATE with type Value.t = Sexpr.Expr.t) :
  Exec.EXTENSION with type path = Path.t and type state = State.t = struct
  type path = Path.t
  and state = State.t

  let key = Path.register_key (Ct_state.empty ())

  let () =
    Path.register_at_fork (fun path path' ->
        Path.set key (Ct_state.fork (Path.get key path)) path')

  module Eval = Eval.Make (Path) (State)

  let ct_cf_secure = ref 0
  and ct_cf_insecure = ref 0
  and ct_cf_unknown = ref 0
  and ct_mem_secure = ref 0
  and ct_mem_insecure = ref 0
  and ct_mem_unknown = ref 0

  let ct_status (kind : Kind.t) (status : Status.t) =
    match (kind, status) with
    | Control_flow, Secure -> ct_cf_secure
    | Control_flow, Insecure _ -> ct_cf_insecure
    | Control_flow, Unknown -> ct_cf_unknown
    | Memory_access, Secure -> ct_mem_secure
    | Memory_access, Insecure _ -> ct_mem_insecure
    | Memory_access, Unknown -> ct_mem_unknown

  let ct_addr_status : Status.t Virtual_address.Htbl.t =
    Virtual_address.Htbl.create 128

  let add_addr_status addr status =
    Virtual_address.Htbl.replace ct_addr_status addr status

  let is_addr_insecure addr =
    match Virtual_address.Htbl.find ct_addr_status addr with
    | Insecure _ -> true
    | (exception Not_found) | Secure | Unknown -> false

  let addr_status_report () =
    let insecure, unknown =
      Virtual_address.Htbl.fold
        (fun addr (status : Status.t) (insecure, unknown) ->
          match status with
          | Insecure _ -> (addr :: insecure, unknown)
          | Unknown -> (insecure, addr :: unknown)
          | Secure -> assert false)
        ct_addr_status ([], [])
    in
    ( List.sort Virtual_address.compare insecure,
      List.sort Virtual_address.compare unknown )

  let toml_ct_report () =
    let open Toml in
    let l_insecure, l_unknown = addr_status_report () in
    let ls_insecure, ls_unknown =
      ( List.map Virtual_address.to_string l_insecure,
        List.map Virtual_address.to_string l_unknown )
    in
    let instructions_status =
      Min.of_key_values
        [
          (Min.key "insecure", Types.TArray (Types.NodeString ls_insecure));
          (Min.key "unknown", Types.TArray (Types.NodeString ls_unknown));
        ]
    in
    let insecurity_models =
      Min.of_key_values
        (Virtual_address.Htbl.fold
           (fun vaddr (status : Status.t) l ->
             match status with
             | Insecure model ->
                 let toml_model = Report.toml model in
                 ( Min.key (Virtual_address.to_string vaddr),
                   Types.TTable toml_model )
                 :: l
             | Unknown -> l
             | _ -> assert false)
           ct_addr_status [])
    in
    Min.of_key_values
      [
        (Min.key "Instructions status", Types.TTable instructions_status);
        (Min.key "Insecurity models", Types.TTable insecurity_models);
      ]

  let is_unknown_report () =
    Stats.get_pending_paths () > 0
    || !ct_cf_unknown + !ct_mem_unknown > 0
    || Stats.get_status Non_executable_code > 0
    || Stats.get_status Max_depth > 0
    || Stats.get_status Enumeration_limit > 0
    || Stats.get_status Unresolved_formula > 0
    || Stats.get_status Die > 0

  let initialization_callback = None

  let mirror var _ path _ state : (State.t, status) Result.t =
    try
      Ct_state.mirror (State.lookup var state) (Path.get key path);
      Ok state
    with Invalid_argument _ -> Error Die

  let declaration_callback =
    let lookup_symbol (env : env) name attr =
      match env.lookup_symbol name attr with
      | Var { info = Symbol (_, (lazy bv)); _ } -> bv
      | _ -> assert false
    in
    Some
      (fun decl env path state ->
        match decl with
        | Globals (secret, names) ->
            Some
              (List.fold_left
                 (fun state name ->
                   let addr = lookup_symbol env name Value
                   and bytesize =
                     Size.Byte.create
                       (Bitvector.to_uint (lookup_symbol env name Size))
                   in
                   let bitsize = Size.Byte.to_bitsize bytesize in
                   let var = Dba.Var.create name ~bitsize ~tag:Empty in
                   env.define var Lexing.dummy_pos;
                   let state = Eval.fresh var state path in
                   let value = State.lookup var state in
                   let state =
                     State.write
                       ~addr:(State.Value.constant addr)
                       value LittleEndian state
                   in
                   if secret then
                     Result.get_ok
                       (mirror var Virtual_address.zero path 0 state)
                   else state)
                 state names)
        | _ -> None)

  let instruction_callback =
    let secret = Printf.sprintf "%%secret%%%d" in
    Some
      (fun inst env ->
        match inst with
        | Secret lval -> (
            match Script.eval_loc lval env with
            | Var var -> [ Symbolize var; Builtin (Mirror var) ]
            | Restrict (var, { hi; lo }) ->
                let size' = hi - lo + 1 in
                let name' = secret size' in
                let var' = Dba.Var.temporary name' (Size.Bit.create size') in
                let rval =
                  Dba_utils.Expr.complement (Dba.Expr.v var') ~lo ~hi var
                in
                [ Symbolize var'; Builtin (Mirror var'); Assign { var; rval } ]
            | Store (bytes, dir, addr, base) ->
                let size' = 8 * bytes in
                let name' = secret size' in
                let var' = Dba.Var.temporary name' (Size.Bit.create size') in
                let rval = Dba.Expr.v var' in
                [
                  Symbolize var';
                  Builtin (Mirror var');
                  Store { base; dir; addr; rval };
                ])
        | _ -> [])

  let process_handler : type a. (module Ir.GRAPH with type t = a) -> a -> unit =
   fun graph ->
    let module G = (val graph) in
    fun graph ->
      G.iter_new_vertex
        (fun vertex ->
          match G.node graph vertex with
          | Fallthrough { kind = Load { addr; _ } | Store { addr; _ }; _ } ->
              ignore
                (G.insert_before graph vertex
                   (Builtin (Check (addr, Memory_access))))
          | Branch { test = expr; _ } | Terminator (Jump { target = expr; _ })
            ->
              ignore
                (G.insert_before graph vertex
                   (Builtin (Check (expr, Control_flow))))
          | _ -> ())
        graph

  let process_callback = Some process_handler

  let taint_analysis e ct_state _ _ : Status.t =
    if Ct_state.is_tainted e ct_state then Unknown else Secure

  let cv_symbol (ct_state : Ct_state.t) (state : State.t) i e =
    let open Sexpr in
    if BvTbl.mem ct_state.secrets e then
      match i with
      | 0 -> Bv.zeros (Expr.sizeof e)
      | 1 -> Bv.ones (Expr.sizeof e)
      | 2 -> Bv.fill (Expr.sizeof e)
      | _ -> Bv.rand (Expr.sizeof e)
    else State.get_a_value e state

  and cv_memory state array addr =
    let open Sexpr in
    Bv.to_char
      (State.get_a_value
         (Expr.load 1 LittleEndian (Expr.constant addr) array)
         state)

  let extract_cv_report (ct_state : Ct_state.t) symbols state
      ((_, _, main, arrays, addr_size) as model) : Report.t =
    let open Sexpr in
    let memory =
      StTbl.fold
        (fun array bytes memory ->
          S.Map.add array
            (BiTbl.fold
               (fun addr byte values ->
                 B.Map.add (Bv.create addr addr_size) (Bv.of_char byte) values)
               bytes B.Map.empty)
            memory)
        arrays S.Map.empty
    in
    let memory =
      if BiTbl.length main > 0 then
        S.Map.add "@"
          (BiTbl.fold
             (fun addr byte values ->
               B.Map.add (Bv.create addr addr_size) (Bv.of_char byte) values)
             main B.Map.empty)
          memory
      else memory
    in
    let public, secret1, secret2 =
      S.Map.fold
        (fun name values (public, secret1, secret2) ->
          let vpublic, vsecret1, vsecret2 =
            List.fold_left
              (fun (public, secret1, secret2) value ->
                let value' =
                  try BvTbl.find ct_state.mirror_e value
                  with Not_found -> value
                in
                if value != value' then
                  ( public,
                    State.get_a_value value state :: secret1,
                    Model.eval model value' :: secret2 )
                else (State.get_a_value value state :: public, secret1, secret2))
              ([], [], []) values
          in
          ( (if vpublic <> [] then S.Map.add name vpublic public else public),
            (if vsecret1 <> [] then S.Map.add name vsecret1 secret1 else secret1),
            if vsecret2 <> [] then S.Map.add name vsecret2 secret2 else secret2
          ))
        symbols
        (S.Map.empty, S.Map.empty, S.Map.empty)
    in
    { memory; public; secret1; secret2 }

  let cv_analysis e (ct_state : Ct_state.t) symbols state : Status.t =
    let open Sexpr in
    match ct_state.models with
    | [] -> Unknown
    | models -> (
        Ct_state.make_context (State.assertions state) ct_state;
        let e' = Ct_state.make_mirror_e e ct_state in
        if e == e' then Secure
        else
          let memory = cv_memory state in
          ct_state.models <-
            List.fold_left
              (fun models (i, model) ->
                let symbols = cv_symbol ct_state state i in
                if
                  Bv.is_one
                    (Model.eval ~symbols ~memory model ct_state.conjunction)
                then (i, model) :: models
                else models)
              [] models;
          let value = State.get_a_value e state in
          match
            List.find
              (fun (i, model) ->
                Bv.diff value
                  (Model.eval
                     ~symbols:(cv_symbol ct_state state i)
                     ~memory model e'))
              ct_state.models
          with
          | exception Not_found -> Unknown
          | _, model ->
              Insecure (extract_cv_report ct_state symbols state model))

  let extract_relse_report (ct_state : Ct_state.t) symbols state : Report.t =
    let open Sexpr in
    let memory =
      List.fold_left
        (fun memory (Load { label; len; dir; addr; _ } : Ct_state.load) ->
          let array = Ct_state.find_root label ct_state in
          let name =
            match array with
            | Root -> "@"
            | Symbol name -> name
            | Layer _ -> assert false
          in
          let base = State.get_a_value addr state in
          let bytes =
            State.get_a_value
              (Expr.load len dir (Expr.constant base) array)
              state
          in
          let values =
            try S.Map.find name memory with Not_found -> B.Map.empty
          in
          S.Map.add name (B.Map.add base bytes values) memory)
        S.Map.empty ct_state.loads
    in
    let secret, public =
      S.Map.partition
        (fun _ values ->
          let value = List.hd values in
          try value != BvTbl.find ct_state.mirror_e value
          with Not_found -> false)
        symbols
    in
    let public =
      S.Map.map
        (fun values ->
          List.rev_map (fun value -> State.get_a_value value state) values)
        public
    and secret1 =
      S.Map.map
        (fun values ->
          List.rev_map (fun value -> State.get_a_value value state) values)
        secret
    and secret2 =
      S.Map.map
        (fun values ->
          List.rev_map
            (fun value ->
              State.get_a_value (BvTbl.find ct_state.mirror_e value) state)
            values)
        secret
    in
    { memory; public; secret1; secret2 }

  let relse_analysis e ct_state symbols state : Status.t =
    let e' = Ct_state.make_mirror_e e ct_state in
    if e == e' then Secure
    else (
      Ct_state.make_context (State.assertions state) ct_state;
      match
        State.assume Sexpr.Expr.(logand (diff e e') ct_state.conjunction) state
      with
      | exception Unknown -> Unknown
      | None -> Secure
      | Some state -> Insecure (extract_relse_report ct_state symbols state))

  let analyses =
    let analyses =
      if Options.relse then [ ("RelSE", relse_analysis) ] else []
    in
    let analyses =
      if Options.cv then ("chosen value", cv_analysis) :: analyses else analyses
    in
    let analyses =
      if Options.taint then ("taint", taint_analysis) :: analyses else analyses
    in
    analyses

  let analyze expr path state =
    let rec apply_analyses e ct_state symbols state analyses : Status.t =
      match analyses with
      | [] -> Unknown
      | (_, analysis) :: analyses ->
          let status : Status.t = analysis e ct_state symbols state in
          if status <> Unknown then status
          else apply_analyses e ct_state symbols state analyses
    in
    apply_analyses expr (Path.get key path)
      (Path.get State.symbols path)
      state analyses

  let check expr (kind : Kind.t) addr path _ state : (State.t, status) Result.t
      =
    if Options.leak_info = InstrLeak && is_addr_insecure addr then Ok state
    else
      let expr, state = Eval.safe_eval expr state path in
      let status : Status.t = analyze expr path state in
      incr (ct_status kind status);
      match status with
      | Secure -> Ok state
      | Insecure _ ->
          add_addr_status addr status;
          Logger.result "Instruction %a has %s leak (%.3fs)" Virtual_address.pp
            addr (Kind.to_string kind) (Stats.get_time ());
          if Options.leak_info = HaltLeak then Error Halt else Ok state
      | Unknown ->
          add_addr_status addr status;
          Ok state

  let builtin_callback =
    Some
      (function
      | Mirror var -> Some (mirror var)
      | Check (expr, kind) -> Some (check expr kind)
      | _ -> None)

  let builtin_printer =
    Some
      (fun ppf -> function
        | Mirror { name; _ } ->
            Format.fprintf ppf "ct secret mirror(%s)" name;
            true
        | Check (expr, kind) ->
            Format.fprintf ppf "ct %a check(%a)" Kind.pp kind
              Dba_printer.Ascii.pp_bl_term expr;
            true
        | _ -> false)

  let at_exit_callback =
    Some
      (fun () ->
        let l_insecure, _ = addr_status_report () in
        let status =
          if l_insecure <> [] then
            Virtual_address.Htbl.find ct_addr_status (List.hd l_insecure)
          else if is_unknown_report () then Unknown
          else Secure
        in
        Option.iter
          (fun filename ->
            let toml_data =
              Toml.Min.of_key_values
                [
                  ( Toml.Min.key "CT report",
                    Toml.Types.TTable (toml_ct_report ()) );
                  ( Toml.Min.key "Exploration",
                    Toml.Types.TTable (Stats.to_toml ()) );
                ]
            in
            let oc = open_out_bin filename in
            let ppf = Format.formatter_of_out_channel oc in
            Toml.Printer.table ppf toml_data;
            close_out oc)
          Options.stats_file;
        Logger.result "Program status is : %a (%0.3f)" Status.pp status
          (Stats.get_time ());
        if Options.leak_info = InstrLeak then (
          let paths = Stats.get_completed_paths ()
          and static_instrs = Stats.get_unique_insts () in
          Logger.info "%d visited path%s covering %d instruction%s" paths
            (if paths > 1 then "s" else "")
            static_instrs
            (if static_instrs > 1 then "s" else "");
          Logger.info "%d / %d control flow checks pass" !ct_cf_secure
            (!ct_cf_secure + !ct_cf_insecure + !ct_cf_unknown);
          Logger.info "%d / %d memory access checks pass" !ct_mem_secure
            (!ct_mem_secure + !ct_mem_insecure + !ct_mem_unknown));
        if is_unknown_report () then
          Logger.warning "@[<v>Exploration is incomplete:%a@]"
            (fun ppf () ->
              let pendings = Stats.get_pending_paths () in
              if pendings > 0 then
                Format.fprintf ppf
                  "@ - timeout has left (at least) %d pending paths \
                   (-sse-timeout)"
                  pendings;
              let non_exec = Stats.get_status Non_executable_code in
              if non_exec > 0 then
                Format.fprintf ppf
                  "@ - %d paths fell into non executable code segments" non_exec;
              let max_depth = Stats.get_status Max_depth in
              if max_depth > 0 then
                Format.fprintf ppf
                  "@ - %d paths have reached the maximal depth and have been \
                   cut (-sse-depth)"
                  max_depth;
              let incomplete_enum = Stats.get_status Enumeration_limit in
              if incomplete_enum > 0 then
                Format.fprintf ppf
                  "@ - some jump targets may have been omitted (-sse-jump-enum)";
              let unknown =
                Stats.get_status Unresolved_formula
                + !ct_cf_unknown + !ct_mem_unknown
              in
              if unknown > 0 then
                Format.fprintf ppf
                  "@ - %d SMT solver queries remain unsolved \
                   (-fml-solver-timeout)"
                  unknown)
            ())
end

let () =
  Exec.register_plugin
    (module struct
      let name = "checkct"

      let grammar_extension =
        [
          Dyp.Bind_to_cons
            [
              ("secret_or_public", "Obj");
              ("comma_separated_ident_rev_list", "Obj");
            ];
          Dyp.Add_rules
            [
              ( ( "secret_or_public",
                  [ Dyp.Regexp (RE_String "secret") ],
                  "default_priority",
                  [] ),
                fun _ _ -> (Libparser.Syntax.Obj (Bool true), []) );
              ( ( "secret_or_public",
                  [ Dyp.Regexp (RE_String "public") ],
                  "default_priority",
                  [] ),
                fun _ _ -> (Libparser.Syntax.Obj (Bool false), []) );
              ( ( "comma_separated_ident_rev_list",
                  [ Dyp.Non_ter ("ident", No_priority) ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [ Libparser.Syntax.String ident ] ->
                      (Libparser.Syntax.Obj (String_list [ ident ]), [])
                  | _ -> assert false );
              ( ( "comma_separated_ident_rev_list",
                  [
                    Dyp.Non_ter ("comma_separated_ident_rev_list", No_priority);
                    Dyp.Regexp (RE_Char ',');
                    Dyp.Non_ter ("ident", No_priority);
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [
                      Libparser.Syntax.Obj (String_list l);
                      _;
                      Libparser.Syntax.String ident;
                    ] ->
                      (Libparser.Syntax.Obj (String_list (ident :: l)), [])
                  | _ -> assert false );
              ( ( "decl",
                  [
                    Dyp.Non_ter ("secret_or_public", No_priority);
                    Dyp.Regexp (RE_String "global");
                    Dyp.Non_ter ("comma_separated_ident_rev_list", No_priority);
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [
                      Libparser.Syntax.Obj (Bool secret);
                      _;
                      Libparser.Syntax.Obj (String_list names);
                    ] ->
                      (Libparser.Syntax.(Decl (Globals (secret, names))), [])
                  | _ -> raise Dyp.Giveup );
              ( ( "fallthrough",
                  [
                    Dyp.Non_ter ("loc", No_priority);
                    Dyp.Regexp (RE_String ":=");
                    Dyp.Regexp (RE_String "secret");
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [ Libparser.Syntax.Loc lval; _; _ ] ->
                      (Libparser.Syntax.Instr (Secret lval), [])
                  | _ -> raise Dyp.Giveup );
              ( ( "instr",
                  [
                    Dyp.Non_ter ("loc", No_priority);
                    Dyp.Regexp (RE_String ":=");
                    Dyp.Regexp (RE_String "secret");
                    Dyp.Non_ter ("accept_newline", No_priority);
                    Dyp.Ter "AS";
                    Dyp.Non_ter ("ident", No_priority);
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [
                      Libparser.Syntax.Loc lval;
                      _;
                      _;
                      _;
                      _;
                      Libparser.Syntax.String name;
                    ] ->
                      let var =
                        ( Ast.Loc.var name ~size:(Ast.Size.sizeof lval),
                          Lexing.dummy_pos )
                      in
                      ( Libparser.Syntax.Stmt
                          [
                            Secret var;
                            Ast.Instr.assign lval
                              (Ast.Expr.loc var, Lexing.dummy_pos);
                          ],
                        [] )
                  | _ -> assert false );
            ];
        ]

      let instruction_printer =
        Some
          (fun ppf -> function
            | Secret (loc, _) ->
                Format.fprintf ppf "%a := secret" Ast.Loc.pp loc;
                true
            | _ -> false)

      let declaration_printer =
        Some
          (fun ppf -> function
            | Globals (secret, names) ->
                Format.fprintf ppf "%s global %a"
                  (if secret then "secret" else "public")
                  (Format.pp_print_list
                     ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                     Format.pp_print_string)
                  names;
                true
            | _ -> false)

      let extension :
          type a b.
          (module EXPLORATION_STATISTICS) ->
          (module Path.S with type t = a) ->
          (module STATE with type t = b) ->
          (module Exec.EXTENSION with type path = a and type state = b) option =
       fun stats path state ->
        match is_enabled () with
        | false -> None
        | true -> (
            let module State = (val state) in
            match State.Value.kind with
            | Senv.Term ->
                if Options.Logger.is_debug_enabled () then
                  Logger.set_log_level "debug";
                Logger.set_debug_level (Options.Logger.get_debug_level ());
                Some
                  (module Make ((val make_options ())) ((val stats))
                            ((val path))
                            (State))
            | _ ->
                Logger.fatal
                  "unable to use 'checkct' within the current symbolic engine")
    end : Exec.PLUGIN)
