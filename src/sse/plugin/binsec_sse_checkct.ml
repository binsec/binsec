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
open Script
module StrMap = Basic_types.String.Map
module BiTbl = Basic_types.Integers.Bigint.Htbl

module Logger = Logger.Sub (struct
  let name = "checkct"
end)

type leak_info =
  | HaltLeak  (** Halts at first leak *)
  | InstrLeak
      (** Reports leaky instructions (each one is reported only once) *)

module Kind = struct
  type t = Control_flow | Memory_access | Multiplication | Dividend | Divisor

  let to_string = function
    | Control_flow -> "control flow"
    | Memory_access -> "memory access"
    | Multiplication -> "multiplication"
    | Dividend -> "dividend"
    | Divisor -> "divisor"

  let of_string = function
    | "control-flow" -> Control_flow
    | "memory-access" -> Memory_access
    | "multiplication" -> Multiplication
    | "dividend" -> Dividend
    | "divisor" -> Divisor
    | _ -> raise (Invalid_argument "of_string")

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Report = struct
  type t = {
    memory : Bitvector.t Bitvector.Map.t StrMap.t;
    public : Bitvector.t list StrMap.t;
    secret1 : Bitvector.t list StrMap.t;
    secret2 : Bitvector.t list StrMap.t;
  }

  let toml t =
    let open Toml in
    let toml_variable_vals m =
      Min.of_key_values
        (StrMap.fold
           (fun name vals res ->
             let toml_vals =
               Types.NodeString (List.map Bitvector.to_string vals)
             in
             (Min.key name, Types.TArray toml_vals) :: res)
           m [])
    and toml_array_vals m =
      Min.of_key_values
        (Bitvector.Map.fold
           (fun addr value res ->
             ( Min.key (Bitvector.to_string addr),
               Types.TString (Bitvector.to_string value) )
             :: res)
           m [])
    in
    let toml_memory_vals m =
      Min.of_key_values
        (StrMap.fold
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
      StrMap.iter
        (fun name list ->
          Format.fprintf ppf "@[<h>  %s : @[<hov>%a@]@]@," name
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Bitvector.pp_hex_or_bin)
            list)
        vars
    and pp_array ppf array =
      Bitvector.Map.iter
        (fun addr value ->
          Format.fprintf ppf "@[<h>  %a : %a@]@," Bitvector.pp_hex_or_bin addr
            Bitvector.pp_hex_or_bin value)
        array
    in
    let pp_memory ppf memory =
      StrMap.iter
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
type Ast.Instr.t += Se_check
type Ast.t += Globals of bool * string list

module type OPTIONS = sig
  val leak_info : leak_info
  val taint : bool
  val cv : bool
  val relse : bool
  val stats_file : string option
  val check_branch : bool
  val check_memory : bool
  val check_mult : bool
  val check_dividend : bool
  val check_divisor : bool
end

let ( === ) = Symbolic.Default.Expr.is_equal
let ( /== ) e e' = not (e === e')

module Ct_state = struct
  open Symbolic.Default

  type load = ([ `Mem ], string, Memory.t) Term.t

  type t = {
    mutable constraints : Expr.t list;
    mutable conjunction : Expr.t;
    secrets : Expr.t Expr.Tbl.t;
    mirror_e : Expr.t Expr.Tbl.t;
    mirror_m : Memory.t Memory.Tbl.t;
    mutable loads : load list;
    roots : Memory.t Memory.Tbl.t;
    mutable models : (int * Model.t) list;
  }

  let empty () =
    {
      constraints = [];
      conjunction = Expr.one;
      secrets = Expr.Tbl.create 8;
      mirror_e = Expr.Tbl.create 64;
      mirror_m = Memory.Tbl.create 16;
      loads = [];
      roots = Memory.Tbl.create 16;
      models = List.init 4 (fun i -> (i, Model.empty ()));
    }

  let fork t =
    {
      t with
      mirror_e = Expr.Tbl.copy t.mirror_e;
      mirror_m = Memory.Tbl.copy t.mirror_m;
      secrets = Expr.Tbl.copy t.secrets;
      roots = Memory.Tbl.copy t.roots;
    }

  let mirror (e : Expr.t) t =
    match e with
    | Var { name; size; label; _ } ->
        let e' = Expr.var ("mirror_" ^ name) size label in
        Expr.Tbl.add t.mirror_e e e';
        Expr.Tbl.add t.secrets e' e
    | _ -> raise_notrace (Invalid_argument "mirror")

  let rec is_tainted (e : Expr.t) t =
    try e /== Expr.Tbl.find t.mirror_e e
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
      (Expr.Tbl.add t.mirror_e e e;
       false)

  and is_tainted_memory (m : Memory.t) t =
    try not (Memory.equal m (Memory.Tbl.find t.mirror_m m))
    with Not_found -> ( match m with Symbol _ -> false | Layer _ -> true)

  let rec make_mirror_e (e : Expr.t) t =
    try Expr.Tbl.find t.mirror_e e
    with Not_found ->
      let e' =
        match e with
        | Cst _ -> e
        | Var _ -> e
        | Load { label; len; dir; addr; _ } ->
            t.loads <- Term.to_mem_exn e :: t.loads;
            let label' = make_mirror_m label t in
            let addr' = make_mirror_e addr t in
            if (not (Memory.equal label label')) || (addr /== addr') then
              Expr.load len dir addr' label'
            else e
        | Unary { x; f; _ } ->
            let x' = make_mirror_e x t in
            if x /== x' then Expr._unary f x' else e
        | Binary { x; y; f; _ } ->
            let x' = make_mirror_e x t and y' = make_mirror_e y t in
            if (x /== x') || (y /== y') then Expr._binary f x' y' else e
        | Expr.Ite { c; t = x; e = y; _ } ->
            let c' = make_mirror_e c t
            and x' = make_mirror_e x t
            and y' = make_mirror_e y t in
            if (c /== c') || (x /== x') || (y /== y') then Expr._ite c' x' y'
            else e
      in
      Expr.Tbl.add t.mirror_e e e';
      e'

  and make_mirror_m (m : Memory.t) t =
    try Memory.Tbl.find t.mirror_m m
    with Not_found ->
      let m' =
        match m with
        | Symbol _ ->
            Memory.Tbl.add t.roots m m;
            m
        | Layer { over; addr; store; _ } ->
            let size = Expr.sizeof addr
            and addr' = make_mirror_e addr t
            and over' = make_mirror_m over t in
            Memory.Tbl.add t.roots m (Memory.Tbl.find t.roots over);
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
                      dirty || (value /== value') ))
                (Store.empty, false) store
            in
            if (addr /== addr') || dirty || not (Memory.equal over over') then
              Memory.layer addr' store' over'
            else m
      in
      Memory.Tbl.add t.mirror_m m m';
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
              (if e /== e' then Expr.logand e' conjunction else conjunction)
    in
    t.conjunction <- visit constraints t t.conjunction;
    t.constraints <- constraints

  let rec find_root array t =
    try Memory.Tbl.find t.roots array
    with Not_found ->
      let root =
        match array with
        | Symbol _ -> array
        | Layer { over; _ } -> find_root over t
      in
      Memory.Tbl.add t.roots array root;
      root
end

type ('value, 'model, 'state, 'path, 'a) field_id +=
  | Ct_state : ('value, 'model, 'state, 'path, Ct_state.t) field_id

let grammar_extension =
  [
    Dyp.Bind_to_cons
      [ ("secret_or_public", "Obj"); ("comma_separated_ident_rev_list", "Obj") ];
    Dyp.Add_rules
      [
        ( ( "secret_or_public",
            [ Dyp.Regexp (RE_String "secret") ],
            "default_priority",
            [] ),
          fun _ _ -> (Binsec_script.Syntax.Obj (Bool true), []) );
        ( ( "secret_or_public",
            [ Dyp.Regexp (RE_String "public") ],
            "default_priority",
            [] ),
          fun _ _ -> (Binsec_script.Syntax.Obj (Bool false), []) );
        ( ( "comma_separated_ident_rev_list",
            [ Dyp.Non_ter ("ident", No_priority) ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ Binsec_script.Syntax.String ident ] ->
                (Binsec_script.Syntax.Obj (String_list [ ident ]), [])
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
                Binsec_script.Syntax.Obj (String_list l);
                _;
                Binsec_script.Syntax.String ident;
              ] ->
                (Binsec_script.Syntax.Obj (String_list (ident :: l)), [])
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
                Binsec_script.Syntax.Obj (Bool secret);
                _;
                Binsec_script.Syntax.Obj (String_list names);
              ] ->
                (Binsec_script.Syntax.(Decl (Globals (secret, names))), [])
            | _ -> raise Dyp.Giveup );
        ( ( "decl",
            [
              Dyp.Regexp (RE_String "check");
              Dyp.Regexp (RE_String "secret");
              Dyp.Regexp (RE_String "erasure");
              Dyp.Regexp (RE_String "over");
              Dyp.Non_ter ("symbol", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _; _; Binsec_script.Syntax.Symbol symbol ] ->
                ( Binsec_script.Syntax.Decl
                    (Script.Return_hook (symbol, [ Se_check; Script.Instr.halt ])),
                  [] )
            | _ -> assert false );
        ( ( "fallthrough",
            [
              Dyp.Non_ter ("loc", No_priority);
              Dyp.Regexp (RE_String ":=");
              Dyp.Non_ter ("secret_or_public", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                Binsec_script.Syntax.Loc lval;
                _;
                Binsec_script.Syntax.Obj (Bool secret);
              ] ->
                ( Binsec_script.Syntax.Instr
                    (if secret then Secret lval else Script.Instr.nondet lval),
                  [] )
            | _ -> raise Dyp.Giveup );
        ( ( "fallthrough",
            [
              Dyp.Regexp (RE_String "check");
              Dyp.Regexp (RE_String "secret");
              Dyp.Regexp (RE_String "erasure");
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [ _; _; _ ] -> (Binsec_script.Syntax.Instr Se_check, [])
            | _ -> raise Dyp.Giveup );
        ( ( "instr",
            [
              Dyp.Non_ter ("loc", No_priority);
              Dyp.Regexp (RE_String ":=");
              Dyp.Non_ter ("secret_or_public", No_priority);
              Dyp.Non_ter ("accept_newline", No_priority);
              Dyp.Ter "AS";
              Dyp.Non_ter ("ident", No_priority);
            ],
            "default_priority",
            [] ),
          fun _ -> function
            | [
                Binsec_script.Syntax.Loc lval;
                _;
                Binsec_script.Syntax.Obj (Bool secret);
                _;
                _;
                Binsec_script.Syntax.String name;
              ] ->
                let var =
                  ( Ast.Loc.var name ~size:(Ast.Size.sizeof lval),
                    Lexing.dummy_pos )
                in
                ( Binsec_script.Syntax.Stmt
                    [
                      (if secret then Secret var else Ast.Instr.nondet var);
                      Ast.Instr.assign lval (Ast.Expr.loc var, Lexing.dummy_pos);
                    ],
                  [] )
            | _ -> assert false );
      ];
  ]

module Make
    (Options : OPTIONS)
    (Engine : ENGINE with type Path.value = Symbolic.Default.Expr.t) :
  EXTENSIONS with type path = Engine.Path.t = struct
  module Path = Engine.Path
  module State = Path.State
  module Value = Path.Value
  module Path_stats = Engine.Metrics.Exploration.Paths
  module Other_stats = Engine.Metrics.Exploration

  type path = Path.t

  let addr_size = Machine.ISA.word_size Engine.isa
  let key = Engine.lookup Ct_state

  type builtin +=
    | Mirror of Dba.Var.t
    | Ct_check of Dba.Expr.t * Kind.t
    | Se_check

  let ct_cf_secure = ref 0
  and ct_cf_insecure = ref 0
  and ct_cf_unknown = ref 0
  and ct_mem_secure = ref 0
  and ct_mem_insecure = ref 0
  and ct_mem_unknown = ref 0
  and ct_mult_secure = ref 0
  and ct_mult_insecure = ref 0
  and ct_mult_unknown = ref 0
  and ct_div_secure = ref 0
  and ct_div_insecure = ref 0
  and ct_div_unknown = ref 0
  and se_secure = ref 0
  and se_insecure = ref 0
  and se_unknown = ref 0

  let ct_status (kind : Kind.t) (status : Status.t) =
    match (kind, status) with
    | Control_flow, Secure -> ct_cf_secure
    | Control_flow, Insecure _ -> ct_cf_insecure
    | Control_flow, Unknown -> ct_cf_unknown
    | Memory_access, Secure -> ct_mem_secure
    | Memory_access, Insecure _ -> ct_mem_insecure
    | Memory_access, Unknown -> ct_mem_unknown
    | Multiplication, Secure -> ct_mult_secure
    | Multiplication, Insecure _ -> ct_mult_insecure
    | Multiplication, Unknown -> ct_mult_unknown
    | (Dividend | Divisor), Secure -> ct_div_secure
    | (Dividend | Divisor), Insecure _ -> ct_div_insecure
    | (Dividend | Divisor), Unknown -> ct_div_unknown

  let se_status (status : Status.t) =
    match status with
    | Secure -> se_secure
    | Insecure _ -> se_insecure
    | Unknown -> se_unknown

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
    Path_stats.get Pending > 0
    || !ct_cf_unknown + !ct_mem_unknown + !se_unknown > 0
    || Path_stats.status Non_executable_code > 0
    || Path_stats.status Max_depth > 0
    || Path_stats.status Enumeration_limit > 0
    || Path_stats.status Unresolved_formula > 0
    || Path_stats.status (Error "") > 0

  let mirror var path : Path.t continuation =
    try
      Ct_state.mirror (State.lookup var (Path.state path)) (Path.get path key);
      Return
    with Invalid_argument _ -> Signal (Error "mirror")

  let command_callback : Ast.t -> Script.env -> Path.t -> bool =
    let lookup_symbol (env : env) name attr =
      match env.lookup_symbol name attr with
      | Var { info = Symbol (_, (lazy bv)); _ } -> bv
      | _ -> assert false
    in
    fun decl env path ->
      match decl with
      | Globals (secret, names) ->
          List.iter
            (fun name ->
              let addr = lookup_symbol env name Value
              and bytesize =
                Size.Byte.create
                  (Bitvector.to_uint (lookup_symbol env name Size))
              in
              let bitsize = Size.Byte.to_bitsize bytesize in
              let var = Dba.Var.create name ~bitsize ~tag:Empty in
              env.define var Lexing.dummy_pos;
              Path.symbolize path var;
              if secret then ignore (mirror var path);
              Path.store path None ~addr:(Dba.Expr.constant addr)
                (Dba.Expr.v var) LittleEndian)
            names;
          true
      | _ -> false

  let instruction_callback : Ast.Instr.t -> Script.env -> Ir.fallthrough list =
    let secret = Printf.sprintf "%%secret%%%d" in
    fun inst env ->
      match inst with
      | Secret lval -> (
          match Script.eval_loc lval env with
          | Var var -> [ Symbolize var; Builtin (Mirror var) ]
          | Restrict (var, { hi; lo }) ->
              let size' = hi - lo + 1 in
              let name' = secret size' in
              let var' = Dba.Var.temporary name' (Size.Bit.create size') in
              let rval =
                Dba_types.Expr.complement (Dba.Expr.v var') ~lo ~hi var
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
      | Se_check -> [ Builtin Se_check ]
      | _ -> []

  let instrumentation_routine : Revision.t -> unit =
    let rec visit_expr graph vertex (e : Dba.Expr.t) =
      match e with
      | Cst _ | Var _ | Load _ -> ()
      | Unary (_, x) -> visit_expr graph vertex x
      | Binary (Mult, x, y) ->
          if Options.check_mult then (
            Revision.insert_before graph vertex
              (Builtin (Ct_check (x, Multiplication)));

            Revision.insert_before graph vertex
              (Builtin (Ct_check (y, Multiplication))));
          visit_expr graph vertex x;
          visit_expr graph vertex y
      | Binary ((DivU | DivS | RemU | RemS), dividend, divisor) ->
          if Options.check_dividend then
            Revision.insert_before graph vertex
              (Builtin (Ct_check (dividend, Dividend)));
          if Options.check_divisor then
            Revision.insert_before graph vertex
              (Builtin (Ct_check (divisor, Divisor)));
          visit_expr graph vertex dividend;
          visit_expr graph vertex divisor
      | Binary (_, x, y) ->
          visit_expr graph vertex x;
          visit_expr graph vertex y
      | Ite (c, x, y) ->
          visit_expr graph vertex c;
          visit_expr graph vertex x;
          visit_expr graph vertex y
    in
    fun graph ->
      Revision.iter_new_vertex
        (fun vertex ->
          match Revision.node graph vertex with
          | Fallthrough { kind = Load { addr; _ }; _ } ->
              if Options.check_memory then
                Revision.insert_before graph vertex
                  (Builtin (Ct_check (addr, Memory_access)))
          | Fallthrough { kind = Store { addr; rval; _ }; _ } ->
              if Options.check_memory then
                Revision.insert_before graph vertex
                  (Builtin (Ct_check (addr, Memory_access)));
              visit_expr graph vertex rval
          | Fallthrough { kind = Assign { rval; _ }; _ } ->
              visit_expr graph vertex rval
          | Branch { test = expr; _ }
          | Terminator { kind = Jump { target = expr; _ }; _ } ->
              if Options.check_branch then
                Revision.insert_before graph vertex
                  (Builtin (Ct_check (expr, Control_flow)))
          | _ -> ())
        graph

  let taint_analysis e ct_state _ : Status.t =
    if Ct_state.is_tainted e ct_state then Unknown else Secure

  let cv_symbol (ct_state : Ct_state.t) model i e =
    let open Symbolic.Default in
    if Expr.Tbl.mem ct_state.secrets e then
      match i with
      | 0 -> Bv.zeros (Expr.sizeof e)
      | 1 -> Bv.ones (Expr.sizeof e)
      | 2 -> Bv.fill (Expr.sizeof e)
      | _ -> Bv.rand (Expr.sizeof e)
    else Path.Model.eval e model

  and cv_memory :
      Path.Model.t -> Symbolic.Default.Memory.symbol -> Bitvector.t -> char =
   fun model (Symbol _ as array) addr ->
    let open Symbolic.Default in
    Bv.to_char
      (Path.Model.eval
         (Expr.load 1 LittleEndian (Expr.constant addr) array)
         model)

  let extract_cv_report :
      Ct_state.t ->
      Value.t list Dba_types.Var.Map.t ->
      Path.Model.t ->
      Symbolic.Default.Model.t ->
      Report.t =
   fun ct_state symbols model0 ({ arrays; _ } as model1) ->
    let open Symbolic.Default in
    let memory =
      Symbol.Tbl.fold
        (fun (Symbol { name; index; _ }) bytes memory ->
          StrMap.add name
            (BiTbl.fold
               (fun addr byte values ->
                 Bitvector.Map.add (Bv.create addr index) (Bv.of_char byte)
                   values)
               bytes Bitvector.Map.empty)
            memory)
        arrays StrMap.empty
    in
    let public, secret1, secret2 =
      Dba_types.Var.Map.fold
        (fun { name; _ } values (public, secret1, secret2) ->
          let vpublic, vsecret1, vsecret2 =
            List.fold_left
              (fun (public, secret1, secret2) value ->
                let value' =
                  try Expr.Tbl.find ct_state.mirror_e value
                  with Not_found -> value
                in
                if value /== value' then
                  ( public,
                    Path.Model.eval value model0 :: secret1,
                    Model.eval model1 value' :: secret2 )
                else (Path.Model.eval value model0 :: public, secret1, secret2))
              ([], [], []) values
          in
          ( (if vpublic <> [] then StrMap.add name vpublic public else public),
            (if vsecret1 <> [] then StrMap.add name vsecret1 secret1
             else secret1),
            if vsecret2 <> [] then StrMap.add name vsecret2 secret2 else secret2
          ))
        symbols
        (StrMap.empty, StrMap.empty, StrMap.empty)
    in
    { memory; public; secret1; secret2 }

  let cv_analysis e (ct_state : Ct_state.t) path : Status.t =
    let open Symbolic.Default in
    match ct_state.models with
    | [] -> Unknown
    | models -> (
        let main_model = List.hd (Path.models path) in
        Ct_state.make_context (Path.predicate path) ct_state;
        let e' = Ct_state.make_mirror_e e ct_state in
        if e === e' then Secure
        else
          let memory = cv_memory main_model in
          ct_state.models <-
            List.fold_left
              (fun models (i, model) ->
                let symbols = cv_symbol ct_state main_model i in
                let model =
                  if
                    List.exists
                      (fun e ->
                        Bv.is_zero (Model.eval ~symbols ~memory model e))
                      ct_state.constraints
                  then Model.empty ()
                  else model
                in
                if
                  Bv.is_one
                    (Model.eval ~symbols ~memory model ct_state.conjunction)
                then (i, model) :: models
                else models)
              [] models;
          let value = Path.Model.eval e main_model in
          match
            List.find
              (fun (i, model) ->
                Bv.diff value
                  (Model.eval
                     ~symbols:(cv_symbol ct_state main_model i)
                     ~memory model e'))
              ct_state.models
          with
          | exception Not_found -> Unknown
          | _, model ->
              Insecure
                (extract_cv_report ct_state (Path.symbols path) main_model model)
        )

  let extract_relse_report (ct_state : Ct_state.t)
      (symbols : Value.t list Dba_types.Var.Map.t) model : Report.t =
    let open Symbolic.Default in
    let memory =
      List.fold_left
        (fun memory (Load { label; len; dir; addr; _ } : Ct_state.load) ->
          let array = Ct_state.find_root label ct_state in
          let name =
            match array with
            | Symbol { name; _ } -> name
            | Layer _ -> assert false
          in
          let base = Path.Model.eval addr model in
          let bytes =
            Path.Model.eval (Expr.load len dir (Expr.constant base) array) model
          in
          let values =
            try StrMap.find name memory with Not_found -> Bitvector.Map.empty
          in
          StrMap.add name (Bitvector.Map.add base bytes values) memory)
        StrMap.empty ct_state.loads
    in
    let public, secret1, secret2 =
      Dba_types.Var.Map.fold
        (fun { name; _ } values bindings ->
          List.fold_left
            (fun (public, secret1, secret2) value ->
              if
                try value /== Expr.Tbl.find ct_state.mirror_e value
                with Not_found -> false
              then
                ( public,
                  (StrMap.update name (fun history ->
                       Some
                         (Path.Model.eval value model
                         :: (match history with None -> [] | Some h -> h))))
                    secret1,
                  (StrMap.update name (fun history ->
                       Some
                         (Path.Model.eval
                            (Expr.Tbl.find ct_state.mirror_e value)
                            model
                         :: (match history with None -> [] | Some h -> h))))
                    secret2 )
              else
                ( (StrMap.update name (fun history ->
                       Some
                         (Path.Model.eval value model
                         :: (match history with None -> [] | Some h -> h))))
                    public,
                  secret1,
                  secret2 ))
            bindings values)
        symbols
        (StrMap.empty, StrMap.empty, StrMap.empty)
    in
    { memory; public; secret1; secret2 }

  let relse_analysis e ct_state path : Status.t =
    let e' = Ct_state.make_mirror_e e ct_state in
    if e === e' then Secure
    else (
      Ct_state.make_context (Path.predicate path) ct_state;
      match
        Path.check_sat_assuming_v ~retain:false path
          (Path.Value.binary And
             (Path.Value.binary Diff e e')
             ct_state.conjunction)
      with
      | exception Symbolic.State.Unknown -> Unknown
      | None -> Secure
      | Some model ->
          Insecure (extract_relse_report ct_state (Path.symbols path) model))

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

  let analyze expr path =
    let rec apply_analyses e ct_state path analyses : Status.t =
      match analyses with
      | [] -> Unknown
      | (_, analysis) :: analyses ->
          let status : Status.t = analysis e ct_state path in
          if status <> Unknown then status
          else apply_analyses e ct_state path analyses
    in
    apply_analyses expr (Path.get path key) path analyses

  let ct_check expr (kind : Kind.t) path : Path.t continuation =
    let addr = Path.pc path in
    if Options.leak_info = InstrLeak && is_addr_insecure addr then Return
    else
      let expr = Path.get_value path expr in
      let status = analyze expr path in
      incr (ct_status kind status);
      match status with
      | Secure -> Return
      | Insecure _ ->
          add_addr_status addr status;
          Logger.result "Instruction %a has %s leak (%.3fs)" Virtual_address.pp
            addr (Kind.to_string kind)
            (Engine.Metrics.Exploration.Timer.get ());
          if Options.leak_info = HaltLeak then raise Exec.Halt else Return
      | Unknown ->
          add_addr_status addr status;
          Return

  let se_check path : Path.t continuation =
    let addr =
      Symbolic.Default.Expr.var "!secret_seeker" addr_size "!secret_seeker"
    in
    let read = Path.read_v path None ~addr 1 LittleEndian in
    let status = analyze read path in
    incr (se_status status);
    (match status with
    | Unknown | Secure -> ()
    | Insecure report ->
        Logger.result "Secret-erasure check failure %@ %a (%.3fs)@ %a"
          Virtual_address.pp (Path.pc path)
          (Engine.Metrics.Exploration.Timer.get ())
          Report.pp report);
    Return

  let builtin_resolver = function
    | Mirror var -> Call (mirror var)
    | Ct_check (expr, kind) -> Call (ct_check expr kind)
    | Se_check -> Call se_check
    | _ -> Unknown

  let builtin_printer ppf = function
    | Mirror { name; _ } ->
        Format.fprintf ppf "secret mirror(%s)" name;
        true
    | Ct_check (expr, kind) ->
        Format.fprintf ppf "ct %a check(%a)" Kind.pp kind
          Dba_printer.Ascii.pp_bl_term expr;
        true
    | Se_check ->
        Format.pp_print_string ppf "secret-erasure check";
        true
    | _ -> false

  let at_exit_callback () =
    let l_insecure, _ = addr_status_report () in
    let status =
      if l_insecure <> [] || !se_insecure > 0 then False
      else if is_unknown_report () then Unknown
      else True
    in
    Option.iter
      (fun filename ->
        let toml_data =
          Toml.Min.of_key_values
            [
              (Toml.Min.key "CT report", Toml.Types.TTable (toml_ct_report ()));
              ( Toml.Min.key "Exploration",
                Toml.Types.TTable (Other_stats.to_toml ()) );
            ]
        in
        let oc = open_out_bin filename in
        let ppf = Format.formatter_of_out_channel oc in
        Toml.Printer.table ppf toml_data;
        close_out oc)
      Options.stats_file;
    Logger.result "Program status is : %s (%0.3f)"
      (match status with
      | False -> "insecure"
      | True -> "secure"
      | Unknown -> "unknown")
      (Other_stats.Timer.get ());
    if Options.leak_info = InstrLeak then (
      let paths = Path_stats.get Completed
      and static_instrs = Other_stats.Addresses.unique () in
      Logger.info "%d visited path%s covering %d instruction%s" paths
        (if paths > 1 then "s" else "")
        static_instrs
        (if static_instrs > 1 then "s" else "");
      if Options.check_branch then
        Logger.info "%d / %d control flow checks pass" !ct_cf_secure
          (!ct_cf_secure + !ct_cf_insecure + !ct_cf_unknown);
      if Options.check_memory then
        Logger.info "%d / %d memory access checks pass" !ct_mem_secure
          (!ct_mem_secure + !ct_mem_insecure + !ct_mem_unknown);
      if Options.check_mult then
        Logger.info "%d / %d multiplication checks pass" !ct_mult_secure
          (!ct_mult_secure + !ct_mult_insecure + !ct_mult_unknown);
      if Options.check_dividend || Options.check_divisor then
        Logger.info "%d / %d division checks pass" !ct_div_secure
          (!ct_div_secure + !ct_div_insecure + !ct_div_unknown);
      let se_checks = !se_secure + !se_insecure + !se_unknown in
      if se_checks > 0 then
        Logger.info "%d / %d secret erasure checks pass" !se_secure se_checks);
    if is_unknown_report () then
      Logger.warning "@[<v>Exploration is incomplete:%a@]"
        (fun ppf () ->
          let pendings = Path_stats.get Pending in
          if pendings > 0 then
            Format.fprintf ppf
              "@ - timeout has left (at least) %d pending paths (-sse-timeout)"
              pendings;
          let non_exec = Path_stats.status Non_executable_code in
          if non_exec > 0 then
            Format.fprintf ppf
              "@ - %d paths fell into non executable code segments" non_exec;
          let max_depth = Path_stats.status Max_depth in
          if max_depth > 0 then
            Format.fprintf ppf
              "@ - %d paths have reached the maximal depth and have been cut \
               (-sse-depth)"
              max_depth;
          let incomplete_enum = Path_stats.status Enumeration_limit in
          if incomplete_enum > 0 then
            Format.fprintf ppf
              "@ - some jump targets may have been omitted (-sse-jump-enum)";
          let unknown =
            Path_stats.status Unresolved_formula
            + !ct_cf_unknown + !ct_mem_unknown + !se_unknown
          in
          if unknown > 0 then
            Format.fprintf ppf
              "@ - %d SMT solver queries remain unsolved (-smt-timeout)" unknown)
        ()

  let instruction_printer ppf = function
    | Secret (loc, _) ->
        Format.fprintf ppf "%a := secret" Ast.Loc.pp loc;
        true
    | Se_check ->
        Format.fprintf ppf "check secret erasure";
        true
    | _ -> false

  let declaration_printer ppf = function
    | Globals (secret, names) ->
        Format.fprintf ppf "%s global %a"
          (if secret then "secret" else "public")
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
             Format.pp_print_string)
          names;
        true
    | _ -> false

  let list : Path.t extension list =
    [
      Grammar_extension grammar_extension;
      Command_handler command_callback;
      Command_printer declaration_printer;
      Instruction_resolver instruction_callback;
      Instruction_printer instruction_printer;
      Instrumentation_routine instrumentation_routine;
      Builtin_resolver builtin_resolver;
      Builtin_printer builtin_printer;
      Fork_callback
        (fun path path' ->
          Path.set path' key (Ct_state.fork (Path.get path key)));
      Exit_callback at_exit_callback;
    ]
end

module Plugin (O : OPTIONS) : PLUGIN = struct
  let name = "checkct"

  let fields : (module PATH) -> field list =
   fun _ ->
    [
      Field
        {
          id = Ct_state;
          default = Ct_state.empty ();
          copy = None;
          merge = None;
        };
    ]

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Engine = (val engine) in
    match Engine.Path.State.more Symbolic.State.ValueKind with
    | Some Symbolic.Default.Term ->
        let module Extensions = Make (O) (Engine) in
        Extensions.list
    | Some _ | None ->
        Logger.fatal
          "unable to use 'checkct' within the current symbolic engine"
end
