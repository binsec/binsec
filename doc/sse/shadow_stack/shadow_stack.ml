module ID = struct
  let name = "tuto"
end

(* Import the definition of [ENGINE], [EXTENSIONS], [PLUGIN], [field] and [extension] types. *)
open Binsec_sse.Types

(* Import the modules [Ast] and [Syntax], and the [eval_expr] function. *)
open Binsec_sse.Script

type Ast.t += Initial_stack of Ast.Expr.t Ast.loc list

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
            | [ _; _; _; Syntax.Obj (Expr_list values) ] ->
                (Syntax.Decl (Initial_stack (List.rev values)), [])
            | _ -> assert false );
      ];
  ]

let command_printer ppf = function
  | Initial_stack values ->
      Format.fprintf ppf "initial call stack %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           (fun ppf (addr, _) -> Expr.pp ppf addr))
        values;
      true
  | _ -> false

module V1 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct
  type path = Engine.Path.t

  (* Import the modules [Size], [Machine] and [Dba]. *)
  open Binsec_kernel

  (* Import the module [Ir] and [Revision]. *)
  open Binsec_sse

  let bits = Machine.ISA.bits Engine.isa

  let bitsize = Machine.Bitwidth.bitsize bits
  and bytesize = Machine.Bitwidth.bytesize bits

  let offset = Size.Byte.to_int bytesize

  let expr_of_addr addr =
    Dba.Expr.constant
      (Bitvector.create
         (Virtual_address.to_bigint addr)
         (Size.Bit.to_int bitsize))

  let shadow_stack_name = Some "shadow_stack"

  let stack_pointer_var =
    Dba.Var.create "shadow_stack_pointer" ~bitsize ~tag:Empty

  let stack_pointer_exp = Dba.Expr.v stack_pointer_var

  let push ret_addr : Ir.fallthrough list =
    (* Push the return address into the shadow stack *)
    [
      Store
        {
          base = shadow_stack_name;
          dir = LittleEndian;
          addr = stack_pointer_exp;
          rval = expr_of_addr ret_addr;
        };
      Assign
        {
          var = stack_pointer_var;
          rval = Dba.Expr.addi stack_pointer_exp offset;
        };
    ]

  let witness_var = Dba.Var.create "shadow_stack_witness" ~bitsize ~tag:Temp
  let witness_exp = Dba.Expr.v witness_var
  let zero_exp = Dba.Expr.constant (Bitvector.zeros (Size.Bit.to_int bitsize))

  let pop_and_check target : Ir.fallthrough list =
    (* Pop an address from the shadow stack and compare return addresses *)
    [
      Assert (Dba.Expr.ugt stack_pointer_exp zero_exp);
      Assign
        {
          var = stack_pointer_var;
          rval = Dba.Expr.subi stack_pointer_exp offset;
        };
      Load
        {
          var = witness_var;
          base = shadow_stack_name;
          dir = LittleEndian;
          addr = stack_pointer_exp;
        };
      Assert (Dba.Expr.equal witness_exp target);
    ]

  let list =
    [
      (* Instrument call sites *)
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              (* The node is a call site *)
              | Fallthrough { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Jump { tag = Call { base; _ }; _ }; _ } ->
                  Revision.insert_list_before graph vertex (push base)
              | _ -> ())
            graph);
      (* Instument return sites *)
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              (* The node is a return site *)
              | Terminator { kind = Goto { target; tag = Return; _ }; _ } ->
                  Revision.insert_list_before graph vertex
                    (pop_and_check (expr_of_addr target))
              | Terminator { kind = Jump { target; tag = Return; _ }; _ } ->
                  Revision.insert_list_before graph vertex
                    (pop_and_check target)
              | _ -> ())
            graph);
      (* Initialization *)
      Initialization_callback
        (fun path -> Engine.Path.assign path stack_pointer_var zero_exp);
      (* Script extension *)
      Grammar_extension grammar_extension;
      Command_printer command_printer;
      Command_handler
        (fun decl env path ->
          match decl with
          | Initial_stack values ->
              Engine.Path.assign path stack_pointer_var
                (List.fold_left
                   (fun stack_pointer_exp addr ->
                     let addr = eval_expr ~size:env.wordsize addr env in
                     Engine.Path.store path shadow_stack_name
                       ~addr:stack_pointer_exp addr LittleEndian;
                     Dba.Expr.addi stack_pointer_exp offset)
                   zero_exp values);
              true
          | _ -> false);
    ]
end

module Plugin_v1 : PLUGIN = struct
  include ID

  let fields : (module PATH) -> field list = fun _ -> []

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Extensions = V1 ((val engine)) in
    Extensions.list
end

(* Make a dedicated sub-logger using [ID.name] as tag. *)
module Log = Binsec_sse.Logger.Sub (ID)

type ('value, 'model, 'state, 'path, 'a) field_id +=
  | Stack : ('value, 'model, 'state, 'path, 'value list) field_id

module V2 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct
  type path = Engine.Path.t

  (* Import the modules [Size], [Machine] and [Dba]. *)
  open Binsec_kernel

  (* Import the module [Ir] and [Revision]. *)
  open Binsec_sse

  type Ir.builtin += Push of Dba.Expr.t | Pop of Dba.Expr.t

  let bits = Machine.ISA.bits Engine.isa
  let bitsize = Machine.Bitwidth.bitsize bits

  let expr_of_addr addr =
    Dba.Expr.constant
      (Bitvector.create
         (Virtual_address.to_bigint addr)
         (Size.Bit.to_int bitsize))

  let shadow_stack_key = Engine.lookup Stack

  let pp_address ppf addr =
    Virtual_address.pp ppf addr;
    match Engine.Debug.reverse_symbol addr with
    | None -> ()
    | Some (name, offset) ->
        Format.pp_print_string ppf " <";
        Format.pp_print_string ppf name;
        if not (Z.equal offset Z.zero) then (
          Format.pp_print_char ppf '+';
          Format.pp_print_string ppf (Z.format "%#x" offset));
        Format.pp_print_char ppf '>'

  let push target path =
    Log.debug ~level:2 "%a: push(%a)" pp_address (Engine.Path.pc path)
      Dba_printer.Ascii.pp_expr target;
    Engine.Path.set path shadow_stack_key
      (Engine.Path.get_value path target
      :: Engine.Path.get path shadow_stack_key)

  let report addr target witness model =
    Log.error "@[<v>%a: Stack tampering@ (goto %a, expected %a)@ %a@]"
      pp_address addr Bitvector.pp_hex_or_bin
      (Engine.Path.Model.eval target model)
      Bitvector.pp_hex_or_bin
      (Engine.Path.Model.eval witness model)
      Engine.Path.Model.pp model

  let pop target path =
    let addr = Engine.Path.pc path in
    Log.debug ~level:2 "%a: pop(%a)" pp_address addr Dba_printer.Ascii.pp_expr
      target;
    match Engine.Path.get path shadow_stack_key with
    | [] ->
        Log.error "%a: return does not match any call" pp_address addr;
        Signal Assertion_failure
    | witness_val :: tail -> (
        Engine.Path.set path shadow_stack_key tail;
        let target_val = Engine.Path.get_value path target in
        (match
           Engine.Path.check_sat_assuming_v ~retain:false path
             (Engine.Path.Value.binary Diff target_val witness_val)
         with
        | None -> ()
        | Some model -> report addr target_val witness_val model);
        match
          Engine.Path.assume_v path
            (Engine.Path.Value.binary Eq target_val witness_val)
        with
        | None -> Signal Assertion_failure
        | Some _ -> Return)

  let list =
    [
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              (* Insert Push builtin *)
              | Fallthrough { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Goto { tag = Call { base; _ }; _ }; _ }
              | Terminator { kind = Jump { tag = Call { base; _ }; _ }; _ } ->
                  Revision.insert_before graph vertex
                    (Builtin (Push (expr_of_addr base)))
              (* Insert Pop Builtin *)
              | Terminator { kind = Goto { target; tag = Return; _ }; _ } ->
                  Revision.insert_before graph vertex
                    (Builtin (Pop (expr_of_addr target)))
              | Terminator { kind = Jump { target; tag = Return; _ }; _ } ->
                  Revision.insert_before graph vertex (Builtin (Pop target))
              | _ -> ())
            graph);
      Builtin_printer
        (fun ppf -> function
          | Push target ->
              Format.fprintf ppf "shadow push %a" Dba_printer.Ascii.pp_expr
                target;
              true
          | Pop target ->
              Format.fprintf ppf "shadow pop %a" Dba_printer.Ascii.pp_expr
                target;
              true
          | _ -> false);
      Builtin_resolver
        (function
        | Push target -> Apply (push target)
        | Pop target -> Call (pop target)
        | _ -> Unknown);
      Command_printer command_printer (* Same as V1 *);
      Command_handler
        (fun decl (env : Script.env) path ->
          match decl with
          | Initial_stack values ->
              Engine.Path.set path shadow_stack_key
                (List.rev_map
                   (fun addr ->
                     Engine.Path.get_value path
                       (eval_expr ~size:env.wordsize addr env))
                   values);
              true
          | _ -> false);
      Grammar_extension grammar_extension (* Same as V1 *);
    ]
end

module Plugin_v2 : PLUGIN = struct
  include ID

  let fields : (module PATH) -> field list =
   fun _ -> [ Field { id = Stack; default = []; copy = None; merge = None } ]

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Extensions = V2 ((val engine)) in
    Extensions.list
end
