# SSE Tutorial \#6: Monitoring the symbolic execution with a custom plugin (part 2)

This tutorial follows the [plugin tutorial part 1](plugins_1.md). If you have not read it yet, we recommend you to take a look at it before delving into this one.

*This post has been updated for the plugin API revision starting from **BINSEC** version `0.11`. For earlier versions, refer to the [previous writing](https://github.com/binsec/binsec/blob/5f1402e92cb6c27a429d2043d2f0d0838b2bc1e1/doc/sse/plugins.md).*

### Builtin instructions

In the last tutorial, we implemented a shadow stack plugin using only standard **DBA** instructions. In this chapter, we will learn how to instrument the symbolic execution with *builtin* instructions, by implementing our shadow stack with custom `Push` and `Pop` builtins.

### Step 1: register a command line argument

Rather than replacing the current version of the plugin, we are going to propose a new one that can coexist with it.

```ocaml
type mode =
  | Inline  (** Use standard DBA assertions *)
  | Builtin  (** Use new push and pop builtins *)
```

To do so, we will create an additional command line option using the module `Builder`.
```ocaml
module Mode = Builder.Variant_choice_assoc (struct
  type t = mode

  let name = "mode"

  let doc =
    "Use standard DBA assertions [inline] or new push and pop builtins \
     [builtin]"

  let default = Inline
  let assoc_map = [ ("inline", Inline); ("builtin", Builtin) ]

end)
```

:information_source: Here, we use `Variant_choice_assoc` to directly map the type `mode`. Other argument kinds include `Boolean`, `Integer`, `String`, etc.

We change the way the plugin is instantiated as follows.

#### shadow_stack.ml
```ocaml
(* Code from previous tutorial. *)
(* [ ... ] *)

(* Make a dedicated sub-logger using [ID.name] as tag. *)
module Log = Binsec_sse.Logger.Sub (ID)

module V2 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct
  type path = Engine.Path.t

  let list = []
end

module Plugin_v2 : PLUGIN = struct
  include ID

  let fields : (module PATH) -> field list = fun _ -> []

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Extensions = V2 ((val engine)) in
    Extensions.list
end
```

#### registration.ml

```ocaml
module Namespace =
  Binsec_cli.Cli.Make_from_logger
    (Shadow_stack.Log)
    (struct
      let name = "Shadow stack tutorial"
      let shortname = "tuto"
    end)

type mode =
  | Inline  (** Use standard DBA assertions *)
  | Builtin  (** Use new push and pop builtins *)

module Mode = Namespace.Builder.Variant_choice_assoc (struct
  type t = mode

  let name = "mode"

  let doc =
    "Use standard DBA assertions [inline] or new push and pop builtins \
     [builtin]"

  let default = Inline
  let assoc_map = [ ("inline", Inline); ("builtin", Builtin) ]
end)

let () =
  Binsec_cli_sse.Plugins.register ~is_enabled:Namespace.is_enabled (fun () ->
      match Mode.get () with
      | Inline -> (module Shadow_stack.Plugin_v1)
      | Builtin -> (module Shadow_stack.Plugin_v2))
```

Now, all we have to do is to fill the second module `V2` and we will be able to control the mode with the command line option `-tuto-mode [inline|builtin]`.

:information_source: In order to output results, we create a dedicated logger which will log the entries using the `[tuto]` tag instead of `[sse]`. Its verbosity level can be configured via the command line (e.g. `-tuto-debug-level`). Since our logger is derived (`Sub`) from the main `sse` logger, our logger will also automatically reflect the changes of the latter one.

### Step 2: adding new builtin instructions

A shadow stack can be implemented using only **DBA** primitives, but sometimes, it may be necessary to do things that are not covered by the core language.
We will illustrate this by adding custom `Push` and `Pop` instructions to represent our shadow stack, together with dedicated error messages.

To make **BINSEC** handle these new instructions, we need to declare them in the intermediate representation. We thus extend the `Ir.Builtin` type with our new instructions:

```ocaml
type Ir.builtin += Push of Dba.Expr.t | Pop of Dba.Expr.t
```

Just like before, we will use the `Instrumentation_routine` to insert our new primitives at *call* and *return* sites.

```ocaml
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
    ]
end
```

:information_source: Here we decided to merge the two `Instrumentation_routine` in a single pass.

We will now need to implement the semantics of these two builtins, so the execution knows what to do when encountering the `Push` and `Pop` primitives.

### Representing the shadow stack

In the [first tutorial](plugins_1.md), we defined the shadow stack directly in the symbolic state of the path, using only DBA variables.
Yet, the plugin interface also allows extending the `Path` structure with additional fields to store arbitrary **OCaml** data.
Such data will be local such that each path can have its own version of the field values during the program exploration.

We are going to use an **OCaml** `list` to implement the new version of our shadow stack.

To do so, we first need to create a new field identifier by extending the type `field_id`:

```ocaml
type ('value, 'model, 'state, 'path, 'a) field_id +=
  | Stack : ('value, 'model, 'state, 'path, 'value list) field_id
```

:information_source: **OCaml** is a strongly typed language. The type `field_id` uses phantom types to explicitly define the type of value that is going to be stored. Here, we define the stack as a `'value list` where the generic type `'value` will match the symbolic values the path operates on. Similarly, it is possible to define a type that depends on the model, the symbolic state, or even the path itself using the corresponding generic types.

We can now add our new field to the `fields` of our plugin:

```ocaml
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
```

:information_source: Here, since an **OCaml** list is immutable, we can use the default `copy` function (*shallow copy*). We also use the default `merge` function such that two paths may be merged only if they have the same call stack.

In the `V2` module, it is now possible to get the `key` associated to our field:
```ocaml
let shadow_stack_key = Engine.lookup Stack
```

The `shadow_stack_key` will be used with the function `Path.get` and `Path.set` to respectively read and write our stack from the `path` instance given in the callbacks.

We now have to perform the following actions:

- register a printer for our new builtin

```ocaml
Builtin_printer of (Format.formatter -> Ir.builtin -> bool)
```

- register a callback for it

```ocaml
Builtin_resolver of (Ir.builtin -> 'path primitive)
```

#### Builtin_printer

Nothing new here, we simply have to define the pretty printer. 
```ocaml
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
```

#### Builtin_resolver

Given a builtin, this callback should return the native **OCaml** function to execute during the symbolic execution (it returns a function only if it can handle the builtin or `Unknown` otherwise).

:information_source: The interface supports two function signatures:
```ocaml
| Call of ('a -> 'a continuation)
| Apply of ('a -> unit)
```
The first one, `Call` has to return a `continuation` as defined in [`Types`](../../src/sse/types.mli).
Here we are going to use `Return` to resume the execution and `Signal` to kill the path if we find a violation.
The second one, `Apply` returns nothing. It is equivalent to a `Call` which always terminates by `Return`.


Here is the code to implement the shadow stack logic.

```ocaml
module V2 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct

  (* [ ... ] *)

  let push target path =
    Log.debug ~level:2 "%a: push(%a)" Virtual_address.pp
      (Engine.Path.pc path) Dba_printer.Ascii.pp_expr target;
    Engine.Path.set path shadow_stack_key
      (Engine.Path.get_value path target
      :: Engine.Path.get path shadow_stack_key)

  let report addr target witness model =
    Log.error "@[<v>%a: Stack tampering@ (goto %a, expected %a)@ %a@]"
      Virtual_address.pp addr
      Bitvector.pp_hex_or_bin
      (Engine.Path.Model.eval target model)
      Bitvector.pp_hex_or_bin
      (Engine.Path.Model.eval witness model)
      Engine.Path.Model.pp model

  let pop target path =
    let addr = Engine.Path.pc path in
    Log.debug ~level:2 "%a: pop(%a)" Virtual_address.pp addr
      Dba_printer.Ascii.pp_expr target;
    match Engine.Path.get path shadow_stack_key with
    | [] ->
        Log.error "%a: return does not match any call" Virtual_address.pp addr;
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
      (* [ ... ] *)
      Builtin_resolver
        (function
        | Push target -> Apply (push target)
        | Pop target -> Call (pop target)
        | _ -> Unknown);
    ]
end
```

Here the `push` function uses `Path.get_value` to evaluate the symbolic value of the return address, then push it (`::`) to the top of the shadow stack.
The `pop` function first checks if the stack is empty, then compares its top value with the actual return target (using `Path.get_value` too).

The advantage of the *builtin* version is that we can report the stack tampering with a dedicated error message instead of the generic assertion failure one.
Especially, we are able to report both the address we are going to jump to and the return address that was expected.

:information_source: **To go further**, it is possible to customize the report log with additional debug information.
For instance, we can use the reverse symbol mapping (`Engine.Debug.reverse_symbol`) to print the name of the function to which an address belongs.
To do so, we have to replace the calls to `Virtual_address.pp` with the following:

```ocaml
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
```

#### Syntax extension

We must not forget to give a callback and a printer to our special statement `Initial_stack` in this version too.

```ocaml
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
```

### Testing the plugin

Our shadow stack is ready again to be put to the test. The final version of the plugin can be found in the folder [doc/sse/shadow_stack](shadow_stack).
Using the same setup as before ([first tutorial](plugins_1.md)), we can run the following command:

```console
$ dune build
$ dune install
$ binsec -sse -sse-script verify.ini -tuto -tuto-mode builtin core.snapshot
...
[tuto:error] 0x7ffff7da0aaf <__memmove_avx_unaligned_erms+0x26f>: Stack tampering
             (goto 0x0000000000000009, expected 0x00000000004011bf)
             --- Model ---
             
             # Array stdin
             #x0000000000000407 : 00 
             #x0000000000000406 : 00 
             #x0000000000000405 : 00 
             #x0000000000000404 : 00 
             #x0000000000000403 : 00 
             #x0000000000000402 : 00 
             #x0000000000000401 : 00 
             #x0000000000000400 : 00 
...
```

Here again, it properly reports the stack tampering that overwrites the return address of the function `memmove`.

### Conclusion

Congratulations, you successfully implemented your first **BINSEC** SSE plugin.
You have seen most of what can be done using both the native and *builtin* instructions. *The two approaches can of course be combined if needed.*  

To go further, the types and interfaces to look at are [`Cli`](../../src/cli/cli.mli) for command line arguments, [`Types`](../../src/sse/types.mli) for the types (including `PLUGIN` signature and list of available callbacks), [`Ir`](../../src/sse/loader/ir.mli) for the `GRAPH` definition, [`Ast`](../../src/script/ast.mli) and [`Script`](../../src/sse/script.mli) for parser types, and [`Path`](../../src/symbolic/path.mli) for the definition of the symbolic path.
Other plugin examples can also be found [in the source tree](../../src/sse/plugin/).

*Now, will you be able to implement your own monitor?*  
Have a nice day :-)

