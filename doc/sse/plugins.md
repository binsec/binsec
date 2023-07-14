# SSE Tutorial \#5: Monitoring the symbolic execution with a custom plugin

As wonderful as the BINSEC symbolic execution engine is, you cannot help but fell something is missing? Of course, it may happens that BINSEC does not collect the data you want or perform the critical check you need.
The good news is BINSEC provides an extension mechanism to help monitoring the symbolic execution without going too deep into its internals.  
*We still have to get hands a little dirty with `OCaml` code, though.*

In this post, we will review step by step the process of making a custom plugin.
We will illustrate it with the example of adding a shadow stack protection to the standard execution.


### Shadow stack

*What is a shadow stack and what is its purpose?*

A shadow stack is a second stack that ideally lives in a separate memory region and that keeps track of the return addresses of the called functions. Each time a function returns, the protection mechanism then ensures that both copies of the return address agree. This is particularly effective against buffer overflow vulnerabilities that result in a stack tampering.

*What a nice feature to add to BINSEC, is not it?*


### Requirements

To write a plugin, you will need:
- an **OCaml** development environment, preferably with `merlin` and `ocamlformat` enabled (it is recommended to create a [local switch](../../INSTALL.md#with-make) with `make switch`);
- the **BINSEC** tool (see [install instructions](../../INSTALL.md) or `opam install binsec`);

### Source tree

We will start from the following directory structure.

```
.
├── dune-project
├── dune
└── tuto.ml
```
#### dune-project
```
(lang dune 3.0)
(using dune_site 0.1)

(package
 (name tuto))
```
#### dune
```
(library
 (public_name tuto.shadow_stack)
 (name tuto)
 (modules tuto)
 (flags
  (:standard -w "-58"))
 (libraries binsec.sse))

(plugin
 (name tuto)
 (libraries tuto.shadow_stack)
 (site
  (binsec plugins)))
```
#### tuto.ml

The following code declares a new *command line* namespace `tuto` and registers an empty plugin (also named `tuto`).

```ocaml
open Binsec
open Libsse
open Types

include Cli.Make (struct
  let name = "Shadow stack tutorial"

  let shortname = "tuto"
end)

let () =
  Exec.register_plugin
    (module struct
      let name = "tuto"

      let grammar_extension = []

      let instruction_printer = None

      let declaration_printer = None

      let extension :
          type a b.
          (module EXPLORATION_STATISTICS) ->
          (module Path.S with type t = a) ->
          (module STATE with type t = b) ->
          (module Exec.EXTENSION with type path = a and type state = b) option =
       fun _stats path state ->
        if is_enabled () then
          Some
            (module struct
              module P = (val path)

              module S = (val state)

              type path = P.t

              and state = S.t

              let initialization_callback = None

              let declaration_callback = None

              let instruction_callback = None

              let process_callback = None

              let builtin_callback = None

              let builtin_printer = None

              let at_exit_callback = None
            end)
        else None
    end : Exec.PLUGIN)
```

:information_source: The function `is_enabled` returns `true` when we use the switch `-tuto` when invoking **BINSEC**.

### Step 1 : adding check instructions

For now, our plugin does nothing since it implements no callback.
The first thing we are interested in is the ability to automatically add check instructions at *call* and *return* sites.

To do so, we have to look at the following callback.

```ocaml
val initialization_callback : (path -> state -> state) option

val process_callback :
  ((module Ir.GRAPH with type t = 'a) -> ('a -> unit)) option
```

#### initialization_callback

This callback will be called during the initialization of the symbolic engine.
It can be used to declare and define the additional variables our plugin will use.

Here, we will make our shadow stack using a separated memory array (we will name `shadow_stack`) and a cursor over it (we will name `shadow_stack_pointer`).

```ocaml
let pointer_size = Kernel_options.Machine.word_size ()

let array = "shadow_stack"

let ptr =
  Dba.Var.create "shadow_stack_pointer"
    ~bitsize:(Size.Bit.create pointer_size)
    ~tag:Temp
	  
let initialization_callback =
  Some
    (fun _ state ->
      let state =
        (* declare our memory array *)
        S.alloc ~array state
      in
      S.assign ptr (* initialize the cursor position *)
        (S.Value.constant (Bitvector.zeros pointer_size))
        state)
```

#### process_callback

This callback will be called each time disassembly meets new instructions.
It takes the control flow graph of the disassembly unit and can be used to insert new instructions on it.

The signature of the graph is given in the `Ir` module. Here, we will use `iter_new_vertex` and `insert_before` to add the shadow stack logic.

At *call* site, we want to push the return address as follows.
```
shadow_stack[shadow_stack_pointer, POINTER_SIZE] := RETURN_ADDRESS
shadow_stack_pointer := shadow_stack_pointer + POINTER_SIZE
```
At *return* site, we need to check the value we are going to jump is the same as the saved one.
```
assert shadow_stack_pointer > 0
shadow_stack_pointer := shadow_stack_pointer - POINTER_SIZE
shadow_stack_witness := shadow_stack[shadow_stack_pointer, POINTER_SIZE]
assert shadow_stack_witness = JUMP_TARGET
```

The corresponding **OCaml** code is the following.
```ocaml
let witness =
  Dba.Var.create "shadow_stack_witness"
    ~bitsize:(Size.Bit.create pointer_size)
    ~tag:Temp

let ptr_r = Dba.Expr.v ptr

and witness_r = Dba.Expr.v witness

let zero = Dba.Expr.constant (Bitvector.zeros pointer_size)

and offset =
  Dba.Expr.constant
    (Bitvector.of_int ~size:pointer_size (pointer_size / 8))

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
```
:information_source: Here, the function `process_handler` is a function that takes a `GRAPH` module and returns its matching function callback. At the same time, we define an helper function `insert_return_check` that will add the check instructions at the `Return` sites. Our callback then simply iterates over the new vertex (the ones that have been added since the last callback), looking for the instruction kinds `Goto` (static jump) and `Jump` (dynamic jump). We are taking advantage of the tag information which indicates if a jump instruction belong to a `Call` site or a `Return` site.

### Step 2 : extending the script parser

Our shadow stack is now functional but there still is an issue. The stack is initialized empty which prevents us from beginning the symbolic exploration from the middle of a function. We have to find a way for the user to declare the initial state of the shadow stack.

To this end, we can extend the script with a new kind of statement.
```ocaml
type Ast.t += Initial_stack of Ast.Expr.t Ast.loc list
```
We now have to perform the following actions.
- register a printer for our new extension
```ocaml
val declaration_printer : (Format.formatter -> Ast.t -> bool) option
```
- register a callback for it
```ocaml
val declaration_callback :
  (Ast.t -> Script.env -> path -> state -> state option) option
```
- extend the script grammar
```ocaml
val grammar_extension :
  ( unit,
    Libparser.obj,
    unit,
    unit,
    Libparser.obj Dyp.dyplexbuf )
  Dyp.dyp_action
  list
```

#### declaration_printer

The printer takes a statement in input, pretty prints it and returns if was able to handle it.
```ocaml
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
```

#### declaration_callback

This callback works in a similar way to the initialization one. The difference is it additionally takes a statement and the parser environment and returns a state only if it can handle the extension.

The following code initialize the stack with the addresses given in argument.
```ocaml
module Eval = Eval.Make (P) (S)

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
```
:information_source: Here, we are using the module `Script` to evaluate a expression coming from the parser (`Ast.Expr.t Ast.loc`) and the module `Eval` to convert **DBA** expressions into symbolic values.

#### grammar_extension

The last step is to make the parser recognize and generate our new statement.
**BINSEC** is using the [**dypgen**](http://dypgen.free.fr/dypgen-doc.pdf) self-extensible parser generator.  
We want to define a new declaration statement rule with the following syntax.
```
decl:
| 'initial' 'call' 'stack' expr [',' expr ]*
```
First three words are going to be tokens while we will use the already defined rule `comma_separated_expr_rev_list` for the last part. The rule can be written as follows.
```ocaml
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
```
:information_source: For more information, please read the section 6 *Dynamic extension of the grammar* in the **dypgen** [documentation](http://dypgen.free.fr/dypgen-doc.pdf). The existing rules are defined in [syntax.dyp](../../src/ast/parser/syntax.dyp) and in the [`Script`](../../src/sse/script.ml) module.

### Testing the plugin

Our shadow stack is ready be put to the test. The [code](../../examples/sse/uboot/uboot.c) is a small example of buffer overflow inspired by a [CVE](https://nvd.nist.gov/vuln/detail/CVE-2019-14192).

First we need to compile and install our new plugin. It can be done as follow.
```console
$ dune build
$ dune install
```

We also have to compile the example. Let us go to its folder [examples/sse/uboot](../../examples/sse/uboot).
```console
$ make uboot.run
```

In order to use the given [script](../../examples/sse/uboot/verify.ini), we have to generate a *core dump*. We can use the [make_coredump.sh](../../utils/make_coredump.sh) to do so.
```console
$ make_coredump.sh core.snapshot uboot.run
```

Now, the following command will run **BINSEC** with our plugin activated. It properly reports a stack tampering that overwrites the return address of the function `memmove`.
```console
$ binsec -sse -sse-script verify.ini -tuto core.snapshot
...
[sse:error]  Assertion failed @ 0x7ffff7da0baf
              --- Model ---
              
              -- empty memory --
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

### Step 3 : register a command line argument

The shadow stack is now perfectly working! Yet, there is still one important point that has not been addressed: the *builtin* instructions.
But rather than replacing the current version, we are going to propose a new one that can coexist with it.

```ocaml
type mode =
  | Inline  (** Use standard DBA assertions *)
  | Builtin  (** Use new push and pop builtins *)
```

To do so, we will create an additional command line switch using the module `Builder`.
```ocaml
module Mode = Builder.Variant_choice_assoc (struct
  type t = mode

  let name = "mode"

  let doc =
    "Use standard DBA assertions [Inline] or new push and pop builtins \
     [Builtin]"

  let default = Inline

  let assoc_map = [ ("inline", Inline); ("builtin", Builtin) ]
end)
```

:information_source: Here, we use `Variant_choice_assoc` to directly map the type `mode`. Other argument kinds include `Boolean`, `Integer`, `String`, etc.

We change the way the plugin in instantiated as follow:
```ocaml
if is_enabled () then
  match Mode.get () with
  | Inline -> Some (module struct (* previous code *) end)
  | Builtin ->
    Some
	  (module struct
        module P = (val path)

        module S = (val state)

        type path = P.t

        and state = S.t

        let initialization_callback = None

        let declaration_callback = None

        let instruction_callback = None

        let process_callback = None

        let builtin_callback = None

        let builtin_printer = None

        let at_exit_callback = None
      end)
else None
```

Now, all we have to do is to fill the second module and we will be able to control the mode with the switch `-tuto-mode [inline|builtin]`.

### Step 4 : adding new builtin instructions

Shadow stack can be implemented using only **DBA** primitives, but sometimes, it may be necessary to do things that are not covered by the core language.
We will illustrate this with the example of the `Push` and `Pop` instructions together with dedicated error messages. 

```ocaml
type Ir.builtin += Push of Dba.Expr.t | Pop of Dba.Expr.t
```

Just like before, we will use the `process_callback` to insert our new primitives at *call* and *return* sites.

```ocaml
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
```

We now have to perform the following actions.
- extend the path to be able to store our stack
```ocaml
val register_key : ?merge:('a -> 'a -> 'a option) -> 'a -> 'a key
```
- register a printer for our new extension
```ocaml
val builtin_printer : (Format.formatter -> Ir.builtin -> bool) option
```
- register a callback for it
```ocaml
val builtin_callback :
  (Ir.builtin ->
  (Virtual_address.t ->
  path ->
  int ->
  state ->
  (state, Types.status) Result.t)
  option)
  option
```

#### register_key

The *path* can be use to store arbitrary data that will be local along a trace.  
:warning: *Be careful as we have to register the data type and give it a default value before the analysis start.*

```ocaml
let key = P.register_key []
```

The `key` will be used with the function `get` and `set` to respectively read and write our stack in the `path` variable given in the callbacks.

#### builtin_printer

Nothing new here, we simply have to define the pretty printer.
```ocaml
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
```

#### builtin_callback

Given a builtin, this callback should return the native **OCaml** function to execute during the symbolic execution (it returns a function only if it can handle the extension or `None` otherwise). The native function itself should returns a state result: either `Ok state` or `Error status`. The latter allows us to terminate a path if something goes wrong, as would have an assertion.

Here is the code to do this.
```ocaml
let push target _addr path _depth state =
  P.set key (target :: P.get key path) path;
  Ok state

let report addr target witness state =
  Logger.error "@[<v>%a: Stack tampering@ (goto %a, expected %a)@ %a@]"
    Virtual_address.pp addr Bitvector.pp_hex_or_bin
    (S.get_a_value target state)
    Bitvector.pp_hex_or_bin
    (S.get_a_value witness state)
    S.pp state

module Eval = Eval.Make (P) (S)

let pop target addr path _depth state =
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
```
The advantage of the *builtin* version is that we can report the stack tampering with dedicate error message instead of the generic assertion failure one.
Especially, we are able to both report the address we are going to jump to together with what was the expected return address.

#### declaration_callback

We must not forget to give a callback to our special statement `Initial_stack` in this version too.

```ocaml
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
```


### Conclusion

Congratulation, you successfully implemented a **BINSEC** SSE plugin.
You have seen most of what can be done using both the native and *builtin* instructions. *The two approaches can of course be combined if needed.*  
The full version of the shadow stack module can be found [here](../../src/sse/plugin/shadow_stack.ml).

To go further, the types and interfaces to look at are [`Cli`](../../src/base/cli.mli) for command line arguments, [`Types`](../../src/sse/types.ml) and [`Ir`](../../src/sse/ir.ml) for types and the `GRAPH` definition, [`Ast`](../../src/ast/ast.mli) and [`Script`](../../src/sse/script.mli) for parser types, [`Path`](../../src/sse/path.mli) and [`Exec`](../../src/sse/exec.mli) for `PLUGIN` signature and callbacks.

*Will you be able to implement your own monitor?*  
Have a nice day :-)

