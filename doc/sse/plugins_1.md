# SSE Tutorial \#5: Monitoring the symbolic execution with a custom plugin (part 1)

As wonderful as the **BINSEC** symbolic execution engine is, you cannot help but feel something is missing? Of course, it may happen that **BINSEC** does not collect the data you want or perform the critical check you need.
The good news is **BINSEC** provides an extension mechanism to help monitoring the symbolic execution without going too deep into its internals.  
*We still have to get hands a little dirty with `OCaml` code, though.*

In this post, we will review step by step the process of making a custom plugin.
We will illustrate it with the example of adding a shadow stack protection to the standard execution.

*This post has been updated for the plugin API revision starting from **BINSEC** version `0.11`. For earlier versions, refer to the [previous writing](https://github.com/binsec/binsec/blob/5f1402e92cb6c27a429d2043d2f0d0838b2bc1e1/doc/sse/plugins.md).*

### Shadow stack

*What is a shadow stack and what is its purpose?*

In computer security, a shadow stack is a second stack that ideally lives in a separate memory region and that keeps track of the return addresses of the called functions. Each time a function returns, the protection mechanism ensures that both copies of the return address agree. This is particularly effective against buffer overflow vulnerabilities that result in a stack tampering.

*What a nice feature to add to **BINSEC**, is not it?*


### Requirements

To write a plugin, you will need:
- an **OCaml** development environment, preferably with `merlin` and `ocamlformat` enabled;
- the **BINSEC** tool version `O.11.0` or greater (see [install instructions](../../INSTALL.md) or `opam install binsec`);

We recommend to use a local switch to create your plugins. To do so, run:
```console
$ cd <plugin-path>
$ opam switch create . 4.14.2
$ eval $(opam env)
$ opam install binsec
```

### Source tree

We will start from the following directory structure.

```
<plugin-path>
├── dune-project
├── dune
├── shadow_stack.ml
└── registration.ml
```
#### dune-project
```
(lang dune 3.17)

(using dune_site 0.1)

(package
 (name tuto))
```
#### dune

```
(library
 (public_name tuto.shadow_stack)
 (name shadow_stack)
 (modules shadow_stack)
 (libraries binsec.sse))

(library
 (public_name tuto.registration)
 (name registration)
 (modules registration)
 (libraries binsec.cli.sse tuto.shadow_stack))

(plugin
 (name tuto)
 (libraries tuto.registration)
 (site
  (binsec plugins)))
```

#### shadow_stack.ml

The following code defines the skeleton of an empty plugin named `tuto`.
While not stricly required, we will use the intermediate module `V1` of type `EXTENSIONS` to encapsulate related values and helper functions.

```ocaml
module ID = struct
  let name = "tuto"
end

(* Import the definition of [ENGINE], [EXTENSIONS], [PLUGIN], [field] and [extension] types. *)
open Binsec_sse.Types

module V1 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct
  type path = Engine.Path.t

  let list = []
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
```

#### registration.ml

The following code uses the module `Cli` to declare a new *command line* namespace `tuto`.
It then registers the above shadow stack plugin to be installed when the function `is_enabled` returns `true`, i.e. when we invoke **BINSEC** using the the command-line option `-tuto`.

```ocaml
module Namespace = Binsec_cli.Cli.Make (struct
  let name = "Shadow stack tutorial"
  let shortname = "tuto"
end)

let () =
  Binsec_cli_sse.Plugins.register ~is_enabled:Namespace.is_enabled (fun () ->
      (module Shadow_stack.Plugin_v1))
```

#### Compile

Lets compile and register to **BINSEC** this empty plugin.
To do so, assuming you use the local switch with `binsec` installed, you can run the following.
```console
$ cd <plugin-path>
$ eval $(opam env)
$ dune build
$ dune install
```

Now the **BINSEC** command line interface should include your plugin. To check it, run:
```console
$ binsec -tuto-help
* Help for Shadow stack tutorial (tuto)  
  
  -tuto               Enable tuto
  -tuto-debug-level   Set tuto debug level [0]
  -tuto-help          Display options list for tuto
  -tuto-loglevel {info|debug|warning|error|fatal|result}
          Display tuto log messages only above or equal this level [info]
  -tuto-quiet         Quiet all channels for tuto
```

### Step 1: adding check instructions

For now, our plugin does nothing since it implements no extension.
Extensions can be added in the `list` field of the `V1` module. They are of type `'path extension` (defined in [`Types`](../../src/sse/types.mli)).

We are first interested in the ability to automatically add instrumentation at *call* and *return* sites.
To do so, we are going to use the **Instrumentation_routine**:

```ocaml
Instrumentation_routine of (Revision.t -> unit)
```

This callback will be called each time disassembly meets new instructions.
It takes the control flow graph of the disassembly unit and can be used to **insert new instructions** on it.

The signature of the graph is given in the [`Ir`](../../src/sse/loader/ir.mli) module . Here, we will use `iter_new_vertex` and `insert_list_before` to add the shadow stack logic.


We will create our shadow stack using a separated memory array (that we will name `shadow_stack`) and a cursor over it (that we will name `shadow_stack_pointer`).

At *call* sites, we want to push the return address as follows.
```
shadow_stack[shadow_stack_pointer, sizeof(RETURN_ADDRESS)] := RETURN_ADDRESS
shadow_stack_pointer := shadow_stack_pointer + sizeof(RETURN_ADDRESS)
```

The corresponding **OCaml** code is the following:

```ocaml
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

      (* shadow_stack[shadow_stack_pointer, sizeof(RETURN_ADDRESS)] := RETURN_ADDRESS *)
      Store
        {
          base = shadow_stack_name;
          dir = LittleEndian;
          addr = stack_pointer_exp;
          rval = expr_of_addr ret_addr;
        };

      (* shadow_stack_pointer := shadow_stack_pointer + sizeof(RETURN_ADDRESS) *)
      Assign
        {
          var = stack_pointer_var;
          rval = Dba.Expr.addi stack_pointer_exp offset;
        };

    ]

  let list = []
end
```

Here, we first query information from the `Machine` module to determine the size of an address.
Then, we define a new **BINSEC** symbolic array name `shadow_stack` and the associated variable `stack_pointer`.
Finally, the `push` function returns the sequence of instructions that store the value of the return address and increment the stack pointer afterward.
Observe that the `Store` instruction uses our custom `shadow_stack` name as a base array instead of the default `@` memory (i.e. `base = None`).

We can now update the list of extension with an `Instrumentation_routine`:

```ocaml
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
    ]
```

:information_source: Here, the function `Instrumentation_routine`  takes a graph of type `Revision.t` and iterates over the new vertices (the ones that have been added since the last callback), looking for the instruction kinds `Goto` (static jump) and `Jump` (dynamic jump). We are taking advantage of the tag information which indicates if a jump instruction belongs to a `Call` site.

We then need to instrument the *return* sites.

At *return* sites, we need to check that the value we are going to jump to is the same as the saved one.
We will do it with the following pseudo-code:
```
assert stack_pointer > 0
shadow_stack_pointer := shadow_stack_pointer - sizeof(RETURN_ADDRESS)
shadow_stack_witness := shadow_stack[shadow_stack_pointer, sizeof(RETURN_ADDRESS)]
assert shadow_stack_witness = JUMP_TARGET
```

We can add the following **OCaml** code to implement this logic:

```ocaml
  module V1 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct

  (* [ ... ] *)

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
      (* [ ... ] *)
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
    ]
end
```

Return site instrumentation works the same as call site instrumentation except we are now looking for the tag `Return` instead of `Call`.

Last things to do is to initialize the value of the new variable `shadow_stack_pointer`.
To do so, we are going to use is the **Initialization_callback**:

```ocaml
Initialization_callback of ('path -> unit)
```

This callback will be called during the initialization of the symbolic engine.
It can be used to declare and define the additional variables our plugin will use.
For now, we will set the initial pointer to `0` (i.e. an empty call stack at entry).

```ocaml
  module V1 (Engine : ENGINE) : EXTENSIONS with type path = Engine.Path.t = struct

  (* [ ... ] *)

  let list =
    [
      (* Instrument call sites *)
      (* [ ... ] *)
      (* Instument return sites *)
      (* [ ... ] *)
      (* Initialization *)
      Initialization_callback
        (fun path -> Engine.Path.assign path stack_pointer_var zero_exp);
    ]
end
```

### Step 2: extending the script parser

Our shadow stack is now functional but there still is an issue.
The stack is initialized empty which prevents us from beginning the symbolic exploration from the middle of a function.
We have to find a way for the user to declare the initial state of the shadow stack.

Especially, we want to extend the script grammar to add a new declaration statement rule with the following syntax.
```
decl:
| 'initial' 'call' 'stack' expr [',' expr ]*
```

To this end, we have to perform the following actions:
- extend the `Ast` type with a new statement kind
```ocaml
type Ast.t += Initial_stack of Ast.Expr.t Ast.loc list
```
- register a printer for this new statement
```ocaml
Command_printer of (Format.formatter -> Ast.t -> bool)
```
- register a callback for it
```ocaml
Command_handler of (Ast.t -> env -> 'path -> bool)
```
- extend the script grammar to be able to use it in configuration scripts
```ocaml
Grammar_extension of ( unit, obj, unit, unit, obj Dyp.dyplexbuf ) Dyp.dyp_action list
```

The following code implements these four actions.

```ocaml
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
  (* [ ... ] *)

  let list =
    [
      (* Instrument call sites *)
      (* [ ... ] *)
      (* Instument return sites *)
      (* [ ... ] *)
      (* Initialization *)
      (* [ ... ] *)
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
                     Engine.Path.store path shadow_stack_name ~addr:stack_pointer_exp addr
                       LittleEndian;
                     Dba.Expr.addi stack_pointer_exp offset)
                   zero_exp values);
              true
          | _ -> false);
    ]
end
```

#### Command_printer

Easy one, the printer takes a statement in input, pretty prints it and returns if it was able to handle it.

#### Command_handler

This callback works in a similar way to the initialization one. The difference is it additionally takes a statement and the parser environment and returns true if the command (here the initial stack declaration) has been treated, false otherwise.

Here, we iterate over the script expressions given to the `Initial_stack` statement, we use the function `eval_expr` to convert them into `Dba` expressions and store their values in the `shadow_stack` array. Each time, we increment the value of the stack pointer and finally update it when all entries have been processed.

#### Grammar_extension

The last step is to make the parser recognize and generate our new statement.
**BINSEC** is using the [**dypgen**](http://dypgen.free.fr/dypgen-doc.pdf) self-extensible parser generator.

Here, we are using three *token* words (`initial`, `call` and `stack`) followed by a rule that is already defined in the script syntax `comma_separated_expr_rev_list`.

:information_source: For more information, please read the section 6 *Dynamic extension of the grammar* in the **dypgen** [documentation](http://dypgen.free.fr/dypgen-doc.pdf). The existing rules are defined in [syntax.dyp](../../src/script/syntax.dyp) and in the [`Script`](../../src/sse/script.ml) module.

### Testing the plugin

Our shadow stack is ready to be put to the test. The final version of the plugin can be found in the folder [doc/sse/shadow_stack](shadow_stack).
The [uboot](../../examples/sse/uboot/uboot.c) code is a small example of buffer overflow inspired by a [CVE](https://nvd.nist.gov/vuln/detail/CVE-2019-14192).

First we need to compile and install our new plugin. It can be done as follows.
```console
$ dune build
$ dune install
```

We also have to compile the example. Let us go to its folder [examples/sse/uboot](../../examples/sse/uboot).
```console
$ make uboot.run
```

In order to use the given [script](../../examples/sse/uboot/verify.ini), we have to generate a *core dump*. We can use the [utils/make_coredump.sh](../../utils/make_coredump.sh) to do so.
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

:information_source: **BINSEC** returns a model where the declared message size is `0` (i.e. the `uint64_t` value read at the offset 1024 of the input stream). Since the code subtracts  the `HEADER_SIZE` without any check, it results in an integer underflow and copies more bytes than the buffer can contain.

### Conclusion

So, the shadow stack is working perfectly!

However, there is still something that have not been addressed yet. Here, we made our shadow stack using only standard DBA assertions, but the plugin mechanism of **BINSEC** also allows to add custom *builtin* instructions to instrument the symbolic execution.
In [the following tutorial](plugins_2.md), you will find how to create another version of the shadow stack using custom `Push` and `Pop` builtins.