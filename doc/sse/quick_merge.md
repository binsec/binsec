# How to \#4: Merge sibling paths with a plugin

In this post, we will introduce some plugin extensions that allow us to interact with the path management of the symbolic execution engine.
It is recommended to be familiar with the core concepts of plugins; if you have not read the first two plugin tutorials ([part 1](plugins_1.md), [part 2](plugins_2.md)), you should take a look at them before going further.

We will see how to implement the quick path merging strategy brought in with **BINSEC** version [0.7](https://github.com/binsec/binsec/releases/tag/0.7.1), yet now using the new plugin interface.

### Quick path merging

*What is the quick path merging strategy?*

During the program exploration, the symbolic engine forks and explores the two sides of a branch each time it encounters a symbolic condition. As the number of branches grows, the total number of possible paths increases exponentially. This rapid growth in execution paths -- known as **path explosion** -- is one of the main bottlenecks of symbolic execution.

An effective approach to mitigate this issue is **path merging**. Instead of exploring each execution path independently, the symbolic engine can combine back multiple paths that reach the same program point. Doing so reduces the number of active paths and allows the symbolic engine to share the exploration of the common trace suffix, avoiding duplicated work. Yet, path merging comes with a trade-off since merged paths may be harder to reason about. Deciding *where*, *when* and *how* to merge paths efficiently is a challenge on its own and has been heavily studied by academic researchers.

*We do not claim to solve this challenge.* Instead, we proposed a simple, opportunistic, yet sometimes very effective, heuristics to decide *where* and *when* to merge sibling paths produced by small *if-then-else* statements (a.k.a diamond patterns). The idea is as follows: when a path forks, we fast-forward the two sides of the branch until they encounter a new forking point (i.e. a symbolic condition that needs to be solved). If both sides end at the same program point, we merge them, otherwise we just continue as normal.

This strategy has been added in **BINSEC** in an ad-hoc manner starting from [0.7](https://github.com/binsec/binsec/releases/tag/0.7.1), but is now implemented as a [plugin](../../src/sse/plugin/binsec_sse_quick_merge.ml). Let us see how it is possible.

### Where to merge

The first thing to do is to declare a new builtin for the *join* operation and to instrument the program with it.  
The simplicity of our heuristic makes it easy, we will uniformly insert our `Join` builtin before the control-flow instructions (conditional branches and jumps).

```ocaml
type Ir.builtin += Join of Dba.Expr.t
```
```ocaml
      Instrumentation_routine
        (fun graph ->
          Revision.iter_new_vertex
            (fun vertex ->
              match Revision.node graph vertex with
              | Fallthrough { kind = Assume exp | Assert exp; _ }
              | Branch { test = exp; _ }
              | Terminator { kind = Jump { target = exp; _ }; _ } ->
                  Revision.insert_before graph vertex (Builtin (Join exp))
              | _ -> ())
            graph);
```

### When to merge

We want to merge sibling paths so we will use the `Fork_callback` to track the two sides of the branch.
First, we will extend the path with two additional fields: `Sibling` and `Waiting`.

```ocaml
type ('value, 'model, 'state, 'path, 'a) field_id +=
  | Sibling : ('value, 'model, 'state, 'path, 'path option) field_id
  | Waiting : ('value, 'model, 'state, 'path, bool) field_id
```
```ocaml
  let fields _ =
    [
      Field { id = Sibling; default = None; copy = None; merge = None };
      Field { id = Waiting; default = false; copy = None; merge = None };
    ]
```

The former (of type `'path option`) will remember if the path is linked to another one.
The second (of type `bool`) will indicate if the path is waiting at a join point.

We can now add our `Fork_callback` to update our new fields.

```ocaml
      Fork_callback
        (fun path0 path1 ->
          Option.iter release (Path.get path0 sibling);
          Path.set path0 sibling (Some path1);
          Path.set path1 sibling (Some path0));
```

Here, our callback simply adds `path1` as a sibling to `path0` and vice versa.
We will come back later to the role of the `release` function.

First, we have to implement the merge logic.
Following our main idea, we first check if the path is linked to a sibling and if the condition is a forking point.
For the latter, we use the function `Path.is_symbolic` which returns `true` when the symbolic term may take multiple values.
If both conditions are met, we check if the sibling is already waiting. If so, we can ask for a path merging using the `Merge` continuation.
Otherwise, we mark the path as *waiting* and remove it from the *running path worklist* using the continuation `Signal Stashed`. 
In all other cases, we simply continue using the `Return` continuation.

```ocaml
      Builtin_resolver
        (function
        | Join exp ->
            Call
              (fun path0 ->
                match Path.get path0 sibling with
                | Some path1 when Path.is_symbolic path0 exp ->
                    Path.set path0 sibling None;
                    if Path.get path1 waiting then (
                      Path.set path1 waiting false;
                      Merge ([ path1 ], Return))
                    else (
                      Path.set path0 waiting true;
                      Signal Stashed)
                | None | Some _ -> Return)
        | _ -> Unknown);
```

### Path management

We are almost done, but there is still a minor issue: *what is happening if one of the siblings dies or forks before a join point*?
To keep it simple, our logic operates on an exclusive pair of paths so if such an event happens, we need to *release* the other side.

Here is the definition of the `release` function. We start by removing its sibling, then, if the path is currently *waiting* (i.e. has been `Stashed`), we resume its execution with the function `Engine.resume` which adds it back to the *running path worklist*.

```ocaml
    let release path =
      Path.set path sibling None;
      if Path.get path waiting then (
        Logger.info "wake up";
        Path.set path waiting false;
        Engine.resume path Return)
```

We already used the `release` function in the `Fork_callback` to handle the *double fork issue*. For the other case, we can use the `Signal_callback` to catch execution interruptions.

Here, if a path receives a signal other than a *wait request*, it will *release* its sibling.

```ocaml
      Signal_callback
        (fun path0 status ->
          match Path.get path0 sibling with
          | Some path1 when status <> Stashed || not (Path.get path0 waiting) ->
              release path1
          | None | Some _ -> ());
```

### Conclusion

The complete code of the plugin can be found [here](../../src/sse/plugin/binsec_sse_quick_merge.ml).
As demonstrated, the plugin API allows us to implement the quick merge strategy within 100 lines of code (boilerplate included!)
Of course, the logic here is quite limited, but it illustrates the core concepts behind the **path management** extensions.  
*Would you be able to implement a smarter merge strategy?*

Have a nice day :-)