# How to \#3: Reproduce the TOPS 2023 secret erasure experiments

In this post, we will exercise the **REL**ational **S**ymbolic **E**xecution engine of **BINSEC** to check that no secret data remains in memory after returning from a given function, also known as **secret-erasure** property.

 Indeed, a critical section should keep the secret data in memory no longer than necessary to prevent an attacker from exploiting a memory disclosure vulnerability and accessing the stored secret. Such code can thus use a so-called *scrubbing function* to fill the secret storage with public data (e.g. all zeros).
 Yet, the compiler may consider such writes as dead assignments and optimize them out, leaving the executable code unprotected.

 The relational analysis plugin `checkct` now proposes a **secret-erasure** check inspired by the [Daniel et al. paper](https://binsec.github.io/assets/publications/papers/2022-tops.pdf) to ensure that memory does not contain any secret past a given point, in the present case, a function return. The following presents how to configure **binsec** to run on the existing *properties vs compiler* benchmark [rel_bench](https://github.com/binsec/rel_bench/tree/main/properties_vs_compilers/secret-erasure). This benchmark offers more than one thousand `x86-32` binary challenges derived from 17 *scrubbing* functions, different compiler versions (`clang` and `gcc`) and multiple optimization levels (`-O0` to `-O3`).

### Setup

We need a working version of **BINSEC** version `0.10` or above with at least one SMT solver available (see [install instructions](../../INSTALL.md)).
Then we can download one of the binary challenges from the benchmark repository.

```console
$ wget https://github.com/binsec/rel_bench/raw/15ed83089660519858865d5a37b45b3a0e5f2195/properties_vs_compilers/secret-erasure/bin/secret-erasure_MEMORY_BARRIER_MFENCE_O3_gcc_8.3.0
$ chmod +x secret-erasure_MEMORY_BARRIER_MFENCE_O3_gcc_8.3.0
```

### Starting from a core dump

As discussed in the previous [How to](./advanced_users.md), starting from a core dump can help to deal with dynamically loaded library, but it can also ease the configuration of the symbolic execution engine.

We can use the [`make_coredump.sh`](../../utils/make_coredump.sh) to automatically generate a snapshot of the process at the entry of the `main` function.
```console
$ make_coredump.sh core.snapshot secret-erasure_MEMORY_BARRIER_MFENCE_O3_gcc_8.3.0
```

We then have to put the following in the script.
```text
starting from core
``` 

### Working without a test harness

In the previous [RELSE tutorial](./relse.md), we used global variables to define what were the secret and public data at the C level.
Here, it is not possible because we already have a compiled binary code. Yet, it is not a big issue for two reasons:
- first, **BINSEC** supports symbolic variable creation at any arbitrary place (e.g. *loc* `:=` *[* `nondet` *|* `secret` *]*);
- furthermore, the benchmark uses `libsym` functions (like `high_input_32`) to introduce new secret or public data -- we only need to stub them to support the whole benchmark.

So we can replace the functions as follows.
```text
replace <high_input_32> (ptr) by
  @[ptr, 32] := secret as high_input_32
  return
end

replace <low_input_32> (ptr) by
  @[ptr, 32] := nondet as low_input_32
  return
end
```

### Dealing with unsupported opcodes

While it tries to be as complete as possible, **BINSEC** may miss the semantic definition of an instruction.
When this issue arises, feel free to open an issue in the [GitHub repository](https://github.com/binsec/binsec/issues).
Alternatively, we can deal with it by replacing the opcode by a stub in the script, similarly to what we do for addresses or functions.

Here, for instance, we can stub the `mfence` instruction by a simple skip.
```text
replace opcode 0f ae f0 by 
end
```

### Secret erasure check

Finally, we can instruct the `checkct` plugin to check the *secret-erasure property* at the end of the function `main`.
```text
check secret erasure over <main>
```

For verification purpose, we should use the `explore all` goal to cover all the execution paths.
The minimal working example script ends up as follows.
```text
starting from core

replace <high_input_32> (ptr) by
  @[ptr, 32] := secret as high_input_32
  return
end

replace <low_input_32> (ptr) by
  @[ptr, 32] := nondet as low_input_32
  return
end

replace opcode 0f ae f0 by 
end

check secret erasure over <main>

explore all
```

We can then invoke **BINSEC** with the following command line.
```console
$ binsec -sse -checkct -sse-script check.ini core.snapshot
```

### Full benchmark evaluation

We automatized the benchmark evaluation with a [bash script](../../examples/relse/secret-erasure/analyze.sh).
It automatically downloads the challenges and outputs the analysis result in the same format as the original version.

```console
$ wget https://raw.githubusercontent.com/binsec/rel_bench/15ed83089660519858865d5a37b45b3a0e5f2195/properties_vs_compilers/secret-erasure/all-opt.log
$ ./analyze.sh all-opt > all-opt-new.log
$ diff all-opt-new.log all-opt.log
```

:information_source: We can split the SE script in several files for readability or reusability purpose. Here, for instance, the analysis reads the definition from the three scripts [check.in](../../examples/relse/secret-erasure/check.ini), [libsym.in](../../examples/relse/secret-erasure/libsym.ini) and [mfence.ini](../../examples/relse/secret-erasure/mfence.ini).

:information_source: If your script contains more stubs that the functions present in the binary, **BINSEC** will complain and stop as it cannot find the extra symbols. We can suppress this error setting the option `-sse-missing-symbol` to either `warn` or `quiet`.

### Conclusion

We have seen how easy it is to run the experiments from the legacy version of [**BINSEC/Rel**](https://github.com/binsec/Rel).
The `checkct` plugin benefits from the **BINSEC** platform progress, including performance improvements and new architecture supports and it should open up new opportunities for verification of the *secret erasure property*.

Have a nice day :-)

