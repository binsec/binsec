# SSE Tutorial \#4: Checking *constant-time* security property

In this post, we will exercise the **REL**ational **S**ymbolic **E**xecution engine of **BINSEC** to check that a given function body conforms to the **constant-time** paradigm.  

Constant-time programming is one of the ways to ensure that a given implementation does not leak information over any sensitive data (e.g. private cryptographic keys) through a *timing* side channel.

Indeed, a run may take more or less time depending on the path taken (control flow leak), and on almost any modern processor, timing may also vary depending on its cache behavior (memory access leak).

**BINSEC** verifies that no sensitive data (tagged as **secret**) affects the control flow decision: whatever the *secrets*, two runs with the same other inputs (tagged as **public**) will follow the same path. At the same time, it also verifies that these two runs will always access the same addresses in memory.

We will illustrate this with the function `memcmp`.
You can find everything about this tutorial in [/examples/relse/quickstart](../../examples/relse/quickstart).

### Requirements

To run to completion we will need:
- the **BINSEC** tool (`>= 0.8`, see [install instructions](../../INSTALL.md));
- a SMT solver, preferably [**ocaml-bitwuzla**](../../INSTALL.md#Dependencies)
  (but would work with **bitwuzla**, **boolector**, **Z3**, etc.).

### The `memcmp` function

We will use the usual definition of the function.
```c
int memcmp(const void *s1, const void *s2, size_t n);
```
The `memcmp` function compares the first `n` bytes of the memory areas `S1` and `s2`. It returns an integer less than, equal to, or greater than zero if the the first `n` bytes of `s1` is found, respectively, to be less than, to match, or be greater than the first `n` bytes of `s2`.

### The test harness

In order to ease the verification process, we will rely on the following test harness.

```c
#include <stdlib.h>

int memcmp(const void *s1, const void *s2, size_t n); /* function under analysis */

#define SIZE (1 << 4) /* 16B */
char s1[SIZE];   /* secret buffer */
char s2[SIZE];   /* public buffer */
size_t n = SIZE; /* public variable */

int main(int argc, char *argv[])
{
	int res = memcmp(s1, s2, n);
	
	/* ensure the result is in [-1..1] */
	res |= res >> 1;
	res |= res >> 2;
	res |= res >> 4;
	res |= res >> 8;
	res |= res >> 16;
	res = (res & 1) | (res >> 31);
	
	exit(res); /* identify halting point and prevent compiler optimizations */
}
```
It just wraps the call to `memcmp`, performs a constant-time reduction of its result before calling the `exit` function.

:information_source: *It is not strictly necessary but using the result of `memcmp` as an argument to `exit` avoid compiler optimizations to remove the call.*

This test harness will allow us to configure more easily the relational engine of **BINSEC**. Indeed, it gives us a well known starting point (`main`), a named ending point (`exit`) and several global symbols to mark as **public** or **secret** (`s1`, `s2` and `n`).

Let us write the corresponding script.

```
starting from <main>

concretize stack
secret global s1
public global s2, n

assume 0 < n < 10

halt at <exit>
reach all
```

The analysis will start from `main`, trying to explore all path that reach `exit`. We mark the variable `s1` as `secret` while `s2` and `n` are tagged `public`.

:information_source: *We can further add constraints on these symbolic variables. For instance, here we limit the number of rounds (`size`) in the range`[1..9]`.* 

:information_source: *We also concretize the stack pointer. Otherwise, the analysis would likely be stuck reasoning on non meaningful memory aliasing.*

### First (*non constant-time*) candidate

Here is a common way to implement the function `memcmp`.

```c
#include <stddef.h>

int memcmp(const void *s1, const void *s2, size_t n)
{
    const char *p1 = (const char *)s1, *p2 = (const char *)s2;
    for (size_t i = 0; i < n; i += 1) {
        if (p1[i] < p2[i]) return -1;
        else if (p1[i] > p2[i]) return 1;
    }
    return 0;
}
```

Let us run **BINSEC** on it.  
First, we need to compile it to obtain its binary version.
```console
$ gcc -g -m32 -static test_harness.c candidate_1.c -o candidate_1
```

Then we can run the following command.
```console
$ binsec -sse -checkct -sse-script checkct.cfg candidate_1
```

It will output the following.
```console
[checkct:result] Instruction 0x080497dc has control flow leak (0.045s)
[checkct:result] Instruction 0x080497fd has control flow leak (0.047s)
...
[checkct:result] Program status is : insecure (0.083)
[checkct:info] 27 visited paths covering 91 instructions
[checkct:info] 39 / 41 control flow checks pass
[checkct:info] 882 / 882 memory access checks pass
```

So now, we know that this candidate for `memcmp` is not constant time.
We can go further by asking **BINSEC** detailed models of the leaks found using the option `-checkct-stats-file`.

```console
$ binsec -sse -checkct -sse-script checkct.cfg -checkct-stats-file candidate_1.toml candidate_1
```
The report contains entries for each detected leak as show in the following.

```toml
["CT report"."Insecurity models".0x080497dc.public]
ebp = ["0x29f7b444"]
ebx = ["0x1e1f5018"]
n = ["0x00000009"]
s2 = ["0x0000000000000000000000003ee7ed5f"]
["CT report"."Insecurity models".0x080497dc.secret1]
s1 = ["0x000000000000000000000000210c23b1"]
["CT report"."Insecurity models".0x080497dc.secret2]
s1 = ["0x993cfe58a8a1fb8c1bc948ebcd42df64"]
```
It gives the valuation of the public and secret value declared in the script.  
It correspond to the following C initialization.
```
/* secret 1 */
char s1[SIZE] = {

  0xb1, 0x23, 0x0c, 0x21, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
/* public */
char s2[SIZE] = {
  0x5f, 0xed, 0xe7, 0x3e, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
size_t n = 0x00000009;
```
```
/* secret 2 */
char s1[SIZE] = {
  0x64, 0xdf, 0x42, 0xcd, 0xeb, 0x48, 0xc9, 0x1b,
  0x8c, 0xfb, 0xa1, 0xa8, 0x58, 0xfe, 0x3c, 0x99
};
```
:warning: *In C, the content of the buffer is written in `little endian` wherease the bitvector given by the model is written in `big endian`. Bytes have to be swapped between the two representations. It is not the case for the variable `n` (integer) that is written in `big endian` both in C and in the model.*

:information_source: Since we compiled the binary with debug information (`-g`), we can further try to link back the leak to the C code. For instance, the following command shows the mapping between lines in C and program addresses`.

```console
$ objdump --dwarf=decodedline candidate_1
```
Here, we can find that the address of the leak at `0x80497df` is between `0x80497de` and `0x80497e5` and thus matches the line `7` in `candidate_1.c`.

```
File name                            Line number    Starting address    View    Stmt
...
candidate_1.c                                  7           0x80497c4               x
candidate_1.c                                  7           0x80497cf               x
candidate_1.c                                  7           0x80497da               x
candidate_1.c                                  7           0x80497de               x
candidate_1.c                                  8           0x80497e5               x
```

### Second (*constant-time*) candidate

Here is another way to implement the function `memcmp` designed for *constant-time* compliance.

```c
#include <stddef.h>

int memcmp(const void *s1, const void *s2, size_t n)
{
    const char *p1 = (const char *)s1, *p2 = (const char *)s2;
	int res = 0;
    for (size_t i = 0; i < n; i += 1) {
		res = res | (~(res >> 31) & ((res - 1) >> 31) & (p1[i] - p2[i]));
    }
    return res;
}
```

Let us get the binary.
```console
$ gcc -m32 -static test_harness.c candidate_2.c -o candidate_2
```

Then, run the analysis.
```console
$ binsec -sse -checkct -sse-script checkct.cfg candidate_2
```
The log is now smaller since the program is proven secure.
```console
...
[checkct:result] Program status is : secure (0.053)
[checkct:info] 9 visited paths covering 92 instructions
[checkct:info] 21 / 21 control flow checks pass
[checkct:info] 452 / 452 memory access checks pass
```

:information_source: *Remember that **BINSEC** only performs bounded verification. The result is valid given the initial assumption, for instance that the size `n` is lower than 10.*

### Threat to completeness

**BINSEC** has 3 output kinds:
- the program is `insecure` as long as one leak has been detected along any visited path;
- the program is `secure` if no leak has been detected and that all paths have been explored;
- the program has an `unknown` status if no leak has been detected so far but at least one check or the whole exploration has been interrupted prematurely.

|                | no leak | any leak |
|----------------|---------|----------|
| **complete**   | secure  | insecure |
| **incomplete** | unknown | insecure |

The analysis is parameterized by 4 adjustment variables.

#### Global timeout

This is the time budget given to the tool to perform the analysis.  
If **BINSEC** runs out of time, some paths will remain in its worklist (`pending`). By default, there is no timeout. Use option `-sse-timeout` to set a timeout in seconds.

Here is an example of diagnostic reported when a timeout occurs.

```console
[checkct:warning] Exploration is incomplete:
                  - timeout has left (at least) 3 pending paths (-sse-timeout)
```

#### Maximal path depth

This is the maximal number of instructions the engine is allowed to execute along a path. This limit avoids the analysis being stuck when the program performs an infinite loop. By default, the limit is `1000` instructions. Use option `-sse-depth` to set another value.

Here is an example of diagnostic reported when maximal path depth should be increased.

```console
[checkct:warning] Exploration is incomplete:
                  - 2 paths have reached the maximal depth and have been cut (-sse-depth)
```

#### Dynamic jump enumeration

This is the maximal number of jump targets that will be computed for a given dynamic jump. By default, the limit is `3` targets. Use option `-sse-jump-enum` to set another value.

:information_source: *The most common dynamic jump is the `ret` instruction used in pair with `call` for functions. As it should jump to its caller along that path, the number of target is only one.*

Here is an example of diagnostic reported when the target enumeration did not run to completion.

```console
[checkct:warning] Exploration is incomplete:
                  - some jump targets may have been omitted (-sse-jump-enum)
```

#### Solver timeout

This is the time budget given to the SMT solver to solve a single query.
If the symbolic constrains are hard, the SMT solver may fails to find a solution. By default, the limit is `5` seconds. Use option `-fml-solver-timeout` to set another value. Special value `0` disable the timeout.

:information_source: *There are two kinds of queries, the exploration queries and the security ones. When unsolved, the first ones may lead to unexplored paths while the second ones mean the leakage status of an instruction can not be determined. In both case, the analysis will not be able to answer the program is `secure`.*

Here is an example of diagnostic reported when the target enumeration did not run to completion.

```console
[checkct:warning] Exploration is incomplete:
                  - 4 SMT solver queries remain unsolved (-fml-solver-timeout)
```
