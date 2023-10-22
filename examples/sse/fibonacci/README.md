# Requirements

Linux x86-64 environment with:
- `binsec`
- `jitpsi` (optionnally, https://github.com/recoules/jitpsi)

# Sources

- binary program:       [fibonacci](./fibonacci)
- SSE script:           [exec.ini](./exec.ini)

# Goal

Compute the 64 least significant bits of the 50000000th element of the Fibonacci sequence.

# Command

```console
$ binsec -sse -sse-depth 500000000 -sse-script exec.ini fibonacci
```
:information_source: Speed up with `-sse-cse` or `-sse-jit`.

# Expect

The program outputs the result to `stdout`.  
BINSEC log should contain the following entry.

```console
[sse:result] C string stdout : "8335614471788137157"
```

# Variation

Change the string input at `rsp + 24` to compute another index of the Fibonacci sequence.

For instance, for the 42th element, update the script with the following.
```
@[rsp + 24, 3] := "42"z
```
