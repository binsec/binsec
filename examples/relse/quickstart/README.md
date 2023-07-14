# Requirements

Linux x86-64 environment with:
- `make`
- `gcc`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ make candidate_1 candidate_2
```

# Sources

- source programs:      candidate_1.c candidate_2.c test_harness.c
- binary programs:      candidate_1 candidate_2
- SSE script:           [checkct.cfg](./checkct.cfg)

# Goal

Check if the candidate functions are constant time.

# Command

```console
$ binsec -sse -checkct -sse-script checkct.cfg candidate_1
$ binsec -sse -checkct -sse-script checkct.cfg candidate_2
```

# Expect

BINSEC log should contain the following entries.

## Candidate_1

```console
[checkct:result] Instruction 0x080497dc has control flow leak (0.046s)
[checkct:result] Instruction 0x080497fd has control flow leak (0.048s)
[checkct:result] Program status is : insecure (0.085)
[checkct:info] 27 visited paths covering 91 instructions
[checkct:info] 39 / 41 control flow checks pass
[checkct:info] 882 / 882 memory access checks pass
```

## Candidate_2

```console
[checkct:result] Program status is : secure (0.054)
[checkct:info] 9 visited paths covering 92 instructions
[checkct:info] 21 / 21 control flow checks pass
[checkct:info] 452 / 452 memory access checks pass
```
