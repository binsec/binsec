# Requirements

Linux x86-64 environment with:
- `binsec`

# Sources

- binary challenge:     [magic](./magic)
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the `magic` function returns `true`.

# Command

```console
$ binsec -sse -sse-script crackme.ini magic
```

# Expect

The value `arg` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Value arg<32> : 0xc0dedead
```
