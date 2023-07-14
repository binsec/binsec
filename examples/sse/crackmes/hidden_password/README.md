# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `gdb`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget  https://crackmes.one/static/crackme/61ffb07c33c5d46c8bcbfc1d.zip
$ unzip -P crackmes.one 61ffb07c33c5d46c8bcbfc1d.zip
$ gdb -x command --args ./hidden_password aaaaaaaaaaaaaaaaaaa
```

# Sources

- gdb script:           command
- binary challenge:     core.snapshot
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs `Good password!`.

# Command

```console
$ binsec -sse -sse-script crackme.ini -sse-depth 10000 \
  -sse-self-written-enum 1 core.snapshot
```

# Expect

The value stream `password` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream password : "hello_world_42"
```

## Credit

[pjenik@seznam.cz](https://crackmes.one/user/pjenik@seznam.cz)
