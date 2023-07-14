# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `gdb`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget  https://crackmes.one/static/crackme/61c8deff33c5d413767ca0ea.zip
$ unzip -P crackmes.one 61c8deff33c5d413767ca0ea.zip
$ gdb -x command --args ./trycrackme
```

# Sources

- gdb script:           command
- binary challenge:     core.snapshot
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs `[+] Correct key!`.

# Command

```console
$ binsec -sse -sse-script crackme.ini -sse-depth 10000 core.snapshot
```

# Expect

The value stream `key` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream key : "34407373373234353336"
```

## Credit

[MrEmpy](https://crackmes.one/user/MrEmpy)
