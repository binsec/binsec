# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `gdb`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget  https://crackmes.one/static/crackme/61e9983133c5d413767ca5ac.zip
$ unzip -P crackmes.one 61e9983133c5d413767ca5ac.zip
$ gdb -x command --args ./gugus.out aaaaaaaaaaaaaaaaaaa
```

# Sources

- gdb script:           command
- binary challenge:     core.snapshot
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs `Access granted`.

# Command

```console
$ binsec -sse -sse-script crackme.ini core.snapshot
```

# Expect

The value stream `password` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream password : "gu!gu?s"
```

## Credit

[bueb810](https://crackmes.one/user/bueb810)
