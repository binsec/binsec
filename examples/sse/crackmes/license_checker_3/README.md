# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `gdb`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget  https://crackmes.one/static/crackme/62327b0433c5d46c8bcc0335.zip
$ unzip -P crackmes.one 62327b0433c5d46c8bcc0335.zip
$ unzip -P crackmes.one 111.zip
$ gdb -x command --args ./license_checker_3 aaaaaaaaaaaaaaaaaaa
```

# Sources

- gdb script:           command
- binary challenge:     core.snapshot
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs `Premium access has been activated !`.

# Command

```console
$ binsec -sse -sse-script crackme.ini core.snapshot
```

# Expect

The value stream `password` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream password : "599999"
```

## Credit

[NomanProdhan](https://crackmes.one/user/NomanProdhan)
