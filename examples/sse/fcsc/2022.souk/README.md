# Requirements

Linux x86-64 environment with:
- `wget`
- `gdb`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget https://github.com/Angelo942/FCSC2022/raw/main/Rev-souk-Easy/souk
$ chmod +x souk
$ gdb -x command --args ./souk
```

# Sources

- gdb script:           command
- binary challenge:     core.snapshot
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs the following.
```
Bravo, tu as réussi à mettre de l'ordre dans ce capharnaüm...
Tu peux valider le challenge avec ce flag !
```

# Command

```console
$ binsec -sse -sse-script crackme.ini \
         -sse-depth 100000  -sse-qmerge 100 core.snapshot
```

# Expect

The value stream `stdin` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] C string stdin : "FCSC{665cfa3e0277a889258cc9f6e24c88fc9db654178558de101b8a19af8fb00575}"
```
