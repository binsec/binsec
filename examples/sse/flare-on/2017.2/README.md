# Requirements

Linux x86-64 environment with:
- `wget`
- `7z`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget http://flare-on.com/files/Flare-On4_Challenges.zip
$ 7z e Flare-On4_Challenges.zip 02/IgniteMe.exe -pflare
```

# Sources

- binary challenge:     IgniteMe.exe
- SSE script:           [crackme.ini](./crackme.ini)
- BINSEC configuration: [config.cfg](./config.cfg)

# Goal

Find the secret such that the challenge outputs `G00d j0b!`.

# Command

```console
$ binsec -config config.cfg
```

# Expect

The value stream `bRead` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream bRead : "R_y0u_H0t_3n0ugH_t0_1gn1t3@flare-on.com"
```
