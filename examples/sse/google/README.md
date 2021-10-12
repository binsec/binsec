# Requirements

Linux x86-64 environment with:
- `wget`
- `bunzip`
- `tail`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget https://github.com/ctfs/write-ups-2016/raw/master/google-ctf-2016/reverse/unbreakable-enterprise-product-activation-150/unbreakable-enterprise-product-activation.bz2
$ bunzip2 -c unbreakable-enterprise-product-activation.bz2 | tail -c +119 > unbreakable-enterprise-product-activation
```

# Sources

- binary challenge:     unbreakable-enterprise-product-activation
- SSE script:           [crackme.ini](./crackme.ini)
- SSE stubs:            [strncpy.stub](./strncpy.stub), [exit.stub](./exit.stub)
- BINSEC configuration: [config.cfg](./config.cfg)

# Goal

Find the secret such that the challenge outputs
`Thank you - product activated!`.

# Command

```console
$ binsec -config config.cfg
```

# Expect

The value stream `key_product` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream key_product : "CTF{0The1Quick2Brown3Fox4Jumped5Over6The7Lazy8Fox9}"
```
