# Requirements

Linux x86-64 environment with:
- `binsec`

# Sources

- binary challenge:     [polygraph.exe](https://hackropole.fr/fr/challenges/reverse/fcsc2025-reverse-polygraph/)
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the output isn't `Liar!!`.

# Command

```console
$ binsec -sse -sse-script poly.ini polygraph.exe -sse-depth 10000000 | egrep bread | sort
```

# Expect

Each byte of the non-printable secret is stored in `bread`.

```console
             bread!4 : 0x2a
             bread!5 : 0xb8
             bread!6 : 0x57
             bread!7 : 0xa5
             bread!8 : 0xfb
             bread!9 : 0xe0
             bread!a : 0xa8
             bread!b : 0x67
             bread!c : 0xbf
             bread!d : 0xd8
             bread!e : 0xab
             bread!f : 0xeb
             bread!g : 0xf1
             bread!h : 0xe9
             bread!i : 0xc8
             bread!j : 0x31
```
