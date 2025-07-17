# Requirements

Linux x86-64 environment with:
- `binsec`

# Setup
Using MSYS2 terminal:

```
$ curl --output polygraph.exe 'https://hackropole.fr/challenges/fcsc2025-reverse-polygraph/public/polygraph.exe'
$ ./polygraph.exe <<< '00112233445566778899AABBCCDDEEFF'
```

# Sources

- binary challenge:     [polygraph.exe](https://hackropole.fr/fr/challenges/reverse/fcsc2025-reverse-polygraph/)
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the output isn't `Liar!!`.

# Command

```console
$ binsec -sse -sse-script crackme.ini polygraph.exe -sse-depth 10000000
```

# Expect

The value `secret_key` holds the secret.
BINSEC log should contain the following entry.

```console
[sse:result] Value secret_key<128> : 0x2ab857a5fbe0a867bfd8abebf1e9c831
```

`secret_value` can be converted to hex value using python:

```py
>>> secret_key = 0x2ab857a5fbe0a867bfd8abebf1e9c831
>>> secret_key.to_bytes(128//8, "big").hex()
'2ab857a5fbe0a867bfd8abebf1e9c831'
```

## Credit

[sheidan](https://github.com/Sh3idan)
