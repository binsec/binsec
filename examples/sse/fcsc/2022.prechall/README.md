# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget https://blog.reinom.com/story/ctf/fcsc2022/welcome/prechall/fcsc2022-prechall.zip
$ unzip fcsc2022-prechall
```

# Sources

- binary challenge:     fcsc.8xp
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs the following.
```
Bravo !
Le flag est:
```

# Command

```console
$ binsec -sse -sse-script crackme.ini fcsc.8xp
```

# Expect

The program outputs the secret to `stdout`.  
BINSEC log should contain the following entry.

```console
[sse:result] C string stdout : "11111111111111111111111111111111\rBravo !\nLe flag est:\nFCSC{4B8CB9E0ADB7F2B3BA4E6CB7156F1243}\r"
```
