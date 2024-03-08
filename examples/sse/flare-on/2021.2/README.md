# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `7z`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget http://flare-on.com/files/Flare-On8_Challenges.zip
$ unzip Flare-On8_Challenges.zip 2/599EA8F84AD975CFB07E0E5732C9BA14.zip
$ 7z x -pflare 02_known.7z
```

# Sources

- binary challenge:     UnlockYourFiles.exe
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge decrypt the files in Files.

# Command

```console
$ binsec -sse -sse-script crackme.ini -sse-depth 2000 UnlockYourFiles.exe
```

# Expect

The value stream `bRead` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream bRead : "No1Trust"
```
