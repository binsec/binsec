# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget http://flare-on.com/files/2015_FLAREOn_Challenges.zip
$ unzip 2015_FLAREOn_Challenges.zip 2/599EA8F84AD975CFB07E0E5732C9BA14.zip
$ unzip -p -P flare 2/599EA8F84AD975CFB07E0E5732C9BA14.zip > very_success.exe
```

# Sources

- binary challenge:     very\_success.exe
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs `You are success`.

# Command

```console
$ binsec -sse -sse-script crackme.ini very_success.exe
```

# Expect

The value stream `bRead` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream bRead : "a_Little_b1t_harder_plez@flare-on.com"
```
