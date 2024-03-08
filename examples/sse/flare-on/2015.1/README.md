# Requirements

Linux x86-64 environment with:
- `wget`
- `unzip`
- `wine`
- `binsec`

# Setup

```console
$ wget http://flare-on.com/files/2015_FLAREOn_Challenges.zip
$ unzip 2015_FLAREOn_Challenges.zip 1/Flare-On_start_2015.exe
$ wine 1/Flare-On_start_2015.exe
```
Accept the agreement and browse to the current directory.

# Sources

- binary challenge:     i\_am\_happy\_you\_are\_to\_playing\_the\_flareon\_challenge.exe
- SSE script:           [crackme.ini](./crackme.ini)

# Goal

Find the secret such that the challenge outputs `You are success`.

# Command

```console
$ binsec -sse -sse-script crackme.ini i_am_happy_you_are_to_playing_the_flareon_challenge.exe
```

# Expect

The value stream `bRead` holds the secret.  
BINSEC log should contain the following entry.

```console
[sse:result] Ascii stream bRead : "bunny_sl0pe@flare-on.com"
```
