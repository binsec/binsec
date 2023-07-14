# Requirements

Linux x86-64 environment with:
- `libunicorn` (https://github.com/unicorn-engine/unicorn)
- `wget`
- `gdb`
- `bitwuzla` (https://bitwuzla.github.io/)
- `binsec`

# Setup

```console
$ wget https://github.com/Angelo942/FCSC2022/raw/main/Rev-Licorne-Easy/licorne
$ chmod +x licorne
```

### Method 1:
```console
$ gdb -x command1 --args ./licorne
```

### Method 2:
```console
$ gdb -x command2 --args ./licorne
```

# Sources

### Method 1:
- gdb script:           [command1](./command1)
- binary challenge:     core1.snapshot
- SSE script:           [crackme1.ini](./crackme1.ini)

### Method 2:
- gdb script:           [command2](./command2)
- binary challenge:     core2.snapshot
- SSE script:           [crackme2.ini](./crackme2.ini)

# Goal

Find the 8 secret numbers such that the challenge outputs the following.
```
Congrats! You can use the flag FCSC{bddb83056668ad24731c28bd35161a7d90923d7a887e728f4866bbe4d32b5947e64b8dab82b547839a0777668fac199e199fe9291df0f5ac199c4555cfd54412} to validate the challenge.
```

# Command

### Method 1:
```console
$ binsec -sse -sse-depth 10000 \
         -sse-script crackme1.ini core1.snapshot
```

### Method 2:
```console
$ binsec -sse -sse-depth 1000000000 \
         -sse-script crackme2.ini -sse-self-written-enum 1 \
		 core2.snapshot
```

# Expect

The program outputs the secret to `stdout`.  
BINSEC log should contain the following entry.

```console
[sse:result] C string stdout : "Congrats! You can use the flag FCSC{bddb83056668ad24731c28bd35161a7d90923d7a887e728f4866bbe4d32b5947e64b8dab82b547839a0777668fac199e199fe9291df0f5ac199c4555cfd54412} to validate the challenge."
```
