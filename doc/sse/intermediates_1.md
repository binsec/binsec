# SSE Tutorial \#2: Solving simple CTF with BINSEC SSE

In this post, we will exercise the **S**tatic **S**ymbolic **E**xecution engine of **BINSEC** over the first 2015 [FLARE On](https://flare-on.com/) challenge.  
We will do so with the additionnal difficulty of not looking at a single line of code disassembly -- we still reserve the right to use good tools to extract meta-data. This way, we will have to focus on the interaction between the program and its environment (for instance, a user typing in a console) in a *quasi*-black box manner.

Since we already know [**BINSEC SSE** basics](beginners.md), it is the time to learn how to modelize the (*system*) environment and, incidentally, deal with dynamically linked functions, by creating *DBA* function mocks.

### Requirements

To run to completion we will need:
- a **Linux** environment on a **x86** machine;
- the **wget**, **unzip** and **wine**  utilities in order to setup the challenge;
- the **strings**, **readpe** (part of the **pev** package) and **radare2** utilities for reverse-engineering;
- the **BINSEC** tool (see [install instructions](../../INSTALL.md));
- a SMT solver, preferably [**ocaml-bitwuzla**](../../INSTALL.md#Dependencies)
  (but would work with **bitwuzla**, **boolector**, **Z3**, etc.).

Some knowledge about Windows DLL and reverse-engineering could also help. 

### Setup

We first need to download the [FLARE On 2015 challenge archive](https://flare-on.com/files/2015_FLAREOn_Challenges.zip) and extract its content.
```console
$ wget http://flare-on.com/files/2015_FLAREOn_Challenges.zip
$ unzip 2015_FLAREOn_Challenges.zip 1/Flare-On_start_2015.exe
```
`Flare-On_start_2015.exe` is mini-installer GUI as it was originally provided by the FLARE website.  
Execute it in `wine`, accept the license aggreement and choose a path to extract the real challenge **i_am_happy_you_are_to_playing_the_flareon_challenge.exe**.
```console
$ wine 1/Flare-On_start_2015.exe
```

### (Some) reverse-engineering

This challenge consists of a single **PE** **x86**-32bit executable **i_am_happy_you_are_to_playing_the_flareon_challenge.exe** as reported by `file`.
```console
$ file i_am_happy_you_are_to_playing_the_flareon_challenge.exe 
i_am_happy_you_are_to_playing_the_flareon_challenge.exe: PE32 executable (console) Intel 80386, for MS Windows
```
For the brave and the unaware, directly running the executable within `wine` will prompt you the following message.
```console
Let's start out easy
Enter the password>
```
So, we are asked to enter `the password` -- the first thing I did to try to outsmart it, but...
failing to feed the secret simply prompts the following *hurtful* message before exiting.
```console
Enter the password>the password
You are failure
```
Let us try having a better understanding.

The command `strings` will output the *human readable* strings of the binary file.
```console
$ strings i_am_happy_you_are_to_playing_the_flareon_challenge.exe
.text
.data
Pj*h
Pj2hX!@
h.!@
kernel32.dll
LoadLibraryA
GetProcAddress
GetLastError
GetStdHandle
AttachConsole
WriteConsoleA
WriteFile
ReadFile
Let's start out easy
Enter the password>
You are success
You are failure
```
Here are some interesting findings:
- the binary likely has the sections `.text` and `.data`;
- the binary likely depends of the dynamic library `kernel32.dll` -- this is very important for what lies ahead;
- the last four entries are undoubtedly the strings the program is displaying.

At this point, I strongly suspect that our goal is to obtain `You are success`.  
But, more worryingly, we need to know how the program interact with the console.

We can extract (among plenty of other things) a call graph by using `r2`.

```console
$ r2 i_am_happy_you_are_to_playing_the_flareon_challenge.exe 
 -- What has been executed cannot be unexecuted
[0x00401000]> aa
[x] Analyze all flags starting with sym. and entry0 (aa)
[0x00401000]> agc
                                                ┌────────────────────┐
                                                │  entry0            │
                                                └────────────────────┘
                                                      v
                                                      │
      ┌───────────────────────────────────────────────│
      │                                         ┌─────│
      │                                         │     └─────────────────────────────────┐
      │                                         │                                       │
┌─────────────────────────────────────┐   ┌──────────────────────────────────┐    ┌─────────────────────────────────┐
│  sym.imp.kernel32.dll_GetStdHandle  │   │  sym.imp.kernel32.dll_WriteFile  │    │  sym.imp.kernel32.dll_ReadFile  │
└─────────────────────────────────────┘   └──────────────────────────────────┘    └─────────────────────────────────┘
```
Here we are fixed, this program is calling the standard C functions [`GetStdHandle`](https://docs.microsoft.com/en-us/windows/console/getstdhandle), [`WriteFile`](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile) and [`ReadFile`](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile).

It is just an educated guess, but genuine usage scenario suggests:
- `WriteFile` arguments:
  - `hFile` is the console returned by `GetStdHandle(STD_OUTPUT_HANDLE)`;
  - `lpBuffer` points to one of the string `Let's start out easy`, `Enter the password>`, `You are success` or `You are failure`;
  - `nNumberOfBytesToWrite` is the size of the strings pointed by `lpBuffer`;
  - `lpNumberOfBytesWritten` points to the actual size of the string written (should be equal to `nNumberOfBytesToWrite`);
  - `lpOverlapped` should be `NULL`;
- `ReadFile` arguments:
  - `hFile` is the console returned by `GetStdHandle(STD_INPUT_HANDLE)`;
  - `lpBuffer` points to the string given by the user;
  - `nNumberOfBytesToRead` is the size of the user input buffer;
  - `lpNumberOfBytesRead` points to the actual size of the user input;
  - `lpOverlapped` should be `NULL`.

We have some idea of what these functions are supposed to do.  
Yet we still ignore at which place they are called.

In fact, for **PE** binaries, the dynamic functions are handled with an `Import Address Table` that contains the place where
the dynamic loader has put the functions at runtime. The tool `readpe` can help us to figure out how to mimic this behavior.
```console
$ readpe -di i_am_happy_you_are_to_playing_the_flareon_challenge.exe
Optional/Image header
    Magic number:                    0x10b (PE32)
    Linker major version:            8
    Linker minor version:            0
    Size of .text section:           0x200
    Size of .data section:           0x200
    Size of .bss section:            0
    Entrypoint:                      0x1000
    Address of .text section:        0x1000
    Address of .data section:        0x1000
    ImageBase:                       0x400000
    Alignment of sections:           0x1000
    Alignment factor:                0x200
    Major version of required OS:    5
    Minor version of required OS:    1
    Major version of image:          5
    Minor version of image:          1
    Major version of subsystem:      4
    Minor version of subsystem:      0
    Size of image:                   0x3000
    Size of headers:                 0x200
    Checksum:                        0
    Subsystem required:              0x3 (IMAGE_SUBSYSTEM_WINDOWS_CUI)
    DLL characteristics:             0
    DLL characteristics names
    Size of stack to reserve:        0x100000
    Size of stack to commit:         0x1000
    Size of heap space to reserve:   0x100000
    Size of heap space to commit:    0x1000
Data directories
    Directory
        IMAGE_DIRECTORY_ENTRY_IMPORT:    0x2000 (40 bytes)
    Directory
        IMAGE_DIRECTORY_ENTRY_IAT:       0x204c (20 bytes)
Imported functions
    Library
        Name:                            kernel32.dll
        Functions
            Function
                Name:                            LoadLibraryA
            Function
                Name:                            GetProcAddress
            Function
                Name:                            GetLastError
            Function
                Name:                            GetStdHandle
            Function
                Name:                            AttachConsole
            Function
                Name:                            WriteConsoleA
            Function
                Name:                            WriteFile
            Function
                Name:                            ReadFile
```
The address `0x40204c` (`ImageBase` + `IMAGE_DIRECTORY_ENTRY_IAT`) holds the IAT which contains 8 entries of 4 byte pointers, in order `LoadLibraryA`, `GetProcAddress`, etc.

For instance, when the program calls the function `GetStdHandle`, it jumps at the address stored at `0x402058` (`0x40204c + 4 x 3` where `3` is the index in the table and `4` the size of pointers).  
In the same way, the effective address of `WriteFile` (respectively `ReadFile`) is stored at `0x402064` (respectively `0x402068`).

Enough of low-level details, we should be ready to use **BINSEC** and find what secret input leads to success. 

### Write up SSE script

We will model the environment according to the guesses we have previously made. Yet, as we may be wrong, we will use defensive assertions to catch unexpected cases.

#### Initialization

As discussed in the previous tutorial, we have to initialize some part of the machine state.
We will then initialize the stack pointer `esp` together with the return address and the section `.text` and `.data` (sizes come from `readpe`).
```text
esp := 0x32ff5c
@[esp, 4] := 0x7b454cef as return_address

@[<.text>, 512] from file
@[<.data>, 512] from file
```

#### Mocking the Import Table

We want to choose addresses to anchor the functions `GetStdHandle`, `WriteFile` and `ReadFile`.  
Here again, the actual value does not matter but it should not collide with other objects.
We will use the following.
```text
@[0x402058, 4] := 0x7b431a7c as GetStdHandle
@[0x402064, 4] := 0x7b442ef0 as WriteFile
@[0x402068, 4] := 0x7b442e00 as ReadFile
```
We are using the `as` syntax to give the values an intelligible name -- it is actually only a shorthand for the following.
```text
GetStdHandle<32> := 0x7b431a7c
@[0x402058] := GetStdHandle
```
To ensure that no other function is called, we can complete the Import table as following.
```text
@[0x40204c, 4] := 0x7b432058 as LoadLibraryA
@[0x402050, 4] := 0x7b44fe10 as GetProcAddress
@[0x402054, 4] := 0x7b43175c as GetLastError
@[0x40205c, 4] := 0x7b430794 as AttachConsole
@[0x402060, 4] := 0x7b43b540 as WriteConsoleA

abort at LoadLibraryA, GetProcAddress, GetLastError, AttachConsole, WriteConsoleA
```

### Modeling `GetStdHandle`

We will use the command `replace` *addr* `by` to provide our function mock.  
```text
replace GetStdHandle by
```
First thing to do is to collect the arguments.
```text
  caller<32> := @[esp, 4]
  nStdHandle<32> := @[esp + 4, 4]
```
Then we want the function to return some predetermined values when called with `STD_INPUT_HANDLE` (`-10`) or `STD_OUTPUT_HANDLE` (`-11`) or fail otherwise.
```text
  case nStdHandle is
    -10: eax := 23
    -11: eax := 27
      _: assert false
  end
```
Finally, we correct the stack and jump to the caller.
```text
  esp := esp + 4
  jump at caller
end
```

### Modeling `WriteFile`

As before, we collect the arguments in the stack, ignoring `lpBuffer`.
```text
replace WriteFile by
  caller<32> := @[esp, 4]
  hFile<32> := @[esp + 4, 4]
  # we ignore lpBuffer
  nNumberOfBytesToWrite<32> := @[esp + 12, 4]
  lpNumberOfBytesWritten<32> := @[esp + 16, 4]
  lpOverlapped<32> := @[esp + 20, 4]
```
We add some assertion to watch our backs.
```text
  assert hFile = 27
  assert lpOverlapped = 0
```
Then we want the function to always write the required amount of bytes and return non zero value.
```text
  if lpNumberOfBytesWritten <> 0 then
    @[lpNumberOfBytesWritten, 4] := nNumberOfBytesToWrite
  end
  eax := 1
```
And finally, return to the caller.
```text
  esp := esp + 4
  jump at caller
end
```

### Modeling `ReadFile`

Again, we collect the arguments in the stack.
```text
replace ReadFile by
  caller<32> := @[esp, 4]
  hFile<32> := @[esp + 4, 4]
  lpBuffer<32> := @[esp + 8, 4]
  nNumberOfBytesToRead<32> := @[esp + 12, 4]
  lpNumberOfBytesRead<32> := @[esp + 16, 4]
  lpOverlapped<32> := @[esp + 20, 4]
```
We add some assertion to watch our backs.
```text
  assert hFile = 23
  assert lpOverlapped = 0
```
Then we want the function to write an arbitrary number of symbolic characters in `lpBuffer`.  
So first thing is to choose how many bytes to read.
```text
  nNumberOfBytesRead<32> := nondet
```
The `nondet` assignment will create a new *unconstrained* symbolic value.  
We may want to restrict it to valid values only using `assume`. Especially, we want it to contain at least the cariage return (`"\r\n"`) and to not exceed the requested amount of bytes.
```text
  assume 2 <= nNumberOfBytesRead <= nNumberOfBytesToRead
```
We can now set the right amount of symbolic bytes using `nondet`, assuming the new character is in range of printable characters.
```text
  for i<32> in 0 to nNumberOfBytesRead - 3 do
    @[lpBuffer + i] := nondet as bRead
    assume " " <= bRead <= "~"
  end
```
Do not forget to add the carriage return.
```text
@[lpBuffer + i] := "\r"
@[lpBuffer + i + 1] := "\n"
```
Also, do not forget to write back the number of bytes read and to return a positive value before returning.
```text
  if lpNumberOfBytesRead <> 0 then
    @[lpNumberOfBytesRead, 4] := nNumberOfBytesRead
  end
  eax := 1
  esp := esp + 4
  jump at caller
end
```

### Setting the target

We will thus try to reach the function `WriteFile` such that its argument `lpBuffer` points to the string `You are success`.  
We want to cut the exploration if the program reach the return address but we can also stop if the `lpBuffer` argument points to the string `You are failure`.
```text
reach WriteFile such that @[@[esp + 8, 4], 15] = "You are success" then print ascii stream bRead
cut at WriteFile if @[@[esp + 8, 4], 15] = "You are failure"
cut at return_address
```
Here, the command `print ascii stream` will concat the different values of the symbolic variable `bRead` and print it as a string.

Let us now test our script. Adding the option `-fml-solver bitwuzla` will significantly speed up the exploration. 
```console
$ binsec -sse -sse-script crackme.ini i_am_happy_you_are_to_playing_the_flareon_challenge.exe -fml-solver bitwuzla
...
[sse:result] Directive :: path 277 reached address 7b442ef0 (0 to go)
[sse:result] Ascii stream bRead : "bunny_sl0pe@flare-on.com"
[sse:info] SMT queries
             Preprocessing simplifications
               total          1306
               sat            330
               unsat          630
               constant enum  346
             
             Satisfiability queries
               total          675
               sat            675
               unsat          0
               unknown        0
               time           1.24
               average        0.00
             
           Exploration
             total paths                      326
             completed/cut paths              300
             pending paths                    26
             stale paths                      0
             failed assertions                0
             branching points                 1011
             max path depth                   203
             visited instructions (unrolled)  4355
             visited instructions (static)    54
```
**BINSEC** returns that the expected password is `bunny_sl0pe@flare-on.com`.  
Let us try it in a real run.
```console
$ wine i_am_happy_you_are_to_playing_the_flareon_challenge.exe
Let's start out easy
Enter the password>bunny_sl0pe@flare-on.com
You are success
```

#### Conclusion

Other methods exist, for instance [the official solution](https://www.fireeye.com/content/dam/fireeye-www/global/en/blog/threat-research/flareon/solution-1.pdf), but today, we solved a CTF challenge with **BINSEC** without even having a look at the binary disassembly. I hope you learn some tricks about reverse engineering and now have a better understanding of how the **Windows** dynamic libraries work. Hopefully, you also have the keys now for using **BINSEC** symbolic execution for solving other CTF by your own. I have heard that [FLARE On](https://flare-on.com/) 2015 challenge 2 and 2017 challenge 2 work exactly the same. Will you be able to solve them with **BINSEC**?

Let us just recap how to proceed:
- identify the success and failure conditions (most likely, the output of some predefined strings);
- identify and handle the external dynamically loaded function (tip, stubs are often reusable as is);
- initialize the initial state for registers and memory (load sections and initialize the stack register);
- run **BINSEC** and get a coffee.

Also, [here](references.md) are some useful information you may want to take a look at.  
Have a nice day :-)


