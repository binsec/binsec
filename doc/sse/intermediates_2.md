# SSE Tutorial \#3: Solving modern CTF with BINSEC SSE and GDB

In this post, we will exercise the **S**tatic **S**ymbolic **E**xecution engine of **BINSEC** over the [MrEmpy](https://crackmes.one/user/MrEmpy) challenge `TryCrackMe`.  
While the logic is not particularly challenging, what is new here is that the
binary is a `x86-64` shared executable file. [Previous tutorial](intermediates_1.md) introduced how to deal with dynamically linked functions by creating
*DBA* mocks, but there are limits to this approach -- we do not want to mock
the entire `libc`. So we propose to use GDB to generate a static snapshot
of the process just after the dynamic linker `ld.so` did all the dirty work.

### Requirements

To run to completion we will need:
- a **Linux** environment on a **x86-64** machine;
- the **wget**, **unzip** utilities in order to setup the challenge;
- the **strings**, **readelf** and **gdb** utilities for reverse-engineering;
- the **BINSEC** tool (see [install instructions](../../INSTALL.md)) with the `unisim-archisec` extension;
- a SMT solver, preferably [**ocaml-bitwuzla**](../../INSTALL.md#Dependencies)
  (but would work with **bitwuzla**, **boolector**, **Z3**, etc.).

Using the [BINSEC docker](https://hub.docker.com/r/binsec/binsec) has two advantages:
- you will start with the best and latest setup;
- you will not run directly the binary on your host system -- it is a little bit more secure.

### Setup

We first need to download the [challenge archive](https://crackmes.one/static/crackme/61c8deff33c5d413767ca0ea.zip) and extract its content.
```console
$ wget  https://crackmes.one/static/crackme/61c8deff33c5d413767ca0ea.zip
$ unzip -P crackmes.one 61c8deff33c5d413767ca0ea.zip
```

### (Some) reverse-engineering

This challenge consists of a single **ELF** **x86**-64bit shared object **trycrackme** as reported by `file`.
```console
$ file trycrackme 
trycrackme: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, BuildID[sha1]=de0cbe0b2eb3316f6a79bef4b5ebdcae03dfb358, for GNU/Linux 4.4.0, not stripped
```
Running the executable prompts you a banner and asks for a key:
```console
$ ./trycrackme 

  _____          ___             _   __  __     
 |_   _| _ _  _ / __|_ _ __ _ __| |_|  \/  |___ 
   | || '_| || | (__| '_/ _` / _| / / |\/| / -_)
   |_||_|  \_, |\___|_| \__,_\__|_\_\_|  |_\___|
           |__/                                 
                       
Put the key:
```
If you are not extremely lucky, entering a random key will prompt a failure
message.
```console
[-] Incorrect key!
```

We can look at the output of the `strings` command to find the success message.
```console
$ strings trycrackme
strings trycrackme 
/lib64/ld-linux-x86-64.so.2
1ojy
sprintf
strncmp
__isoc99_scanf
puts
__stack_chk_fail
strlen
__cxa_finalize
__libc_start_main
libc.so.6
GLIBC_2.7
GLIBC_2.4
GLIBC_2.2.5
_ITM_deregisterTMCloneTable
__gmon_start__
_ITM_registerTMCloneTable
u3UH
4@ss7245H
[]A\A]A^A_
  _____          ___             _   __  __     
 |_   _| _ _  _ / __|_ _ __ _ __| |_|  \/  |___ 
   | || '_| || | (__| '_/ _` / _| / / |\/| / -_)
   |_||_|  \_, |\___|_| \__,_\__|_\_\_|  |_\___|
           |__/                                 
                       
Put the key: 
%02x
[-] Incorrect key!
[+] Correct key!
;*3$"
GCC: (GNU) 11.1.0
abi-note.c
__abi_tag
init.c
crtstuff.c
deregister_tm_clones
__do_global_dtors_aux
completed.0
__do_global_dtors_aux_fini_array_entry
frame_dummy
__frame_dummy_init_array_entry
trycrackme.c
__FRAME_END__
__init_array_end
_DYNAMIC
__init_array_start
__GNU_EH_FRAME_HDR
_GLOBAL_OFFSET_TABLE_
__libc_csu_fini
strncmp@GLIBC_2.2.5
_ITM_deregisterTMCloneTable
puts@GLIBC_2.2.5
_edata
strlen@GLIBC_2.2.5
__stack_chk_fail@GLIBC_2.4
banner
__libc_start_main@GLIBC_2.2.5
__data_start
__gmon_start__
__dso_handle
_IO_stdin_used
__libc_csu_init
__bss_start
main
__isoc99_scanf@GLIBC_2.7
sprintf@GLIBC_2.2.5
__TMC_END__
_ITM_registerTMCloneTable
__cxa_finalize@GLIBC_2.2.5
.symtab
.strtab
.shstrtab
.interp
.note.gnu.property
.note.gnu.build-id
.note.ABI-tag
.gnu.hash
.dynsym
.dynstr
.gnu.version
.gnu.version_r
.rela.dyn
.rela.plt
.init
.text
.fini
.rodata
.eh_frame_hdr
.eh_frame
.init_array
.fini_array
.dynamic
.got
.got.plt
.data
.bss
.comment
```
It is not too hard to figure out that we are looking for `[+] Correct key!`.

Talking about dynamic functions, we can simply use `readelf`.
```console
$ readelf --dyn-syms trycrackme 

Symbol table '.dynsym' contains 13 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND 
     1: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND strncmp@GLIBC_2.2.5 (2)
     2: 0000000000000000     0 NOTYPE  WEAK   DEFAULT  UND _ITM_deregisterTMCloneTab
     3: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND puts@GLIBC_2.2.5 (2)
     4: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND strlen@GLIBC_2.2.5 (2)
     5: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND __stack_chk_fail@GLIBC_2.4 (3)
     6: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND printf@GLIBC_2.2.5 (2)
     7: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND __libc_start_main@GLIBC_2.2.5 (2)
     8: 0000000000000000     0 NOTYPE  WEAK   DEFAULT  UND __gmon_start__
     9: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND __isoc99_scanf@GLIBC_2.7 (4)
    10: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND sprintf@GLIBC_2.2.5 (2)
    11: 0000000000000000     0 NOTYPE  WEAK   DEFAULT  UND _ITM_registerTMCloneTable
    12: 0000000000000000     0 FUNC    WEAK   DEFAULT  UND __cxa_finalize@GLIBC_2.2.5 (2)
```
The program seems to take its input from `__isoc99_scanf` and use standard
output functions `puts` and `printf`. But it also use utility functions
we do not want to handle like `strncmp`, `strlen` or especially `sprintf`.

It time to let GDB provide us with the actual code as if the binary was initially statically linked.

### Generate a Core Dump

We will generate a core dump file with the following GDB script.
```
set env LD_BIND_NOW=1
set env GLIBC_TUNABLES=glibc.cpu.hwcaps=-AVX2_Usable
b main
start
generate-core-file core.snapshot
kill
quit
```
First, we set the environment variable `LD_BIND_NOW`, so the dynamic linker
will resolve all dynamic symbol at startup, not waiting the functions to be
called. This way, we get a full snapshot of the program code.

Then, we disable `AVX` instruction set since it is not yet supported by
the **BINSEC** decoder.

We set a breakpoint at `main` function, where we want to start our analysis
and start the program.

Finally, we generate a core file named `core.snapshot` and quit GDB.
Pretty easy right?

We can automate this by passing the script to GDB with the following command:
```console
$ gdb -x <script_name> ./trycrackme
```

### Write up SSE script

The most significant novelty is that we will start our script with
the following command.
```
starting from core
```

This way, we instruct **BINSEC** to load the full initial state of the
process, including registers but also the additional code section coming
from the different shared libraries.

Then the rest is pretty straightforward if you have sharpened your
reverse engineering skills since the [last tutorial](intermediates_1.md).

We save the return address in order to stop the exploration at the `main`
return.

```
return_address<64> := @[rsp, 8]
```

We skip the printing function `puts` and `printf`.

```
replace <puts>, <printf> by
  caller<64> := @[rsp, 8]
  rsp := rsp + 8
  jump at caller
end
```

Then we mock the input function `__isoc99_scanf`, assuming it only require
a null terminated string (format `"%s"`). This way, we make symbolic the
bytes at the pointer given in the second parameter.

```
replace <__isoc99_scanf> by
  caller<64> := @[rsp, 8]
  assert @[rdi, 3] = "%s"z
  len<64> := nondet
  assume 0 < len < 50
  all_printables<1> := true
  for i<64> in 0 to len - 1 do
    @[rsi + i] := nondet as key
    all_printables := all_printables && " " <= key <= "~"
  end
  @[rsi + i] := "\x00"
  assume all_printables
  rsp := rsp + 8
  jump at caller
end
```

Finally, we set the directives:
- we want to reach the point where the program outputs `[+] Correct key!`;
- we stop the exploration if the program outputs, `[-] Incorrect key!`, the `main` returns or the program exits abnormally with `__stack_chk_fail`.

```
reach <printf> such that @[rdi, 16] = "[+] Correct key!"
then print ascii stream key

cut at <printf> if @[rdi, 18] = "[-] Incorrect key!"

halt at <__stack_chk_fail>
halt at return_address
```
The script is available [here](../../examples/sse/crackmes/trycrackme/crackme.ini).

Now, all we have to do is launch the following **BINSEC** command line.
```console
binsec -sse -sse-script crackme.ini -sse-depth 10000 core.snapshot
```
We use option `-sse-depth 10000` in order to increase the maximum trace size of the exploration from `1000` to `10000`.

Then is the solution.
```console
[sse:result] Ascii stream key : "34407373373234353336"
[sse:info] SMT queries
             Preprocessing simplifications
               total          25099
               sat            7259
               unsat          12924
               constant enum  4916
             
             Satisfiability queries
               total          468
               sat            235
               unsat          233
               unknown        0
               time           0.59
               average        0.00
             
           Exploration
             total paths                      216
             completed/cut paths              209
             pending paths                    7
             stale paths                      0
             failed assertions                0
             branching points                 25117
             max path depth                   7264
             visited instructions (unrolled)  146245
             visited instructions (static)    826
```


#### Conclusion

We are going to face more modern binaries, especially `x86-64` shared files.
GDB can help dealing with the complexity of dynamic linker by creating
a full snapshot of the process initial state.  
There are other similar challenges in [crackmes examples](../../examples/sse/crackmes), would you be able to solve with **BINSEC** them by yourself?

Let us just recap how to proceed:
- generate a core dump with GDB, setting the `LD_BIND_NOW` environment variable;
- identify the success and failure conditions (most likely, the output of some predefined strings);
- identify and handle the input and output functions.

Have a nice day :-)


