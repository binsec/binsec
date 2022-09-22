# SSE Tutorial \#1: First steps with BINSEC SSE

In this post, we will exercise the **S**tatic **S**ymbolic **E**xecution engine of **BINSEC** over the small *CTF* puzzle named [**magic**](../../examples/sse/quickstart/magic).   
You can find everything about this tutorial in [/examples/sse/quickstart](../../examples/sse/quickstart).

What are we looking for? *Capture The Flag* games can take the form of a reverse-engineering challenges to find an input (password, etc.) that will reach a special state of the program such as leading to an exploit, a crash or simply printing a victory message. 

### Requirements

To run to completion we will need:
- a **Linux** environment, preferably on a **x86** machine;
- the **strings**, **objdump** and **readelf** utilities (part of the **binutils** package);
- the **BINSEC** tool (see [install instructions](../../INSTALL.md));
- a SMT solver, preferably [**ocaml-bitwuzla**](../../INSTALL.md#Dependencies)
  (but would work with **bitwuzla**, **boolector**, **Z3**, etc.).

Some knowledge about **x86** assembly and reverse-engineering could also help. 

### (Basic) reverse-engineering

At first, the command `file` can give some useful information:
```console
$ file magic
magic: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), statically linked, not stripped
```
This puzzle consists of a single **ELF** **x86**-32bit executable that does not contain dynamic function calls and contains its symbols. This will make the job easier.

At this point, we can simply run the executable to see what is happening -- a good practice would be to run it in a virtual environment, but here, we swear it does not contain any malicious code.
```console
$ ./magic
FEEDC0DE
1234
8BADF00D
$ echo $?
1
```
The program is prompting for some inputs, but failing to provide the expected (secret) value terminates the program with the (hex-speak) message `ate bad food` and the error code `1`.

Using the command `strings` shows the human readable strings of the program.
```console
$ strings magic
FEEDC0DE
DEADFEED
8BADF00D
_start
magic
.symtab
.strtab
.shstrtab
.text
```
Here are some interesting findings:
- the file likely contains the sections `.text`, `.symtab`, `.strtab` and `.shstrtab`;
- the file likely has two symbols `_start` (the entrypoint of standard programs) and `magic` (is it an hint?);
- we already met the messages `FEEDC0DE` and `8BADF00D`, but not yet `DEADFEED`.

Let us try the last one.
```console
$ ./magic
FEEDC0DE
DEADFEED
8BADF00D
$ echo $?
1
```
Hum... I guess it would have been too easy ¯\\\_(ツ)\_/¯

Let us try to validate the other assumptions about the strings with the command `readelf`.
```console
$ readelf -st magic
There are 5 section headers, starting at offset 0x1d0:

Section Headers:
  [Nr] Name
       Type            Addr     Off    Size   ES   Lk Inf Al
       Flags
  [ 0] 
       NULL            00000000 000000 000000 00   0   0  0
       [00000000]: 
  [ 1] .text
       PROGBITS        08048054 000054 00010a 00   0   0  1
       [00000007]: WRITE, ALLOC, EXEC
  [ 2] .symtab
       SYMTAB          00000000 000160 000040 10   3   2  4
       [00000000]: 
  [ 3] .strtab
       STRTAB          00000000 0001a0 00000e 00   0   0  1
       [00000000]: 
  [ 4] .shstrtab
       STRTAB          00000000 0001ae 000021 00   0   0  1
       [00000000]: 

Symbol table '.symtab' contains 4 entries:
   Num:    Value  Size Type    Bind   Vis      Ndx Name
     0: 00000000     0 NOTYPE  LOCAL  DEFAULT  UND 
     1: 08048054     0 SECTION LOCAL  DEFAULT    1 
     2: 080480e0     0 NOTYPE  GLOBAL DEFAULT    1 _start
     3: 08048054    30 FUNC    GLOBAL DEFAULT    1 magic
```
The file has the 4 expected sections and the symbol `magic` is a function which even has the luxury of having a size (thank you non stripped binary).

We will now dive into the assembly code using `objdump`.
```console
$ objdump --disassemble=magic magic

magic:     file format elf32-i386


Disassembly of section .text:

08048054 <magic>:
 8048054:	8b 54 24 04          	mov    0x4(%esp),%edx
 8048058:	31 c0                	xor    %eax,%eax
 804805a:	89 d1                	mov    %edx,%ecx
 804805c:	0f c9                	bswap  %ecx
 804805e:	84 c9                	test   %cl,%cl
 8048060:	79 0f                	jns    8048071 <magic+0x1d>
 8048062:	d1 ea                	shr    %edx
 8048064:	f5                   	cmc    
 8048065:	d1 d0                	rcl    %eax
 8048067:	71 f9                	jno    8048062 <magic+0xe>
 8048069:	05 07 f6 f6 6a       	add    $0x6af6f607,%eax
 804806e:	0f 94 c0             	sete   %al
 8048071:	c3                   	ret
```
It looks like the function takes a single `doubleword` argument -- the **x86**-32 stores argument in the stack (`0x4(%esp)`) -- and performs some obscure computation on it.

If we look at the disassembly of the symbol `_start` (`$ objdump --disassemble=_start magic`), we can see that the function `magic` is called at the address `0x8048126` and that the return value `%al` is checked right after for `null` value.

Of course, we could try to reverse-engineer all the code by hand but we will use the **BINSEC** platform instead.
We will try to find the `0x4(%esp)` values that leads to different return value of the function.
Let us dive in.

### First try with BINSEC SSE

The easiest way to launch the **BINSEC** symbolic execution is to write a script file.
Let us create our first script -- arbitrarily called `crackme.ini`, together.

In the first place, we do not want to start the execution from the standard entrypoint `_start`.
So, here is the way to specify our own entrypoint. 
```text
starting from <magic>
```
The command accepts arbitrary constant expressions, including symbol name surrounded by `<` `>`.

The next step is to define the goal of the exploration: we will use the command `reach`.
```text
reach <magic:last> then print @[esp + 4, 4] and print al
```
We are using `<magic:last>` that translates to the address of the last byte of the function `magic` (according to debug information) and that (as if by magic) corresponds to the address of the final `ret` instruction.
Upon reaching the goal, we want **BINSEC** to give us the value of the stack argument -- `4(%esp)` translates into DBA as `@[esp + 4, 4]` where `esp + 4` is the address and the second `4` is the number of bytes addressed -- and the return value `%al`.

Let us run **BINSEC** with the following.
```console
$ binsec -sse -sse-script crackme.ini magic
[sse:result] Directive :: path 0 reached address 08048071 (0 to go)
[sse:result] Value @[(esp<32> + 4<32>),4] : 0x80808000
[sse:result] Value eax<32>{0,7} : 0x00
[sse:info] SMT queries
             Preprocessing simplifications
               total          3
               sat            1
               unsat          0
               constant enum  2
             
             Satisfiability queries
               total          68
               sat            37
               unsat          31
               unknown        0
               time           0.31
               average        0.00
             
           Exploration
             total paths                      3
             completed/cut paths              0
             pending paths                    3
             stale paths                      0
             failed assertions                0
             branching points                 33
             max path depth                   135
             visited instructions (unrolled)  135
             visited instructions (static)    12
```
**BINSEC** found that function `magic` returns `0` when it is feed with value `0x80808000`.  
Let us check the behavior of the program when we feed this value.
```console
./magic
FEEDC0DE
80808000
8BADF00D
$ echo $?
1
```
It is still not it.

Since the return value `0` seems to be a dead end, we will ask **BINSEC** to find an input value such that `%al` is not `null`.
```text
reach <magic:last> such that al <> 0 then print @[esp + 4, 4]
```

This time, **BINSEC** gives us another value, actually looking as hex-speak too.
```console
$ binsec -sse -sse-script crackme.ini magic
[sse:warning] Jump (08048071, 1) : goto @[(esp<32> - 4<32>),4] #return
              could have led to invalid address 80808080; skipping
[sse:warning] Jump (08048071, 1) : goto @[(esp<32> - 4<32>),4] #return
              could have led to invalid address 00000000; skipping
[sse:warning] Jump (08048071, 1) : goto @[(esp<32> - 4<32>),4] #return
              could have led to invalid address 01010101; skipping
[sse:result] Directive :: path 2 reached address 08048071 (0 to go)
[sse:result] Value @[(esp<32> + 4<32>),4] : 0xc0dedead
[sse:info] SMT queries
             Preprocessing simplifications
               total          2
               sat            0
               unsat          0
               constant enum  2
             
             Satisfiability queries
               total          74
               sat            42
               unsat          32
               unknown        0
               time           0.34
               average        0.00
             
           Exploration
             total paths                      6
             completed/cut paths              3
             pending paths                    3
             stale paths                      0
             failed assertions                0
             branching points                 35
             max path depth                   139
             visited instructions (unrolled)  142
             visited instructions (static)    13
```
Let us try again with this new value.
```console
$ ./magic
FEEDC0DE
C0DEDEAD
DEADFEED
$ echo $?
0
```
It is a win this time, the return code of the program is `0`!

### Script strengthening

Here our script worked almost the first time. Yet, there are some points that may not work as expected in a more complex example.

For instance, we are printing the content of the stack at the time of the `reach` but it could perfectly have changed since the start of the function. To avoid this problem, we can save the initial value of `4(%esp)` in a new independent variable.
```text
arg<32> := @[esp + 4, 4]
```
We have to write the size annotation `<32>` to make the **BINSEC** parser know the bit-width of the newly created variable. We then can change the goal objective with the new name.
```text
reach <magic:last> such that al <> 0 then print arg
```

We may also want to concretize (or add constraints) to some registers or memory locations -- by default, everything starts symbolic.
Especially, we (really) often do not want the stack pointer `%esp` to point anywhere in the memory so it is preferable to give it an arbitrary, but yet realistic, value (for instance `0xffffccf1`).
```text
esp := 0xffffccf1
```

The other point to discuss is the address of our objective `<magic:last>`.  
It works because the file has symbol information, but also because the last instruction of the function is actually located at the last address of the function. Unlike in source languages, the exit point of the function is not always easy to get. Moreover, it does not have to be unique.  
A convenient way to deal with this problem is to fix the return address of the function to an arbitrary value (for instance `0x0804812b`) an then use this address as our goal. 

We can do that by adding the following lines -- in **x86**, the return address of a function is put on the stack at `(%esp)`. 
```text
return_address<32> := 0x0804812b
@[esp, 4]          := return_address

reach return_address such that al <> 0 then print arg
```

Last but not least, we may not want the execution to go past the return address.
Indeed, the exploration engine may try to explore at some uninitialized addresses, as we have already seen.
```console
[sse:warning] Jump (08048071, 1) : goto @[(esp<32> - 4<32>),4] #return
              could have led to invalid address 01010101; skipping
```
But, the exploration may also continue, exploring a lot of unwanted paths and thus take (much) more time.  
It would be the case for instance in the last version of our script if the exploration did not reach an unsupported instruction `int 0x80`.
```console
[sse:warning] Cut @ (0804815b, 0) : #unsupported cd 80
```
*As a general rule, the sooner the paths are cut, the better.*  
We can add cut directives as the following to avoid falling in the worst cases.
```text
cut at return_address
```

We are now done with `magic`.

### Conclusion

Congratulation, you successfully used **BINSEC** symbolic execution to solve a small puzzle.  
Let us just recap how we proceeded:
- give the entrypoint of the exploration;
- set the goal of the exploration;
- save some initial value for later use;
- initialize some values (especially the stack pointer and return address);
- do not forget to cut unwanted paths.

Now you will find [here](intermediates_1.md) an other example of how to use **BINSEC** symbolic execution to solve a [FLARE On](https://flare-on.com/) CTF challenge.  
Have a nice day :-)
