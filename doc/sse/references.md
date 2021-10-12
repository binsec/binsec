# Sybolic Execution Reference Manual

## DBA Expression

```
      <e> ::= <cst> | <lv> | <unop> <e> | <e> <binop> <e> | <e> "?" <e> ":" <e> | "(" <e> ")"
     <lv> ::= <var> | <mem>
    <mem> ::= "@[" <e> [["," ("<-" | "->")] "," <n>] "]"
   <unop> ::= "~" | "-" | "uext"<n> <e> | "sext"<n> <e> | <e> "{" <n> ".." <n> "}"
  <binop> ::= <boolean> | <cmp> | <arith> | <bitwize> | "::"
<boolean> ::= "!" | "&&" | "||"
    <cmp> ::= "=" | "<>" | "<"["u"] | "<s" | "<="["u"] | "<=s" | ">"["u"] | ">s" | ">="["u"] | ">=s"
  <arith> ::= "+" | "-" | "*" | "/"["u"] | "/s" | "%"["u"] | "%s" | "asr"
<bitwize> ::= "&" | "|" | "^" | "lsl" | "lsr" | "rol" | "ror"
```

#### Constant `CST`

- Boolean value (e.g. `true` or `false`)

- Numeric value
  - binary format (e.g. `0b101010`)
  - hexadecimal format (e.g. `0x2a` or `0x2A`)
  - decimal format (e.g. `42`)
  
- ASCII string (e.g. `"ASCII string"` is equivalent to `0x676e69727473204949435341`)

- Symbol value (e.g. `<.text>`)

#### Variable *ident[*`<`*bit-size*`>`*]* 
(e.g. `eax` or `eax<32>`)

#### Memory access `@[`*address[[*`,`*endianness]*`,`*byte-size]*`]`

- arbitrary *address* expression (e.g `0x4000`, `esp - 4`, etc.)
- little endian multi-byte access `<-`  
  (e.g. `@[0x4000,<-,4]` is equivalent to `@[0x4003] :: @[0x4002] :: @[0x4001] :: @[0x4000]`)
- big endian multi-byte access `->`  
  (e.g. `@(0x4000,->,4]` is equivalent to `@[0x4000] :: @[0x4001] :: @[0x4002] :: @[0x4003]`)
- *byte size* should be constant positive interger [default: 1] 

#### Parenthesis `(`*expr*`)`
(e.g. `4 * (ecx + 1)`) 

#### Arithmetic operator

- negation `-`***expr***  
  (e.g. `-eax`)

- addition ***expr*** `+` ***expr***  
  (e.g. `eax + edx`)

- substraction ***expr*** `-` ***expr***  
  (e.g. `eax - edx`)

- multiplication ***expr*** `*` ***expr***  
  (e.g. `eax * edx`)

- unsigned division ***expr*** `/`***[u]*** ***expr***  
  (e.g. `eax / edx` or `eax /u edx`)

- signed division ***expr*** `/s` ***expr***  
  (e.g. `eax /s edx`)

- unsigned remainder ***expr*** `%`***[u]*** ***expr***  
  (e.g. `eax % edx` or `eax %u edx`)
  
- signed remainder ***expr*** `%s` ***expr***  
  (e.g. `eax %s edx`)
  
- arithmetic shift ***expr*** `asr` ***expr***  
  (e.g. `eax asr edx`)

#### Boolean operator

- negation `!`***bool***  
  (e.g. `!zf`)
  
- conjonction ***bool***`&&`***bool***  
  (e.g. `zf && sf`)

- disjonction ***bool***`||`***bool***  
  (e.g. `zf || sf`)

#### Comparison operator

- equal ***expr*** `=` ***expr*** *[chainable]*  
  (e.g. `eax = edx = ecx`)
  
- distinct ***expr*** `<>` ***expr***  
  (e.g. `eax <> edx` is equivalent to `!(eax = edx)`)
  
- unsigned lower than ***expr*** `<`***[u]*** ***expr*** *[chainable]*  
  (e.g. `0 < eax < 31` or `0 <u eax <u 31`)
  
- signed lower than ***expr*** `<s` ***expr*** *[chainable]*  
  (e.g. `-31 <s eax <s 31`)
  
- unsigned lower or equal ***expr*** `<=`***[u]*** ***expr*** *[chainable]*  
  (e.g. `0 <= eax <= 31` or `0 <=u eax <=u 31`)
  
- signed lower or equal ***expr*** `<=s` ***expr*** *[chainable]*  
  (e.g. `-31 <=s eax <=s 31`)

- unsigned greater than ***expr*** `>`***[u]*** ***expr*** *[chainable]*  
  (e.g. `31 > eax > 0` or `31 >u eax >u 0`)
  
- signed greater than ***expr*** `>s` ***expr*** *[chainable]*  
  (e.g. `31 >s eax >s -31`)
  
- unsigned greater or equal ***expr*** `>=`***[u]*** ***expr*** *[chainable]*  
  (e.g. `31 >= eax >= 0` or `31 >=u eax >=u 0`)
  
- signed greater or equal ***expr*** `>=s` ***expr*** *[chainable]*  
  (e.g. `31 >=s eax >=s -31`)
  


#### Bitwize operator

- bitwize not `~`***expr***  
  (e.g. `~eax`)
  
- bitwize and ***expr*** `&` ***expr***  
  (e.g. `eax & edx`)
  
- bitwize or ***expr*** `|` ***expr***  
  (e.g. `eax | edx`)
  
- bitwize xor ***expr*** `^` ***expr***  
  (e.g. `eax ^ edx`)
  
- logical shift left ***expr*** `lsl` ***expr***  
  (e.g. `eax lsl edx`)
  
- logical shift right ***expr*** `lsr` ***expr***  
  (e.g. `eax lsr edx`)

- left rotation ***expr*** `rol` ***expr***  
  (e.g. `eax rol edx`)
  
- right rotation ***expr*** `ror` ***expr***  
  (e.g. `eax ror edx`)

- zero extension `uext`***bit-size*** ***expr***   
  (e.g. `uext32 al`)

- sign extension `sext`***bit-size*** ***expr***  
  (e.g. `sext32 al`)

- restriction ***expr***`{`***hi***`..`***lo***`}`  
  (e.g. `eax{7..0}` is equivalent to `al`)

- concatenation ***expr*** `::` ***expr***
  (e.g. `edx :: eax`)

#### Ternary operator *bool* `?` *expr* `:` *expr*  
  (e.g. `eax = edx ? ecx : ebx`)


## DBA Instruction

```
     <chunk> ::= [ <bloc> ] <terminator>
      <bloc> ::= [<label> ":"] <stmt> [;] ..
<terminator> ::= "jump" "at" <e> | "halt"
      <stmt> ::= <instr> | <if> | <for> | <while> | <switch>
	    <if> ::= "if" <e> "then" <bloc> ["else" <bloc>] "end"
	   <for> ::= "for" <var> "in" <e> "to" <e> "do" <bloc> "end"
	 <while> ::= "while" <e> "do" <bloc> "end"
	<switch> ::= "case" <e> "is" (<cst> ":" <bloc>) .. "end"
	 <instr> ::= <assign> | "assert" <e> | "assume" <e> | "goto" <label>
    <assign> ::= <lv> ":=" (<e> | "undef" | "nondet") ["as" <var>]
```

#### Deterministic assignment *lval* `:=` *expr*
  (e.g. `eax := 4`)
  
#### Nondeterministic assignment *lval* `:=` `nondet` *[*`as` *ident]*
  (e.g. `@[esp + 4, 4] := nondet as arg`)
  
#### Definition removal *lval* `:=` `undef`
  (e.g. `of := undef`)
  
#### Assert `assert` *bool*
  (e.g. `assert eax = 0`)
  
#### Assume `assume` *bool*
  (e.g. `assume eax = 0`)
  
#### Label `.`*ident* `:` *instr*
  (e.g. `.loop: ecx := ecx - 1`)
  
#### Goto `goto` `.`*ident*
  (e.g. `goto .loop`)
  
#### If then else `if` *bool* `then` *stmts* *[*`else` *stmts]* `end`
  (e.g. `if eax = 0 then edx := ecx + 8 else ecx := edx - 4 end`)
  
#### For loop `for` *ident* `in` *expr* `to` *expr* `do` *stmts* `end`
  (e.g. `for i<32> in 0 to 31 do @[edi + i] := 0 end`)
  
#### While loop `while` *bool* `do` *stmts* `end`
  (e.g. `while @[esi] <> 0 do esi := esi + 1 end`)
  
## SSE Script

```
<script> ::= (<assign> | <pragma> | <goal>) ..
<pragma> ::= "starting" "from" <e> | "assume" <e> 
           | "load" <mem> "from" "file" | "load" <section> "from" "file" | "replace" <e> "by" <chunk> 
  <goal> ::= "reach" <e> [<n> "times"] ["such" "that" <e>] ["then" <action> ["and" <action> ..]]
           | "cut" "at" <e> ["if" <e>]
           | "at" <e> ("assume" | "assume") <e>
           | "at" <e> enumerate <e> ["(" <n> ")"]
<action> ::= "print" "formula" ["for" <e> ["as" <var>] ["," <e> ["as" <var>] ..]]
           | "print" "model"
           | "print" [<format>] <e>
           | "print" "ascii" "stream" <var>
<format> ::= "bin" | "dec" | "hex" | "ascii"
```

#### Set entrypoint `starting` `from` *expr*
  (e.g. `starting from 0x4000`)
  
#### Initialize memory from section `load` *section* `from` `file`
  (e.g. `load .data from file`)
  
#### Initialize specific memory from file `load` *memory-access* `from` `file`
  (e.g. `load @[0x4000, 256] from file`)
  
#### Set a DBA stub `replace` *addr* `by` *chunk*
  (e.g. `replace <printf> by esp := esp + 4; jump at @[esp - 4]`)
  
#### Set reach target `reach` *addr [n* `times`*] [*`such` `that` *bool] [*`then` *actions]*
  (e.g. `reach 0x4000 such that al = 0 then print ecx`)
  
##### Print SMT model `print` `model`
  (e.g. `reach 0x4000 then print model`)
  
##### Print full SMT formula `print` `formula`
  (e.g. `reach 0x4000 then print formula`)
  
##### Print SMT formula slice `print` `formula` `for` *exprs*
  (e.g. `reach 0x4000 then print formula for eax`)
  
##### Print expression (last) value `print` *[format]* *expr*
  (e.g. `reach 0x4000 then print eax`)
  
##### Print variable values as stream `print` `ascii` `stream` *ident*
  (e.g. `reach 0x4000 then print ascii stream input_byte`)
  
#### Set address blacklist `cut` `at` *addr [*`if` *bool]*
  (e.g. `cut at 0x4000 if al = 0`)
  
#### Add path assumption `at` *addr* `assume` *bool*
  (e.g. `at 0x4000 assume al = 0`)
  
#### Check path constraint `at` *addr* `assert` *bool*
  (e.g. `at 0x4000 assert al = 0`)
  
#### Enumerate possible values `at` *addr* `enumerate` *expr [*`(` *times* `)`*]*
  (e.g. `at 0x4000 enumerate eax (10)`)
  
## Command line option

#### `-sse-depth <n>`
Set exploration maximal depth *[default:*`1000`*]*
  (e.g. `-sse-depth 100000`)
  
#### `-sse-heuristics {dfs|bfs|nurs}`
Change the search heuristics *[default:*`dfs`*]*

#### `-sse-jump-enum <n>` 
Change the maximum number of jump targets computed fo dynamic jumps *[default:*`3`*]*

#### `-sse-keep-going`
Ignore errors returned by the SMT solver (`stale path`) *[default:*`abort`*]*

#### `-sse-load-ro-sections`
Load the content of all read-only sections *[default:*`false`*]*

#### `-sse-script <file>[,<file>..]`
Read script from files.

#### `-sse-seed <n>`
Give a specific seed for random number generators *[default:*`0`*]*

#### `-sse-timeout <n>`
Set a global timout in second for symbolic execution *[default:*`inf`*]*

#### `-fml-solver {z3|cvc4|yices|boolector|bitwuzla}`
Set solver to use *[default:*`z3`*]*

#### `-fml-solver-timeout <n>`
Set a timout in second for solver queries *[default:*`5s`*]*
