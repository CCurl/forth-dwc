# DWC Forth - AI Coding Assistant Instructions

## Project Overview
DWC (DWord-Code) is a minimal Forth interpreter/VM inspired by Tachyon, implemented in ~200 lines of C code. Each instruction is a 32-bit DWORD that is either a primitive (0-45), a literal (top 3 bits set, ANDed with 0x3FFFFFFF), or an execution token (code address).

**Core Implementation:** [dwc-vm.c](dwc-vm.c), [dwc-vm.h](dwc-vm.h), [system.c](system.c)  
**Bootstrap Code:** [boot.fth](boot.fth), [base.fth](base.fth)  
**Build:** 17KB on Windows (32-bit), ~21KB on Linux

Refer to README.md for detailed documentation on VM primitives, system primitives, built-in words, and embedding instructions.

## Critical Architecture Patterns

### The Three-Way Instruction Encoding
Every instruction in `code[]` array is decoded as:
- `<= 45`: Execute primitive (see `PRIMS()` macro in [dwc-vm.c](dwc-vm.c#L5-L46))
- `& 0x40000000`: Literal value (mask with `0x3FFFFFFF` to get actual value)
- Otherwise: Code address (XT) to execute

### Memory Layout
```
mem[MEM_SZ] (16MB):
  - code[0..here]: Dictionary grows upward from 0
  - mem[last..MEM_SZ]: Dictionary entries grow downward from top
  - vars area: Variable storage at 64K*4 cells from base
```

**The `(h)` / `(l)` Indirection Pattern:**
- `(h)` and `(l)` are constants holding *addresses* of `here` and `last` variables
- Enables indirect access: `(h) @` reads current HERE, `(h) !` updates it
- Used in [boot.fth](boot.fth#L3-L4): `here` and `last` are convenience wrappers
- Critical for metaprogramming: words can modify dictionary pointers
- Same pattern used for stacks: `(sp)`, `(rsp)`, `(lsp)` hold stack pointer addresses

### Dictionary Entry Structure (DE_T)
```c
struct { ucell xt; byte sz; byte fl; byte ln; char nm[NAME_LEN+1]; }
// fl bits: 0x80=IMMEDIATE, 0x40=INLINE
// Entries are 4-byte aligned, grow downward from mem[MEM_SZ]
```

### Transient Words (t0-t9)
Words named `t0` through `t9` (case-sensitive) are NOT added to the dictionary. They're stored in `tmpWords[]` array for temporary factoring without cluttering the dictionary. See [boot.fth](boot.fth#L35) and [base.fth](base.fth) for usage patterns.

### INLINE Words
When a word is marked INLINE (via `inline` word), its definition is copied directly into the caller up to the first `exit`, similar to C macros. Not INLINE means a call is compiled. See [boot.fth](boot.fth#L6) for common inline definitions like `cell`, `1+`, etc.

**Performance Implications:**
- INLINE eliminates call/return overhead (saves 2 instructions per invocation)
- Best for: constants (`cell`), simple math (`1+`, `2*`), accessors
- Avoid for: large definitions (code bloat), recursive words (infinite expansion)
- Example from [boot.fth](boot.fth#L6-L9): `cell`, `cells`, `1+` all INLINE
- Non-INLINE adds dictionary entry call; INLINE copies body directly

## Build & Test Workflows

### Windows (Visual Studio)
```powershell
# Build from VS solution
msbuild dwc.sln /p:Configuration=Release
.\Release\dwc.exe boot.fth
```

### Linux/Unix (makefile)
```bash
make              # Builds with clang -m32 -Oz
make test         # Runs ./dwc base.fth
make run          # Runs ./dwc (loads boot.fth by default)
```

**Default behavior:** If no file argument, loads `boot.fth`. Command-line args create `argc`, `arg0`, `arg1`, etc. constants.

## Forth Coding Conventions

### State Management
- State 0 = INTERPRET, 1 = COMPILE, 999 = BYE (exit)
- `:` switches to COMPILE and adds word to dictionary
- `;` compiles EXIT and returns to INTERPRET
- `[` and `]` manually switch states (both IMMEDIATE)

### Number Literal Prefixes
- `%`: Binary (e.g., `%1010`)
- `#`: Decimal (e.g., `#42`)
- `$`: Hex (e.g., `$DEADBEEF`)
- `'x'`: Character literal (e.g., `'A'` = 65)

### Variable Patterns
```forth
val name@           \ Creates efficient variable (uses literal + data cell)
(val) (name)        \ Creates accessor constant to data cell
: name! (name) ! ;  \ Setter word
```
Notes:
- `val` compiles `LIT 0` for the new word; `(val)` then returns the address of that literal cell so updating it changes what the word pushes.
Example:
```forth
val t0     (val) t1
t8 t1 !    \ now t0 pushes t8
1234 t1 !  \ now t0 pushes 1234
```

### For Loops
DWC uses `for`/`next` with `i` for index (0-based):
```forth
10 for i . next  \ Prints 0 1 2 3 4 5 6 7 8 9
```

**Loop Stack Mechanism (`lstk[]`):**
- `L0` = current index (starts at 0, incremented by `next`)
- `L1` = upper limit (exclusive, from TOS at `for`)
- `L2` = loop start PC (for jumping back)
- Loop stack grows by 3 cells per `for`, shrinks by 3 on loop exit
- Nested loops work automatically (each uses next 3 cells)
- `lsp` register points to current loop frame

### Comments
- `( ... )`: Block comments (words between parens, built-in via `doComment()`)
- `\`: Line comments (skips to EOL)

## Key System Primitives

**Dictionary:** `find` (33), `add-word` (43)  
**I/O:** `ztype` (32), `emit` (36), `key` (34), `key?` (35)  
**Files:** `fopen` (37), `fclose` (38), `fread` (39), `fwrite` (40)  
**Control:** `outer` (44) - runs outer interpreter on string address  
**System:** `system` (45) - executes shell command

## Locals (x, y, z)

DWC provides three local variables via a stack-based mechanism using cached pointers:
- `x0`, `y0`, `z0` are `val` words that push addresses (updated by `+L`/`-L`)
- `x1`, `y1`, `z1` are accessor constants to the data cells holding those addresses
- `+L` pushes a new 3-cell frame (increments x/y/z pointers by 12 bytes) if space available
- `−L` pops the current frame (decrements by 12 bytes) if not at stack base
- `+L1 ( x -- )` pops 1 value and sets x
- `+L2 ( x y -- )` pops 2 values and sets y and x (convenience wrapper for `+L` with auto-assignment)
- `+L3 ( x y z -- )` pops 3 values and sets z, y, x (convenience wrapper for `+L` with auto-assignment)
- Usage pattern:
  ```forth
  : my-word ( a b -- result ) +L2 x@ y@ +  -L ;
  ```
  The `+L2` pops 2 values and sets y=b, x=a. Then fetch and operate on them with `x@` and `y@`.

**Key insight:** Pointers are cached and updated on frame push/pop, so `x@`/`y@`/`z@` each require only one call + one fetch (no offset arithmetic at access time).

**Example: String Reversal with Locals**
```forth
: s-rev ( str -- str )
  dup dup s-end 1- +L2 begin
    x@ y@ >= if -L exit then
    c@x c@y c!x+ c!y-
  again ;
```
This uses `+L2` to set start/end pointers in locals `x@`/`y@`, with `c!x+`/`c!y-` for efficient swaps.

## Debugging Patterns

### Inspecting Dictionary
```forth
last .hex        \ Show last dictionary entry address
here .hex        \ Show HERE pointer (next free code location)
(h) @ (l) @ .hex .hex  \ Raw pointers
```

### Stack Inspection
Access internal stacks via constants defined in `dwcInit()`:
- `stk` / `(sp)` - data stack
- `rstk` / `(rsp)` - return stack  
- `lstk` / `(lsp)` - loop stack

### Memory Dumps
See [block-001.fth](block-001.fth#L17-L20) for `dump` word implementation.

## Critical Files

- **[dwc-vm.c](dwc-vm.c):** Core VM (~200 lines) with `inner()` interpreter loop and `outer()` parser
  - `PRIMS(X)` macro: defines all 46 primitives (0-45)
  - `inner(pc)`: main execution loop (switch on instruction type)
  - `outer(src)`: parser/compiler (reads words, compiles or executes)
  - `nextWord()`: tokenizer (reads next whitespace-delimited word)
  - `isNum()`: number parser (handles %, #, $, 'x' prefixes)
  - `findInDict()`: dictionary lookup (linear search)
  - `addToDict()`: dictionary entry creation (grows downward)
  - `compileNum()`: efficient literal compilation (inline if small)

- **[dwc-vm.h](dwc-vm.h):** Platform abstractions, type definitions, VM/system interface
  - `DE_T`: dictionary entry struct
  - `NVP_T`: name-value pair for constants
  - Memory size, stack sizes, flag definitions
  - Platform-specific conditionals (Windows vs. Unix)

- **[system.c](system.c):** Platform-specific I/O
  - Windows: `_kbhit()`, `_getch()`, `Sleep()`
  - Unix/Linux: termios raw mode, `select()` for non-blocking input
  - File operations: `fopen`, `fclose`, `fread`, `fwrite` (cross-platform via FILE*)
  - `timer()`, `ms()`, `zType()`, `emit()`
  - `boot()`: loads initial Forth file
  - `repl()`: read-eval-print loop (used if no boot file)

- **[boot.fth](boot.fth):** Minimal bootstrap (~100 lines)
  - Hard-coded state management: `:`, `;`
  - Control flow: `if`/`then`, `-if`, `begin`/`again`/`while`/`until`
  - Variable pattern: `val`/`(val)`
  - Locals: `x@`/`y@`/`z@` with `+L`/`-L` frame management
  - String literals: `z"` and `."` words
  - Basic arithmetic and stack ops
  - Number formatting: `<#`, `#`, `#s`, `#>`
  - Essential for all higher-level Forth code

- **[base.fth](base.fth):** Extended library (~400 lines)
  - Advanced locals patterns (alternative stack, push/pop)
  - ANSI color and screen control
  - String utilities (copy, reverse, search)
  - Benchmarking words (loop tests, fibonacci)
  - Introspection: `see` word (disassembler)
  - Fixed-point math
  - Debugging: `dump`, `.s`, `.stk`

- **[block-001.fth](block-001.fth):** Utilities and examples (~200 lines)
  - Advanced number formatting and display
  - Color codes and screen positioning
  - Benchmarking examples
  - Custom stack implementations
  - Usage examples for most library words

## When Modifying C Code

1. **Primitives**: Add to `PRIMS()` macro in [dwc-vm.c](dwc-vm.c#L5). The macro expands to enum, switch case, and name array.
2. **Memory size**: Change `MEM_SZ` in [dwc-vm.h](dwc-vm.h#L18) (default 16MB).
3. **Platform I/O**: Edit [system.c](system.c) for `key()`, `qKey()`, `ms()`, file ops.
4. **Constants**: Add to `nv[]` array in `dwcInit()` to expose to Forth ([dwc-vm.c](dwc-vm.c#L171-L178)).

## When Writing Forth Code

- **Every word must include a stack effect comment** in the format `( input -- output )`. This documents the contract clearly.
- **Prefer chaining multiple small 1-line definitions** over large multi-line definitions. Each word passes its result to the next via the stack.
- **Almost all words in DWC are 1-liners.** Example: `#c`, `#.`, `#n`, `#`, `#s`, `<#`, `#>`, `(.)` are all single-line compositions. This style makes code modular, testable, and easier to factor.
- Use `inline` for performance-critical small words (< 5 instructions)
- Use `t0-t9` for helper words that don't need to be in dictionary
- Prefer `for`/`next` over `begin`/`until` for counted loops
- Use `val`/`(val)` pattern for variables instead of traditional Forth `variable`
- Check [boot.fth](boot.fth) first - many common words already defined there

## Word Categories Reference

### Primitives (0-31)
Direct VM operations: `exit`, `lit`, `jmp`, `jmpz`, `jmpnz`, `njmpz`, `njmpnz`, `dup`, `drop`, `swap`, `over`, `!` (store), `@` (fetch), `c!` (byte store), `c@` (byte fetch), `>r` (to return), `r@` (read return), `r>` (from return), `*`, `+`, `-`, `/mod`, `<`, `=`, `>`, `+!`, `for`, `i`, `next`, `and`, `or`, `xor`

### System Primitives (32-45)
I/O and control: `ztype`, `find`, `key`, `key?`, `emit`, `fopen`, `fclose`, `fread`, `fwrite`, `ms`, `timer`, `add-word`, `outer`, `system`

### Built-in Constants
Stack/memory access: `(h)` (HERE addr), `(l)` (LAST addr), `(sp)` (data stack ptr addr), `stk` (data stack addr), `(rsp)` (return stack ptr addr), `rstk` (return stack addr), `(lsp)` (loop stack ptr addr), `lstk` (loop stack addr), `state` (STATE addr), `base` (BASE addr), `mem` (memory base), `mem-sz`, `>in` (input buffer ptr), `version`, `output-fp`

### Bootstrap Words (from [boot.fth](boot.fth))
**Control flow:** `:`, `;`, `if`, `then`, `-if`, `if0`, `-if0`, `begin`, `again`, `while`, `-while`, `until`

**Basic definitions:** `cell`, `cells`, `1+`, `1-`, `2cells`, `3cells`, `->code`, `code@`, `code!`, `,` (comma), `inline`, `immediate`, `bye`

**Variables:** `val`, `(val)` - efficient variable mechanism

**Locals:** `x@`, `x!`, `y@`, `y!`, `z@`, `z!`, `x++`, `x--`, `x@+`, `x@-`, `c@x`, `c@x+`, `c@x-`, `c!x`, `c!x+`, `c!x-` (and y/z variants), `+L`, `-L`, `+L1`, `+L2`, `+L3`

**Stack utilities:** `rdrop`, `tuck`, `nip`, `2dup`, `2drop`, `-rot`, `0=`, `0<`, `<=`, `>=`, `?dup`, `depth`, `.s`

**Arithmetic:** `negate`, `abs`, `<=`, `>=`, `++`, `--`, `btwi`, `*/`, `/`, `mod`

**I/O:** `cr`, `tab`, `space`, `spaces`, `emit`, `type`, `execute`, `unloop`

**Strings:** `pad`, `fill`, `cmove`, `cmove>`, `s-len`, `s-end`, `s-cpy`, `s-cat`, `s-catc`, `s-catn`, `s-eqn`, `s-eq`, `s-rev`

**Number output:** `<#`, `#`, `#s`, `#>`, `(.)`, `.`, `.word`, `words`, `words-n`, `.nwb`, `decimal`, `hex`, `binary`, `.hex`, `.hex4`, `.hex8`, `.bin`, `.bin16`, `.dec`, `.hex/dec`

**Dictionary:** `min`, `max`, `compiling?`, `allot`, `var`, `dict-end`, `find`, `last`, `here`

**Files:** `fopen-r`, `fopen-w`, `->file`, `->stdout`, `->stdout!`, `rb`

### Library Words (from [base.fth](base.fth))
**Locals patterns:** alternative x/y/z stacks, push/pop operations (`>p`, `<p`, `>x`, `>xy`, `>xyz`, `<x`, `<xy`, `<xyz`)

**Screen/ANSI:** `csi`, `->cr`, `->rc`, `cls`, `clr-eol`, `cur-on`, `cur-off`, `cur-block`, `cur-bar`, `cur-rt`, `bg`, `fg`, `color`, `black`, `red`, `green`, `yellow`, `blue`, `purple`, `cyan`, `grey`, `white`

**Benchmarking:** `timer`, `ms`, `fib`, `bm-while`, `bm-loop`, `bm-fib`, `bm-fibs`, `mil`, `bb`, `bm-all`

**Introspection:** `see`, `.prim?`, `.lit?`, `find-xt`, `next-xt`, `.lit-jmp?`, `see-range`

**Fixed-point:** `f.`, `f*`, `f/`, `f+`, `f-`

**Utilities:** `lg`, `ll`, `vi`, `dump`, `.pstk`

## Dictionary Entry Structure

Each dictionary entry is a struct with:
```
struct DE_T {
  ucell xt;          // 4 bytes: execution token (code address)
  byte sz;           // 1 byte:  total entry size (for traversal)
  byte fl;           // 1 byte:  flags (0x80=IMMEDIATE, 0x40=INLINE)
  byte ln;           // 1 byte:  name length
  char nm[NAME_LEN+1]; // name string (up to 26 bytes with null)
}
```

Entries are:
- **4-byte aligned:** size is always a multiple of 4
- **Grown downward:** new entries added at decreasing addresses from `mem[MEM_SZ]`
- **Traversed via size:** walk dictionary by adding `dp->sz` to get next entry

Flags:
- `0x80` (IMMEDIATE): word executes even during COMPILE state
- `0x40` (INLINE): definition is copied into caller up to first EXIT (like C macros)
- `0x00`: normal word - compiled as a call during COMPILE state

## Common Code Patterns

### Variable Definition and Use
```forth
val counter   (val) (counter)    
\ val counter creates: LIT 0 EXIT
\ (val) (counter) creates accessor to the 0 cell
\ Example: 42 (counter) ! now counter pushes 42
```

### String Processing
```forth
: string-example ( -- )  \ Demonstrate string operations
    z" hello" s-len . cr         \ Get length of string literal
    pad z" test" s-cpy           \ Copy string to pad area
    z" suffix" s-cat             \ Concatenate
    '!' s-catc                   \ Add character
    ztype cr                     \ Print result
;
```

### Memory Operations with Locals
```forth
: copy-block ( from to size -- )  \ Copy memory block
    +L3 z! y! x!                 \ Set up: x=from, y=to, z=size
    z@ for                       \ Loop 'size' times
        c@x+ c!y+                \ Fetch from x, store to y, both increment
    next
    -L                           \ Pop locals frame
;
```

### Conditional Compilation
```forth
: min ( a b -- min )
    over over > if swap then drop ; 
```
Note: `-if` (non-destructive) leaves TOS on stack, `if` consumes it.

### Efficient Loop with Early Exit
```forth
: search ( addr target -- found-addr|0 )  \ Find char in string
    +L2                          \ y=target, x=addr (already popped from stack)
    begin
        c@x dup 0= if            \ Check for null terminator
            -L drop 0 exit
        then
        y@ = if -L x@ exit then  \ Found match
        x++ 1+ again
    again
;
```

## Compilation Model: STATE and Immediate Words

### STATE Values
- `0` (INTERPRET): words execute immediately
- `1` (COMPILE): words generate code (or execute if IMMEDIATE flag set)
- `999` (BYE): exit DWC

### Parsing Flow
When `outer()` processes a word:
1. Check for comments `(` or `\` - skip to matching `)` or EOL
2. Check for hard-coded state changes: `:` (COMPILE) or `;` (back to INTERPRET)
3. Try to parse as number (prefix: `%` binary, `#` decimal, `$` hex, `'x'` char literal)
4. Look up in dictionary via `findInDict()`
5. If found:
   - **INTERPRET state**: Always execute (call `inner()` with word's XT)
   - **COMPILE state**:
     - If IMMEDIATE flag set: execute (as if INTERPRET)
     - If INLINE flag set: copy word's code body until EXIT into current definition
     - Else: compile a call to the word's XT

### INLINE Mechanism
When a word marked INLINE is compiled:
- Instead of emitting a call (XT), the assembler copies all instructions from the word's definition up to the first EXIT
- This eliminates call/return overhead
- Trade-off: increases code size (duplication)
- Best for: tiny words (<5 instructions), frequently called
- Avoid for: large definitions, recursive words

### Example: Inline vs Normal
```forth
: 1+ ( n -- n' ) 1 + ; inline        \ Copied into caller
: 2+ ( n -- n' ) 1+ 1+ ;             \ Calls 1+ twice (inlined each time)

: slow-add ( a b -- c ) 2+ + ;       \ Compiles as: call 2+, then call +
```

## Debugging & Introspection

### Stack Inspection
```forth
(sp) @         \ Get current data stack pointer (number of items - 1)
depth          \ Get data stack depth
.s             \ Print entire data stack
```

### Dictionary Inspection
```forth
last .hex      \ Address of last dictionary entry
here .hex      \ Address of next code location (HERE pointer)
words          \ List all words with word counts per line
words-n n      \ List first n words
```

### Memory Dumping
```forth
addr n dump    \ Show n bytes from addr in hex + ASCII
```

### Code Inspection (see word)
```forth
see word-name  \ Disassemble word's definition
               \ Shows: primitives by name, literals in hex, XT addresses
```

The `see` word works by:
1. Finding the word in dictionary
2. Walking instructions from its XT until EXIT
3. Decoding each instruction:
   - `<= 45`: print primitive name
   - `& 0x40000000`: print as literal
   - Otherwise: look up as dictionary entry and print word name

## Performance Considerations

### Code Size vs Speed
- **INLINE words:** Zero call overhead but duplicate code (use sparingly)
- **Normal words:** One call/return per invocation (~2 instructions)
- **Primitives:** Direct VM operations (fastest)

### Memory Layout Impact
- Code grows upward from `mem[0]`, space for ~100K instructions on 16MB system
- Dictionary grows downward from `mem[MEM_SZ-1]`, space for ~1000 entries
- Variables area at `64K * 4` cells from base - separate from code/dictionary
- If dictionary or code grows too large, they collide: monitor with `.banner`

### Common Optimizations
- Factor frequently-used operations into INLINE words
- Use locals (`x@`/`y@`/`z@`) instead of return stack for temporary values (faster)
- Prefer `for`/`next` over `begin`/`until` (loop stack is more efficient)
- Use `cell+` (adds 4 to address) sparingly; instead pre-compute offsets

### Stack Performance
- Data stack operations: O(1) (push/pop from array)
- Return stack operations: O(1) (used for nesting, `>r`/`r>` are fast)
- Loop stack operations: O(1) (used by `for`/`next`)
- Dictionary lookups: O(n) linear search from `last` to `mem[MEM_SZ]`
