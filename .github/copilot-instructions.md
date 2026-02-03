# DWC Forth - AI Coding Assistant Instructions

## Project Overview
DWC (DWord-Code) is a minimal Forth interpreter/VM inspired by Tachyon, implemented in ~200 lines of C code. Each instruction is a 32-bit DWORD that is either a primitive (0-45), a literal (top 3 bits set, ANDed with 0x3FFFFFFF), or an execution token (code address).

**Core Implementation:** [dwc-vm.c](dwc-vm.c), [dwc-vm.h](dwc-vm.h), [system.c](system.c)  
**Bootstrap Code:** [boot.fth](boot.fth), [base.fth](base.fth)  
**Build:** 17KB on Windows (32-bit), ~21KB on Linux

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
- Usage pattern:
  ```forth
  : my-word ( a b -- result ) +L2 a y! b x!  x@ y@ +  -L ;
  ```
  The `+L2` (push 2 values) sets up frame, setters store to `x0`/`y0`, then accessors fetch via `x@`/`y@`.

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

- **[dwc-vm.c](dwc-vm.c)**: Core VM with `inner()` interpreter loop and `outer()` parser
- **[dwc-vm.h](dwc-vm.h)**: Platform abstractions, type definitions, VM/system interface
- **[system.c](system.c)**: Platform-specific I/O (Windows/Linux keyboard, file handling)
- **[boot.fth](boot.fth)**: Minimal bootstrap (control flow, variables, basic words)
- **[base.fth](base.fth)**: Extended library (locals via x/y/z, strings, utilities)
- **[block-001.fth](block-001.fth)**: Number formatting, screen control (ANSI), benchmarks

## When Modifying C Code

1. **Primitives**: Add to `PRIMS()` macro in [dwc-vm.c](dwc-vm.c#L5). The macro expands to enum, switch case, and name array.
2. **Memory size**: Change `MEM_SZ` in [dwc-vm.h](dwc-vm.h#L18) (default 16MB).
3. **Platform I/O**: Edit [system.c](system.c) for `key()`, `qKey()`, `ms()`, file ops.
4. **Constants**: Add to `nv[]` array in `dwcInit()` to expose to Forth ([dwc-vm.c](dwc-vm.c#L171-L178)).

## When Writing Forth Code

- Use `inline` for performance-critical small words (< 5 instructions)
- Use `t0-t9` for helper words that don't need to be in dictionary
- Prefer `for`/`next` over `begin`/`until` for counted loops
- Use `val`/`(val)` pattern for variables instead of traditional Forth `variable`
- Check [boot.fth](boot.fth) first - many common words already defined there
