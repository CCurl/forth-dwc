# forth-dwc: a minimal DWORD-Code based Forth

DWC is an extremely minimal Forth system that can run stand-alone or be embedded into another program.

DWC has 32 base primitives, 13 system primitives.<br/>
DWC is implemented in 3 files: (dwc-vm.c, dwc-vm.h, system.c). <br/>
The VM itself is 250 lines of code.

On Windows, a 32-bit Release build compiles to a 17k executable. <br/>
On a Linux box, it is about 24k.

**DWC** stands for "dword-code". This is inspired by Tachyon. <br/>
In a DWC program, each instruction is a DWORD. <br/>
- If <= the last primitive (44), then it is a primitive.
- Else, if >= LIT_BITS ($3FFFFFFF), then it is a literal anded with LIT_BITS.
- Else, it is the XT (code address) of a word in the dictionary.

## ColorForth influences

DWC supports control chars in the whitespace to change the state.<br/>
DWC has 4 states: COMPILE, DEFINE, INTERPRET, and COMMENT. <br/>
This gives the operator more flexibility.

| Ascii | State |
|:--    |:-- |
| 1     | COMPILE   |
| 2     | DEFINE    |
| 3     | INTERPRET |
| 4     | COMMENT   |

### DWC also hard-codes the following IMMEDIATE state-change words:

| Word | Action |
|:--   |:-- |
| :    | Change state to DEFINE. |
| ;    | Compile EXIT and change state to INTERPRET. |
| [    | Change state to INTERPRET. |
| ]    | Change state to COMPILE. |
| (    | Change state to COMMENT. |
| )    | Change state to COMPILE. |
| ((   | Change state to COMMENT. |
| ))   | Change state to INTERPRET. |

## What DWC does in each state

| State     | Behavior |
|:--        |:-- |
| COMPILE   | Compile the current word/number. |
| DEFINE    | Add the current word to the dictionary, change to COMPILE. |
| INTERPRET | Execute the current word. |
| COMMENT   | Ignore the current word if it is not ')' or '))'. |

**NOTE: When in the COMMENT state, only ')' or '))' changes the state.**

## INLINE words

An INLINE word is somewhat similar to a macro in other languages.<br/>
When a word is INLINE, its definition is copied to the target.<br/>
When not INLINE, a call is made to the word instead.

## Transient words

Words 't0' through 't9' are transient and are not added to the dictionary.<br/>
They help with factoring code and and keep the dictionary uncluttered.<br/>
They are case sensitive: 't0' is a transient word, 'T0' is not.

## The VM Primitives

| Primitive | Word     | Stack        | Action |
|:--        |:--       |:--           |:-- |
|           |          |              | --- **DWC primitives** --- |
|   0       | exit     | (--)         | PC = RTOS. Discard RTOS. If (PC=0) then stop. |
|   1       | lit      | (--)         | Push code[PC]. Increment PC. |
|   2       | jmp      | (--)         | PC = code[PC]. |
|   3       | jmpz     | (n--)        | If (TOS==0) then PC = code[PC] else PC = PC+1. Discard TOS. |
|   4       | jmpnz    | (n--)        | If (TOS!=0) then PC = code[PC] else PC = PC+1. Discard TOS. |
|   5       | njmpz    | (n--n)       | If (TOS==0) then PC = code[PC] else PC = PC+1. |
|   6       | njmpnz   | (n--n)       | If (TOS!=0) then PC = code[PC] else PC = PC+1. |
|   7       | dup      | (n--n n)     | Push TOS. |
|   8       | drop     | (n--)        | Discard TOS. |
|   9       | swap     | (a b--b a)   | Swap TOS and NOS. |
|  10       | over     | (a b--a b a) | Push NOS. |
|  11       | !        | (n a--)      | CELL store NOS through TOS. Discard TOS and NOS. |
|  12       | @        | (a--n)       | CELL fetch TOS through TOS. |
|  13       | c!       | (b a--)      | BYTE store NOS through TOS. Discard TOS and NOS. |
|  14       | c@       | (a--b)       | BYTE fetch TOS through TOS. |
|  15       | >r       | (n--)        | Push TOS onto the return stack. Discard TOS. |
|  16       | r@       | (--n)        | Push RTOS. |
|  17       | r>       | (--n)        | Push RTOS. Discard RTOS. |
|  18       | *        | (a b--c)     | TOS = NOS*TOS. Discard NOS. |
|  19       | +        | (a b--c)     | TOS = NOS+TOS. Discard NOS. |
|  20       | -        | (a b--c)     | TOS = NOS-TOS. Discard NOS. |
|  21       | /mod     | (a b--r q)   | TOS = NOS/TOS. NOS = NOS modulo TOS. |
|  22       | <        | (a b--f)     | If (NOS<TOS) then TOS = 1 else TOS = 0. Discard NOS. |
|  23       | =        | (a b--f)     | If (NOS=TOS) then TOS = 1 else TOS = 0. Discard NOS. |
|  24       | >        | (a b--f)     | If (NOS<TOS) then TOS = 1 else TOS = 0. Discard NOS. |
|  25       | +!       | (n a--)      | Add NOS to the cell at TOS. Discard TOS and NOS. |
|  26       | '        | (--a)        | Push the address of the next word from the dictionary. |
|  27       | for      | (n--)        | Start a FOR loop. |
|  28       | next     | (--)         | End the current FOR loop. |
|  29       | and      | (a b--c)     | TOS = NOS and TOS. Discard NOS. |
|  30       | or       | (a b--c)     | TOS = NOS or TOS. Discard NOS. |
|  31       | xor      | (a b--c)     | TOS = NOS xor TOS. Discard NOS. |
|           |          |              | --- **System primitives** --- |
|  32       | key      | (--n)        | Push the next keypress. Wait until one is available. |
|  33       | ?key     | (--n)        | Push 1 if a keypress is available, else 0. |
|  34       | emit     | (n--)        | Output char TOS. Discard TOS. |
|  35       | ztype    | (a--)        | Output null-terminated string TOS. Discard TOS. |
|  36       | fopen    | (nm md--h)   | Open file NOS using mode TOS (h=0 if error). |
|  37       | fclose   | (h--)        | Close file TOS. Discard TOS. |
|  38       | fread    | (a sz h--n)  | Read NOS chars from file TOS to a. |
|  39       | fwrite   | (a sz h--n)  | Write NOS chars from file TOS from a. |
|  40       | ms       | (n--)        | Wait/sleep for TOS milliseconds |
|  41       | timer    | (--n)        | Push the current system time. |
|  42       | add-word | (--)         | Add the next word to the dictionary. |
|  43       | outer    | (a--)        | Run the outer interpreter on TOS. Discard TOS. |
|  44       | system   | (a--)        | Execute system(TOS). Discard TOS. |

## Embedding DWC in your C or C++ project

See system.c. It embeds the DWC VM into a C program.
