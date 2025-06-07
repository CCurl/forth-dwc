# forth-dwc: a minimal DWORD-Code based Forth 

DWC is an extremely minimal Forth system that can run stand-alone or be embedded into another program.

DWC is implemented in 3 files, 30 base primitives, 11 system primitives.<br/>
The VM itself is under 250 lines of code.

On Windows, a 32-bit Release build compiles to a 17k executable. <br/>
On a Linux box, it is about 24k.

**DWC** stands for "dword-code". This is inspired by Tachyon. <br/>
In a DWC program, each instruction is a DWORD. <br/>
- If <= the last primitive (31), then it is a primitive.
- Else, if <= LIT_BITS ($3FFFFFFF), then it is a literal.
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

### DWC also hard-codes the following state-change words:

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

## Temporary words

Words 't0' through 't9' are temporary and are not added to the dictionary.<br/>
They are case sensitive: 't0' is a temporary word, 'T0' is not.<br/>
This helps with factoring code and helps keep the dictionary uncluttered.

## The VM Primitives

| Primitive | Word     | Action |
|:--        |:--       |:-- |
|           |          | --- **DWC primitives** --- |
|  0        | exit     | PC = RTOS. Discard RTOS. If (PC=0) then stop. |
|  1        | (lit)    | Push code[PC]. Increment PC. |
|  2        | (jmp)    | PC = code[PC]. |
|  3        | (jmpz)   | If (TOS=0) then PC = code[PC] else PC = PC+1. Discard TOS. |
|  4        | (jmpnz)  | If (TOS!=0) then PC = code[PC] else PC = PC+1. Discard TOS. |
|  5        | dup      | Push TOS. |
|  6        | drop     | Discard TOS. |
|  7        | swap     | Swap TOS and NOS. |
|  8        | over     | Push NOS. |
|  9        | !        | CELL store NOS through TOS. Discard TOS and NOS. |
| 10        | @        | CELL fetch TOS through TOS. |
| 11        | c!       | BYTE store NOS through TOS. Discard TOS and NOS. |
| 12        | c@       | BYTE fetch TOS through TOS. |
| 13        | >r       | Push TOS onto the return stack. Discard TOS. |
| 14        | r@       | Push RTOS. |
| 15        | r>       | Push RTOS. Discard RTOS. |
| 16        | *        | TOS = NOS*TOS. Discard NOS. |
| 17        | +        | TOS = NOS+TOS. Discard NOS. |
| 18        | -        | TOS = NOS-TOS. Discard NOS. |
| 19        | /mod     | TOS = NOS/TOS. NOS = NOS%TOS. |
| 20        | <        | If (NOS<TOS) then TOS = 1 else TOS = 0. Discard NOS. |
| 21        | =        | If (NOS=TOS) then TOS = 1 else TOS = 0. Discard NOS. |
| 22        | >        | If (NOS<TOS) then TOS = 1 else TOS = 0. Discard NOS. |
| 23        | add-word | Add the next word to the dictionary. |
| 24        | '        | Push the address of the next word from the dictionary. |
| 25        | for      | Start a FOR loop. |
| 26        | next     | End the current FOR loop. |
| 27        | and      | TOS = NOS and TOS. Discard NOS. |
| 28        | or       | TOS = NOS or TOS. Discard NOS. |
| 29        | xor      | TOS = NOS xor TOS. Discard NOS. |
|           |          | --- **System primitives** --- |
| 30        | key      | Push the next keypress. Wait until one is available. |
| 31        | ?key     | Push 1 if a keypress is available, else 0. |
| 32        | emit     | Output char TOS. Discard TOS. |
| 33        | ztype    | Output null-terminated string TOS. Discard TOS. |
| 34        | fopen    | Open file NOS using mode TOS (0 if error). |
| 35        | fclose   | Close file TOS. Discard TOS. |
| 36        | fread    | Read NOS chars from file TOS. |
| 37        | fwrite   | Write NOS chars from file TOS. |
| 38        | ms       | Wait/sleep for MS milliseconds |
| 39        | timer    | Push the current system time. |
| 40        | system   | Execute system(TOS). Discard TOS. |

## Embedding DWC in your C project

See system.c. It embeds the DWC VM into a C program.
