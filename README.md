# forth-dwc: a minimal DWORD-Code based Forth 

DWC is an extremely minimal single-file Forth system that can run stand-alone or be embedded into another program.

DWC is implemented in a single file, 275 lines, 32 primitives.

On Windows, a 32-bit Release build compiles to a 16k executable. <br/>
On a Linux box, it is about 24k.

**DWC** stands for "dword-code". This is inspired by Tachyon. <br/>
In a DWC program, each instruction is a DWORD. <br/>
- If <= the last primitive (31), then it is a primitive.
- Else, if <= NUM_BITS ($3FFFFFFF), then it is a literal.
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

## The VM Primitives

| Primitive | Word     | Action |
|:--        |:--       |:-- |
|  0        | exit     | PC = RTOS. Discard RTOS. If (PC=0) then stop. |
|  1        | (lit)    | Push code[PC]. Increment PC. |
|  2        | (jmp)    | PC = code[PC]. |
|  3        | (jmpz)   | If (TOS=0) then PC = code[PC] else PC = PC+1. Discard TOS. |
|  4        | (jmpnz)  | If (TOS!=0) then PC = code[PC] else PC = PC+1. Discard TOS. |
|  5        | 1+       | Increment TOS. |
|  6        | dup      | Push TOS. |
|  7        | drop     | Discard TOS. |
|  8        | swap     | Swap TOS and NOS. |
|  9        | !        | CELL store NOS through TOS. Discard TOS and NOS. |
| 10        | @        | CELL fetch TOS through TOS. |
| 11        | c!       | BYTE store NOS through TOS. Discard TOS and NOS. |
| 12        | c@       | BYTE fetch TOS through TOS. |
| 13        | >r       | Push TOS onto the return stack. Discard TOS. |
| 14        | r@       | Push RTOS. |
| 15        | r>       | Push RTOS. Discard RTOS. |
| 16        | timer    | Push clock(). |
| 17        | *        | TOS = NOS*TOS. Discard NOS. |
| 18        | +        | TOS = NOS+TOS. Discard NOS. |
| 19        | -        | TOS = NOS-TOS. Discard NOS. |
| 20        | /mod     | TOS = NOS/TOS. NOS = NOS%TOS. |
| 21        | <        | If (NOS<TOS) then TOS = 1 else TOS = 0. Discard NOS. |
| 22        | =        | If (NOS=TOS) then TOS = 1 else TOS = 0. Discard NOS. |
| 23        | >        | If (NOS<TOS) then TOS = 1 else TOS = 0. Discard NOS. |
| 24        | emit     | Output char TOS to STDOUT. Discard TOS. |
| 25        | ztype    | Output null-terminated string TOS to STDOUT. Discard TOS. |
| 26        | add-word | Add the next word to the dictionary. |
| 27        | for      | Start a FOR loop. |
| 28        | next     | End the current FOR loop. |
| 29        | and      | TOS = NOS and TOS. Discard NOS. |
| 30        | or       | TOS = NOS or TOS. Discard NOS. |
| 31        | xor      | TOS = NOS xor TOS. Discard NOS. |

## Embedding DWC in your C project

- Include dwc.c in your project.
- Rename the main() to dwc_init().
- Tweak the renamed function to suit your needs.
- Add additional primitives to PRIMS macro to suit your needs.
- Call dwc_init() to initialize the VM.
- Pass Forth code to outer().
- The stack is dstk[]. 
- The stack pointer is dsp. Set it to 0 to clear the stack.
- The top of the stack is dstk[dsp].
