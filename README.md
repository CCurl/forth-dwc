# forth-dwc

A extremely minimal single-file Forth system that can run stand-alone or be embedded into another program.

One single file, 275 lines, 32 primitives.

On Windows, a 32-bit Release build compiles to a 16k executable.

It's larger on a Linux box, about 24k.

This system has 4 states: COMPILE=1, DEFINE=2, INTERPRET=3, and COMMENT=4.

| State     | Action |
|:--        |:-- |
| COMPILE   | Compile the current word using ','. |
| DEFINE    | Add the current word to the dictionary, change to COMPILE. |
| INTERPRET | Execute the current word is executed. |
| COMMENT   | Only ')' or '))' changes the state to either INTERPRET or COMPILE. |

It supports control chars in the whitespace (CTRL-A to CTRL-D) to change state.<br/>
This is inspired by ColorForth and gives the operator more flexibility.

| Ascii | State |
|:--    |:-- |
| 1     | COMPILE   |
| 2     | DEFINE    |
| 3     | INTERPRET |
| 4     | COMMENT   |

This system hard-codes the following state-change words:
| Word | Action |
|:--   |:-- |
| :    | Change to DEFINE. |
| ;    | Add EXIT and change to INTERPRET. |
| [    | Change to INTERPRET. |
| ]    | Change to COMPILE. |
| (    | Change to COMMENT. |
| )    | Change to INTERPRET. |
| ((   | Change to COMMENT. |
| ))   | Change to COMPILE. |
