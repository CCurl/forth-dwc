(( see <x> ))
: .prim? ( xt--f ) dup 45 < if ." primitive " .hex/dec 1 exit then drop 0 ;
: t0 ( n-- ) ." lit " $3fffffff and .hex/dec ;
: .lit? ( b--f ) b $3fffffff > if b t0 1 exit then 0 ;
: find-xt ( xt--de 1 | 0 ) a >r last a!
    begin
        a dict-end < if0 r> a! drop 0 exit then
        @a over = if drop a 1 r> a! exit then
        a dup cell+ c@ + a!
    again
: next-xt ( de--xt ) >r last t!
    begin
        t@ dict-end < if0 rdrop here exit then
        t@ cell+ c@ t@ + b!
        b r@ = if rdrop @t exit then
        b t!
    again ;
: .lit-jmp? ( b-- ) b (lit) (jmpnz) btwi if space a+ code@ .hex/dec then ;
: t2 ( a@-- ) cr a .hex4 ." : " a+ code@ dup .hex4 b!
    space .lit? if exit then
    b find-xt if 4 spaces .word then .lit-jmp? ;
: see-range ( f t-- ) t! a! begin a t@ >= if exit then t2 again ;
: see ' ?dup if0 ." -not found-" exit then
    a!  @a  .prim? if exit then
    a .hex ':' emit space a .word
    a next-xt t!  @a  a! a t@ see-range ;
