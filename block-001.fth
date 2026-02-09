( some tests )

cr ." this is block-001.fth" cr
pad z" hi " s-cpy z" there-" s-cat 123 s-catn '!' s-catc ztype cr
: .xyz ." ( " x@ . y@ . z@ . ')' emit cr ;
1 2 3 z! y! x! .xyz
4 5 6 +L3 tab .xyz +L tab tab .xyz -L tab .xyz -L .xyz -L .xyz

: ll z" ls -l" system ;
: lg z" lazygit" system ;

: .nwb ( n width base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;
: .hex     ( n-- )  #2 $10 .nwb ;

: aemit ( ch-- )  dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )  +L1 $10 for c@x+ aemit next -L ;
: dump  ( addr n-- )  0 +L3 y@ for
     z@+ if0 x@ cr .hex ." : " then c@x+ .hex space
     z@ $10 = if 0 z! space space x@ $10 - t0 then
   next -L ;

( some benchmarks )
: lap ( --n ) timer ;
: .lap ( n-- ) lap swap - space . ." ticks" cr ;

: mil 1000 dup * * ;
: fib ( n--fib ) 1- dup 2 < if drop 1 exit then dup fib swap 1- fib + ;
: t0 ( n a-- ) ztype '(' emit dup (.) ')' emit lap swap ;
: bm-while ( n-- ) z" while " t0 begin 1- -while drop .lap ;
: bm-loop  ( n-- ) z" loop "  t0 for next .lap ;
: bm-fib   ( n-- ) z" fib "   t0 fib space (.) .lap ;
: bm-fibs  ( n-- ) 1 +L1 for x@+ bm-fib next -L ;
: bb ( -- ) 1000 mil bm-loop ;
: bm-all ( -- ) 250 mil bm-while bb 30 bm-fib ;

( simple fixed point )
: f. ( n-- )    100 /mod (.) '.' emit abs 2 10 .nwb ;
: f* ( a b--c ) * 100 / ;
: f/ ( a b--c ) swap 100 * swap / ;
: f+ ( a b--c ) + ; inline
: f- ( a b--c ) - ; inline

\ A stack
16 cells var tstk      \ the stack start
vhere cell - const t9  \ t9 is the stack end
val tsp@   (val) t1    \ the stack pointer
: tsp! ( n-- ) t1 ! ;  \ set the stack pointer
tstk tsp!              \ Initialize
\ for a normal stack, use these definitions
\ : tsp++ ( -- ) tsp@ cell + t9   min tsp! ;
\ : tsp-- ( -- ) tsp@ cell - tstk max tsp! ;
\ for a circular stack, use these definitions
: tsp++ ( -- )  tsp@ cell +  dup t9   > if drop tstk then tsp! ;
: tsp-- ( -- )  tsp@ cell -  dup tstk < if drop t9   then tsp! ;
: t!    ( n-- ) tsp@ ! ;
: t@    ( --n ) tsp@ @ ;
: >t    ( n-- ) tsp++ t! ;
: t>    ( --n ) tsp@ @  tsp-- ;
: t6    ( -- )  dup tsp@ = if ." sp:" then dup @ . cell + ;
: .tstk ( -- )  '(' emit space tstk 16 for t6 next drop ')' emit ;
( some stack tests )
16 [[ tsp-- for i >t next .tstk cr ]]
32 [[ for tsp++ t@ . next cr .tstk cr ]] 
32 [[ for t> . next cr .tstk cr ]]
