(( Some simple benchmarks ))
: t0 ztype '(' emit dup (.) ')' emit timer swap ;
: fib ( n--fib ) 1- dup 2 < if drop 1 exit then dup fib swap 1- fib + ;
: elapsed timer swap - ." , time: " . cr ;
: bm-while z" while " t0 begin 1- dup while drop elapsed ;
: bm-loop  z" loop "  t0 for next elapsed ;
: bm-fib   z" fib"    t0 fib space (.) elapsed ;
: bm-fibs 1 b! for b+ bm-fib next ;
: mil #1000 dup * * ;
: bm-all 250 mil bm-while 1000 mil bm-loop 30 bm-fib ;
: bb 1000 mil bm-loop ;

