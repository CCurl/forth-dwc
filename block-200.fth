(( Some simple benchmarks ))
: t0 ztype '(' emit dup (.) ')' emit timer swap ;
: fib ( n--fib ) 1- dup 2 < if drop 1 exit then dup fib swap 1- fib + ;
: elapsed timer swap - ." , time: " . cr ;
: bm-while ( n-- ) z" while " t0 begin 1- -while drop elapsed ;
: bm-loop  ( n-- ) z" loop "  t0 for next elapsed ;
: bm-fib   ( n-- ) z" fib"    t0 fib space (.) elapsed ;
: bm-fibs  ( n-- ) 1 >b for b+ bm-fib next <b ;
: mil ( n--m ) #1000 dup * * ;
: bb 1000 mil bm-loop ;
: bm-all 250 mil bm-while bb 30 bm-fib ;
