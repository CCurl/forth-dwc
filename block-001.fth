( Formatting number output )
: .nwb ( n width base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;
: decimal  ( -- )  #10 base ! ;
: hex      ( -- )  $10 base ! ;
: binary   ( -- )  %10 base ! ;
: .hex     ( n-- )  #2 $10 .nwb ;
: .hex4    ( n-- )  #4 $10 .nwb ;
: .hex8    ( n-- )  #8 $10 .nwb ;
: .bin     ( n-- )  #8 %10 .nwb ;
: .bin16   ( n-- ) #16 %10 .nwb ;
: .dec     ( n-- )  #1 #10 .nwb ;
: .hex/dec ( n-- ) dup ." ($" .hex ." /#" .dec ')' emit ;

: aemit ( ch-- )    dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )  >a $10 for c@a+ aemit next adrop ;
: dump  ( addr n-- ) swap >a 0 >t for
     t@+ if0 a@ cr .hex ." : " then c@a+ .hex space
     t@ $10 = if 0 t! space space a@ $10 - t0 then
   next tdrop adrop ;
   
   ( Screen / Colors )
: csi          27 emit '[' emit ;
: ->cr ( c r-- ) csi (.) ';' emit (.) 'H' emit ;
: ->rc ( r c-- ) swap ->cr ;
: cls          csi ." 2J" 1 dup ->cr ;
: clr-eol      csi ." 0K" ;
: cur-on       csi ." ?25h" ;
: cur-off      csi ." ?25l" ;
: cur-block    csi ." 2 q" ;
: cur-bar      csi ." 5 q" ;
: cur-rt       csi (.) 'C' emit ;

: bg    ( color-- ) csi ." 48;5;" (.) 'm' emit ;
: fg    ( color-- ) csi ." 38;5;" (.) 'm' emit ;
: color ( bg fg-- ) fg bg ;
: black   0 fg ;      : red    203 fg ;
: green  40 fg ;      : yellow 226 fg ;
: blue   63 fg ;      : purple 201 fg ;
: cyan  117 fg ;      : grey   246 fg ;
: white 255 fg ;

( Some simple benchmarks )
: t0 ztype '(' emit dup (.) ')' emit timer swap ;
: fib ( n--fib ) 1- dup 2 < if drop 1 exit then dup fib swap 1- fib + ;
: elapsed timer swap - ." , time: " . cr ;
: bm-while ( n-- ) z" while " t0 begin 1- -while drop elapsed ;
: bm-loop  ( n-- ) z" loop "  t0 for next elapsed ;
: bm-fib   ( n-- ) z" fib"    t0 fib space (.) elapsed ;
: bm-fibs  ( n-- ) 1 >b for b@ b+ bm-fib next bdrop ;
: mil ( n--m ) #1000 dup * * ;
: bb 1000 mil bm-loop ;
: bm-all 250 mil bm-while bb 30 bm-fib ;

( see <x> )
: .prim? ( xt--f ) dup 45 < if ." primitive " .hex/dec 1 exit then drop 0 ;
: t0 ( n-- ) ." lit " $3fffffff and .hex/dec ;
: .lit? ( b--f ) b@ $3fffffff > if b@ t0 1 exit then 0 ;
: find-xt ( xt--de 1 | 0 ) a@ >r last a!
    begin
        a@ dict-end < if0 r> a! drop 0 exit then
        @a over = if drop a@ 1 r> a! exit then
        a@ dup cell+ c@ + a!
    again
: next-xt ( de--xt ) >r last t!
    begin
        t@ dict-end < if0 rdrop here exit then
        t@ cell+ c@ t@ + b!
        b@ r@ = if rdrop @t exit then
        b@ t!
    again ;
: .lit-jmp? ( b-- ) b@ (lit) (njmpnz) btwi if space a@+ code@ .hex/dec then ;
: t2 ( a-- ) cr a@ .hex4 ." : " a@+ code@ dup .hex4 b!
    space .lit? if exit then
    b@ find-xt if 4 spaces .word then .lit-jmp? ;
: see-range ( f t-- ) t! a! begin a@ t@ >= if exit then t2 again ;
: see find ?dup if0 ." -not found-" exit then
    a!  @a  .prim? if exit then
    a@ .hex ':' emit space a@ .word
    a@ next-xt t!  @a  a! a@ t@ see-range ;

( a stack inspired by Peter Jakacki )
( this provides efficient access to the top 3 entries, x,y,z )
( it is easy enough to extend this if desired )
( but pushing and popping entries is fairly expensive )

$10 cells var t1
t1 cell+ const t2
t2 cell+ const t3

: x    ( --n ) t1 @ ; : x!  ( n-- ) t1 ! ;
: y    ( --n ) t2 @ ; : y!  ( n-- ) t2 ! ;
: z    ( --n ) t3 @ ; : z!  ( n-- ) t3 ! ;
: @x+  ( n-- ) x @ cell t1 +! ;
: >p   ( n-- ) >r t1 t1 r@ cells + $10 r> - move> ;
: <p   ( n-- ) >r t1 r@ cells + t1 $10 r> - move ;
: >x   ( n-- )     1 >p x! ;
: >xy  ( x y-- )   2 >p y! x! ;
: >xyz ( x y z-- ) 3 >p z! y! x! ;
: <x   ( n-- ) 1 <p ;
: <xy  ( n-- ) 2 <p ;
: <xyz ( n-- ) 3 <p ;
: .pstk t1 >a $10 for @a+ . next adrop ;

( shell words )
: lg z" lazygit" system ;
: ll z" ls -l" system ;
: vi z" vi boot.fth" system ;

( simple fixed point )
: f. 100 /mod (.) '.' emit abs 2 10 .nwb ;
: f* * 100 / ;
: f/ swap 100 * swap / ;
: f+ + ;
: f- - ;

( Startup message )
: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    yellow ." DWC " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes." cr
    yellow ."     Code: " white vars mem - cell / . ." cells, used: " here . cr
    yellow ."     Vars: " white disk vars - . ." bytes, used: " vhere vars - . cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr ;
.banner

." hello."
