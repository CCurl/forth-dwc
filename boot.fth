(  comments are built in!  )
(  this comment leaves the state in COMPILE  )
(( this comment leaves the state in INTERPRET ))

: cell  4 ;
: wc-sz 4 ;
: last (l) @ ;
: here (h) @ ;
: vhere (vh) @ ;
: immediate $80 last wc-sz + 1 + c! ;

: bye 999 state ! ;
: (exit)    0 ; 
: (lit)     1 ;
: (jmp)     2 ;
: (jmpz)    3 ;
: (jmpnz)   4 ;
: (=)      21 ;
: (ztype)  30 ;

: ->code wc-sz * code + ;
: , here dup 1 + (h) ! ->code ! ;
: dict-end vars vars-sz + ;

: cell+ cell + ;
: cells cell * ;
: 1+ 1 + ;
: 1- 1 - ;
: 2* 2 * ;

: comp? ( --n ) state @ 1 = ;
: if  (jmpz)  , here 0 ,  ; immediate
: if0 (jmpnz) , here 0 ,  ; immediate
: then here swap ->code ! ; immediate
: begin here ; immediate
: again (jmp)   , , ; immediate
: while (jmpnz) , , ; immediate
: until (jmpz)  , , ; immediate

: decimal #10 base ! ;
: hex     $10 base ! ;
: binary  %10 base ! ;

: aligned ( a1--a2 ) #4 over #3 and - #3 and + ;
: align ( -- ) vhere aligned (vh) ! ;
: allot ( n-- ) vhere + (vh) ! ;
: vc, ( c-- ) vhere c! 1 allot ;
: v,  ( n-- ) vhere ! cell allot ;

: const add-word (lit) , , (exit) , ;
: var vhere const ;

((  val and (val) define a very efficient variable mechanism  ))
((  Usage:  val xx   (val) (xx)   : xx! (xx) ! ;  ))
: val   add-word (lit) , 0 , (exit) , ;
: (val) add-word (lit) , here 3 - ->code , (exit) , ;

: tuck swap over ;
: nip  swap drop ;
: ?dup dup if dup then ;
: 0= ( n -- f ) 0 = ;
: 0< ( n -- f ) 0 < ;
: +! ( n a-- )  dup >r @ + r> ! ;
: ++ ( a-- )     1 swap +! ;
: -- ( a-- )    -1 swap +! ;

val a (val) (a)
: a!   (a) ! ;
: a+   a dup 1+ a! ;
: a+c  a dup cell+ a! ;
: @a   a c@ ;
: @a+  a+ c@ ;
: @ac  a @ ;
: @a+c a+c @ ;
: !a+  a+ c! ;
: !a   a  c! ;

val b (val) (b)
: b! (b) ! ;
: b+ b dup 1+ b! ;

val t (val) (t)
: t! (t) ! ;
: t+ (t) @ dup 1+ t! ;
: @t+ t+ @ ;

: bl 32 ;
: space bl emit ;
: tab 9 emit ;
: cr 13 emit 10 emit ;

: negate 0 swap - ;
: abs dup 0< if negate then ;

: /   /mod nip  ;
: mod /mod drop ;

var (neg)    1 allot
var buf     65 allot align
var (buf) cell allot
: ?neg ( n--n' ) dup 0< dup (neg) c! if negate then ;
: #c   ( c-- )   (buf) -- (buf) @ c! ;
: #.   ( -- )    '.' #c ;
: #n   ( n-- )   '0' + dup '9' > if 7 + then #c ;
: #    ( n--m )  base @ /mod swap #n ;
: #s   ( n--0 )  # dup if #s exit then ;
: <#   ( n--m )  ?neg buf 65 + (buf) ! 0 #c ;
: #>   ( n--a )  drop (neg) @ if '-' #c then (buf) @ ;

: ?dup ( n--n n | 0 ) dup if dup then ;
: execute ( xt-- ) ?dup if >R then ;
: (.)   <# #s #> ztype ;
: . (.) space ;
: ? @ . ;

: 0sp 0 (sp) ! ;
: depth (sp) @ 1- ;
: .s '(' emit space depth if
        (stk) cell+ a! depth for @a+c . next 
    then ')' emit ;

: (") ( --a ) vhere dup a! >in ++
    begin >in @ c@ >r >in ++
        r@ 0= r@ '"' = or
        if  r> drop 0 !a+
            comp? if (lit) , , a (vh) ! then exit
        then
        r> !a+
    again ;

: z" (") ; immediate
: ." (") comp? if (ztype) , exit then ztype ;  immediate

: words last a! 0 b! 0 t! begin
        a dict-end < if0 '(' emit t . ." words)" exit then
        a wc-sz + 3 + ztype tab
        (t) ++ b+ 9 > if cr 0 b! then
        a wc-sz + c@ a + a!
    again ;

(( Formatting number output ))
: .nwb ( n wid base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;
: .hex   ( n-- )  #2 $10 .nwb ;
: .hex4  ( n-- )  #4 $10 .nwb ;
: .hex8  ( n-- )  #8 $10 .nwb ;
: .bin   ( n-- )  #8 %10 .nwb ;
: .bin16 ( n-- ) #16 %10 .nwb ;

align var mkr 3 cells allot
: marker here mkr !   last mkr cell+ !   vhere mkr 2 cells + ! ;
: forget mkr @ (h) !  mkr cell+ @ (l) !  mkr 2 cells + @ (vh) ! ;

: aemit ( ch-- )     dup #32 < over #126 > or if drop '.' then emit ;
: t0    ( addr-- )   a >r a! $10 for @a+ aemit next r> a! ;
: dump  ( addr n-- ) swap a! 0 t! for
     t+ if0 a cr .hex ." : " then @a+ .hex space
     t $10 = if 0 t! space space a $10 - t0 then 
   next ;

marker

(( Some simple benchmarks ))
: t. ztype '(' emit dup (.) ')' emit timer swap ;
: fib ( n--fib ) 1- dup 2 < if drop 1 exit then dup fib swap 1- fib + ;
: elapsed timer swap - ." , time: " . cr ;
: bm-while z" while " t. begin 1- dup while drop elapsed ;
: bm-loop  z" loop "  t. for next elapsed ;
: bm-fib   z" fib"    t. fib space (.) elapsed ;
: bm-fibs 1 b! for b+ bm-fib next ;
: mil #1000 dup * * ;
: bm-all 250 mil bm-while 1000 mil bm-loop 30 bm-fib ;
: bb 1000 mil bm-loop ;

: .version version <# # # #. # # #. #s #> ztype ;
: .banner
    ." dwc - version " .version ."  - Chris Curl" cr
    ."   Heap: " vars-sz . ." bytes, used: " vhere vars - . cr
    ."   Code: " code-sz (.) ." , used: " here . cr
    ."   Dict: " dict-end last - .  ." bytes used" cr
    ;
.banner (( forget ))
