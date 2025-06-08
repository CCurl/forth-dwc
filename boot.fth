(  comments are built in!  )
(  this comment leaves the state in COMPILE  )
(( this comment leaves the state in INTERPRET ))

: cell 4 ;
: last (l) @ ;
: here (h) @ ;
: vhere (vh) @ ;
: immediate $80 last cell + 1 + c! ;
: cells cell * ;
: cell+ cell + ;

: bye 999 state ! ;
: (exit)    0 ; 
: (lit)     1 ;
: (jmp)     2 ;
: (jmpz)    3 ;
: (jmpnz)   4 ;
: (=)      21 ;
: (ztype)  33 ;

: ->code cells code + ;
: , here dup 1 + (h) ! ->code ! ;

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
: var align vhere const ;
vars vars-sz + const dict-end

((  val and (val) define a very efficient variable mechanism  ))
((  Usage:  val xx   (val) (xx)   : xx! (xx) ! ;  ))
: val   add-word (lit) , 0 , (exit) , ;
: (val) add-word (lit) , here 3 - ->code , (exit) , ;

: tuck swap over ;
: nip  swap drop ;
: ?dup dup if dup then ;
: 0= ( n--f ) 0 = ;
: 0< ( n--f ) 0 < ;
: <= ( a b--f ) > 0= ;
: >= ( a b--f ) < 0= ;
: min ( a b-a|b ) over over > if swap then drop ;
: max ( a b-a|b ) over over < if swap then drop ;
: btwi ( n l h--f ) >r over <= swap r> <= and ;
: +! ( n a-- )  dup >r @ + r> ! ;
: ++ ( a-- )     1 swap +! ;
: -- ( a-- )    -1 swap +! ;
val a@ (val) (a)
: a!   (a) ! ;
: a++  a@ 1+ a! ;
: a@+  a@ a++ ;
: a@+c a@ dup cell+ a! ;
: @a   a@ c@ ;
: @a+  a@+ c@ ;
: @ac  a@  @ ;
: @a+c a@+c @ ;
: !a+  a@+ c! ;
: !a   a@ c! ;

val b@ (val) (b)
: b! (b) ! ;
: b@+ b@ dup 1+ b! ;

val t@ (val) (t)
: t! (t) ! ;
: t++ t@ 1+ t! ;
: t@+ t@ t++ ;
: @tc t@ @ ;

: bl 32 ;
: space bl emit ;
: spaces for space next ;
: tab 9 emit ;
: cr 13 emit 10 emit ;

: negate 0 swap - ;
: abs dup 0< if negate then ;

: /   /mod nip  ;
: mod /mod drop ;

var (neg)    1 allot
var buf     65 allot
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
: execute ( xt-- ) ?dup if >r then ;
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
            comp? if (lit) , , a@ (vh) ! then exit
        then
        r> !a+
    again ;

: z" (") ; immediate
: ." (") comp? if (ztype) , exit then ztype ;  immediate

: words last a! 0 b! 0 t! begin
        a@ dict-end < if0 '(' emit t@ . ." words)" exit then
        a@ cell + 3 + ztype tab
        t++ b@+ 9 > if cr 0 b! then
        a@ dup cell+ c@ + a!
    again ;

(( Files ))
: fopen-r ( nm--fh ) z" rb" fopen ;
: fopen-w ( nm--fh ) z" wb" fopen ;
: ->file ( fh-- ) output-fp ! ;
: ->stdout ( fh-- ) 0 ->file ;

(( Colors ))
: csi          27 emit '[' emit ;
: ->cr ( c r-- ) csi (.) ';' emit (.) 'H' emit ;
: ->rc ( r c-- ) swap ->cr ;
: cls          csi ." 2J" 1 dup ->cr ;
: clr-eol      csi ." 0K" ;
: cur-on       csi ." ?25h" ;
: cur-off      csi ." ?25l" ;
: cur-block    csi ." 2 q" ;
: cur-bar      csi ." 5 q" ;

: bg    ( color-- ) csi ." 48;5;" (.) 'm' emit ;
: fg    ( color-- ) csi ." 38;5;" (.) 'm' emit ;
: color ( bg fg-- ) fg bg ;
: black   0 fg ;      : red    203 fg ;
: green  40 fg ;      : yellow 226 fg ;
: blue   63 fg ;      : purple 201 fg ;
: cyan  117 fg ;      : grey   246 fg ;
: white 255 fg ;

(( Formatting number output ))
: .nwb ( n wid base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;
: .hex   ( n-- )  #2 $10 .nwb ;
: .hex4  ( n-- )  #4 $10 .nwb ;
: .hex8  ( n-- )  #8 $10 .nwb ;
: .bin   ( n-- )  #8 %10 .nwb ;
: .bin16 ( n-- ) #16 %10 .nwb ;
: .dec   ( n-- )  #1 #10 .nwb ;
: .hex/dec ( n-- ) dup ." ($" .hex ." /#" .dec ')' emit ;

: aemit ( ch-- )     dup #32 < over #126 > or if drop '.' then emit ;
: t0    ( addr-- )   a@ >r a! $10 for @a+ aemit next r> a! ;
: dump  ( addr n-- ) swap a! 0 t! for
     t@+ if0 a@ cr .hex ." : " then @a+ .hex space
     t@ $10 = if 0 t! space space a@ $10 - t0 then 
   next ;

var t0 3 cells allot
: marker here t0 !   last t0 cell+ !   vhere t0 2 cells + ! ;
: forget t0 @ (h) !  t0 cell+ @ (l) !  t0 2 cells + @ (vh) ! ;

marker

(( see <x> ))
: code@ ( h--dwc ) ->code @ ;
: .word ( de-- ) cell+ 3 + ztype ;
: .prim? ( xt--f ) dup 41 < if
        ."  is a primitive " .hex/dec 1 exit
    then drop 0 ;
: find-xt ( xt--de 1 | 0 ) a@ >r last a!
    begin
        a@ dict-end < if0 r> a! drop 0 exit then
        @ac over = if drop a@ 1 r> a! exit then
        a@ dup cell+ c@ + a!
    again
: next-xt ( de--xt ) >r last t!
    begin
        t@ dict-end < if0 r> drop here exit then
        t@ cell+ c@ t@ + b!
        b@ r@ = if r> drop @tc exit then
        b@ t!
    again ;
: .lit? ( b@--f ) b@ $3fffffff > if
        b@ $3fffffff and ." lit " .hex/dec 1 exit
    then 0 ;
: .lit-jmp ( b@-- ) 0 b@ < b@ 5 < and if space a@+ code@ .hex/dec then ;
: t2 ( a@-- ) cr a@ .hex4 ." : " a@+ code@ dup .hex4 b!
    space .lit? if exit then
    b@ find-xt if 4 spaces .word then .lit-jmp ;
: see ' ?dup if0 ." -not found-" exit then
    a! a@ .hex ':' emit space a@ .word 
    @ac .prim? if exit then
    a@ next-xt t! @ac a!
    : t1 a@ t@ < if t2 t1 exit then ;

(( Some simple benchmarks ))
: t0 ztype '(' emit dup (.) ')' emit timer swap ;
: fib ( n--fib ) 1- dup 2 < if drop 1 exit then dup fib swap 1- fib + ;
: elapsed timer swap - ." , time: " . cr ;
: bm-while z" while " t0 begin 1- dup while drop elapsed ;
: bm-loop  z" loop "  t0 for next elapsed ;
: bm-fib   z" fib"    t0 fib space (.) elapsed ;
: bm-fibs 1 b! for b@+ bm-fib next ;
: mil #1000 dup * * ;
: bm-all 250 mil bm-while 1000 mil bm-loop 30 bm-fib ;
: bb 1000 mil bm-loop ;

: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    ." dwc " green .version white ."  - Chris Curl" cr
    yellow ."   Heap: " white vars-sz . ." bytes, used: " vhere vars - . cr
    yellow ."   Code: " white code-sz (.) ." , used: " here . cr
    yellow ."   Dict: " white dict-end last - .  ." bytes used" cr
    ;
.banner (( forget ))
