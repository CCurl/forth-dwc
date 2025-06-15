(  comments are built in!  )
(  this comment leaves the state in COMPILE  )
(( this comment leaves the state in INTERPRET ))

: last (l) @ ;
: here (h) @ ;
: vhere (vh) @ ;
: cell 4 ;
$40 last cell + 1 + c!
: cells cell * ;
: cell+ cell + ;
: immediate $80 last cell+ 1 + c! ;
: inline    $40 last cell+ 1 + c! ;

: bye 999 state ! ;
: (exit)    0 ;  inline
: (lit)     1 ;  inline
: (jmp)     2 ;  inline
: (jmpz)    3 ;  inline
: (jmpnz)   4 ;  inline
: (=)      21 ;  inline
: (ztype)  35 ;  inline

: ->code cells mem + ;
: code@ ( h--dwc )  ->code @ ;
: code! ( dwc h-- ) ->code ! ;
: , here dup 1+ (h) ! code! ;

: comp? ( --n ) state @ 1 = ;
: if  (jmpz)  , here 0 ,  ; immediate
: if0 (jmpnz) , here 0 ,  ; immediate
: then here swap code! ; immediate
: begin here ; immediate
: again (jmp)   , , ; immediate
: while (jmpnz) , , ; immediate
: until (jmpz)  , , ; immediate

: decimal #10 base ! ;
: hex     $10 base ! ;
: binary  %10 base ! ;

: aligned ( a1--a2 ) #4 over #3 and - #3 and + ;
: align ( -- ) vhere aligned (vh) ! ;
: allot ( n-- ) (vh) +! ;
: vc, ( c-- ) vhere c! 1 allot ;
: v,  ( n-- ) vhere ! cell allot ;

: const add-word (lit) , , (exit) , ;
: var align vhere const ;
mem mem-sz + const dict-end
64 1024 * cells mem + const vars
vars (vh) !

((  val and (val) define a very efficient variable mechanism  ))
((  Usage:  val xx   (val) (xx)   : xx! (xx) ! ;  ))
: val   add-word (lit) , 0 , (exit) , ;
: (val) add-word (lit) , here 3 - ->code , (exit) , ;

: rdrop r> drop ; inline
: tuck swap over ;
: nip  swap drop ;
: 2dup over over ;
: 2drop drop drop ;
: ?dup dup if dup then ;
: 0= ( n--f ) 0 = ;
: 0< ( n--f ) 0 < ;
: 2* dup + ;
: <= ( a b--f ) > 0= ;
: >= ( a b--f ) < 0= ;
: min ( a b-a|b ) 2dup > if swap then drop ;
: max ( a b-a|b ) 2dup < if swap then drop ;
: btwi ( n l h--f ) >r over <= swap r> <= and ;
: ++ ( a-- )   1 swap +! ;
: -- ( a-- )  -1 swap +! ;
: negate ( n--n' ) 0 swap - ;
: abs    ( n--n' ) dup 0< if negate then ;

(( a temporary circular stack ))
var tstk $20 cells allot inline
val tsp   (val) t0   : tsp! t0 ! ;
: tsp++ ( -- )   tsp 1+ $1f and tsp! ;
: tdrop ( -- )   tsp 1- $1f and tsp! ;
: t-tos ( --a )  tsp cells tstk + ;
: t!    ( n-- )  t-tos ! ;
: t@    ( --n )  t-tos @ ;
: t++   ( -- )   1 t-tos +! ;
: >t    ( n-- )  tsp++ t! ;
: t>    ( --n )  t@ tdrop ;
: t@+   ( --n )  t@ t++ ;
: @tc   ( --n )  t@ @ ;

val a@   (val) t0
: a!   ( n-- ) t0  ! ;
: a++  ( -- )  1   t0 +! ;
: a@+  ( --n ) a@  a++ ;
: a@+c ( --n ) a@  dup cell+ a! ;
: @a   ( --n ) a@  c@ ;
: @a+  ( --n ) a@+ c@ ;
: @ac  ( --n ) a@   @ ;
: @a+c ( --n ) a@+c @ ;
: !a+  ( n-- ) a@+ c! ;
: !a   ( n-- ) a@  c! ;
: a>t  ( -- )  a@  >t ;
: t>a  ( -- )  t>  a! ;

val b@   (val) t0
: b!   ( n-- ) t0  ! ;
: b++  ( -- )  1   t0 +! ;
: b@+  ( --n ) b@  b++ ;
: !b+  ( n-- ) b@+ c! ;
: b>t  ( -- )  b@  >t ;
: t>b  ( -- )  t>  b! ;

: ab>t a>t b>t ;
: t>ba t>b t>a ;

: bl 32 ; inline
: space bl emit ;
: spaces for space next ;
: tab 9 emit ;
: cr 13 emit 10 emit ;

: /   /mod nip  ;
: mod /mod drop ;
: */ ( n m q--n' ) >r * r> / ;

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
: .nwb ( n width base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;
: ? @ . ;

: 0sp 0 (sp) ! ;
: depth (sp) @ 1- ;
: .s '(' emit space depth if
        (stk) cell+ a! depth for @a+c . next
    then ')' emit ;

: (") ( --a ) vhere dup a>t a! >in ++
    begin >in @ c@ >r >in ++
        r@ 0= r@ '"' = or
        if  rdrop 0 !a+
            comp? if (lit) , , a@ (vh) ! then t>a exit
        then
        r> !a+
    again ;

: z" (") ; immediate
: ." (") comp? if (ztype) , exit then ztype ;  immediate

: .word ( de-- ) cell+ 3 + ztype ;
: words last a! 0 b! 0 >t begin
        a@ dict-end < if0 '(' emit t> . ." words)" exit then
        a@ .word tab
        t++ b@+ 9 > if cr 0 b! then
        a@ dup cell+ c@ + a!
    again ;

: [[ vhere >t here >t 1 state ! ;
: ]] (exit) , 0 state ! t> dup >r (h) ! t> (vh) ! ; immediate

(( Files ))
: fopen-r ( nm--fh ) z" rb" fopen ;
: fopen-w ( nm--fh ) z" wb" fopen ;
: ->file ( fh-- ) output-fp ! ;
: ->stdout ( fh-- ) 0 ->file ;

(( Strings / Memory ))
: fill  ( a n c-- ) a>t  >r >r a! r> for r@ !a+  next rdrop t>a ;
: cmove ( f t n-- ) ab>t >r b! a! r> for @a+ !b+ next t>ba ;
: s-len ( str--n ) a>t 0 a! begin dup c@ if0 drop a@ t>a exit then 1+ a++ again ;
: s-end ( str--end ) dup s-len + ;
: s-cpy ( dst src--dst ) 2dup s-len 1+ cmove ;
: s-cat ( dst src--dst ) over s-end over s-len 1+ cmove ;
: p1 vhere 100 + ; : p2 p1 100 + ;

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
: .hex   ( n-- )  #2 $10 .nwb ;
: .hex4  ( n-- )  #4 $10 .nwb ;
: .hex8  ( n-- )  #8 $10 .nwb ;
: .bin   ( n-- )  #8 %10 .nwb ;
: .bin16 ( n-- ) #16 %10 .nwb ;
: .dec   ( n-- )  #1 #10 .nwb ;
: .hex/dec ( n-- ) dup ." ($" .hex ." /#" .dec ')' emit ;

: aemit ( ch-- )     dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )   a>t a! $10 for @a+ aemit next t>a ;
: dump  ( addr n-- ) swap a! 0 t! for
     t@+ if0 a@ cr .hex ." : " then @a+ .hex space
     t@ $10 = if 0 t! space space a@ $10 - t0 then
   next ;

var t0 3 cells allot
: marker here t0 !   last t0 cell+ !   vhere t0 2 cells + ! ;
: forget t0 @ (h) !  t0 cell+ @ (l) !  t0 2 cells + @ (vh) ! ;

(( see <x> ))
: t0 ( n-- ) ." primitive " .hex/dec ;
: .prim? ( xt--f ) dup 44 < if t0 1 exit then drop 0 ;
: t0 ( n-- ) ." lit " $3fffffff and .hex/dec ;
: .lit? ( b@--f ) b@ $3fffffff > if b@ t0 1 exit then 0 ;
: find-xt ( xt--de 1 | 0 ) a@ >r last a!
    begin
        a@ dict-end < if0 r> a! drop 0 exit then
        @ac over = if drop a@ 1 r> a! exit then
        a@ dup cell+ c@ + a!
    again
: next-xt ( de--xt ) >r last t!
    begin
        t@ dict-end < if0 rdrop here exit then
        t@ cell+ c@ t@ + b!
        b@ r@ = if rdrop @tc exit then
        b@ t!
    again ;
: .lit-jmp? ( b@-- ) 0 b@ < b@ 5 < and if space a@+ code@ .hex/dec then ;
: t2 ( a@-- ) cr a@ .hex4 ." : " a@+ code@ dup .hex4 b!
    space .lit? if exit then
    b@ find-xt if 4 spaces .word then .lit-jmp? ;
: see-range ( f t-- ) t! a! begin a@ t@ >= if exit then t2 again ;
: see ' ?dup if0 ." -not found-" exit then
    a! @ac .prim? if exit then
    a@ .hex ':' emit space a@ .word
    a@ next-xt t! @ac a! a@ t@ see-range ;

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

(( Blocks  ))
mem 2 mil + const blocks
: block-sz 2048 ;
: #blocks 512 ;
: disk-sz #blocks block-sz * ;
: disk-read z" blocks.fth" fopen-r dup a!
    if blocks disk-sz a@ fread drop a@ fclose then ;
: flush z" blocks.fth" fopen-w dup a!
    if blocks disk-sz a@ fwrite drop a@ fclose then ;
: block-addr ( n--a ) block-sz * blocks + ;
disk-read
: load ( n-- ) block-addr outer ;

(( Editor  ))
val off (val) t0 : off! t0 ! ;
val row (val) t0 : row! t0 ! ;
val col (val) t0 : col! t0 ! ;
var ed-buf block-sz allot
: rows 23 ;   : cols 89 ; (( NB: 23*89 = 2047 ))
: off->rc off rows /mod row! col! ;
: rc->off row rows * col + off! ;
: blk>buf ( n-- ) block-addr ed-buf block-sz cmove ;
: buf>blk ( n-- ) ed-buf swap block-addr block-sz cmove ;
: show 1 1 ->cr ed-buf a! rows for cols for @a+ 32 max emit next cr next ;

: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    ." dwc " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes, used: " vhere vars - . cr
    yellow ."     Code: " white 64 1024 * (.) ." , used: " here . cr
    yellow ."     Vars: " white vhere vars - .  ." bytes used" cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr ;
.banner   marker
