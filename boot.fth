(  comments are built in!  )
(  this comment leaves the state in COMPILE  )
(( this comment leaves the state in INTERPRET ))

: last (l) @ ;
: here (h) @ ;
: vhere (vh) @ ;
: cell  4 ;
$40 last cell + 1 + c! ( make inline )
: cells cell * ;
: cell+ cell + ;
: immediate $80 last cell+ 1 + c! ;
: inline    $40 last cell+ 1 + c! ;
: 1+ 1 + ; inline
: 1- 1 - ; inline

: bye 999 state ! ;
: (exit)    0 ;  inline
: (lit)     1 ;  inline
: (jmp)     2 ;  inline
: (jmpz)    3 ;  inline
: (jmpnz)   4 ;  inline
: (njmpz)   5 ;  inline
: (njmpnz)  6 ;  inline
: (ztype)  35 ;  inline

: ->code cells mem + ;
: code@ ( h--dwc )  ->code @ ;
: code! ( dwc h-- ) ->code ! ;
: , here dup 1+ (h) ! code! ;

: comp? ( --n ) state @ 1 = ;
: if   (jmpz)   , here 0 , ; immediate
: -if  (njmpz)  , here 0 , ; immediate
: if0  (jmpnz)  , here 0 , ; immediate
: -if0 (njmpnz) , here 0 , ; immediate
: then here swap code!   ; immediate

: begin here ; immediate
: again (jmp)     , , ; immediate
: while (jmpnz)   , , ; immediate
: -while (njmpnz) , , ; immediate
: until (jmpz)    , , ; immediate

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

: rdrop ( -- ) r> drop ; inline
: tuck  swap over ;
: nip   swap drop ;
: 2dup  over over ;
: 2drop drop drop ;
: ?dup  -if dup then ;
: 0= ( n--f ) 0 = ;
: 0< ( n--f ) 0 < ;
: 2+ ( n--m ) 2 + ;
: 2* ( n--m ) dup + ;
: <= ( a b--f ) > 0= ;
: >= ( a b--f ) < 0= ;
: min ( a b-a|b ) 2dup > if swap then drop ;
: max ( a b-a|b ) 2dup < if swap then drop ;
: btwi ( n l h--f ) >r over <= swap r> <= and ;
: ++ ( a-- )   1 swap +! ;
: -- ( a-- )  -1 swap +! ;
: negate ( n--n' ) 0 swap - ;
: abs    ( n--n' ) dup 0< if negate then ;
: cr 13 emit 10 emit ;
: tab 9 emit ;
: space 32 emit ;
: spaces for space next ;
: /   /mod nip  ;
: mod /mod drop ;
: */ ( n m q--n' ) >r * r> / ;
: execute ( xt-- ) ?dup if >r then ;

var (neg)    1 allot
var buf     65 allot
var (buf) cell allot
: ?neg ( n--n' ) dup 0< dup (neg) c! if negate then ;
: #c   ( c-- )   (buf) -- (buf) @ c! ;
: #.   ( -- )    '.' #c ;
: #n   ( n-- )   '0' + dup '9' > if 7 + then #c ;
: #    ( n--m )  base @ /mod swap #n ;
: #s   ( n--0 )  # -if #s exit then ;
: <#   ( n--m )  ?neg buf 65 + (buf) ! 0 #c ;
: #>   ( n--a )  drop (neg) @ if '-' #c then (buf) @ ;
: (.)   <# #s #> ztype ;
: . (.) space ;

: 0sp 0 (sp) ! ;
: depth (sp) @ 1- ;
: .s '(' emit space depth if
        (stk) cell+ >r depth for r@ @ . r> cell+ >r next
    then ')' emit rdrop ;

(( a circular stack ))
(( t8: stack start,   t9: stack end ))
(( t4: stack pointer, t5: stack pointer address ))
var t8 $10 cells allot inline
vhere cell - const t9
val t4  (val)  t5
: t2    ( -- )  cell t5 +! t4 t9 > if t8 t5 ! then ;
: tdrop ( -- )  -4   t5 +! t4 t8 < if t9 t5 ! then ;
: t!    ( n-- ) t4 ! ;
: >t    ( n-- ) t2 t! ;
: t@    ( --n ) t4 @ ;
: t>    ( --n ) t@ tdrop ;
: t++   ( -- )  1 t4 +! ;
: t+    ( --n ) t@ t++ ;
: @t    ( --n ) t@ @ ;
: .tstk '(' emit space $10 for t2 t@ . next ')' emit ;
t8 t5 !

(( ColorForth variables ))

val a    (val)  t0
: a!    ( n-- ) t0 ! ;
: a++   ( -- )  1 t0 +! ;
: a+    ( --n ) a a++ ;
: @a    ( --n ) a  @ ;
: @a+   ( --n ) a  @ cell t0 +! ;
: @a-   ( --n ) a  @ -4   t0 +! ;
: c@a+  ( --c ) a c@ a++ ;
: c@a-  ( --c ) a c@ -1 t0 +! ;
: >a    ( n-- ) a >t a! ;
: <a    ( -- )  t> a! ;

val b   (val)  t0
: b!   ( n-- ) t0 ! ;
: b++  ( -- )  1 t0 +! ;
: b+   ( --n ) b b++ ;
: !b+  ( n-- ) b  ! cell t0 +! ;
: !b-  ( n-- ) b  ! -4   t0 +! ;
: c!b+ ( c-- ) b+ c! ;
: c!b- ( c-- ) b c! -1 t0 +! ;
: >b   ( n-- ) b >t b! ;
: <b   ( -- )  t> b! ;

: (") ( --a ) vhere dup >b >in ++
    begin >in @ c@ >r >in ++
        r@ 0= r@ '"' = or
        if  rdrop 0 c!b+
            comp? if (lit) , , b (vh) ! then
            <b exit
        then
        r> c!b+
    again ;

: z" (") ; immediate
: ." (") comp? if (ztype) , exit then ztype ;  immediate

: .word ( de-- ) cell+ 3 + ztype ;
: words last a! 0 b! 0 >t begin
        a dict-end < if0 '(' emit t> . ." words)" exit then
        a .word tab t++
        a cell+ 2+ c@ 7 > if b++ then 
        b+ 9 > if cr 0 b! then
        a dup cell+ c@ + a!
    again ;

: [[ vhere >t  here >t  1 state ! ;
: ]] (exit) , 0 state ! t> dup >r (h) ! t> (vh) ! ; immediate

(( Strings / Memory ))
: fill   ( a num ch-- ) >r swap >b for r@ c!b+ next <b rdrop ;
: move   ( f t n-- ) >r >b >a r> for  @a+  !b+ next <a <b ;
: cmove  ( f t n-- ) >r >b >a r> for c@a+ c!b+ next <a <b ;
: move>  ( f t n-- ) >r r@ 1- cells + >b r@ 1- cells + >a r> for  @a-  !b- next <a <b ;
: cmove> ( f t n-- ) >r r@ 1-       + >b r@ 1-       + >a r> for c@a- c!b- next <a <b ;
: s-len  ( str--len ) >a 0 begin c@a+ if0 <a exit then 1+ again ;
: s-end  ( str--end ) dup s-len + ;
: s-cpy  ( dst src--dst ) 2dup s-len 1+ cmove ;
: s-cat  ( dst src--dst ) over s-end over s-len 1+ cmove ;
: s-catc ( dst ch--dst )  over s-end >b c!b+ 0 c!b+ <b ;
: s-catn ( dst num--dst ) <# #s #> s-cat ;

(( Files ))
: fopen-r  ( nm--fh ) z" rb" fopen ;
: fopen-w  ( nm--fh ) z" wb" fopen ;
: ->file   ( fh-- )   output-fp ! ;
: ->stdout ( -- )     0 ->file ;

(( Formatting number output ))
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

: aemit ( ch-- )     dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )   >a $10 for c@a+ aemit next <a ;
: dump  ( addr n-- ) swap >a 0 >t for
     t+ if0 a cr .hex ." : " then c@a+ .hex space
     t@ $10 = if 0 t! space space a $10 - t0 then
   next tdrop <a ;

var t0 3 cells allot
: marker here t0 !   last t0 cell+ !   vhere t0 2 cells + ! ;
: forget t0 @ (h) !  t0 cell+ @ (l) !  t0 2 cells + @ (vh) ! ;

(( Disk: 512 blocks - 2048 bytes each ))
var fn 32 allot
vars 1024 1024 * + const disk
: block-sz 2048 ;
: block-fn ( n--a ) fn z" block-" s-cpy swap <# # # #s #> s-cat z" .fth" s-cat ;
: block-addr  ( n--a ) block-sz * disk + ;
: t1 drop ." -nf-" ;
: write-block ( n-- ) dup block-fn fopen-w ?dup if0 t1 exit then
    >r block-addr block-sz r@ fwrite drop r> fclose ;
: read-block  ( n-- ) dup block-fn fopen-r ?dup if0 t1 exit then
    >r block-addr block-sz r@ fread  drop r> fclose ;

: load ( n-- ) dup read-block block-addr outer ;
: load-next ( n-- ) dup read-block block-addr >in ! ;

(( 0 load ))




(( Screen / Colors ))
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
: .lit-jmp? ( b-- ) b (lit) (njmpnz) btwi if space a+ code@ .hex/dec then ;
: t2 ( a@-- ) cr a .hex4 ." : " a+ code@ dup .hex4 b!
    space .lit? if exit then
    b find-xt if 4 spaces .word then .lit-jmp? ;
: see-range ( f t-- ) t! a! begin a t@ >= if exit then t2 again ;
: see ' ?dup if0 ." -not found-" exit then
    a!  @a  .prim? if exit then
    a .hex ':' emit space a .word
    a next-xt t!  @a  a! a t@ see-range ;




(( a somewhat circular stack inspired by Peter Jakacki ))
(( this provides efficient access to the top 2 entries, p1-p2 ))
(( it is easy enough to extend this to p3 if desired ))
(( but pushing and popping entries is fairly expensive ))

var t1 $10 cells allot
  t1 cell+ const t2

: p1   ( --n ) t1 @ ;
: p2   ( --n ) t2 @ ;
: p1!  ( n-- ) t1 ! ;
: p2!  ( n-- ) t2 ! ;
: @p1+ ( n-- ) p1 @ cell t1 +! ;
: >>p  ( n-- ) >r t1 t1 r@ cells + $10 r> - move> ;
: <<p  ( n-- ) >r t1 r@ cells + t1 $10 r> - move ;
: >p   ( n-- )   1 >>p p1! ;
: 2>p  ( x y-- ) 2 >>p p2! p1! ;
: <p   ( n-- ) 1 <<p ;
: .pstk t1 >a $10 for @a+ . next <a ;




(( shell words ))
: lg z" lazygit" system ;
: ll z" ls -l" system ;

(( Startup message ))
: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    ." dwc " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes." cr
    yellow ."     Code: " white vars mem - cell / . ." cells, used: " here . cr
    yellow ."     Vars: " white disk vars - . ." bytes, used: " vhere vars - . cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr ;
.banner   marker

." hello."
