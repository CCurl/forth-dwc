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
: var ( n-- ) align vhere const allot ;
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

   1 var (neg)
  65 var buf
cell var (buf)
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
: .s '(' emit space depth ?dup if
        (stk) swap for cell+ dup @ . next drop
    then ')' emit ;

(( a circular stack ))
(( t8: stack start,   t9: stack end ))
(( t4: stack pointer, t5: stack pointer address ))
$10 cells var t8
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
: b++  ( -- )   1 t0 +! ;
: b--  ( --n ) -1 t0 +! ;
: b+   ( --n ) b b++ ;
: !b+  ( n-- ) b  ! cell t0 +! ;
: !b-  ( n-- ) b  ! -4   t0 +! ;
: c!b+ ( c-- ) b c! b++ ;
: c!b- ( c-- ) b c! b-- ;
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
: words last a! 0 b! 1 >t begin
        a dict-end < if0 '(' emit t> . ." words)" exit then
        a .word tab t++
        a cell+ 2+ c@ 7 > if b++ then 
        b+ 9 > if cr 0 b! then
        a dup cell+ c@ + a!
    again ;

cell var t4
cell var t5
: [[ here t4 !  vhere t5 !  1 state ! ;
: ]] (exit) , 0 state ! t4 @ dup >r (h) ! t5 @ (vh) ! ; immediate

(( Strings / Memory ))
: fill   ( a num ch-- ) >r swap >b for r@ c!b+ next <b rdrop ;
: move   ( f t n-- ) >r >b >a r> ?dup if for  @a+  !b+ next then <a <b ;
: cmove  ( f t n-- ) >r >b >a r> ?dup if for c@a+ c!b+ next then <a <b ;
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

: aemit ( ch-- )    dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )  >a $10 for c@a+ aemit next <a ;
: dump  ( addr n-- ) swap >a 0 >t for
     t+ if0 a cr .hex ." : " then c@a+ .hex space
     t@ $10 = if 0 t! space space a $10 - t0 then
   next tdrop <a ;

cell var t0
cell var t1
cell var t2
: marker  here t0 !   last t1 !   vhere t2 ! ;
: forget  t0 @ (h) !  t1 @ (l) !  t2 @ (vh) ! ;

(( Disk: 512 blocks - 2048 bytes each ))
32 var fn
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
: cur-rt       csi (.) 'C' emit ;

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
: bm-fibs  ( n-- ) 1 >b for b b++ bm-fib next <b ;
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
(( it is easy enough to extend this to p3/p4 if desired ))
(( but pushing and popping entries is fairly expensive ))

$10 cells var t1
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





(( Block #8 - vkey ))
[ #256  #59 + const key-f1   [ #256  #60 + const key-f2
[ #256  #61 + const key-f3   [ #256  #62 + const key-f4
[ #256  #71 + const key-home   (( VT: 27 91 72 ))
[ #256  #72 + const key-up     (( VT: 27 91 65 ))
[ #256  #73 + const key-pgup   (( VT: 27 91 53 126 ))
[ #256  #75 + const key-left   (( VT: 27 91 68 ))
[ #256  #77 + const key-right  (( VT: 27 91 67 ))
[ #256  #79 + const key-end    (( VT: 27 91 70 ))
[ #256  #80 + const key-down   (( VT: 27 91 66 ))
[ #256  #81 + const key-pgdn   (( VT: 27 91 54 126 ))
[ #256  #82 + const key-ins    (( VT: 27 91 50 126 ))
[ #256  #83 + const key-del    (( VT: 27 91 51 126 ))
[ #256 #119 + const key-chome  (( VT: 27 91 ?? ??? ))
[ #256 #117 + const key-cend   (( VT: 27 91 ?? ??? ))
: vk2 ( --k ) key 126 = if0 27 exit then
    a 50 = if key-ins   exit then
    a 51 = if key-del   exit then
    a 53 = if key-pgup  exit then
    a 54 = if key-pgdn  exit then    27 ;
: vk1 ( --k ) key a!
    a 68 = if key-left  exit then
    a 67 = if key-right exit then
    a 65 = if key-up    exit then
    a 66 = if key-down  exit then
    a 72 = if key-home  exit then
    a 70 = if key-end   exit then
    a 49 > a 55 < and if vk2 exit then   27 ;
: vt-key ( --k )  key 91 = if vk1 exit then 27 ;
: vkey ( --k ) key dup if0 drop #256 key + exit then ( Windows FK )
    dup 224 = if drop #256 key + exit then ( Windows )
    dup  27 = if drop vt-key exit then ; ( VT )


: printable? ( c--f ) dup 31 > swap 127 < and ;
: bs 8 emit ; [ inline
: accept ( dst-- ) dup >r >b 0 >a
  begin key a!
     a   3 =  a 27 = or if 0 r> c! <a <b exit then
     a  13 = if 0 c!b+ <a <b rdrop exit then
     a   8 = if 127 a! then ( Windows: 8=backspace )
     a 127 = if r@ b < if b-- bs space bs then then
     a printable? if a dup c!b+ emit then
  again ;


( fixed point )
: f. 100 /mod (.) '.' emit abs 2 10 .nwb ;
: f* * 100 / ;
: f/ swap 100 * swap / ;
: f+ + ;
: f- - ;


(( Editor  ))
val blk   (val) t0  : blk! t0 ! ;
val off   (val) t0  : off! t0 ! ;
val row   (val) t0  : row! t0 ! ;    : +row ( n-- ) t0 +! ;
val col   (val) t0  : col! t0 ! ;    : +col ( n-- ) t0 +! ;
val show? (val) t0  : show! 1 t0 ! ; : shown 0 t0 ! ;
val mode  (val) t0  : mode! t0 ! ;
block-sz var ed-buf
: rows 23 ;   : cols 89 ; (( NB: 23*89 = 2047 ))
: off->rc ( off--c r ) cols /mod ;
: rc->off ( r c--off ) swap cols * + ;
: ed-norm ( -- ) off 0 max block-sz min off! off off->rc row! col! ;
: ed-mv   ( r c-- ) +col +row row col rc->off off! ed-norm ;
: ed-ch   ( r c--a ) rc->off ed-buf + ;
: blk>buf ( n-- ) block-addr ed-buf block-sz 1+ cell / move ;
: buf>blk ( n-- ) ed-buf swap block-addr block-sz 1+ cell / move ;
: ed-hl   ( -- ) cols 2+ for '-' emit next ;
: ed-vl   ( -- ) 2 >a rows for 1 a ->cr '|' emit  cols 2+ a+ ->cr '|' emit next <a ;
: ed-box  ( -- ) 1 1 ->cr green ed-hl ed-vl cr ed-hl ;
: ed-draw ( -- ) ed-buf >a rows for cols for c@a+ 32 max emit next cr cur-rt next <a ;
: ed-foot ( -- )  ;
: ed->cur ( -- ) col 2+ row 2+ ->cr ;
: ed-show ( -- ) show? if 2 2 ->cr white ed-draw then shown ed-foot ;
: ed-redraw ( -- ) cls show! ed-box ;
: ed-key ( key-- ) dup . 3 = if 999 mode! then ;
: ed-init ( blk-- ) blk! blk read-block blk blk>buf ed-redraw 0 off! ed-norm ;
: edit ( blk-- ) ed-init begin ed-show ed->cur vkey ed-key mode 999 = until ;
: ed ( -- ) blk edit ;
1 blk!

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
