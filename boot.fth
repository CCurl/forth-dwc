(  comments are built in!  )
(  this comment leaves the state in COMPILE  )
(( this comment leaves the state in INTERPRET ))

: last (l) @ ;
: here (h) @ ;
: vhere (vh) @ ;
: cell  4 ;
$40 last cell + 1+ c!
: cells cell * ;
: cell+ cell + ;
: immediate $80 last cell+ 1+ c! ;
: inline    $40 last cell+ 1+ c! ;

: bye 999 state ! ;
: (exit)    0 ;  inline
: (lit)     1 ;  inline
: (jmp)     2 ;  inline
: (jmpz)    3 ;  inline
: (jmpnz)   4 ;  inline
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
: ?dup  dup if dup then ;
: 0= ( n--f ) 0 = ;
: 0< ( n--f ) 0 < ;
: 2+ ( n--m ) 1+ 1+ ;
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
: ?dup ( n--n n | 0 ) dup if dup then ;
: execute ( xt-- ) ?dup if >r then ;

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
: (.)   <# #s #> ztype ;
: . (.) space ;
: .nwb ( n width base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;

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

val x    (val)  t0
: x!    ( n-- ) t0 ! ;
: x++   ( -- )  1 t0 +! ;
: @x+   ( --n ) x  @ cell t0 +! ;
: @x-   ( --n ) x  @ -4   t0 +! ;
: c@x+  ( --c ) x c@ x++ ;
: c@x-  ( --c ) x c@ -1 t0 +! ;
: >x    ( n-- ) x >t x! ;
: x>>   ( -- )  t> x! ;

val y   (val)  t0
: y!   ( n-- ) t0 ! ;
: y++  ( -- )  1 t0 +! ;
: y+   ( --n ) y y++ ;
: !y+  ( n-- ) y  ! cell t0 +! ;
: !y-  ( n-- ) y  ! -4   t0 +! ;
: c!y+ ( c-- ) y+ c! ;
: c!y- ( c-- ) y c! -1 t0 +! ;
: >y   ( n-- ) y >t y! ;
: y>>  ( -- )  t> y! ;

(( Strings / Memory ))
: fill   ( a num ch-- ) >r swap >y for r@ c!y+ next y>> rdrop ;
: move   ( f t n-- ) >r >y >x r> for  @x+  !y+ next x>> y>> ;
: cmove  ( f t n-- ) >r >y >x r> for c@x+ c!y+ next x>> y>> ;
: move>  ( f t n-- ) >r r@ 1- cells + >y r@ 1- cells + >x r> for  @x-  !y- next x>> y>> ;
: cmove> ( f t n-- ) >r r@ 1-       + >y r@ 1-       + >x r> for c@x- c!y- next x>> y>> ;
: s-len  ( str--len ) >x 0 begin c@x+ if0 x>> exit then 1+ again ;
: s-end  ( str--end ) dup s-len + ;
: s-cpy  ( dst src--dst ) 2dup s-len 1+ cmove ;
: s-cat  ( dst src--dst ) over s-end over s-len 1+ cmove ;
: s-catc ( dst ch--dst )  over s-end >y c!y+ 0 c!y+ y>> ;
: s-catn ( dst num--dst ) <# #s #> s-cat ;

(( ColorForth variables ))
val a  (val) t0
: a!   ( n-- ) t0 ! ;
: a+   ( --n ) a 1 t0 +! ;
: @a   ( --n ) a  @ ;
: c@a+ ( --n ) a+ c@ ;
: c!a+ ( n-- ) a+ c! ;
: >a   ( -- )  a >t a! ;
: adrop ( -- ) t> a! ;

val b    (val) t0
: b!   ( n-- ) t0  ! ;
: b+   ( --n ) b 1 t0 +! ;

: (") ( --a ) vhere dup >y >in ++
    begin >in @ c@ >r >in ++
        r@ 0= r@ '"' = or
        if  rdrop 0 c!y+
            comp? if (lit) , , y (vh) ! then
            y>> exit
        then
        r> c!y+
    again ;

: z" (") ; immediate
: ." (") comp? if (ztype) , exit then ztype ;  immediate

: .word ( de-- ) cell+ 3 + ztype ;
: words last x! 0 y! 0 >t begin
        x dict-end < if0 '(' emit t> . ." words)" exit then
        x .word tab t++
        x cell+ 2+ c@ 7 > if y++ then 
        y+ 9 > if cr 0 y! then
        x dup cell+ c@ + x!
    again ;

: [[ vhere >t  here >t  1 state ! ;
: ]] (exit) , 0 state ! t> dup >r (h) ! t> (vh) ! ; immediate

(( Files ))
: fopen-r  ( nm--fh ) z" rb" fopen ;
: fopen-w  ( nm--fh ) z" wb" fopen ;
: ->file   ( fh-- )   output-fp ! ;
: ->stdout ( -- )     0 ->file ;

(( Formatting number output ))
: decimal ( -- ) #10 base ! ;
: hex     ( -- ) $10 base ! ;
: binary  ( -- ) %10 base ! ;
: .hex   ( n-- )  #2 $10 .nwb ;
: .hex4  ( n-- )  #4 $10 .nwb ;
: .hex8  ( n-- )  #8 $10 .nwb ;
: .bin   ( n-- )  #8 %10 .nwb ;
: .bin16 ( n-- ) #16 %10 .nwb ;
: .dec   ( n-- )  #1 #10 .nwb ;
: .hex/dec ( n-- ) dup ." ($" .hex ." /#" .dec ')' emit ;

: aemit ( ch-- )     dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )   >x $10 for c@x+ aemit next x>> ;
: dump  ( addr n-- ) swap a! 0 t! for
     t+ if0 a cr .hex ." : " then c@a+ .hex space
     t@ $10 = if 0 t! space space a $10 - t0 then
   next ;

var t0 3 cells allot
: marker here t0 !   last t0 cell+ !   vhere t0 2 cells + ! ;
: forget t0 @ (h) !  t0 cell+ @ (l) !  t0 2 cells + @ (vh) ! ;

(( Disk: 512 blocks - 2048 bytes each ))
var fn 32 allot
vars 1024 1024 * + const disk
: block-sz 2048 ;
: block-fn ( n--a ) fn z" block-" s-cpy swap <# # # #s #> s-cat z" .fth" s-cat ;
: block-addr  ( n--a ) block-sz * disk + ;
: write-block ( n-- ) dup block-fn fopen-w ?dup if0 drop exit then
    >r block-addr block-sz r@ fwrite drop r> fclose ;
: read-block ( n-- )  dup block-fn fopen-r ?dup if0 drop exit then
    >r block-addr block-sz r@ fread  drop r> fclose ;

: load ( n-- ) dup read-block block-addr outer ;
: load-next ( n-- ) dup read-block block-addr >in ! ;

0 load
