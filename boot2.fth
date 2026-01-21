( these are created later as -last- and -here- )
( they are used later for reboot )
(h) @   (l) @ 

: last (l) @ ;
: here (h) @ ;
: inline    $40 last 4 + 1 + c! ;
: cell    4 ; inline
: 2cells  8 ; inline
: 3cells 12 ; inline
: -cell  -4 ; inline
: cells cell * ; inline
: cell+ cell + ; inline
: cell- cell - ; inline
: ->code ( off--addr ) cells mem + ;
: code@ ( off--op )  ->code @ ;
: code! ( op off-- ) ->code ! ;
: immediate $80 last cell+ 1 + c! ;
: [ ( -- ) 0 state ! ; immediate
: ] ( -- ) 1 state ! ; immediate
: 1+ 1 + ; inline
: 1- 1 - ; inline

: bye 999 state ! ;
: (exit)    0 ;
: (lit)     1 ;
: (jmp)     2 ;
: (jmpz)    3 ;
: (jmpnz)   4 ;
: (njmpz)   5 ;
: (njmpnz)  6 ;
: (ztype)  32 ;

: , here dup 1+ (h) ! code! ;
: const add-word (lit) , , (exit) , ;

(  val and (val) define a very efficient variable mechanism  )
(  Usage:  val a@   (val) (a)   : a! (xx) ! ;  )
: val   0 const ;
: (val) here 2 - ->code const ;

32 ->code const (vh)
: vhere (vh) @ ;

( the original here and last  )
const -l-   const -h-

( STATES: 0=INTERPRET, 1=COMPILE )
: [ 0 state ! ; immediate
: ] 1 state ! ; immediate
: comp? ( --n ) state @ 1 = ;
: if   (jmpz)   , here 0 , ; immediate
: -if  (njmpz)  , here 0 , ; immediate
: if0  (jmpnz)  , here 0 , ; immediate
: -if0 (njmpnz) , here 0 , ; immediate
: then here swap code!     ; immediate

: begin here ; immediate
: again (jmp)     , , ; immediate
: while (jmpnz)   , , ; immediate
: -while (njmpnz) , , ; immediate
: until (jmpz)    , , ; immediate

: allot ( n-- ) (vh) +! ;
: vc, ( c-- ) vhere c! 1 allot ;
: v,  ( n-- ) vhere ! cell allot ;

: var ( n-- ) vhere const allot ;
mem mem-sz + const dict-end
64 1024 * cells mem + const vars
vars (vh) !

( Local variables x, y, z )
60 cells var t0         ( the stack )
vhere 3cells - const t2 ( the stack top limit )
val sp@   (val) t1      ( the stack pointer )
t0 t1 !                 ( initialize sp )
: +L ( -- ) sp@ 3cells + dup t2 > if drop t2 then t1 ! ;
: -L ( -- ) sp@ 3cells - dup t0 < if drop t0 then t1 ! ;

: x@   ( --n ) sp@ @ ;            : x!   ( n-- ) sp@ ! ;
: y@   ( --n ) sp@ cell + @ ;     : y!   ( n-- ) sp@ cell + ! ;
: z@   ( --n ) sp@ 2cells + @ ;    : z!  ( n-- ) sp@ 2cells + ! ;

: c@x  ( --c ) x@ c@ ;            : c!x  ( c-- ) x@ c! ;
: c@x+ ( --c ) x@ dup 1+ x! c@ ;  : c@x- ( --c ) x@ dup 1- x! c@ ;
: c!x+ ( c-- ) x@ dup 1+ x! c! ;  : c!x- ( c-- ) x@ dup 1- x! c! ;

: c@y  ( --c ) y@ c@ ;            : c!y  ( c-- ) y@ c! ;
: c@y+ ( --c ) y@ dup 1+ y! c@ ;  : c@y- ( --c ) y@ dup 1- y! c@ ;
: c!y+ ( c-- ) y@ dup 1+ y! c! ;  : c!y- ( c-- ) y@ dup 1- y! c! ;

: rdrop ( -- ) r> drop ; inline
: tuck  swap over ;
: nip   swap drop ;
: ?dup  -if dup then ;
: 2drop drop drop ;
: 2dup  over over ; inline
: min ( a b-a|b ) 2dup > if swap then drop ;
: max ( a b-a|b ) 2dup < if swap then drop ;
: 0= ( n--f ) 0 =    ; inline
: 0< ( n--f ) 0 <    ; inline
: 2+ ( n--m ) 2 +    ; inline
: 2* ( n--m ) dup +  ; inline
: <= ( a b--f ) > 0= ;
: >= ( a b--f ) < 0= ;
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
: unloop ( -- ) (lsp) @ 3cells - lstk max (lsp) ! ; inline

   1 var (neg)
  65 var buf
cell var (buf)
: ?neg ( n--n' ) dup 0< dup (neg) c! if negate then ;
: #c   ( c-- )   -1 (buf) +! (buf) @ c! ;
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
        stk swap for cell+ dup @ . next drop
    then ')' emit ;

( T - a circular stack )
7 cells var tstk
vhere const t9 cell allot
val tsp@   (val) t0
: tsp! t0 ! ;
: t@    ( --n ) tsp@ @ ;
: t!    ( n-- ) tsp@ ! ;
: tdrop ( -- )  tsp@ cell- dup tstk < if drop t9 then tsp! ;
: >t    ( n-- ) tsp@ cell+ dup t9 > if drop tstk then tsp! t! ;
: t>    ( --n ) t@ tdrop ;
: t+    ( -- )  t@ 1+ t! ;
: t@+   ( --n ) t@ t+ ;
: @t    ( --n ) t@ @ ;
: .tstk '(' emit space 8 for i cells tstk + @ . next ')' emit ;
tstk tsp!

( ColorForth variables )
val a@   (val)  t0
: a!    ( n-- ) t0 ! ;
: >a    ( n-- ) a@ >t a! ;
: adrop ( -- )  t> a! ;
: a+    ( -- )  a@ 1+ a! ;
: a@+   ( --n ) a@ a+ ;
: @a    ( --n ) a@ @ ;
: @a+   ( --n ) a@ @  a@ cell+ a! ;
: @a-   ( --n ) a@ @  a@ cell- a! ;
: c@a   ( --n ) a@  c@ ;
: c@a+  ( --n ) a@+ c@ ;
: c@a-  ( --n ) a@ c@  a@ 1- a! ;

val b@   (val)  t0
: b!    ( n-- ) t0 ! ;
: >b    ( n-- ) b@ >t b! ;
: bdrop ( -- )  t> b! ;
: b+    ( -- )  1 t0 +! ;
: b-    ( -- ) -1 t0 +! ;
: b@+   ( --n ) b@ b+ ;
: !b+   ( n-- ) b@ ! cell  t0 +! ;
: !b-   ( n-- ) b@ ! -cell t0 +! ;
: c@b   ( --c ) b@ c@ ;
: c!b+  ( c-- ) b@ c! b+ ;
: c!b-  ( c-- ) b@ c! b- ;

: t3 ( --a ) vhere dup >b >in ++
    begin >in @ c@ >r >in ++
        r@ 0= r@ '"' = or
        if  rdrop 0 c!b+
            comp? if (lit) , , b@ (vh) ! then
            bdrop exit
        then
        r> c!b+
    again ;

: z" t3 ; immediate
: ." t3 comp? if (ztype) , exit then ztype ;  immediate
: type ( str num-- ) for dup c@ emit 1+ next drop ;

: .word ( de-- ) cell+ 3 + ztype ;
: words last >a 0 >b 1 >t begin
        a@ dict-end < if0 '(' emit t> . ." words)" bdrop adrop exit then
        a@ .word tab t+
        a@ cell+ 2+ c@ 7 > if b+ then 
        b@+ 9 > if cr 0 b! then
        a@ dup cell+ c@ + a!
    again ;

( Compile and execute brackets )
cell var t4
cell var t5
: [[ here t4 !  vhere t5 !  1 state ! ;
: ]] (exit) , 0 state ! t4 @ dup >r (h) ! t5 @ (vh) ! ; immediate

( Strings / Memory )
64 var pad
: fill   ( a num ch-- ) +L z! y! x! y@ for z@ c!x+ next -L ;
: cmove  ( f t n-- ) +L z! y! x! z@ if z@ for c@x+ c!y+ next then -L ;
: cmove> ( f t n-- ) >r r@ 1- + >b r@ 1- + >a r> for c@a- c!b- next adrop bdrop ;
: s-len  ( str--len ) >a 0 begin c@a+ if0 adrop exit then 1+ again ;
: s-end  ( str--end ) dup s-len + ;
: s-cpy  ( dst src--dst ) 2dup s-len 1+ cmove ;
: s-cat  ( dst src--dst ) over s-end over s-len 1+ cmove ;
: s-catc ( dst ch--dst )  over s-end >b c!b+ 0 c!b+ bdrop ;
: s-catn ( dst num--dst ) <# #s #> s-cat ;
: s-eq   ( s1 s2--f ) >a >b
    begin
        c@a c@b = if0 0 bdrop adrop exit then
        c@a if0 1 bdrop adrop exit then
        a+ b+ 
    again ;

( Files )
: fopen-r   ( nm--fh ) z" rb" fopen ;
: fopen-w   ( nm--fh ) z" wb" fopen ;
: ->file    ( fh-- )   output-fp ! ;
: ->stdout  ( -- )     0 ->file ;
: ->stdout! ( -- )     output-fp @ fclose ->stdout ;

: t4 100000 ;
: t5 mem t4 + ;
: rb ( -- ) z" boot2.fth" fopen-r ?dup if0 ." -nf-" exit then
    a! t5 t4 a@ fread drop a@ fclose
    -h- (h) ! -l- (l) ! 
    t5 outer ;

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

( Memory dump )
: aemit ( ch-- )    dup 32 126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )  >a $10 for c@a+ aemit next adrop ;
: dump  ( addr n-- ) swap >a 0 >t for
     t@+ if0 a@ cr .hex ." : " then c@a+ .hex space
     t@ $10 = if 0 t! space space a@ $10 - t0 then
   next tdrop adrop ;

( ANSI color codes )
: csi  27 emit '[' emit ;
: fg   csi ." 38;5;" (.) 'm' emit ;
: black    0 fg ;      : red     203 fg ;
: green   40 fg ;      : yellow  226 fg ;
: blue    63 fg ;      : purple  201 fg ;
: cyan   117 fg ;      : grey    246 fg ;
: white  255 fg ;

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

( shell words )
: lg z" lazygit" system ;
: ll z" ls -l" system ;
: vi z" vi boot2.fth" system ;

( simple fixed point )
: f. 100 /mod (.) '.' emit abs 2 10 .nwb ;
: f* * 100 / ;
: f/ swap 100 * swap / ;
: f+ + ;
: f- - ;

( Version and Banner )
: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    yellow ." DWC " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes." cr
    yellow ."     Code: " white vars mem - cell / . ." cells, used: " here . cr
    yellow ."     Vars: " white last vars - . ." bytes, used: " vhere vars - . cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr ;
.banner

." hello."
