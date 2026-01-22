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

: 2dup  over over ; inline
: min ( a b-a|b ) 2dup > if swap then drop ;
: max ( a b-a|b ) 2dup < if swap then drop ;

( A temp stack for locals mostly )
32 cells var t8        ( the stack bottom )
vhere cell - const t9  ( the stack top limit )
val t0     (val) t1    ( the stack pointer )
t8 t1 !                ( initialize sp )
: >t   ( n-- ) t0 cell + t9 min dup t1 ! ! ;
: tdrop ( -- ) t0 cell - t8 max t1 ! ;
: t@   ( n-- ) t0 @ ;
: t!   ( n-- ) t0 ! ;
: t>   ( --n ) t@ tdrop ;

( The A stack )
16 cells var t8       ( the stack bottom )
vhere cell - const t9 ( the stack top )
val t0    (val) t1    ( the stack pointer )
t8 t1 !               ( initialize the sp )
: >a   ( n-- ) t0 cell + t9 min dup t1 ! ! ;
: adrop ( -- ) t0 cell - t8 max t1 ! ;
: a@   ( --n ) t0 @ ;             : a!   ( n-- ) t0 ! ;
: a>   ( --n ) a@ adrop ;
: c@a  ( --c ) a@ c@ ;            : c!a  ( c-- ) a@ c! ;
: c@a+ ( --c ) a@ dup 1+ a! c@ ;  : c!a+ ( c-- ) a@ dup 1+ a! c! ;
: c@a- ( --c ) a@ dup 1- a! c@ ;  : c!a- ( c-- ) a@ dup 1- a! c! ;
: a++  ( -- ) 1 t0 +! ;

( The A stack )
16 cells var t8       ( the stack bottom )
vhere cell - const t9 ( the stack top )
val t0    (val) t1    ( the stack pointer )
t8 t1 !               ( initialize the sp )
: >a   ( n-- ) t0 cell + t9 min dup t1 ! ! ;
: adrop ( -- ) t0 cell - t8 max t1 ! ;
: a@   ( --n ) t0 @ ;             : a!   ( n-- ) t0 ! ;
: a>   ( --n ) a@ adrop ;
: c@a  ( --c ) a@ c@ ;            : c!a  ( c-- ) a@ c! ;
: c@a+ ( --c ) a@ dup 1+ a! c@ ;  : c!a+ ( c-- ) a@ dup 1+ a! c! ;
: c@a- ( --c ) a@ dup 1- a! c@ ;  : c!a- ( c-- ) a@ dup 1- a! c! ;
: a++  ( -- ) 1 t0 +! ;

: rdrop ( -- ) r> drop ; inline
: tuck  swap over ;
: nip   swap drop ;
: -rot  ( a b c -- c a b ) swap >r swap r> ;
: ?dup  -if dup then ;
: 2drop drop drop ;
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
        a@ .word tab t@ 1+ t!
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
: fill   ( a num ch-- ) -rot for 2dup c! 1+ next 2drop ;
: cmove  ( f t n-- ) >r >b >a  r> for c@a+ c!b+ next adrop bdrop ;
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
        a++   b+ 
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
     t@ 1+ dup t! if0 a@ cr .hex ." : " then c@a+ .hex space
     t@ $10 = if 0 t! space space a@ $10 - t0 then
   next tdrop adrop ;

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

( ANSI color codes )
: csi  27 emit '[' emit ;
: fg   csi ." 38;5;" (.) 'm' emit ;
: black    0 fg ;      : red     203 fg ;
: green   40 fg ;      : yellow  226 fg ;
: blue    63 fg ;      : purple  201 fg ;
: cyan   117 fg ;      : grey    246 fg ;
: white  255 fg ;

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
