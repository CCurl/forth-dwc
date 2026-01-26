( these are created later as -last- and -here- )
( they are used later for rebooting )
(h) @   (l) @ 

: last (l) @ ;
: here (h) @ ;
: inline    $40 last 4 + 1 + c! ;
: cell    4 ; inline
: -cell  -4 ; inline
: 2cells  8 ; inline
: 3cells 12 ; inline
: cells cell * ; inline
: ->code ( off--addr ) cells mem + ;
: code@  ( off--op )  ->code @ ;
: code!  ( op off-- ) ->code ! ;
: immediate $80 last cell + 1 + c! ;
: , ( dw-- ) here dup 1 + (h) ! code! ;

: bye 999 state ! ;
: (exit)    0 ;
: (lit)     1 ;
: (jmp)     2 ;
: (jmpz)    3 ;
: (jmpnz)   4 ;
: (njmpz)   5 ;
: (njmpnz)  6 ;
: (ztype)  32 ;

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

( val and (val) define a very efficient variable mechanism )
( Usage:  val a@   (val) (a)   : a! (xx) ! ; )
: const ( n-- ) add-word (lit) , , (exit) , ;
:  val  ( -- ) 0 const ;
: (val) ( -- ) here 2 - ->code const ;

( the original here and last - used by 'rb' )
const -last-   const -here-

32 ->code const (vh)
: vhere (vh) @ ;
64 1024 * cells mem + const vars
vars (vh) !

: compiling? ( --n ) state @ 1 = ;
: allot ( n-- ) (vh) +! ;
: var   ( n-- ) vhere const allot ;
mem mem-sz + const dict-end

: min  ( a b-a|b ) over over > if swap then drop ;
: max  ( a b-a|b ) over over < if swap then drop ;
: ?dup ( a--a|0 )  -if dup then ;

( A stack for locals - 3 at a time )
30 cells var t8           ( t8: the locals stack start )
vhere 3cells - const t9   ( t9: the locals stack end )
val t0    (val) t1        ( t0: the stack pointer )
t8 t1 !                   ( Initialize )
: +L ( -- )  t0 3cells + t9 min t1 ! ;
: -L ( -- )  t0 3cells - t8 max t1 ! ;

: x@  ( --n ) t0 @ ;           : x!   ( n-- ) t0 ! ;
: y@  ( --n ) t0 cell   + @ ;  : y!   ( n-- ) t0 cell   + ! ;
: z@  ( --n ) t0 2cells + @ ;  : z!   ( n-- ) t0 2cells + ! ;

: +L1 ( x -- )    +L x! ;
: +L2 ( x y-- )   +L y! x! ;
: +L3 ( x y z-- ) +L z! y! x! ;

: x++ ( -- )  x@ 1 + x! ;  : x@+  ( --n ) x@ x++ ;
: x-- ( -- )  x@ 1 - x! ;  : x@-  ( --n ) x@ x-- ;
: c@x ( --b ) x@ c@ ;      : c@x+ ( --b ) x@+ c@ ;  : c@x- ( --b ) x@- c@ ;
: c!x ( b-- ) x@ c! ;      : c!x+ ( b-- ) x@+ c! ;  : c!x- ( b-- ) x@- c! ;

: y++ ( -- )  y@ 1 + y! ;  : y@+  ( --n ) y@ y++ ;
: y-- ( -- )  y@ 1 - y! ;  : y@-  ( --n ) y@ y-- ;
: c@y ( --b ) y@ c@ ;      : c@y+ ( --b ) y@+ c@ ;  : c@y- ( --b ) y@- c@ ;
: c!y ( b-- ) y@ c! ;      : c!y+ ( b-- ) y@+ c! ;  : c!y- ( b-- ) y@- c! ;

: z++ ( -- )  z@ 1 + z! ;  : z@+  ( --n ) z@ z++ ;

( Strings )
: t3 ( --a ) +L vhere dup z! x! 1 >in +!
    begin
        >in @ c@ y! 1 >in +!
        y@ 0 = y@ '"' = or
        if  0 c!x+  z@
            compiling? if (lit) , , x@ (vh) ! then
            -L exit
        then
        y@ c!x+
    again ;

: z" t3 ; immediate
: ." t3 compiling? if (ztype) , exit then ztype ;  immediate

( Files )
: fopen-r   ( nm--fh ) z" rb" fopen ;
: fopen-w   ( nm--fh ) z" wb" fopen ;
: ->file    ( fh-- )   output-fp ! ;
: ->stdout  ( -- )     0 ->file ;
: ->stdout! ( -- )     output-fp @ fclose ->stdout ;

( reboot and vi )
: t4 100000 ;
: t5 vars t4 + ;
: rb ( -- )
    z" base.fth" fopen-r ?dup if0 ." -nf-" exit then
    t5 x! t4 for 0 c!x+ next
    x! t5 t4 x@ fread drop x@ fclose
    -here- (h) !  -last- (l) ! 
    t5 outer ;

( More core words )
: 1+ 1 + ; inline
: 1- 1 - ; inline
: [ 0 state ! ; immediate  ( 0 = INTERPRET )
: ] 1 state ! ;            ( 1 = COMPILE )
: rdrop ( -- ) r> drop ; inline
: tuck  ( a b--b a b )   swap over ; inline
: nip   ( a b--b )       swap drop ; inline
: 2dup  ( a b--a b a b ) over over ; inline
: 2drop ( a b-- )        drop drop ; inline
: -rot ( a b c--c a b )  swap >r swap r> ;
: 0= ( n--f ) 0 =    ; inline
: 0< ( n--f ) 0 <    ; inline
: <= ( a b--f ) > 0= ;
: >= ( a b--f ) < 0= ;
: ++ ( a-- )   1 swap +! ;
: -- ( a-- )  -1 swap +! ;
: btwi ( n l h--f ) >r over <= swap r> <= and ;
: negate ( n--n' ) 0 swap - ;
: abs ( n--n' ) dup 0< if negate then ;
: cr  ( -- )     13 emit 10 emit ;
: tab ( -- )      9 emit ;
: space  ( -- )  32 emit ;
: spaces ( n-- ) for space next ;
: /   ( a b--q ) /mod nip  ;
: mod ( a b--r ) /mod drop ;
: */  ( n m q--n' ) >r * r> / ;
: execute  ( xt-- ) ?dup if >r then ;
: unloop  ( -- ) (lsp) @ 3 - 0 max (lsp) ! ;
: type  ( a n-- ) for dup c@ emit 1+ next drop ;

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
: (.)  ( n-- )   <# #s #> ztype ;
: .    ( n-- )   (.) space ;

: 0sp 0 (sp) ! ;
: depth ( --n ) (sp) @ 1- ;
: .s '(' emit space depth ?dup if
        stk swap for cell + dup @ . next drop
    then ')' emit ;

: .word ( de-- ) cell + 3 + ztype ;
: words ( -- ) +L last x! 0 y! 1 z! begin
        x@ dict-end < if0 '(' emit z@ . ." words)" -L exit then
        x@ .word tab z++
        x@ cell + 2 + c@ 7 > if y++ then 
        y@+ 12 > if cr 0 y! then
        x@ dup cell + c@ + x!
    again ;

: words-n ( n-- ) +L last x! 0 y! for
        x@ .word tab
        y@+ 12 > if cr 0 y! then
		x@ dup cell + c@ + x!
    next -L ;

cell var t4   cell var t5
: [[ here t4 !  vhere t5 !  1 state ! ;
: ]] (exit) , 0 state ! t4 @ dup >r (h) ! t5 @ (vh) ! ; immediate

( Strings / Memory )
: pad    ( --a ) vhere $100 + ;
: fill   ( a num ch-- ) -rot for 2dup c! 1+ next 2drop ;
: cmove  ( f t n-- )  +L3  z@ if  z@ for c@x+ c!y+ next then -L ;
: cmove> ( f t n-- )  +L3  y@ z@ + 1- y!  x@ z@ + 1- x!  z@ for c@x- c!y- next -L ;
: s-len  ( str--len ) +L1 0 begin c@x+ if0 -L exit then 1+ again ;
: s-end  ( str--end ) dup s-len + ;
: s-cpy  ( dst src--dst ) 2dup s-len 1+ cmove ;
: s-cat  ( dst src--dst ) over s-end  over s-len 1+  cmove ;
: s-catc ( dst ch--dst )  over s-end  +L1  c!x+  0 c!x+  -L ;
: s-catn ( dst num--dst ) <# #s #> s-cat ;
: s-eqn  ( s1 s2 n--f ) +L3 z@ for
	   c@x+ c@y+ = if0 -L 0 unloop exit then
	next -L 1 ;
: s-eq   ( s1 s2--f ) dup s-len 1+ s-eqn ;

( Formatting number output )
: .nwb ( n width base-- )
    base @ >r  base !  >r <# r> 1- for # next #s #> ztype  r> base ! ;
: decimal  ( -- )  #10 base ! ;
: hex      ( -- )  $10 base ! ;
: binary   ( -- )  %10 base ! ;
: .hex     ( n-- )  #2 $10 .nwb ;

: aemit ( ch-- )  dup #32 #126 btwi if0 drop '.' then emit ;
: t0    ( addr-- )  +L1 $10 for c@x+ aemit next -L ;
: dump  ( addr n-- ) +L2 0 z! y@ for
     z@+ if0 x@ cr .hex ." : " then c@x+ .hex space
     z@ $10 = if 0 z! space space x@ $10 - t0 then
   next -L ;

( ANSI color codes )
: csi  27 emit '[' emit ;
: ->cr ( c r-- ) csi (.) ';' emit (.) 'H' emit ;
: cls  csi ." 2J" 1 dup ->cr ;
: fg   csi ." 38;5;" (.) 'm' emit ;
: black    0 fg ;      : red     203 fg ;
: green   40 fg ;      : yellow  226 fg ;
: blue    63 fg ;      : purple  201 fg ;
: cyan   117 fg ;      : grey    246 fg ;
: white  255 fg ;

( *** Banner *** )
: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    yellow ." DWC " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes." cr
    yellow ."     Code: " white vars mem - cell / . ." cells, used: " here . cr
    yellow ."     Vars: " white last vars - . ." bytes, used: " vhere vars - . cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr 
    ." hello." ;
.banner

( *** App code - start *** )

: vi z" vi base.fth" system ;
: lg z" lazygit" system ;

( simple fixed point )
: f. ( n-- )    100 /mod (.) '.' emit abs 2 10 .nwb ;
: f* ( a b--c ) * 100 / ;
: f/ ( a b--c ) swap 100 * swap / ;
: f+ ( a b--c ) + ;
: f- ( a b--c ) - ;

( some tests )

cr ." default app: tests ..." cr
pad z" hi " s-cpy z" there-" s-cat 123 s-catn '!' s-catc ztype cr
: .xyz ." ( " x@ . y@ . z@ . ')' emit cr ;
1 2 3 z! y! x! .xyz
4 5 6 +L3 tab .xyz +L tab tab .xyz -L tab .xyz -L .xyz -L .xyz
: bb timer 1000000000 for next timer swap - . ." ms/us" cr ;

( A stack )
16 cells var stk       ( the stack start )
vhere cell - const t9  ( t9 is the stack end )
val sp@   (val) t1     ( the stack pointer )
: sp! ( n-- ) t1 ! ;   ( set the stack pointer )
stk sp!                ( Initialize )
( for a normal stack, use these definitions )
\ : sp++ ( -- ) sp@ cell + t9  min sp! ;
\ : sp-- ( -- ) sp@ cell - stk max sp! ;
( for a circular stack, use these definitions )
: sp++ ( -- )  sp@ cell +  dup t9  > if drop stk then sp! ;
: sp-- ( -- )  sp@ cell -  dup stk < if drop t9  then sp! ;
: t!   ( n-- ) sp@ ! ;
: t@   ( --n ) sp@ @ ;
: >t   ( n-- ) sp++ t! ;
: t>   ( --n ) sp@ @  sp-- ;
: t6   ( -- )  dup sp@ = if ." sp:" then dup @ . cell + ;
: .stk ( -- )  '(' emit space stk 16 for t6 next drop ')' emit ;
( some stack tests )
16 [[ sp-- for i >t next .stk cr ]]
32 [[ for sp++ t@ . next cr .stk cr ]] 
32 [[ for t> . next cr .stk ]] 

( *** App code - end *** )
