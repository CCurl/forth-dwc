(  comments are built in!  )

: last (l) @ ;
: here (h) @ ;
: inline    $40 last 4 + 1 + c! ;
: cell   4 ; inline
: -cell -4 ; inline
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
32 ->code const (vh)
: vhere (vh) @ ;

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

: aligned ( a1--a2 ) #4 over #3 and - #3 and + ;
: align ( -- ) vhere aligned (vh) ! ;
: allot ( n-- ) (vh) +! ;
: vc, ( c-- ) vhere c! 1 allot ;
: v,  ( n-- ) vhere ! cell allot ;

: var ( n-- ) align vhere const allot ;
mem mem-sz + const dict-end
64 1024 * cells mem + const vars
vars (vh) !

(  val and (val) define a very efficient variable mechanism  )
(  Usage:  val xx   (val) (xx)   : xx! (xx) ! ;  )
: val   add-word (lit) , 0 , (exit) , ;
: (val) add-word (lit) , here 3 - ->code , (exit) , ;

: rdrop ( -- ) r> drop ; inline
: tuck  swap over ;
: nip   swap drop ;
: 2dup  over over ;
: 2drop drop drop ;
: ?dup  -if dup then ;
: 0= ( n--f ) 0 =    ; inline
: 0< ( n--f ) 0 <    ; inline
: 2+ ( n--m ) 2 +    ; inline
: 2* ( n--m ) dup +  ; inline
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

: .word ( de-- ) cell+ 3 + ztype ;
: words last >a 0 >b 1 >t begin
        a@ dict-end < if0 '(' emit t> . ." words)" bdrop adrop exit then
        a@ .word tab t+
        a@ cell+ 2+ c@ 7 > if b+ then 
        b@+ 9 > if cr 0 b! then
        a@ dup cell+ c@ + a!
    again ;

cell var t4
cell var t5
: [[ here t4 !  vhere t5 !  1 state ! ;
: ]] (exit) , 0 state ! t4 @ dup >r (h) ! t5 @ (vh) ! ; immediate

( Strings / Memory )
: fill   ( a num ch-- ) >r swap >b for r@ c!b+ next bdrop rdrop ;
: move   ( f t n-- ) >r >b >a r> ?dup if for  @a+  !b+ next then adrop bdrop ;
: cmove  ( f t n-- ) >r >b >a r> ?dup if for c@a+ c!b+ next then adrop bdrop ;
: move>  ( f t n-- ) >r r@ 1- cells + >b r@ 1- cells + >a r> for  @a-  !b- next adrop bdrop ;
: cmove> ( f t n-- ) >r r@ 1-       + >b r@ 1-       + >a r> for c@a- c!b- next adrop bdrop ;
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
: fopen-r  ( nm--fh ) z" rb" fopen ;
: fopen-w  ( nm--fh ) z" wb" fopen ;
: ->file   ( fh-- )   output-fp ! ;
: ->stdout ( -- )     0 ->file ;

cell var t0
cell var t1
cell var t2
: marker  here t0 !   last t1 !   vhere t2 ! ;
: forget  t0 @ (h) !  t1 @ (l) !  t2 @ (vh) ! ;

( Disk: 128 blocks, 8192 bytes each )
mem 14 1024 1024 * * + const disk
32 var fn
val blk@   (val) t0
: #blks     ( --n )   128 ;
: blk-sz    ( --n )   8192 ;
: blk!      ( n-- )   0 max #blks 1- min t0 ! ;
: blk-fn    ( --a )   fn z" block-" s-cpy blk@ <# # # #s #> s-cat z" .fth" s-cat ;
: blk-addr  ( --a )   blk@ blk-sz * disk + ;
: blk-clr   ( -- )    blk-addr blk-sz 0 fill ;
: t2        ( fh-- )  blk-clr >r blk-addr blk-sz r@ fread  drop r> fclose ;
: read-blk  ( -- )    blk-fn fopen-r ?dup if0 ." -nf-" drop exit then t2 ;
: t1        ( fh-- )  >r blk-addr blk-sz r@ fwrite drop r> fclose ;
: write-blk ( -- )    blk-fn fopen-w ?dup if0 ." -err-" drop exit then t1 ;
: nt-blk    ( -- )    0 blk-addr blk-sz + 1- c! ;
: load      ( n-- )   blk! read-blk nt-blk blk-addr outer ;
: load-next ( n-- )   blk! read-blk nt-blk blk-addr >in ! ;
0 blk!

marker

1 load
