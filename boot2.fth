(( bootstrap ))

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
: align   ( -- )     vhere aligned (vh) ! ;
: allot   ( n-- )    (vh) +! ;

: const ( n-- ) add-word (lit) , , (exit) , ;
: var align vhere const ;
mem mem-sz + const dict-end
64 1024 * cells mem + const vars
vars (vh) !

((  val and (val) define a very efficient variable mechanism  ))
: val   add-word (lit) , 0 , (exit) , ;
: (val) add-word (lit) , here 3 - ->code , (exit) , ;

: space 32 emit ;
: rdrop r> drop ; inline
: ++ ( a-- )  1 swap +! ;
: -- ( a-- ) -1 swap +! ;
: cr 13 emit 10 emit ;

var (neg)    1 allot
var buf     65 allot
var (buf) cell allot
: ?neg ( n--n' ) dup 0 < dup (neg) c! if 0 swap - then ;
: #c   ( c-- )   (buf) -- (buf) @ c! ;
: #n   ( n-- )   '0' + dup '9' > if 7 + then #c ;
: #    ( n--m )  base @ /mod swap #n ;
: #s   ( n--0 )  # dup if #s exit then ;
: <#   ( n--m )  ?neg buf 65 + (buf) ! 0 #c ;
: #>   ( n--a )  drop (neg) @ if '-' #c then (buf) @ ;
: (.)  ( n-- )   <# #s #> ztype ;
: .    ( n-- )   (.) space ;

: 0sp 0 (sp) ! ;
: depth (sp) @ 1- ;
: .s '(' emit space depth if
        (stk) cell+ >r depth for r@ @ . r> cell+ >r next
    then ')' emit rdrop ;

val a  (val) t0
: a!   ( n-- ) t0 ! ;
: a+   ( --n ) a 1 t0 +! ;
: @a   ( --n ) a  @ ;
: c@a+ ( --n ) a+ c@ ;
: c!a+ ( n-- ) a+ c! ;

: (") ( --a ) vhere dup a! >in ++
    begin >in @ c@ >r >in ++
        r@ 0 = r@ '"' = or
        if  rdrop 0 c!a+
            comp? if (lit) , , a (vh) ! then exit
        then
        r> c!a+
    again ;

: z" (") ; immediate
: ." (") comp? if (ztype) , exit then ztype ;  immediate

: fopen-r ( nm md--fh ) z" rb" fopen ;
: block-sz 2048 ;
: #blocks 512 ;
#blocks block-sz * const disk-sz
vars    1000000  + const disk
." loading disk ... " z" disk.fth" fopen-r a!
disk disk-sz a fread a fclose . ." bytes" cr

disk outer
