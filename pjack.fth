(( a somewhat circular stack inspired by Peter Jakacki ))
(( this provides efficient access to the top 3 entries, r1-r3 ))
(( but pushing and popping entries is fairly expensive ))
var t1 $10 cells allot
  t1 cell+ const t2
  t2 cell+ const t3

: r1   ( --n ) t1 @ ;
: r2   ( --n ) t2 @ ;
: r3   ( --n ) t3 @ ;
: r1!  ( n-- ) t1 ! ;
: @r1+ ( n-- ) r1 @ cell t1 +! ;
: pdrop ( -- ) t2 t1 $0f move ;
: >p  ( n-- )  t1 t2 $0f move> r1! ;
: p>  ( --n )  r1 pdrop ;
: n-ppush ( <n> cnt-- ) for >p next ;
: n-pdrop ( n-- ) for pdrop next ;
: .pstk t1 >p $10 for @r1+ . next pdrop ;

