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
: r2!  ( n-- ) t2 ! ;
: @r1+ ( n-- ) r1 @ cell t1 +! ;
: 1>p  ( n-- )   t1 t2 $0f move> r1! ;
: 2>p  ( x y-- ) t1 t3 $0e move> r2! r1! ;
: 1<p  ( -- )  t2 t1 $0f move ;
: 2<p  ( -- )  t3 t1 $0e move ;
: .pstk t1 1>p $10 for @r1+ . next 1<p ;
