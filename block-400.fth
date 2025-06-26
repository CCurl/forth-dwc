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
