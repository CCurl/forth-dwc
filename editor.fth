(( Editor  ))
val off (val) t0 : off! t0 ! ;
val row (val) t0 : row! t0 ! ;
val col (val) t0 : col! t0 ! ;
var ed-buf block-sz allot
: rows 23 ;   : cols 89 ; (( NB: 23*89 = 2047 ))
: off->rc off rows /mod row! col! ;
: rc->off row rows * col + off! ;
: blk>buf ( n-- ) block-addr ed-buf block-sz cell / move ;
: buf>blk ( n-- ) ed-buf swap block-addr block-sz cell / move ;
: show 1 1 ->cr ed-buf a! rows for cols for c@a+ 32 max emit next cr next ;

