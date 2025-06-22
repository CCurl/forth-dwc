(( Strings / Memory ))
: 2dup ( n--n n ) over over ;
: move   ( f t n-- ) >r >y x! r> for  @x+  !y+ next y>> ;
: cmove  ( f t n-- ) >r >y x! r> for c@x+ c!y+ next y>> ;
: move>  ( f t n-- ) >r r@ 1- cells + y! r@ 1- cells + x! r> for  @x-  !y- next ;
: cmove> ( f t n-- ) >r r@ 1-       + y! r@ 1-       + x! r> for c@x- c!y- next ;
: fill   ( a num ch-- ) x! swap >y for x c!y+ next y>> ;
: s-len  ( str--len ) 0 >y x! begin c@x+ if0 y> exit then y++ again ;
: s-end  ( str--end ) dup s-len + ;
: s-cpy  ( dst src--dst ) 2dup s-len 1+ cmove ;
: s-cat  ( dst src--dst ) over s-end over s-len 1+ cmove ;
: s-catc ( dst ch--dst ) over s-end >y c!y+ 0 y> c! ;
: s-catn ( dst num--dst ) <# #s #> s-cat ;

: c+! ( n a-- ) dup >r c@ + r> c! ;

(( shell words ))
: lg z" lazygit" system ;
: ll z" ls -l" system ;

." hello."
