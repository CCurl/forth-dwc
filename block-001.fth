(( shell words ))
: lg z" lazygit" system ;
: ll z" ls -l" system ;

(( Startup message ))
: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    ." dwc " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes." cr
    yellow ."     Code: " white vars mem - cell / . ." cells, used: " here . cr
    yellow ."     Vars: " white disk vars - . ." bytes, used: " vhere vars - . cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr ;
.banner   marker

." hello."
