(( shell words ))
: lg z" lazygit" system ;
: ll z" ls -l" system ;

(( Startup message ))
: .version version <# # # #. # # #. #s 'v' #c #> ztype ;
: .banner
    ." dwc " green .version white ."  - Chris Curl" cr
    yellow ."   Memory: " white mem-sz . ." bytes, used: " vhere vars - . cr
    yellow ."     Code: " white 64 1024 * (.) ." , used: " here . cr
    yellow ."     Vars: " white vhere vars - .  ." bytes used" cr
    yellow ."     Dict: " white dict-end last - .  ." bytes used" cr ;
.banner   marker

." hello."
