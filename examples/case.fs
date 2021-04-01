
cr s" Loading case.fs" type cr

: n-case ( n -- )
    case ." value is "
	1 of ." One" endof
	2 of ." Two" endof
	3 of ." Three" endof
	dup .
    endcase ;
	
