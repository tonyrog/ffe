
cr s" Loading loops.fs" type cr

: u-down ( n-- )
    begin
	." value = " dup . cr
	1- ?dup 0=
    until ;

: u-up ( n-- )
    begin
	." value = " dup . cr
	1+ dup 10 =
    until drop ;


: r-down ( n -- )
    begin ?dup while
	    ." value = " dup . cr
	    1-
    repeat ;

: r-up ( n -- )
    begin dup 10 < while
	    ." value = " dup . cr
	    1+
    repeat drop ;

: l-up ( n -- )
    0 do ." value = " i . cr loop ;

: l-up2 ( n -- )
    0 do ." value = " i . cr 2 +loop ;

: l-down2 ( n -- )
    0 do ." value = " i . cr -2 +loop ;

: give-up
    0 do ." i=" i . cr
	i 5 = if leave then
    loop ;
