
\ module fmath

\ square a number
: sqr ( x -- x ) dup * ;

\ sum u numbers on stack
: sqrsum ( x1 .. xu u -- x ) 0 swap 0 #DO swap sqr + #LOOP ;

: sqrsum2 ( x1 x2 -- x ) sqr swap sqr + ;






