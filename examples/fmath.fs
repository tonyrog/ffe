
\ module fmath

\ ( x -- (x*x) )
: sqr dup * ;

\ ( <a1> .. <an> <n> -- <sum> )
: sum 0 #DO + #LOOP ;

: sqrsum 0 #DO sqr + #LOOP ;
