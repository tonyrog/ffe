\ Bootstrapping words

: < - 0< ;
: +! ( value addr -- )
    dup @  ( value addr value2 )
    rot    ( addr value2 value )
    +      ( addr sum )
    swap ! ;

\
\ NULL ( -- )
\  executed when no words are found will leave the endless loop
\  NOTE this is special case NULL is compile to "" by fcompi
\
: NULL r> drop ;

\
\  ++ : ( addr -- )  increament value at address addr 
\
: ++ 1 swap +! ;

\ 
\  -- : ( addr -- )  decrements value at address addr )
\
: -- -1 swap +! ;

\  =   ( a b -- bool ) test if a and b are equal
: = - 0= ;

\
\ >    ( a b -- bool ) test if  ( a  > b )
\
: > swap < ;

\ set base 16
: hex  16 base ! ;

\ set base 10 
: decimal 10 base ! ;

\ set base 8 
: octal 8 base ! ;

\ set base 2
: bin 2 base ! ;

