Forth Flavoured Erlang
======================

This is forth on steorids in an Erlang environment send and receiving
message running in parallell.

And it is nearly functional ...


module based forth.

words are organized into modules/vocabularies, words are
called by using <module>:<function>, built in words are
access directly without the : module separator.

The words in interactive mode are added into a special module.

: sqr dup * ;


In a module:

module foo

\ ( x -- (x*x) )
: sqr dup * ;

\ ( <a1> .. <an> <n> -- <sum> )
: sum 0 do + loop ;

If compiled then this is translated into the module foo.


-module(foo).
-export(['_ffe_sqr'/0,'_ffe_sum'/0]).
	
'_ffe_sqr'() ->
  	{ 0, <<"sqr">>, fun ffe:docol/4, ffe:star/0, ffe:semis/0 }).

'_ffe_sum'() ->
        { 0, <<"sum">>, fun ffe:docol/4, ffe:lit/4, 0, 
	  fun ffe:pdo/0, 
	      fun fee:plus/0, 
	  fun ffe:ploop/0, -2, ffe:semis/0 }.
