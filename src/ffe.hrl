%%
%% Macros for primitives
%%

-define(BYE,       bye).    %% fixme
-define(STACK_OVERFLOW,  -3).
-define(STACK_UNDERFLOW, -4).    %% case clause?
-define(ARITH,           -10).   %% arith
-define(UNDEF,           -13).   %% undefined word
-define(QUIT,            -56).
-define(INTRRUPT,        -28).   %% user interrupt


-define(rote(SP,RP,IP,WP,Next),
	[A,B,C|SP1] = SP,
	Next([C,A,B|SP1],RP,IP,WP)).

-define(rev_rote(SP,RP,IP,WP,Next),
	[A,B,C|SP1] = SP,
	Next([B,C,A|SP1],RP,IP,WP)).

-define(plus(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([A+B|SP1],RP,IP,WP)).

-define(one_plus(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([A+1|SP1],RP,IP,WP)).

-define(minus(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([B-A|SP1],RP,IP,WP)).

-define(one_minus(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([A-1|SP1],RP,IP,WP)).

-define(star(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([A*B|SP1],RP,IP,WP)).

-define(slash(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [A,B|SP]-> Next([B div A|SP],RP,IP,WP)
	end).

-define(mod(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [A,B|SP]-> Next([B rem A|SP],RP,IP,WP)
	end).

-define(slash_mod(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [A,B|SP]-> Next([B rem A,B div A|SP],RP,IP,WP)
	end).

-define(negate(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([-A|SP1],RP,IP,WP)).

-define(over(SP,RP,IP,WP,Next),
	[_,B|_] = SP,
	Next([B|SP],RP,IP,WP)).

-define(drop(SP,RP,IP,WP,Next),
	[_|SP1] = SP,
	Next(SP1,RP,IP,WP)).

-define(swap(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([B,A|SP1],RP,IP,WP)).
		
	
