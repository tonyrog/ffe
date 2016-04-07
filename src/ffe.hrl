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

-define(next(SP,RP,IP,WP),
	ffe:next(SP,RP,IP,WP)).

-define(rote(SP,RP,IP,WP,Next),
	[A,B,C|SP1] = SP,
	Next([C,A,B|SP1],RP,IP,WP)).

-define(rev_rote(SP,RP,IP,WP,Next),
	[A,B,C|SP1] = SP,
	Next([B,C,A|SP1],RP,IP,WP)).

-define(plus(SP,RP,IP,WP,Next),
	case SP of
	    [A,B|SP1] when is_number(A), is_number(B) ->
		Next([A+B|SP1],RP,IP,WP);
	    [A,{B,T}|SP1] when is_integer(A), is_integer(B), is_tuple(T) ->
		Next([{A+B,T}|SP1],RP,IP,WP);
	    [{A,T},B|SP1] when is_integer(A), is_integer(B), is_tuple(T) ->
		Next([{A+B,T}|SP1],RP,IP,WP)
	end).

-define(one_plus(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when is_integer(A) ->
		Next([A+1|SP1],RP,IP,WP);
	    [{A,T}|SP1] when is_integer(A), is_tuple(T) ->
		Next([{A+1,T}|SP1],RP,IP,WP)
	end).

-define(minus(SP,RP,IP,WP,Next),
	case SP of
	    [A,B|SP1] when is_number(A), is_number(B) ->
		Next([B-A|SP1],RP,IP,WP);
	    [A,{B,T}|SP1] when is_integer(A), is_integer(B), is_tuple(T) ->
		Next([{B-A,T}|SP1],RP,IP,WP);
	    [{A,T},B|SP1] when is_integer(A), is_integer(B), is_tuple(T) ->
		Next([{B-A,T}|SP1],RP,IP,WP)
	end).

-define(one_minus(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when is_integer(A) ->
		Next([A-1|SP1],RP,IP,WP);
	    [{A,T}|SP1] when is_integer(A), is_tuple(T) ->
		Next([{A-1,T}|SP1],RP,IP,WP)
	end).

-define(star(SP,RP,IP,WP,Next),
	case SP of
	    [A,B|SP1] when is_integer(A), is_integer(B) ->
		Next([A*B|SP1],RP,IP,WP)
	end).

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

-define(dupe(SP,RP,IP,WP,Next),
	[A|_] = SP,
	Next([A|SP],RP,IP,WP)).

-define(abs(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([abs(A)|SP1],RP,IP,WP)).

-define(lshift(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([B bsl A|SP1],RP,IP,WP)).

-define(rshift(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([B bsr A|SP1],RP,IP,WP)).

%% unsigned?
-define(arshift(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([B bsr A|SP1],RP,IP,WP)).

-define('and'(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([A band B|SP1],RP,IP,WP)).

-define('or'(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([A bor B|SP1],RP,IP,WP)).

-define('xor'(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([A bxor B|SP1],RP,IP,WP)).

-define(spat(SP,RP,IP,WP,Next),
	Next([SP|SP],RP,IP,WP)).

-define(rpat(SP,RP,IP,WP,Next),
	Next([RP|SP],RP,IP,WP)).

-define(spstore(SP,RP,IP,WP,Next),
	[SP1|_] = SP,
	Next(SP1,RP,IP,WP)).

-define(rpstore(SP,RP,IP,WP,Next),
	[RP1|SP1] = SP,
	Next(SP1,RP1,IP,WP)).

-define('min'(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([erlang:min(A,B)|SP1],RP,IP,WP)).

-define('max'(SP,RP,IP,WP,Next),
	[A,B|SP1] = SP,
	Next([erlang:max(A,B)|SP1],RP,IP,WP)).

-define(invert(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([bnot A|SP1],RP,IP,WP)).

-define(zero_equals(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when A =:= 0 ->
		Next([-1|SP1],RP,IP,WP);
	    [_|SP1] ->
		Next([0|SP1],RP,IP,WP)
	end).

-define(zero_less(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when A < 0 ->
		Next([-1|SP1],RP,IP,WP);
	    [_|SP1] ->
		Next([0|SP1],RP,IP,WP)
	end).
