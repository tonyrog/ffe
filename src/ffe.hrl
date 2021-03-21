%%
%% Macros for primitives
%%
-ifndef(__FFE_HRL__).
-define(__FFE_HRL__, true).

-define(BYE,       bye).    %% fixme
-define(STACK_OVERFLOW,  -3).
-define(STACK_UNDERFLOW, -4).    %% case clause?
-define(ARITH,           -10).   %% arith
-define(UNDEF,           -13).   %% undefined word
-define(QUIT,            -56).
-define(INTRRUPT,        -28).   %% user interrupt

-define(EXPORT(W), -export([W/0, W/4])).
-define(WORD(W,Xt), <<W>> => fun ?MODULE:Xt/0).
-define(XT(W), W() -> {0, <<??W>>, fun ?MODULE:W/4 }).
-define(XT(Nm,Cf), Cf() -> {0, <<Nm>>, fun ?MODULE:Cf/4 }).
-define(IXT(W), W() -> {?IMMEDIATE, <<??W>>, fun ?MODULE:W/4 }).
-define(IXT(Nm,Cf), Cf() -> {?IMMEDIATE, <<Nm>>, fun ?MODULE:Cf/4 }).
-define(TRUE, -1).
-define(FALSE, 0).
-define(BOOL(X), if (X) -> ?TRUE; true -> ?FALSE end).

%% Field pointer, reference into PFA
%% -define(WPTR(Offs,W), [(Offs)|(W)]).
%% debug version
-define(WPTR(Offs,W), {wptr,(Offs),(W)}).

-define(FIXME(), erlang:display_string("FIXME\n")).

-define(next(SP,RP,IP,WP),
	ffe:next(SP,RP,IP,WP)).

-define(rot(SP,RP,IP,WP,Next),
	[A,B,C|SP1] = SP,
	Next([C,A,B|SP1],RP,IP,WP)).

-define(m_rot(SP,RP,IP,WP,Next),
	[A,B,C|SP1] = SP,
	Next([B,C,A|SP1],RP,IP,WP)).

-define(plus(SP,RP,IP,WP,Next),
	case SP of
	    [B,A|SP1] when is_integer(A), is_integer(B) ->
		Next([A+B|SP1],RP,IP,WP);
	    [B,?WPTR(A,W)|SP1] when is_integer(B) ->
		Next([?WPTR(A+B,W)|SP1],RP,IP,WP);
	    [?WPTR(B,W),A|SP1] when is_integer(A) ->
		Next([?WPTR(A+B,W)|SP1],RP,IP,WP)
	end).

-define(one_plus(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when is_integer(A) ->
		Next([A+1|SP1],RP,IP,WP);
	    [?WPTR(I,W)|SP1] ->
		Next([?WPTR(I+1,W)|SP1],RP,IP,WP)
	end).

-define(two_plus(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when is_integer(A) ->
		Next([A+2|SP1],RP,IP,WP);
	    [?WPTR(I,W)|SP1] ->
		Next([?WPTR(I+2,W)|SP1],RP,IP,WP)
	end).

-define(minus(SP,RP,IP,WP,Next),
	case SP of
	    [B,A|SP1] when is_integer(A), is_integer(B) ->
		Next([A-B|SP1],RP,IP,WP);
	    [B,?WPTR(A,W)|SP1] when is_integer(B) ->
		Next([?WPTR(A-B,W)|SP1],RP,IP,WP);
	    [?WPTR(B,W),A|SP1] when is_integer(A) ->
		Next([?WPTR(A-B,W)|SP1],RP,IP,WP)
	end).

-define(one_minus(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when is_integer(A) ->
		Next([A-1|SP1],RP,IP,WP);
	    [?WPTR(I,W)|SP1] ->
		Next([?WPTR(I-1,W)|SP1],RP,IP,WP)
	end).

-define(two_minus(SP,RP,IP,WP,Next),
	case SP of
	    [A|SP1] when is_integer(A) ->
		Next([A-2|SP1],RP,IP,WP);
	    [?WPTR(I,W)|SP1] ->
		Next([?WPTR(I-2,W)|SP1],RP,IP,WP)
	end).

-define(star(SP,RP,IP,WP,Next),
	case SP of
	    [A,B|SP1] when is_integer(A), is_integer(B) ->
		Next([A*B|SP1],RP,IP,WP)
	end).

-define(star_slash(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [C,B,A|SP1] when is_integer(A), is_integer(B) ->
		Next([(A*B) div C|SP1],RP,IP,WP)
	end).

-define(star_slash_mod(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [C,B,A|SP1] when is_integer(A), is_integer(B) ->
		T = A*B,
		Next([T rem C, T div C|SP1],RP,IP,WP)
	end).

-define(slash(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [B,A|SP]-> Next([B div A|SP],RP,IP,WP)
	end).

-define(slash_mod(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [B,A|SP]-> Next([A rem B,A div B|SP],RP,IP,WP)
	end).

-define(mod(SP,RP,IP,WP,Next),
	case SP of
	    [0|_] -> throw({?ARITH, "division by zero"});
	    [B,A|SP]-> Next([A rem B|SP],RP,IP,WP)
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
	[B,A|SP1] = SP,
	Next([A,B|SP1],RP,IP,WP)).

-define(dup(SP,RP,IP,WP,Next),
	[A|_] = SP,
	Next([A|SP],RP,IP,WP)).

-define(abs(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([abs(A)|SP1],RP,IP,WP)).

-define(lshift(SP,RP,IP,WP,Next),
	[B,A|SP1] = SP,
	Next([A bsl B|SP1],RP,IP,WP)).

%% FIXME: unsigned
-define(rshift(SP,RP,IP,WP,Next),
	[B,A|SP1] = SP,
	Next([A bsr B|SP1],RP,IP,WP)).

-define(arshift(SP,RP,IP,WP,Next),
	[B,A|SP1] = SP,
	Next([A bsr B|SP1],RP,IP,WP)).

-define('and'(SP,RP,IP,WP,Next),
	[B,A|SP1] = SP,
	Next([A band B|SP1],RP,IP,WP)).

-define('or'(SP,RP,IP,WP,Next),
	[B,A|SP1] = SP,
	Next([A bor B|SP1],RP,IP,WP)).

-define('xor'(SP,RP,IP,WP,Next),
	[B,A|SP1] = SP,
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
	[A|SP1] = SP,
	Next([?BOOL(A=:=0)|SP1],RP,IP,WP)).

-define(zero_less(SP,RP,IP,WP,Next),
	[A|SP1] = SP,
	Next([?BOOL(A<0)|SP1],RP,IP,WP)).

-endif.
