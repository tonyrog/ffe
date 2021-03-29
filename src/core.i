%% -*- erlang -*-
%%   FFE core words
%%
-ifndef(__CORE__).
-define(__CORE__, true).

-include("ffe.hrl").

core_words() ->
    #{ 
       ?WORD("+",          plus),
       ?WORD("-",          minus),
       ?WORD(",",          comma),
       ?WORD(".",          dot),
       ?WORD("0=",         zero_equals),
       ?WORD("0<",         zero_less),
       ?WORD("1-",         one_minus),
       ?WORD("1+",         one_plus),
       ?WORD("2*",         two_star),
       ?WORD("2/",         two_slash),
       ?WORD("2!",         two_store),
       ?WORD("2@",         two_fetch),
       ?WORD("2drop",      two_drop),
       ?WORD("2dup",       two_dup),
       ?WORD("2over",      two_over),
       ?WORD("2swap",      two_swap),
       ?WORD("'",          tick),
       ?WORD("!",          store),
       ?WORD("<#",         bracket_number),
       ?WORD("#",          number_sign),
       ?WORD("#s",         number_sign_s),
       ?WORD("#>",         number_bracket),
       ?WORD("*",          star),
       ?WORD("*/",         star_slash),
       ?WORD("*/mod",      star_slash_mod),
       ?WORD(".\"",        dot_quote),
       ?WORD("/",          slash),
       ?WORD("/mod",       slash_mod),
       ?WORD(":",          colon),
       ?WORD(";",          semicolon),
       ?WORD("?dup",       qdup),
       ?WORD("@",          fetch),
       ?WORD("[",          left_bracket),
       ?WORD("[']",        bracket_tick),
       ?WORD("[char]",     bracket_care),
       ?WORD("]",          right_bracket),
       ?WORD("+!",         plus_store),
       ?WORD("+loop",      plus_loop),
       ?WORD("<",          less),
       ?WORD("=",          equal),
       ?WORD(">",          greater),
       ?WORD(">body",      to_body),
       ?WORD(">r",         tor),
       ?WORD("r>",         rfrom),
       ?WORD("abs",        abs),
       ?WORD("and",        'and'),
       ?WORD("arshift",    arshift),
       ?WORD("bl",         bl),
       ?WORD("count",      count),
       ?WORD("cr",         cr),
       ?WORD("drop",       drop),
       ?WORD("dup",        dup),
       ?WORD("emit",       emit),
       ?WORD("find",       find),
       ?WORD("hold",       hold),
       ?WORD("invert",     invert),
       ?WORD("lshift",     lshift),
       ?WORD("max",        max),
       ?WORD("min",        min),
       ?WORD("mod",        mod),
       ?WORD("negate",     negate),
       ?WORD("or",         'or'),
       ?WORD("over",       over),
       ?WORD("quit",       quit),
       ?WORD("rot",        rot),
       ?WORD("rshift",     rshift),
       ?WORD("swap",       swap),
       ?WORD("space",      space),
       ?WORD("spaces",     spaces),
       ?WORD("type",       type),
       ?WORD("word",       word),
       ?WORD("xor",        'xor'),

       %% fixme sort
      ?WORD("sp@",        spat),
      ?WORD("rp@",        rpat),
      ?WORD("sp!",        spstore),
      ?WORD("rp!",        rpstore),
      ?WORD("i",          i),
      ?WORD("j",          j),
      ?WORD("leave",      leave)
     }.


?XT("+", plus).
plus(stack) ->
    "( n1 n2 -- n3 )";
plus(documentation) ->
    "Add n2 to n1 giving the sum n3".
plus(SP,RP,IP,WP) ->
    case SP of
	[B,A|SP1] when is_integer(A), is_integer(B) ->
	    next([A+B|SP1],RP,IP,WP);
	[B,?WPTR(A,W)|SP1] when is_integer(B) ->
	    next([?WPTR(A+B,W)|SP1],RP,IP,WP);
	[?WPTR(B,W),A|SP1] when is_integer(A) ->
	    next([?WPTR(A+B,W)|SP1],RP,IP,WP)
    end.

?XT("-", minus).
minus(stack) ->
    "( n1 n2 -- n3 )";
minus(documentation) ->
    "Subtract n2 from n1 giving the difference n3".
minus(SP,RP,IP,WP) ->
    case SP of
	[B,A|SP1] when is_integer(A), is_integer(B) ->
	    next([A-B|SP1],RP,IP,WP);
	[B,?WPTR(A,W)|SP1] when is_integer(B) ->
	    next([?WPTR(A-B,W)|SP1],RP,IP,WP);
	[?WPTR(B,W),A|SP1] when is_integer(A) ->
	    next([?WPTR(A-B,W)|SP1],RP,IP,WP)
    end.

%% , is currently a bit special
?XT(",", comma).
comma([Value|SP],RP,IP,WP) ->
    comma_(Value),
    next(SP,RP,IP,WP).

%% print stack value
?XT(".", dot).
dot([Value|SP],RP,IP,WP) ->
    emit_value(Value),
    emit_char($\s),
    next(SP,RP,IP,WP).

?XT("1-", one_minus).
one_minus(SP,RP,IP,WP) ->
    case SP of
	[A|SP1] when is_integer(A) ->
	    next([A-1|SP1],RP,IP,WP);
	[?WPTR(I,W)|SP1] ->
	    next([?WPTR(I-1,W)|SP1],RP,IP,WP)
    end.

?XT("1+", one_plus).
one_plus(SP,RP,IP,WP) ->
    case SP of
	[A|SP1] when is_integer(A) ->
	    next([A+1|SP1],RP,IP,WP);
	[?WPTR(I,W)|SP1] ->
	    next([?WPTR(I+1,W)|SP1],RP,IP,WP)
    end.

?XT("2*", two_star).
two_star([A|SP],RP,IP,WP) ->
    next([A bsl 1|SP],RP,IP,WP).

?XT("2/", two_slash).
two_slash([A|SP],RP,IP,WP) ->
    next([A bsr 1|SP],RP,IP,WP).

?XT("2@", two_fetch).
two_fetch([Addr|SP],RP,IP,WP) ->
    X1 = fetch_at(Addr),
    X2 = fetch_at(add_addr(Addr, 1)),
    next([X2,X1|SP],RP,IP,WP).

?XT("2!", two_store).
two_store([Addr,X2,X1|SP],RP,IP,WP) ->
    store_at(add_addr(Addr, 1), X2),
    store_at(Addr,X1),
    next(SP,RP,IP,WP).

?XT("2drop", two_drop).
two_drop([_,_|SP],RP,IP,WP) ->
    next(SP,RP,IP,WP).

?XT("2dup", two_dup).
two_dup(SP0=[B,A|_],RP,IP,WP) ->
    next([B,A|SP0],RP,IP,WP).

?XT("2over", two_over).
two_over(SP0=[_,_,B,A|_],RP,IP,WP) ->
    next([B,A|SP0],RP,IP,WP).

?XT("2swap", two_swap).
two_swap([D,C,B,A|SP],RP,IP,WP) ->
    next([B,A,D,C|SP],RP,IP,WP).

?XT("'", tick).
%% lookup a word
tick(SP, RP, IP, WP) ->
    Name = word($\s),
    case find_word_(Name) of
	{_,Xt} -> 
	    next([Xt|SP], RP,IP, WP);
	false -> 
	    throw({?UNDEF, Name})
    end.

?XT("!", store).
store([Addr,X|SP],RP,IP,Code) ->
    store_at(Addr,X),
    next(SP,RP,IP,Code).

?XT("*", star).
star(SP,RP,IP,WP) ->
    case SP of
	[A,B|SP1] when is_integer(A), is_integer(B) ->
	    next([A*B|SP1],RP,IP,WP)
    end.

?XT("*/", star_slash).
star_slash(SP,RP,IP,WP) ->
    case SP of
	[0|_] -> throw({?ARITH, "division by zero"});
	[C,B,A|SP1] when is_integer(A), is_integer(B) ->
	    next([(A*B) div C|SP1],RP,IP,WP)
    end.

?XT("*/", star_slash_mod).
star_slash_mod(SP,RP,IP,WP) ->
    case SP of
	[0|_] -> throw({?ARITH, "division by zero"});
	[C,B,A|SP1] when is_integer(A), is_integer(B) ->
	    T = A*B,
	    next([T rem C, T div C|SP1],RP,IP,WP)
    end.

%% print string
%% ." String"
%% compile: compile string literal that is printed at runtime
?XT(".\"", dot_quote).
dot_quote(SP,RP,IP,WP) ->
    compile_only(),
    String = word($"),
    comma_(fun ?MODULE:lit/0),
    comma_(String),
    comma_(fun ?MODULE:count/0),
    comma_(fun ?MODULE:type/0),
    next(SP,RP,IP,WP).

?XT("/", slash).
slash(SP,RP,IP,WP) ->
    case SP of
	[0|_] -> throw({?ARITH, "division by zero"});
	[B,A|SP]-> next([B div A|SP],RP,IP,WP)
    end.

?XT("/mod", slash_mod).
slash_mod(SP,RP,IP,WP) ->    
    case SP of
	[0|_] -> throw({?ARITH, "division by zero"});
	[B,A|SP]-> next([A rem B,A div B|SP],RP,IP,WP)
    end.

%% colon definition
?IXT(":", colon).
colon(SP, RP, IP, WP) ->
    interpreting(),
    cf_reset(),
    set_state(?COMPILE),
    Name = word($\s),
    here({0,Name,fun ffe:docol/4}),
    next(SP, RP, IP, WP).

?IXT(";", semicolon).
semicolon(SP,RP,IP,WP) ->
    compile_only(),
    case get_csp() of
	[] ->
	    Def = comma_(fun ?MODULE:semis/0),
	    Xt = fun() -> Def end,
	    here({}),  %% clear defintion area
	    NoName = (get_state() band ?NONAME) =:= ?NONAME,
	    set_state(0),
	    if NoName ->
		    next([Xt|SP],RP,IP,WP);
	       true ->
		    define(?nf(Def), Xt),
		    next(SP,RP,IP,WP)
	    end;
	_ ->
	    throw({-22, control_structure})
    end.

?XT("?dup", qdup).
qdup(SP0=[X|_],RP,IP,WP) ->
    if X=:=0 ->
	    next(SP0,RP,IP,WP);
       true ->
	    next([X|SP0],RP,IP,WP)
    end.

?XT("@", fetch).
fetch([Addr|SP],RP,IP,Code) ->
    X = fetch_at(Addr),
    next([X|SP],RP,IP,Code).

?IXT("]", right_bracket).
right_bracket(SP,RP,IP,WP) ->
    enter_compile(),
    next(SP,RP,IP,WP).

?IXT("[", left_bracket).
left_bracket(SP,RP,IP,WP) ->
    leave_compile(),
    next(SP,RP,IP,WP).

%% [char]
?IXT("[char]", bracket_care).
bracket_care(SP,RP,IP,WP) ->
    compile_only(),
    Char = char(),
    comma_(fun ?MODULE:lit/0),
    comma_(Char),
    next(SP,RP,IP,WP).

%% [CODE']

?IXT("[']", bracket_tick).
%% compile only 
bracket_tick(SP,RP,IP,WP) ->
    compile_only(),
    Name = word($\s),
    case find_word_(Name) of
	{_,Xt} -> 
	    comma_(Xt),
	    next(SP, RP, IP, WP);
	false -> 
	    throw({?UNDEF, Name})
    end.

?XT("<", less).
less([B,A|SP],RP,IP,WP) ->
    next([?BOOL(A<B)|SP],RP,IP,WP).

?XT("<#", bracket_number).
bracket_number(SP,RP,IP,WP) ->
    hold_begin(),
    next(SP,RP,IP,WP).

%% Note that we do not use double precision number on stack
%% since we have bignums but we may have to simulate it somehow?
?XT("#", number_sign).
number_sign([N|SP],RP,IP,WP) ->
    N1 = hold_digits(1,N),
    next([N1|SP],RP,IP,WP).

?XT("hold", hold).
hold([Char|SP],RP,IP,WP) ->
    hold_char(Char),
    next(SP,RP,IP,WP).

?XT("#s", number_sign_s).
number_sign_s([N|SP],RP,IP,WP) ->
    N1 = if N =:= 0 ->
		 hold_char($0),
		 0;
	    true ->
		 hold_digits(-1,N)
	 end,
    next([N1|SP],RP,IP,WP).

?XT("#>", number_bracket).
number_bracket(SP,RP,IP,WP) ->
    Addr = hold_end(),
    next([byte_size(Addr),Addr|SP],RP,IP,WP).

?XT("=", equal).
equal([B,A|SP],RP,IP,WP) ->
    next([?BOOL(A=:=B)|SP],RP,IP,WP).

?XT("<", greater).
greater([B,A|SP],RP,IP,WP) ->
    next([?BOOL(A>B)|SP],RP,IP,WP).

?XT(">body", to_body).
to_body([Xt|SP],RP,IP,WP) ->
    W = Xt(),
    next([?WPTR(?PFA,W)|SP],RP,IP,WP).

?XT(">r", tor).
tor([E|SP],RP,IP,WP) ->
    next(SP,[E|RP],IP,WP).

?XT("r>", rfrom).
rfrom(SP,[E|RP],IP,WP) ->
    next([E|SP],RP,IP,WP).

?XT("rot", rot).
rot(SP,RP,IP,WP) ->
    [A,B,C|SP1] = SP,
    next([C,A,B|SP1],RP,IP,WP).

?XT("mod", mod).
mod(SP,RP,IP,WP) ->
    case SP of
	[0|_] -> throw({?ARITH, "division by zero"});
	[B,A|SP]-> next([A rem B|SP],RP,IP,WP)
    end.

?XT("negate", negate).
negate(SP,RP,IP,WP) ->
    [A|SP1] = SP,
    next([-A|SP1],RP,IP,WP).
    
?XT("over", over).
over(SP,RP,IP,WP) ->
    [_,B|_] = SP,
    next([B|SP],RP,IP,WP).

?XT("quit", quit).
quit(_SP,_RP,_IP,_WP) ->
    throw({?QUIT, quit}).

?XT("drop", drop).
drop(SP,RP,IP,WP) ->
    [_|SP1] = SP,
    next(SP1,RP,IP,WP).

?XT("swap", swap).
swap(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A,B|SP1],RP,IP,WP).

?XT("dup", dup).
dup(SP,RP,IP,WP) ->
    [A|_] = SP,
    next([A|SP],RP,IP,WP).

%% emit character
?XT("emit", emit).
emit([Char|SP],RP,IP,WP) ->
    if is_integer(Char), Char >= 0, Char =< 255 ->
	    emit_char(Char),
	    next(SP,RP,IP,WP);
       true ->
	    throw({?ARITH, "character range"})
    end.

?XT("find", find).
find([Name|SP],RP,IP,WP) ->
    case find_word_(Name) of
	false ->
	    next([?FALSE|SP],RP,IP,WP);
	{true,Xt} ->
	    next([1,Xt|SP],RP,IP,WP);
	{false,Xt} ->
	    next([-1,Xt|SP],RP,IP,WP)
    end.

?XT("lshift", lshift).
lshift(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A bsl B|SP1],RP,IP,WP).

?XT("rshift", rshift).
rshift(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A bsr B|SP1],RP,IP,WP).    


arshift() ->
    { 0, <<"arshift">>, fun ?MODULE:arshift/4 }.
arshift(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A bsr B|SP1],RP,IP,WP).

?XT(bl).
bl(SP,RP,IP,WP) -> next([$\s|SP],RP,IP,WP).

?XT(count).
count(SP=[Addr|_],RP,IP,WP) ->
    if is_binary(Addr) ->
	    next([byte_size(Addr)|SP],RP,IP,WP);
       true ->
	    next([0|SP],RP,IP,WP)
    end.

?XT(cr).
cr(SP,RP,IP,WP) ->
    emit_chars([$\r,$\n]),
    next(SP,RP,IP,WP).

?XT("abs", abs).
abs(SP,RP,IP,WP) ->
    [A|SP1] = SP,
    next([abs(A)|SP1],RP,IP,WP).

?XT("and", 'and').
'and'(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A band B|SP1],RP,IP,WP).

?XT("or", 'or').
'or'(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A bor B|SP1],RP,IP,WP).    

?XT("invert", invert).
invert(SP,RP,IP,WP) ->
    [A|SP1] = SP,
    next([bnot A|SP1],RP,IP,WP).

?XT("xor", 'xor').
'xor'(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    next([A bxor B|SP1],RP,IP,WP).    

?XT(min).
min(SP,RP,IP,WP) ->
    [A,B|SP1] = SP,
    next([erlang:min(A,B)|SP1],RP,IP,WP).

?XT(max).
max(SP,RP,IP,WP) ->
    [A,B|SP1] = SP,
    next([erlang:max(A,B)|SP1],RP,IP,WP).

?XT("0=", zero_equals).
zero_equals(SP,RP,IP,WP) ->
    [A|SP1] = SP,
    next([?BOOL(A=:=0)|SP1],RP,IP,WP).

?XT("0<", zero_less).
zero_less(SP,RP,IP,WP) ->
    [A|SP1] = SP,
    next([?BOOL(A<0)|SP1],RP,IP,WP).

?XT("space", space).
space(SP,RP,IP,WP) ->
    emit_char($\s),
    next(SP,RP,IP,WP).

?XT("spaces", spaces).
spaces([U|SP],RP,IP,WP) ->
    emit_chars(lists:duplicate(U,$\s)),
    next(SP,RP,IP,WP).

?XT("type", type).
type([U,Addr|SP],RP,IP,WP) ->
    if is_binary(Addr) ->
	    emit_chars(altout(), U, Addr),
	    next(SP,RP,IP,WP);
       true ->
	    next(SP,RP,IP,WP)
    end.

?XT("word", word).
word([Char|SP],RP,IP,WP) ->
    Word = word(Char),
    next([Word|SP],RP,IP,WP).

?XT("sp@", spat).
spat(SP,RP,IP,WP) ->
    next([SP|SP],RP,IP,WP).

?XT("rp@", rpat).
rpat(SP,RP,IP,WP) ->
    next([RP|SP],RP,IP,WP).

?XT("sp!", spstore).
spstore(SP,RP,IP,WP) ->
    [SP1|_] = SP,
    next(SP1,RP,IP,WP).

?XT("rp!", rpstore).
rpstore(SP,_RP,IP,WP) ->
    [RP1|SP1] = SP,
    next(SP1,RP1,IP,WP).

?XT("i", i).
i(SP,[Ix|_]=RP,IP,Code) ->
    next([Ix|SP],RP,IP,Code).

?XT("j", j).
j(SP,[_,_,Jx|_]=RP,IP,Code) ->
    next([Jx|SP],RP,IP,Code).

-endif.
