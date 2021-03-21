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
       ?WORD("#",          number_sign),
       ?WORD("#>",         number_bracket),
       ?WORD("#s",         number_sign),       
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
       ?WORD("<#",         less_number),
       ?WORD("=",          equal),
       ?WORD(">",          greater),
       ?WORD(">body",      to_body),
       ?WORD(">r",         tor),
       ?WORD("r>",         rfrom),
       ?WORD("abs",        abs),
       ?WORD("and",        'and'),
       ?WORD("arshift",    arshift),
       ?WORD("count",      count),
       ?WORD("cr",         cr),
       ?WORD("drop",       drop),
       ?WORD("dup",        dup),
       ?WORD("emit",       emit),
       ?WORD("find",       find),
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
       ?WORD("xor",        'xor')
     }.


?XT("+", plus).
plus(SP,RP,IP,WP) -> ?plus(SP,RP,IP,WP,next).

?XT("-", minus).
minus(SP,RP,IP,WP) -> ?minus(SP,RP,IP,WP,next).

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
    ?one_minus(SP,RP,IP,WP,next).

?XT("1+", one_plus).
oe_plus(SP,RP,IP,WP) ->
    ?one_plus(SP,RP,IP,WP,next).

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
    case tick_(Name) of
	{_,Xt} -> 
	    next([Xt|SP], RP,IP, WP);
	false -> 
	    throw({?UNDEF, Name})
    end.

?XT("!", store).
store([Addr,X|SP],RP,IP,Code) ->
    store_at(Addr,X),
    next(SP,RP,IP,Code).

?XT("#", number_sign).
number_sign(SP,RP,IP,WP) ->
    ?FIXME(),
    ?next(SP,RP,IP,WP).

?XT("#>", number_bracket).
number_bracket(SP,RP,IP,WP) ->
    ?FIXME(),
    ?next(SP,RP,IP,WP).

?XT("*", star).
star(SP,RP,IP,WP) ->   ?star(SP,RP,IP,WP,next).

?XT("*/", star_slash).
star_slash(SP,RP,IP,WP) -> ?star_slash(SP,RP,IP,WP,next).

?XT("*/", star_slash_mod).
star_slash_mod(SP,RP,IP,WP) -> ?star_slash_mod(SP,RP,IP,WP,next).

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
    ?slash(SP,RP,IP,WP,next).

?XT("/mod", slash_mod).
slash_mod(SP,RP,IP,WP) ->
    ?slash_mod(SP,RP,IP,WP,next).

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
    case csp() of
	[] ->
	    Def = comma_(fun ?MODULE:semis/0),
	    Xt = fun() -> Def end,
	    here({}),  %% clear defintion area
	    set_state(0),
	    case ?nf(Def) of
		"" -> 
		    next([Xt|SP],RP,IP,WP);
		Name ->
		    define(Name, Xt),
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
?IXT("[char]", bracket_char).
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
    case tick_(Name) of
	{_,Xt} -> 
	    comma_(Xt),
	    next(SP, RP, IP, WP);
	false -> 
	    throw({?UNDEF, Name})
    end.

?XT("<", less).
less([B,A|SP],RP,IP,WP) ->
    next([?BOOL(A<B)|SP],RP,IP,WP).

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
    ?rot(SP,RP,IP,WP,next).

?XT("mod", mod).
mod(SP,RP,IP,WP) ->
    ?mod(SP,RP,IP,WP,next).

?XT("negate", negate).
negate(SP,RP,IP,WP) ->
    ?negate(SP,RP,IP,WP,next).

?XT("over", over).
over(SP,RP,IP,WP) ->
    ?over(SP,RP,IP,WP,next).

?XT("quit", quit).
quit(_SP,_RP,_IP,_WP) ->
    throw({?QUIT, quit}).

?XT("drop", drop).
drop(SP,RP,IP,WP) ->
    ?drop(SP,RP,IP,WP,next).

?XT("swap", swap).
swap(SP,RP,IP,WP) ->
    ?swap(SP,RP,IP,WP,next).

?XT("dup", dup).
dup(SP,RP,IP,WP) ->
    ?dup(SP,RP,IP,WP,next).

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
    case tick_(Name) of
	false ->
	    next([?FALSE|SP],RP,IP,WP);
	{true,Xt} ->
	    next([1,Xt|SP],RP,IP,WP);
	{false,Xt} ->
	    next([-1,Xt|SP],RP,IP,WP)
    end.

?XT("lshift", lshift).
lshift(SP,RP,IP,WP) ->
    ?lshift(SP,RP,IP,WP,next).

?XT("rshift", rshift).
rshift(SP,RP,IP,WP) ->
    ?rshift(SP,RP,IP,WP,next).

arshift() ->
    { 0, <<"arshift">>, fun ?MODULE:arshift/4 }.
arshift(SP,RP,IP,WP) ->
    ?arshift(SP,RP,IP,WP,next).

?XT("abs", abs).
abs(SP,RP,IP,WP) ->
    ?abs(SP,RP,IP,WP,next).

?XT("and", 'and').
'and'(SP,RP,IP,WP) ->
    ?'and'(SP,RP,IP,WP,next).

?XT("or", 'or').
'or'(SP,RP,IP,WP) ->
    ?'or'(SP,RP,IP,WP,next).

?XT("invert", invert).
invert(SP,RP,IP,WP) ->
    ?invert(SP,RP,IP,WP,next).

?XT("xor", 'xor').
'xor'(SP,RP,IP,WP) ->
    ?'xor'(SP,RP,IP,WP,next).

?XT(min).
min(SP,RP,IP,WP) ->
    ?min(SP,RP,IP,WP,next).

?XT(max).
max(SP,RP,IP,WP) ->
    ?max(SP,RP,IP,WP,next).

?XT("0=", zero_equals).
zero_equals(SP,RP,IP,WP) ->
    ?zero_equals(SP,RP,IP,WP,next).

?XT("0<", zero_less).
zero_less(SP,RP,IP,WP) ->
    ?zero_less(SP,RP,IP,WP,next).

?XT("word", word).
word([Char|SP],RP,IP,WP) ->
    Word = word(Char),
    next([Word|SP],RP,IP,WP).

-endif.
