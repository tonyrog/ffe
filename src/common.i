%% -*- erlang -*-
%%   FFE common words
%%
-ifndef(__COMMON__).
-define(__COMMON__, true).

-include("ffe.hrl").

common_words() ->
    #{
      ?WORD("2-",          two_minus),
      ?WORD("2+",          two_plus),
      ?WORD(",\"",         comma_string), %% compile string
      ?WORD("[defined]",   defined),
      ?WORD("[undefined]", defined)
     }.

?XT("2+", two_plus).
two_plus(SP,RP,IP,WP) ->
    ?two_plus(SP,RP,IP,WP,next).

?XT("2-", two_minus).
two_minus(SP,RP,IP,WP) ->
    ?two_minus(SP,RP,IP,WP,next).

?IXT("[defined]", defined).
defined(SP,RP,IP,WP) ->
    Name = word($\s),
    case find_word_(Name) of
	{_,_Xt} -> 
	    next([?TRUE|SP], RP, IP, WP);
	false -> 
	    next([?FALSE|SP], RP, IP, WP)
    end.

?IXT("[undefined]", undefined).
undefined(SP,RP,IP,WP) ->
    Name = word($\s),
    case find_word_(Name) of
	{_,_Xt} -> 
	    next([?FALSE|SP], RP, IP, WP);
	false -> 
	    next([?TRUE|SP], RP, IP, WP)
    end.

-endif.
