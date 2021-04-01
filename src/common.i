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
      ?WORD("[undefined]", undefined),
      ?WORD("include",     include),
      %% not so common
      ?WORD("-rot",        minus_rot)
     }.

?XT("2+", two_plus).
two_plus(SP,RP,IP,WP) ->
    case SP of
	[A|SP1] when is_integer(A) ->
	    next([A+2|SP1],RP,IP,WP);
	[?WPTR(I,W)|SP1] ->
	    next([?WPTR(I+2,W)|SP1],RP,IP,WP)
    end.

?XT("2-", two_minus).
two_minus(SP,RP,IP,WP) ->
    case SP of
	[A|SP1] when is_integer(A) ->
	    next([A-2|SP1],RP,IP,WP);
	[?WPTR(I,W)|SP1] ->
	    next([?WPTR(I-2,W)|SP1],RP,IP,WP)
    end.

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

?XT("include", include).
include(SP,RP,IP,WP) ->
    Filename = word($\s),
    case file_open(Filename, [read]) of
	[0,FileID] ->
	    SP1 = file_include(FileID,SP),
	    next(SP1,RP,IP,WP);
	[_IOR,_] ->
	    throw__(SP,RP,IP,WP,{?ERR_FILENOENT,Filename})
    end.
    
%% not so common?
?XT("-rot", minus_rot).
minus_rot(SP,RP,IP,WP) ->
    [A,B,C|SP1] = SP,
    next([B,C,A|SP1],RP,IP,WP).

-endif.
