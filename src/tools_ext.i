%% -*- erlang -*-
%%   FFE tools ext words
%%
-ifndef(__TOOLS_EXT__).
-define(__TOOLS_EXT__, true).

tools_ext_words() ->
    #{
      ?WORD("[if]",       bracket_if),
      ?WORD("[then]",     bracket_then),
      ?WORD("[else]",     bracket_else),
      ?WORD("bye",        bye)

     }.

?XT("bye", bye).
bye(_SP,_RP,_IP,_WP) ->
    throw({?BYE, exit}).

?IXT("[if]", bracket_if).
bracket_if([Flag|SP],RP,IP,WP) ->
    if Flag =/= 0 ->
	    next(SP,RP,IP,WP);
       true ->
	    bracket_skip(),  %% until either else/then
	    next(SP,RP,IP,WP)
    end.

?IXT("[else]", bracket_else).
bracket_else(SP,RP,IP,WP) ->
    bracket_skip(),  %% until then
    next(SP,RP,IP,WP).

?IXT("[then]", bracket_then).
bracket_then(SP,RP,IP,WP) ->
    next(SP,RP,IP,WP).

bracket_skip() ->
    IF = fun ?MODULE:bracket_if/0,
    THEN = fun ?MODULE:bracket_then/0,
    ELSE = fun ?MODULE:bracket_else/0,
    bracket_skip_(IF, THEN, ELSE, []).

bracket_skip_(IF, THEN, ELSE, Stack) ->
    case word(?SPACE) of
	eof -> eof;
	Name ->
	    case find_word_(Name) of
		{true,IF} ->
		    bracket_skip_(IF, THEN, ELSE, [THEN|Stack]);
		{true,THEN} when Stack =:= [] ->  ok;
		{true,ELSE} when Stack =:= [] ->  ok;
		{true,THEN} when hd(Stack) =:= THEN ->
		    bracket_skip_(IF, THEN, ELSE, tl(Stack));
		_ ->
		    bracket_skip_(IF, THEN, ELSE, Stack)
	    end
    end.

-endif.
