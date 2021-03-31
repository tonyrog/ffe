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
    bracket_skip_(fun word/1, ?BL, IF, THEN, ELSE, []).

bracket_skip_(Word, Delimiter, IF, THEN, ELSE, Stack) ->
    case Word(Delimiter) of
	eof -> eof;
	Name ->
	    case find_word_(Name) of
		{true,IF} ->
		    bracket_skip_(fun word/1,?BL,IF,THEN,ELSE,[THEN|Stack]);
		{true,THEN} when Stack =:= [] ->  ok;
		{true,ELSE} when Stack =:= [] ->  ok;
		{true,THEN} when hd(Stack) =:= THEN ->
		    bracket_skip_(fun word/1,?BL,IF,THEN,ELSE,tl(Stack));
		{_, Xt} ->
		    case maps:get(Xt, skip_map(), false) of
			eol -> %% backslash seen skip to end of line
			    set_in(get_span()),
			    bracket_skip_(fun word/1,?BL,IF,THEN,ELSE,Stack);
			false ->  %% not escaped use BLank separator
			    bracket_skip_(fun word/1,?BL,IF,THEN,ELSE,Stack);
			{Parse,Delimiter1} ->
			    bracket_skip_(Parse,Delimiter1,IF,THEN,ELSE,Stack)
		    end;
		_ ->
		    bracket_skip_(fun word/1, ?BL,IF,THEN,ELSE,Stack)
	    end
    end.

%% ,"      comma_quote   skip until "
%% ."      dot_quote     skip until "
%% s"      s_quote       skip until "
%% c"      c_qoute       skip until "
%% abort"  abort_quote   skip until "
%% \\      backslash     skip until eol
%% (       paren         skip until )
%% 
skip_map() ->
    #{
       (fun ?MODULE:comma_quote/0) => {fun parse/1, ?Q},
       (fun ?MODULE:dot_quote/0) => {fun parse/1, ?Q},
       (fun ?MODULE:s_quote/0) => {fun parse/1, ?Q},
       (fun ?MODULE:c_quote/0) => {fun parse/1, ?Q},
       (fun ?MODULE:abort_quote/0) => {fun parse/1, ?Q},
       (fun ?MODULE:backslash/0) => eol,
       (fun ?MODULE:paren/0)     => {fun parse/1, $)}
    }.

-endif.
