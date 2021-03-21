%% -*- erlang -*-
%%   FFE core extension words
%%
-ifndef(__CORE_EXT__).
-define(__CORE_EXT__, true).

-include("ffe.hrl").

core_ext_words() ->
    #{
      ?WORD(":noname",     colon_noname),
      ?WORD("?do",         qdo),
      ?WORD("2>r",         two_tor),
      ?WORD("2r@",         two_r_fetch),
      ?WORD("2r>",         two_rfrom),
      ?WORD("parse",       parse)
     }.

?XT("2>r", two_tor).
two_tor([B,A|SP],RP,IP,WP) ->
    next(SP,[B,A|RP],IP,WP).

?XT("2r>", two_rfrom).
two_rfrom(SP,[B,A|RP],IP,WP) ->
    next([B,A|SP],RP,IP,WP).

?XT("2r@", two_rfetch).
two_rfetch(SP,RP0=[B,A|_],IP,WP) ->
    next([B,A|SP],RP0,IP,WP).
    
?IXT(":noname", colon_noname).
colon_noname(SP,RP,IP,WP) ->
    interpreting(),
    cf_reset(),
    set_state(?COMPILE),
    here({0,"",fun ffe:docol/4}),
    next(SP, RP, IP, WP).

?XT("parse", parse).
parse([Char|SP],RP,IP,WP) ->
    Word = parse(Char),
    next([byte_size(Word),Word|SP],RP,IP,WP).
    

-endif.
