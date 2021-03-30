%% -*- erlang -*-
%%   FFE tools ext words
%%
-ifndef(__TOOLS_EXT__).
-define(__TOOLS_EXT__, true).

tools_ext_words() ->
    #{
      ?WORD("bye",        bye)
     }.

?XT("bye", bye).
bye(_SP,_RP,_IP,_WP) ->
    throw({?BYE, exit}).

-endif.
