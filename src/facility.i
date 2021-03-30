%% -*- erlang -*-
%%   FFE facility words
%%
-ifndef(__FACILITY__).
-define(__FACILITY__, true).

-include("ffe.hrl").

facility_words() ->
    #{
      ?WORD("key?",        key_question)
     }.

?XT("key?", key_question).
key_question(SP,RP,IP,Code) ->
    Bool = ffe_tio:input_ready(ffe:source_id()),
    next([?BOOL(Bool)|SP],RP,IP,Code).

-endif.
